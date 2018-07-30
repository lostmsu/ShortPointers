module Lost.Pointers.StructPool

open System.Collections.Generic

open Lost.Into
open Lost.Pointers
open Lost.Pointers.Pools
open Lost.PhantomData

type Bits = System.Collections.BitArray


[<Struct>]
type StructPool<'P, 'T when 'T: struct and 'P:> IInto<int>> =
  private
    { data: 'T[];
      pointerFactory: int -> 'P;
      allocated: System.Collections.BitArray;
      phantom: PhantomData<'P>; }
  
  member this.Reset() =
    this.data.Initialize()
    this.allocated.SetAll(false)

  interface IPool<'P, 'T> with
    member this.GetReference(pointer) = &this.data.[into <| getAddress pointer]

let makePool<'P, 'T when 'T: struct and 'P:> IInto<int>> pointerFactory size =
  { data = Array.zeroCreate<'T> size;
    allocated = Bits(size);
    pointerFactory = pointerFactory;
    phantom = phantom<'P> }

let makePoolGeneric<'P, 'T
                     when 'P:> System.IConvertible
                      and 'P: struct
                      and 'T: struct> size =
  makePool<Ptr<'P>,'T>
    Ptr<'P>.op_Explicit
    size

let inline makePoolAuto< ^P, 'T 
                        when ^P : (static member op_Explicit: int -> ^P)
                         and 'P :> IInto<int>
                         and 'T : struct> size =
  makePool< ^P, 'T>
    (fun a -> (^P : (static member op_Explicit: int -> ^P) (a)))
    size

let New pool (value:'T) =
    Seq.cast pool.allocated
    |> Seq.tryFindIndex (not << id)
    |> Option.map (fun address ->
      pool.allocated.[address] <- true
      pool.data.[address] <- value
      let ptr = pool.pointerFactory address
      TypedPointer<'P,'T>(ptr)
    )

let NewArray pool values =
  if Array.length values = 0 then raise(System.ArgumentException())
  let tryFindEmptyRange() =
    let mutable count = 0
    Seq.cast pool.allocated
    |> Seq.tryFindIndex (fun allocated ->
      count <- if allocated then count + 1 else 0
      count = values.Length)
    |> Option.map (fun i -> i - values.Length)

  match tryFindEmptyRange() with
  | None -> None
  | Some(address) ->
  for i = 0 to values.Length - 1 do
    pool.allocated.[address + i] <- true
    pool.data.[address + i] <- values.[i]
  let ptr = pool.pointerFactory address
  Some(TypedPointer<'P,'T>(ptr))

let private collectGarbage
          (pool: StructPool<'P, 'T >)
          (roots: seq<ITypedPointer<'P, 'T>>) =
  let alive = Bits(pool.data.Length)
  let visited = Bits(pool.data.Length)
  let visitQueue = Queue<ITypedPointer<'P, 'T>>()
  for root in roots do
    visitQueue.Enqueue(root)
    alive.[into <| getAddress root] <- true
  let wasVisited address = into address < pool.data.Length
                            && visited.[into address]
  while visitQueue.Count > 0 do
    let aliveObject = visitQueue.Dequeue()
    if not(wasVisited <| getAddress aliveObject) then
      let obj: byref<'T> = &Ref pool aliveObject
      for reference in getReferences obj do
        visitQueue.Enqueue(reference)
  for i = 0 to pool.allocated.Length - 1 do
    pool.allocated.[i] <- alive.[i]
  
let GcNew (pool: StructPool<'P, 'T >)
          (roots: seq<ITypedPointer<'P, 'T>>)
          (value: 'T) =
  match New pool value with
  | Some p -> Some(p)
  | None ->
    collectGarbage pool roots
    New pool value

let GcNewArray pool roots values =
  match NewArray pool values with
  | Some p -> Some p
  | None ->
  collectGarbage pool roots
  NewArray pool values

let Release (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) =
    let index = into <| getAddress pointer
    if pool.allocated.[index] then
        pool.allocated.[index] <- false
    else
        invalidOp "double deallocation"


  
let poolTest() =
    let pool = makePoolAuto<Ptr<byte>, int> 10
    let allocated = Array.map (New pool) [|0..10|]
    assert(Array.last allocated = None)
    let first = Option.get(Array.head allocated)
    for i = 0 to 4 do
        let ptr = Option.get allocated.[i * 2]
        let ref1 = &Ref pool ptr
        let ref2 = &Ref pool ptr
        assert(ref1 = i * 2)
        ref1 <- i * 3
        assert(ref1 = ref2)
        Release pool ptr

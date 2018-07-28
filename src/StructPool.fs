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
  
  interface IPool<'P, 'T> with
    member this.GetReference(pointer) = &this.data.[into <| getAddress pointer]

let makePool<'P, 'T when 'T: struct and 'P:> IInto<int>> pointerFactory size =
  { data = Array.zeroCreate<'T> size;
    allocated = Bits(size);
    pointerFactory = pointerFactory;
    phantom = phantom<'P> }

let inline makePoolAuto< ^P, 'T 
                        when ^P : (static member op_Explicit: int -> ^P)
                         and 'P :> IInto<int>
                         and 'T : struct> size =
  makePool< ^P, 'T>
    (fun a -> (^P : (static member op_Explicit: int -> ^P) (a)))
    size

let New pool value =
    let mutable result = None
    for start = 0 to pool.allocated.Length - 1 do
        if Option.isNone result then ()
        elif not pool.allocated.[start]
        then
            pool.allocated.[start] <- true
            pool.data.[start] <- value
            let ptr = pool.pointerFactory start
            result <- Some(TypedPointer<'P, 'T >(ptr))
    result

let GcNew (pool: StructPool<'P, 'T >)
          (roots: seq<ITypedPointer<'P, 'T>>)
          (value: 'T) =
  let collectGarbage() =
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

  match New pool value with
  | Some p -> Some(p)
  | None ->
    collectGarbage()
    New pool value

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

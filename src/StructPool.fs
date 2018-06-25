module Lost.Pointers.StructPool

open System.Collections.Generic

open Lost.Into
open Lost.PhantomData

type Bits = System.Collections.BitArray


type StructPool<'P, 'T when 'T: struct> =
    { data: 'T[];
      allocated: System.Collections.BitArray;
      phantom: PhantomData<'P>; }

let inline makePool<'P, 'T when 'T: struct> size =
    { data = Array.zeroCreate<'T> size;
      allocated = Bits(size);
      phantom = phantom<'P> }

let inline Ref (pool: StructPool<'P,'T>)
               (pointer: ITypedPointer<'P, 'T>) = &pool.data.[into pointer.Address]

let inline New< ^P, 'T
            when ^P : (static member op_Explicit: int -> ^P)
             and ^P : equality // WTF?
             and 'T : struct > (pool: StructPool< ^P, 'T >) (value: 'T) =
    let mutable result = None
    for start = 0 to pool.allocated.Length - 1 do
        if result <> None then ()
        elif not pool.allocated.[start]
        then
            pool.allocated.[start] <- true
            pool.data.[start] <- value
            let ptr = (^P : (static member op_Explicit: int -> ^P) (start))
            result <- Some(TypedPointer< ^P, 'T >(ptr))
    result

let inline GcNew< ^P, 'T
            when ^P : (static member op_Explicit: int -> ^P)
             and ^P:> IInto<int>
             and ^P : equality // WTF?
             and 'T :> IManaged< ^P, 'T>
             and 'T: struct > (pool: StructPool< ^P, 'T >) (value: 'T)
                              (roots: seq<ITypedPointer< ^P, 'T>>) =
  let collectGarbage() =
    let alive = Bits(pool.data.Length)
    let visited = Bits(pool.data.Length)
    let visitQueue = Queue()
    for root in roots do
      visitQueue.Enqueue(root)
      alive.[into root.Address] <- true
    let wasVisited(address: ^P) = into address < pool.data.Length
                               && visited.[into address]
    while visitQueue.Count > 0 do
      let aliveObject = visitQueue.Dequeue()
      if not(wasVisited aliveObject.Address) then
        let obj = Ref pool aliveObject
        for reference in obj.GetReferences() do
          visitQueue.Enqueue(reference)
    for i = 0 to pool.allocated.Length - 1 do
      pool.allocated.[i] <- alive.[i]

  match New pool value with
  | Some p -> Some(p)
  | None ->
    collectGarbage()
    New pool value

let inline Release< ^P, 'T when 'T : struct and 'T : unmanaged 
                            and ^P: (static member op_Explicit: ^P -> int) >
           (pool: StructPool<'P,'T>) (pointer: TypedPointer<'P, 'T>) =
    if pool.allocated.[int pointer.Address] then
        pool.allocated.[int pointer.Address] <- false
    else
        invalidOp "double deallocation"


[<Struct>]
type BytePtr =
    | BytePtr of byte
    static member op_Explicit (value: int) = BytePtr(byte value)
    static member op_Explicit (value: BytePtr) =
        let (BytePtr value) = value
        int value
    
    interface IInto<int> with
      member this.Into = BytePtr.op_Explicit this

let poolTest() =
    let pool = makePool<BytePtr, int> 10
    let wordPool = makePool<uint16, int> 10
    let allocated = Array.map (New pool) [|0..10|]
    assert(Array.last allocated = None)
    let (Some first) = Array.head allocated
    for i = 0 to 4 do
        let (Some ptr) = allocated.[i * 2]
        let ref1 = Ref pool ptr
        let ref2 = Ref pool ptr
        assert(ref1 = i * 2)
        ref1 <- i * 3
        assert(ref1 = ref2)
        Release pool ptr

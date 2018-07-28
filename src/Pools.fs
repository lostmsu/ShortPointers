namespace Lost.Pointers.Pools

open Lost.Pointers

[<AutoOpen>]
module Pools =
  type IPool<'P, 'T when 'T: struct> =
    abstract member GetReference: ITypedPointer<'P, 'T> -> byref<'T>
  
  let Ref<'TPool, 'P, 'T when 'TPool :> IPool<'P, 'T>> (pool: 'TPool) pointer =
    &pool.GetReference pointer
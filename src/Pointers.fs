namespace Lost.Pointers

type IPointer<'P> =
    abstract member Address: 'P

type ITypedPointer<'P, 'T when 'T: struct> =
    inherit IPointer<'P>

type IManaged<'P, 'T when 'T: struct> =
  abstract member GetReferences: unit -> seq<ITypedPointer<'P, 'T>>

[<Struct>]
type TypedPointer<'P, 'T when 'T: struct>(ptr: 'P) =
    member this.Address = ptr

    interface ITypedPointer<'P, 'T> with
        member this.Address = this.Address
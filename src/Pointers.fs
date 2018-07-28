namespace Lost.Pointers

[<AutoOpen>]
module Pointers =
  type IPointer<'P> =
      abstract member Address: 'P

  type ITypedPointer<'P, 'T when 'T: struct> =
      inherit IPointer<'P>

  type IManaged<'P, 'T when 'T: struct> =
    abstract member GetReferences: unit -> seq<ITypedPointer<'P, 'T>>
  
  let getReferences<'T,'P when 'T :> IManaged<'P, 'T>> (managed: 'T) = managed.GetReferences()

  let getAddress(p: IPointer<_>) = p.Address

  [<Struct>]
  type TypedPointer<'P, 'T when 'T: struct>(ptr: 'P) =
      member this.Address = ptr

      interface ITypedPointer<'P, 'T> with
          member this.Address = this.Address
  
  let inline inlineNull< ^P, 'T 
      when 'T: struct
       and ^P: (static member Zero: ^P) > =
    TypedPointer< ^P,'T>(LanguagePrimitives.GenericZero)

  let inline isNull pointer = getAddress pointer = LanguagePrimitives.GenericZero
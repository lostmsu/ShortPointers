namespace Lost.Pointers

open System.Linq.Expressions

open Lost.Into

type private PtrFromInt<'T when 'T : struct and 'T :> System.IConvertible> private () =
  static let convertBack =
    let parameter = Expression.Parameter(typeof<int>, "value")
    let body = Expression.Convert(parameter, typeof<'T>)
    Expression.Lambda<System.Func<int, 'T>>(body, parameter).Compile()
  
  static member ToPtr = convertBack

[<Struct>]
type Ptr<'T when 'T : struct and 'T :> System.IConvertible> =
  private | Pointer of 'T

  static member op_Explicit (value: int) = Pointer(PtrFromInt.ToPtr.Invoke(value))
  static member op_Explicit (value: Ptr<'T>) = value.Untyped.ToInt32(null)
  
  static member Zero = Ptr<'T>.op_Explicit 0

  member private this.Untyped: 'T =
    let (Pointer value) = this
    value
  
  interface IInto<int> with
    member this.Into = Ptr<'T>.op_Explicit this
  
  interface System.IConvertible with
    member this.ToBoolean(provider) = this.Untyped.ToBoolean(provider)
    member this.ToByte(provider) = this.Untyped.ToByte(provider)
    member this.ToChar(provider) = this.Untyped.ToChar(provider)
    member this.ToDateTime(provider) = this.Untyped.ToDateTime(provider)
    member this.ToDecimal(provider) = this.Untyped.ToDecimal(provider)
    member this.ToDouble(provider) = this.Untyped.ToDouble(provider)
    member this.ToInt16(provider) = this.Untyped.ToInt16(provider)
    member this.ToInt32(provider) = this.Untyped.ToInt32(provider)
    member this.ToInt64(provider) = this.Untyped.ToInt64(provider)
    member this.ToSByte(provider) = this.Untyped.ToSByte(provider)
    member this.ToSingle(provider) = this.Untyped.ToSingle(provider)
    member this.ToString(provider) = this.Untyped.ToString(provider)
    member this.ToType(conversionType, provider) = this.Untyped.ToType(conversionType, provider)
    member this.ToUInt16(provider) = this.Untyped.ToUInt16(provider)
    member this.ToUInt32(provider) = this.Untyped.ToUInt32(provider)
    member this.ToUInt64(provider) = this.Untyped.ToUInt64(provider)
    member this.GetTypeCode() = this.Untyped.GetTypeCode()

[<AutoOpen>]
module GenericConversions =
  let genericNull<'P,'T when 'T: struct
                         and 'P: struct
                         and 'P:> System.IConvertible> = TypedPointer<'P,'T>(PtrFromInt.ToPtr.Invoke(0))
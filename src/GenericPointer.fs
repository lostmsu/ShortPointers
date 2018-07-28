namespace Lost.Pointers

open System.Linq.Expressions

open Lost.Into

type private PtrFromInt<'T when 'T : struct and 'T :> System.IConvertible> private () =
  static let convertBack =
    let parameter = Expression.Parameter(typeof<int>, "value")
    let body = Expression.Convert(parameter, typeof<'T>)
    Expression.Lambda<System.Func<int, 'T>>(body, parameter).Compile()
  
  static member ConvertBack = convertBack

[<Struct>]
type Ptr<'T when 'T : struct and 'T :> System.IConvertible> =
  private | Pointer of 'T

  static member op_Explicit (value: int) = Pointer(PtrFromInt.ConvertBack.Invoke(value))
  static member op_Explicit (value: Ptr<'T>) =
    let (Pointer value) = value
    value.ToInt32(null)
  
  interface IInto<int> with
    member this.Into = Ptr<'T>.op_Explicit this


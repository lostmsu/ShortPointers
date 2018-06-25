module Lost.Into

type IInto<'T> =
  abstract member Into: 'T

let into<'T> (v: IInto<'T>) = v.Into
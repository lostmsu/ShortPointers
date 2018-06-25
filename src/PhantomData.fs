module Lost.PhantomData

type PhantomData<'T> = private Phantom of unit

let phantom<'T> : PhantomData<'T> = Phantom()
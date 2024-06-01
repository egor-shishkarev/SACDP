namespace Lazy

open System.Threading

// Lock-free implementation of Lazy
type LockFreeLazy<'a> (supplier: unit -> 'a) =
    let mutable value: 'a option = None

    interface ILazy<'a> with
        override this.Get() =
            match value with
            | None ->
                let result = supplier()
                Interlocked.CompareExchange(&value, Some result, None) |> ignore
                value.Value
            | Some v -> v

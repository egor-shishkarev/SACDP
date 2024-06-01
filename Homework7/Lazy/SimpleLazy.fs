namespace Lazy

// Simple implementation of Lazy for single thread working
type SimpleLazy<'a> (supplier: unit -> 'a) =
    let mutable value: 'a option = None

    interface ILazy<'a> with
        override this.Get() =
            match value with
            | None ->
                let result = supplier()
                value <- Some result
                result
            | Some v -> v
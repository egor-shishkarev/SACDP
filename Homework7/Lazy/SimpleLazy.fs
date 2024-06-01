namespace Lazy

// Simple implementation of Lazy for single thread working
type SimpleLazy<'a> (supplier: unit -> 'a) =
    let mutable innerSupplier: (unit -> 'a) option = Some supplier
    let mutable value: 'a option = None

    interface ILazy<'a> with
        override this.Get() =
            match value with
            | None ->
                let result = innerSupplier.Value()
                value <- Some result
                innerSupplier <- None
                result
            | Some v -> v

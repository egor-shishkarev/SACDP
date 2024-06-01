namespace Lazy

// Implementation of Lazy for multiple thread working
type ThreadSafeLazy<'a> (supplier: unit -> 'a) =
    let mutable value: 'a option = None
    let lockObject = obj()
    
    interface ILazy<'a> with
        override this.Get() =
            match value with
            | None ->
                lock lockObject (fun () ->
                    match value with
                    | None ->
                        let result = supplier()
                        value <- Some result
                        result
                    | Some v -> v
                )
            | Some v -> v
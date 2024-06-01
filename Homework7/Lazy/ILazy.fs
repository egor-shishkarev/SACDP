namespace Lazy

// Interface for multiple Lazy implementation
type ILazy<'a> =
    abstract member Get: unit -> 'a

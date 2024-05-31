module PointFree

type equalFunctions =
    static member func x l = List.map (fun y -> y * x) l;
    static member func'1 x = List.map (fun y -> y * x);
    static member func'2 x = List.map ((*) x);
    static member func'3 = List.map << (*);

let inline heterogenousAdd2(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) = value1 + value2
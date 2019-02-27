﻿// Learn more about F# at http://fsharp.org

open System

type NumberExtensions =
    static member Increment (x:float) = x + 1.0    
    static member Increment (x:int) = x + 1
    //etc

let inline increment number =
    let inline call (_:^E) (number:^N) = ( (^E or ^N) : (static member Increment: ^N -> ^N) number)
    call (Unchecked.defaultof<NumberExtensions>) number //!

type Ext = Ext
    with
        static member Bar (ext : Ext, flt : float) = 1.0 + flt
        static member Bar (ext : Ext, flt : float32) = 1.0f + flt

let inline bar (x : ^a) (y: ^b)=
    (^b : (static member Bar : ^b * ^a -> ^a) (y, x))

let inline bar1 (x : ^a) =
    ((^b or ^a) : (static member Bar : ^b * ^a -> ^a) (Ext, x))

let inline bar2 (x : ^a) =
    ((^b or ^a) : (static member Bar : ^b * ^a -> ^a) (Ext, x))

type Vector(x: float, y : float) =
   member this.x = x
   member this.y = y
   static member (~-) (v : Vector) =
     Vector(-1.0 * v.x, -1.0 * v.y)
   static member (*) (v : Vector, a) =
     Vector(a * v.x, a * v.y)
   static member (*) (a, v: Vector) =
     Vector(a * v.x, a * v.y)
   static member (*) (a: Vector, v: Vector) =
     Vector(a.x * v.x, a.y * v.y)
   override this.ToString() =
     this.x.ToString() + " " + this.y.ToString()

let inline heterogenousAdd(value1 : ^T when ^T : (static member (+) : ^T * ^U -> ^T), value2 : ^U) = value1 + value2

let inline square
     (x: ^a when ^a: (static member (*): ^a -> ^a -> ^a)) = x*x // работает для int и т.д

let inline squareE
     x = (^a: (static member (*): ^a -> ^a -> ^a) (x,x)) // работает только для Vector


[<EntryPoint>]
let main argv =
    let v1 = Vector(1.0, 2.0)
    let ff = heterogenousAdd(5, 6)
    let r = bar1 7.0
    printfn "%A" r
    let d = squareE v1
    printfn "%A" d

    let m = square v1
    printfn "%A" m

    let x = square 5
    printfn "%A" x

    let v = squareE 5
    printfn "%A" d
    0

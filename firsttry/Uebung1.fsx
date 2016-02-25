#r @"exerciseTester.dll"

//#r @"C:\Users\Pascal\Documents\Visual Studio 2015\Projects\firsttry\firsttry\exerciseTester.dll"
//Aufgabe 2a
let rec exp3 n=
    if n<1 then 1
    else 3*exp3 (n-1)

let rec exp3_2 = pown 3 //angewendet auf basis 3

Test.test2a exp3

//Aufgabe 2b
let rec fak n=
    if n = 0 
    then 1 
    else n * fak (n - 1)

let rec fak2 x = List.fold (*) 1 [1..x]


Test.test2b fak

//Aufgabe 2d
let rec sum f ug og =
   if ug > og then 0
   else (f ug) + sum f (ug+1) og

let sum' f ug og = List.fold (fun x y -> x + f y) 0 [ug..og]
let sum'' (f: int -> int) ug og = List.sum(List.map f [ug..og])

let sum''' (f: int -> int) ug og = 
    [ug..og]  |> List.map f  //zuerst wird dieses hinten angefügt
              |> List.sum   //danach wird auf diesem das vorhergehende angewendet
//let (|>) f g = g f
3 |> fun x -> x*x
  |> fun x -> x+x
  |> (+) <| 8
//Test.test2d sum

//Aufgabe 2c
let rec SumSq m n=
   sum (fun x->x*x) m n

SumSq 5 2
Test.test2c SumSq



//Aufgabe 4




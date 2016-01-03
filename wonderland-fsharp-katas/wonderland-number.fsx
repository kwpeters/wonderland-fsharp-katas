// See the file wonderland-number.md for detailed information.

let haveSameDigits (n1:int,n2:int) =
    (string n1 |> Set.ofSeq) = (string n2 |> Set.ofSeq)

let multiplicands = [2..6]
let sixDigits = [100000..1000000/List.last multiplicands]

let isWonderlandNumber x =
    List.fold (fun acc n -> acc && haveSameDigits (x,n*x)) true multiplicands

let wonderlandNumber () = 
    let rec findNum i =
        if i>List.last sixDigits then -1    // Safety check
        else if isWonderlandNumber i then i
        else
            findNum (i + 1)

    findNum (List.head sixDigits)


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    let wonderNum = wonderlandNumber ()

    test <@ (string wonderNum).Length = 6 @>

    test <@ haveSameDigits (wonderNum, 2 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 3 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 4 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 5 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 6 * wonderNum) @>

// run the tests
tests ()

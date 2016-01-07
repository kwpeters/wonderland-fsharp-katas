// See the file magic-square.md for detailed information.

let values = [| 1.0 .. 0.5 .. 5.0 |]

let dim = 3

type Square = float[,]
let maxIndex = dim - 1
let indexes = [ 0 .. maxIndex ]

let row (sq:Square) i = [ for col in indexes -> sq.[i,col] ]
let col (sq:Square) i = [ for row in indexes -> sq.[row,i] ]

let sumRow (sq:Square) row =
    [ for col in indexes -> sq.[row,col] ] |> List.sum

let sumColumn (sq:Square) col =
    [ for row in indexes -> sq.[row,col] ] |> List.sum

let sumDownDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i,i] ] |> List.sum

let sumUpDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i, maxIndex - i] ] |> List.sum

let testSumRows magic = 
    indexes |> List.map (sumRow magic) |> Set.ofList |> Set.count = 1

let testSumCols magic = 
    indexes |> List.map (sumColumn magic) |> Set.ofList |> Set.count = 1

let testSumDiagonals magic =
    sumDownDiagonal magic = sumUpDiagonal magic

let getPermutations array =
    let rec permutations list taken = 
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations list (Set.add l taken)  do
                  yield l::perm }
    permutations (Array.toList array) Set.empty


let isMagicSquare square =
    (testSumRows square) && (testSumCols square) && (testSumDiagonals square)
   
let magicSquare () =
    getPermutations values
    |> Seq.map (fun v -> 
        Array2D.init dim dim (fun row col -> 
            v.[row * dim + col]))
    |> Seq.filter isMagicSquare
    |> Seq.head



#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    let magic = magicSquare ()

    // all the rows sum to the same number
    test <@ indexes |> List.map (sumRow magic) |> Set.ofList |> Set.count = 1 @>

    // all the columns sum to the same number
    test <@ indexes |> List.map (sumColumn magic) |> Set.ofList |> Set.count = 1 @>

    // all the diagonals sum to the same number
    test <@ sumDownDiagonal magic = sumUpDiagonal magic @>

// run the tests
tests ()

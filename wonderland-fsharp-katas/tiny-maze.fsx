// See the file tiny-maze.md for detailed information.

type Cell =
    | Start
    | Exit
    | Empty
    | Wall

type Maze = Cell [,]

type Path =
    | X
    | O

type Solution = Path [,]

type ValidCell =
    | ValidCell of int * int
    | NotExists

let solve (maze:Maze) : Solution =
    let height = Array2D.length1 maze
    let width = Array2D.length2 maze
    let visited = Array2D.create height width false

    let isValidCell r c = 
        let height = Array2D.length1 maze
        let width = Array2D.length2 maze
        0 <= r && r < height && 0 <= c && c < width && maze.[r,c] <> Wall

    let isExit r c = 
        isValidCell r c && maze.[r,c] = Exit

    let isValidNotVisitedCell r c (visited : bool[,]) maze = 
        isValidCell r c && (not visited.[r,c])

    let getNextValidNeighbour r c visited maze = 
        if isValidNotVisitedCell (r+1) c visited maze then ValidCell (r+1,c)
        else if isValidNotVisitedCell (r-1) c visited maze then ValidCell (r-1,c)
        else if isValidNotVisitedCell r (c+1) visited maze then ValidCell (r,c+1)
        else if isValidNotVisitedCell r (c-1) visited maze then ValidCell (r,c-1)
        else NotExists


    let rec innerSolver row col visited pathSolution : (int*int) list =
        let nextValidCell = getNextValidNeighbour row col visited maze
        match nextValidCell with
        | NotExists -> failwith "There is no solution"
        | ValidCell (nr,nc) ->  if isExit nr nc then (nr,nc):: pathSolution
                                else
                                    visited.[nr,nc] <- true
                                    innerSolver nr nc visited ((nr,nc):: pathSolution)

    let path = innerSolver 0 0 visited [(0,0)]
    Array2D.mapi (fun r c e -> if List.contains (r,c) path then X else O) maze
     


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // sample 3x3 maze
    let maze3x3 =
        [ [Start; Empty; Wall]
          [Wall;  Empty; Wall]
          [Wall;  Empty; Exit]]
        |> array2D

    // sample 3x3 maze solution
    let solution3x3 =
        [ [X; X; O]
          [O; X; O]
          [O; X; X]]
        |> array2D

    test <@ solution3x3 = solve maze3x3 @>

    // sample 4x4 maze
    let maze4x4 =
         [[Start; Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Empty]
          [Wall;  Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Exit ]]
         |> array2D

    // sample 4x4 maze solution
    let solution4x4 =
        [[X; X; X; O]
         [O; O; X; O]
         [O; O; X; O]
         [O; O; X; X]]
        |> array2D

    test <@ solution4x4 = solve maze4x4 @>


// run the tests
tests ()

// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart


type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace


type Card = Suit * Rank


let ascendingSuits = [Spade; Club; Diamond; Heart];
let ascendingHeads = [Jack; Queen; King; Ace];
let ascendingRanks =
    [
        for v in 2 .. 10 -> Value v
        for head in ascendingHeads -> head
    ]


let ascendingDeck =
    seq {
        for rank in ascendingRanks do
            for suit in ascendingSuits -> suit, rank
    }


let cardToInt (theCard: Card): int =
    ascendingDeck |> Seq.findIndex (fun curCard -> curCard = theCard)


let playRound (card1:Card, card2:Card) =
    [card1; card2]
    |> List.maxBy(fun curCard -> cardToInt(curCard))


let pop a =
    (List.head(a), a.[1..])


type GameResult =
    {
        WinningPlayer: int
        NumIterations: int
    }

let playGame (hand1:Card list, hand2:Card list): GameResult =

    // Recursive function that implements playGame().
    let rec playGameImpl (hand1: Card list, hand2: Card list, iteration: int) =
        if hand1.IsEmpty then
            { WinningPlayer=2; NumIterations=iteration }
        elif hand2.IsEmpty then
            { WinningPlayer=1; NumIterations=iteration }
        else
            let (card1, remainingHand1) = pop hand1
            let (card2, remainingHand2) = pop hand2
            let winningCard = playRound(card1, card2)
            if (winningCard = card1) then
                playGameImpl (remainingHand1 @ [card1; card2], remainingHand2, iteration + 1)
            else
                playGameImpl (remainingHand1, remainingHand2 @ [card1; card2], iteration + 1)

    // Let the recursion begin.
    playGameImpl (hand1, hand2, 0)



#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote



let tests () =

    // playRound
    printfn "the highest rank wins the cards in the round"
    test <@ playRound ((Heart, Value 7), (Club, Jack)) = (Club, Jack) @>

    printfn "queens are higher rank than jacks"
    test <@ playRound ((Spade, Queen), (Spade, Jack)) = (Spade, Queen) @>

    printfn "kings are higher rank than queens"
    test <@ playRound ((Spade, Queen), (Spade, King)) = (Spade, King) @>

    printfn "aces are higher rank than kings"
    test <@ playRound ((Spade, Ace), (Spade, King)) = (Spade, Ace) @>

    printfn "if the ranks are equal, clubs beat spades"
    test <@ playRound ((Spade, Value 8), (Club, Value 8)) = (Club, Value 8) @>

    printfn "if the ranks are equal, diamonds beat clubs"
    test <@ playRound ((Diamond, Value 8), (Club, Value 8)) = (Diamond, Value 8) @>

    printfn "if the ranks are equal, hearts beat diamonds"
    test <@ playRound ((Diamond, Value 8), (Heart, Value 8)) = (Heart, Value 8) @>

    // playGame
    printfn "TODO: the player loses when they run out of cards"
    test <@ playGame ([(Heart, Value 9); (Club, Value 4)], [(Diamond, Queen); (Spade, Value 2)]) = {WinningPlayer=2; NumIterations=4} @>

// run the tests
tests ()

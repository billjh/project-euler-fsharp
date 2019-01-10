type Suit = Heart | Diamond | Club | Spade

type CardValue = int

[<Literal>]
let Jack = 11

[<Literal>]
let Queen = 12

[<Literal>]
let King = 13

[<Literal>]
let Ace = 14

type Card = CardValue * Suit

type Hand = Card * Card * Card * Card * Card

type HandTypeEnum = 
    | RoyalFlush    = 10
    | StraightFlush = 9
    | FourOfAKind   = 8
    | FullHouse     = 7
    | Flush         = 6
    | Straight      = 5
    | ThreeOfAKind  = 4
    | TwoPairs      = 3
    | OnePair       = 2
    | HighCard      = 1

type HandType = HandTypeEnum * CardValue []

let parseValue = function
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | 'T' -> 10
    | v when v >= '2' && v <= '9' -> (int v - int '0')
    | e -> invalidArg "Value" (string e)

let parseSuit = function
    | 'H' -> Heart
    | 'D' -> Diamond
    | 'C' -> Club
    | 'S' -> Spade
    | e -> invalidArg "Suit" (string e)

let parseCard (s:string) =
    match s.ToCharArray() with
    | [| v ; s |] -> parseValue v, parseSuit s
    | _ -> invalidArg "Card" s

let cardsToHand cards = 
    match cards with
    | [| c1 ; c2 ; c3 ; c4 ; c5 |] -> (c1, c2, c3, c4, c5) |> Hand
    | _ -> invalidArg "Cards" "length not 5"

let parseTwoHands (s:string) = 
    let cards = s.Split(' ') |> Array.map parseCard
    cards |> Array.take 5 |> cardsToHand,
    cards |> Array.skip 5 |> cardsToHand

let getCards (hand:Hand) =
    match hand with
    | c1, c2, c3, c4, c5 -> [| c1 ; c2 ; c3 ; c4 ; c5 |]

let allSame = function
    | [||] -> true
    | arr -> Array.forall ((=) arr.[0]) arr

let isSameSuit = getCards >> Array.map snd >> allSame

let getDupes =
    getCards
    >> Array.map fst
    >> Array.groupBy id
    >> Array.map (fun (c, cs) -> Array.length cs, c)
    >> Array.sortBy (fun (dup, cv) -> (-dup, -cv))

let getHandType (hand:Hand) =
    match getDupes hand with
    | [| (4, a) ; (1, _) |]                   -> HandTypeEnum.FourOfAKind,  [| a |]
    | [| (3, a) ; (2, _) |]                   -> HandTypeEnum.FullHouse,    [| a |]
    | [| (3, a) ; (1, _) ; (1, _) |]          -> HandTypeEnum.ThreeOfAKind, [| a |]
    | [| (2, a) ; (2, b) ; (1, c) |]          -> HandTypeEnum.TwoPairs,     [| a ; b ; c |]
    | [| (2, a) ; (1, b) ; (1, c) ; (1, d) |] -> HandTypeEnum.OnePair,      [| a ; b ; c ; d |]
    | cardGroups ->
        let cards = cardGroups |> Array.map snd
        let highestValue = Array.head cards
        let isStraight = (highestValue - Array.last cards = 4)
        let isFlush = isSameSuit hand
        match isStraight, isFlush with
        | true, true -> 
            match highestValue with
            | Ace                             -> HandTypeEnum.RoyalFlush,    [||]
            | _                               -> HandTypeEnum.StraightFlush, [| highestValue |]
        | true, false                         -> HandTypeEnum.Straight,      [| highestValue |]
        | false, true                         -> HandTypeEnum.Flush,         cards
        | false, false                        -> HandTypeEnum.HighCard,      cards

System.IO.File.ReadAllLines "./p054_poker.txt"
    |> Array.map parseTwoHands
    |> Array.map (fun (h1, h2) -> (getHandType h1, getHandType h2))
    |> Array.filter (fun (h1, h2) -> h1 > h2)
    |> Array.length
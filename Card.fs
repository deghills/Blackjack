module Card
    open System

    type Card = 
        |Two 
        |Three 
        |Four 
        |Five 
        |Six 
        |Seven 
        |Eight 
        |Nine
        |Ten
        |Jack 
        |Queen 
        |King
        |Ace

    let evaluate highAce = function
        |Two    -> 2
        |Three  -> 3
        |Four   -> 4
        |Five   -> 5
        |Six    -> 6
        |Seven  -> 7
        |Eight  -> 8
        |Nine   -> 9
        |Ten |Jack |Queen |King -> 10
        |Ace -> if highAce then 11 else 1


    let evaluateHandLow     = List.map (evaluate false) >> List.sum
    let evaluateHandHigh    = List.map (evaluate true) >> List.sum

    let checkBustLow    = evaluateHandLow >> fun x -> x > 21
    let checkBustHigh   = evaluateHandHigh >> fun x -> x > 21

    let toString = 
        function
        |Two    -> "Two "
        |Three  -> "Three "
        |Four   -> "Four "
        |Five   -> "Five "
        |Six    -> "Six "
        |Seven  -> "Seven "
        |Eight  -> "Eight "
        |Nine   -> "Nine "
        |Ten    -> "Ten "
        |Jack   -> "Jack "
        |Queen  -> "Queen "
        |King   -> "King "
        |Ace    -> "Ace "

    let toStringHand cards = cards |> List.map toString |> List.reduce(+)

    let print = toString >> Console.Write
    let println = toString >> Console.WriteLine

    let printHand = List.map print >> ignore

    let private quadruple l = 
        let rec loop acc remaining =
            match remaining with
            | [] -> acc
            | head :: tail -> loop (head :: head :: head :: head :: acc) tail
        loop [] l

    let orderedShoe = 
        [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]
        |> quadruple |> quadruple

    let randomShoePermutation() = 
        let rng = new Random()
        orderedShoe |> List.sortBy (fun x -> rng.Next(51))

    let drawCard shoe hand =
        match shoe with
        |topCard :: remaining ->
            remaining, topCard :: hand

        |[] ->
            do Console.WriteLine()
            do Console.WriteLine("NEW SHOE")
            do Console.WriteLine()
            let newShoe = randomShoePermutation()
            newShoe.Tail, newShoe.Head :: hand

    let drawCards shoe hand nOfCards =
        let rec loop remaining acc depth = 
            match depth with
            |n when (n = 0) ->
                remaining, acc

            |n ->
                let newShoe, newHand = drawCard remaining acc
                loop newShoe newHand (n-1)
        loop shoe hand nOfCards
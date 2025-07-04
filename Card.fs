﻿module Card
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

    let evaluate hand =
        let rec evaluateAces acc aces = 
            let maxEvaluation = acc + aces * 11

            if aces <= 0 
                then acc
            elif maxEvaluation <= 21 
                then maxEvaluation
            else 
                evaluateAces (acc+1) (aces-1)

        let rec loop acc aces = function 
            |[] ->
                evaluateAces acc aces

            |Ace :: cards ->
                loop acc (aces+1) cards

            |Two :: cards ->
                loop (acc+2) aces cards

            |Three :: cards ->
                loop (acc+3) aces cards

            |Four :: cards ->
                loop (acc+4) aces cards

            |Five :: cards ->
                loop (acc+5) aces cards

            |Six :: cards ->
                loop (acc+6) aces cards

            |Seven :: cards ->
                loop (acc+7) aces cards

            |Eight :: cards ->
                loop (acc+8) aces cards

            |Nine :: cards ->
                loop (acc+9) aces cards

            |Ten :: cards | Jack :: cards | Queen :: cards | King :: cards ->
                loop (acc+10) aces cards

        loop 0 0 hand

    let checkBust = evaluate >> (<) 21

    let toString = function
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

    let toStringHand = List.map toString >> List.reduce (+) >> (fun s -> s + "\b")

    let print = toString >> Console.Write
    let println = toString >> Console.WriteLine

    let printHand = List.iter print

    let orderedShoe =
        let quadruple l = 
            let rec loop acc remaining =
                match remaining with
                | [] -> acc
                | head :: tail -> loop (head :: head :: head :: head :: acc) tail
            loop [] l

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
            do 
                Console.WriteLine()
                Console.WriteLine("NEW SHOE")
                Console.WriteLine()
            let newShoe = randomShoePermutation()
            newShoe.Tail, newShoe.Head :: hand

    let drawCards shoe hand nOfCards =
        let rec loop depth remaining acc = 
            match depth with
            |n when (n <= 0) ->
                remaining, acc

            |n ->
                drawCard remaining acc
                ||> loop (n-1)
        loop shoe hand nOfCards
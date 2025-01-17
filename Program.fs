open System
open Card
open GameState

[<EntryPoint>]
let Game _ =

    let rec deal (shoe :Card list) =

        do Console.WriteLine "//////"
        do Console.WriteLine "////// !NEW ROUND!"
        do Console.WriteLine "//////"

        match shoe with 
        | card1 :: card2 :: card3 :: card4 :: cards ->

            let playerHand = [card1; card2]
            if evaluateHandHigh playerHand = 21
                then 
                    do Console.WriteLine $"Blackjack for the player! ({Card.printHand playerHand})"
                    do deal cards
                else

            {
                PlayerHand  = playerHand
                DealerHand  = [card3; card4]
                Shoe        = cards
            } |> playerTurn
        | _ -> failwith "deal unsuccessful"

    and playerTurn (state :GameState) =
        
        do Console.WriteLine()

        do Console.WriteLine $"Your hand:"
        do state.PlayerHand 
            |> List.map Card.print 
            |> ignore 
            |> Console.WriteLine

        if (checkBustLow state.PlayerHand)
            then 
                do Console.WriteLine $"The player busted!"
                do deal state.Shoe

        do Console.WriteLine $"Dealer's face-up card:"
        do state.DealerHand.[0] |> Card.print |> Console.WriteLine

        do Console.WriteLine "hit or stay?"

        let rec prompt() =
            match Console.ReadLine() with
            |"hit" 
                -> do playerTurn {                    
                    state with 
                        PlayerHand = state.Shoe.Head :: state.PlayerHand
                        Shoe = state.Shoe.Tail 
                }
                
            |"stay" 
                -> do dealerTurn state

            |_ 
                -> prompt()

        prompt()

    and dealerTurn (state :GameState) =
        
        do Console.WriteLine()
        do Console.WriteLine "Dealer's Hand"
        do state.DealerHand |> List.map Card.print |> ignore
        do Console.ReadLine() |> ignore

        let handValue = evaluateHandLow state.DealerHand
        if handValue > 21
            then 
                do Console.WriteLine $"The House busted!"
                do deal state.Shoe

        elif handValue > 16
            then do resolution state

        else dealerTurn { state with 
                                DealerHand  = state.Shoe.Head :: state.DealerHand
                                Shoe        = state.Shoe.Tail }

        match evaluateHandLow state.DealerHand with
        |handValue when handValue > 21
            ->  do Console.WriteLine $"The House busted!"
                do deal state.Shoe

        |handValue when handValue > 16
            ->  do resolution state

        |_  -> dealerTurn { state with 
                                DealerHand  = state.Shoe.Head :: state.DealerHand
                                Shoe        = state.Shoe.Tail }

    and resolution (state :GameState) =
        let playerScore = 
            if checkBustHigh state.PlayerHand then evaluateHandLow state.PlayerHand else evaluateHandHigh state.PlayerHand

        let dealerScore =
            if checkBustHigh state.DealerHand then evaluateHandLow state.DealerHand else evaluateHandHigh state.DealerHand

        match playerScore, dealerScore with
        |p, d when p > d
            -> do Console.WriteLine $"You win the hand! Player ({p}) vs House ({d})"

        |p, d when p < d
            -> do Console.WriteLine $"The house wins the hand! Player ({p}) vs House ({d})"

        |p, d when p = d
            -> do Console.WriteLine $"It's a tie! Player ({p}) vs House ({d})"

        |_  -> 
            do Console.WriteLine "Unexpected case in pattern match"
            ()

        deal state.Shoe

    let testShoe = Card.randomShoePermutation
    
    deal(testShoe)

    0
open System
open Card
open GameState

[<EntryPoint>]
let Game _ =

    let rec deal (shoe :Card list) =       

        do Console.WriteLine "//////"
        do Console.WriteLine "////// !NEW ROUND!"
        do Console.WriteLine "//////"

        let shoe', playerHand = Card.drawCards shoe [] 2
        let shoe'', dealerHand = Card.drawCards shoe' [] 2

        if evaluateHandHigh playerHand = 21
            then 
                do Console.WriteLine $"Blackjack for the player! ({Card.printHand playerHand})"
                do deal shoe''
            else

        playerTurn {
            PlayerHand  = playerHand
            DealerHand  = dealerHand
            Shoe        = shoe''
        }

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
            else

        do Console.WriteLine $"Dealer's face-up card:"
        do state.DealerHand.[0] |> Card.print |> Console.WriteLine
        do Console.WriteLine "hit or stay?"

        let rec prompt() =
            match Console.ReadLine() with
            |"hit" -> 
                let Shoe', PlayerHand' = Card.drawCard state.Shoe state.PlayerHand
                do playerTurn {                    
                    state with 
                        Shoe = Shoe'
                        PlayerHand = PlayerHand' }
                
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

        match evaluateHandLow state.DealerHand with
        |handValue when handValue > 21
            ->  do Console.WriteLine $"The House busted!"
                do deal state.Shoe

        |handValue when handValue > 16 ->
            do resolution state

        |_  -> 
            let Shoe', DealerHand' = Card.drawCard state.Shoe state.DealerHand
            dealerTurn { state with 
                                Shoe        = Shoe'
                                DealerHand  = DealerHand' }

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

        |_  
            -> failwith "Unexpected case in pattern match"

        deal state.Shoe
    
    deal(Card.randomShoePermutation())

    0
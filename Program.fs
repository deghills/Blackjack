open System
open Card
open GameState

[<EntryPoint>]
let Game _ =

    let startingBankroll = 200<Dollars>
    let threeToTwo = 15<Dollars>

    let rec deal (state :GameState) =

        do Console.WriteLine "//////"
        do Console.WriteLine "////// !NEW ROUND!"
        do Console.WriteLine "//////"
        do Console.WriteLine $"Your bankroll: {state.Bankroll}"

        do Console.WriteLine "What is your bet?"
        let rec prompt() = 
            match Console.ReadLine() |> Int32.TryParse with
            |true, result -> result * 1<Dollars>
            |false, _ -> prompt()

        let bet' = prompt()

        let shoe', playerHand = Card.drawCards state.Shoe [] 2
        let shoe'', dealerHand = Card.drawCards shoe' [] 2

        if evaluateHandHigh playerHand = 21
            then 
                do Console.WriteLine $"Blackjack for the player! ({Card.printHand playerHand})"
                do Console.WriteLine $"You win {threeToTwo}"
                do deal 
                    { state with 
                        Shoe = shoe''
                        Bankroll = state.Bankroll + threeToTwo
                    }
            else

        playerTurn {
            PlayerHand  = playerHand
            DealerHand  = dealerHand
            Shoe        = shoe''
            Bankroll    = state.Bankroll
            BetSize     = bet'
        }

    and playerTurn (state :GameState) =
        
        do Console.WriteLine()
        do Console.WriteLine $"Your hand:"
        do state.PlayerHand 
            |> List.map Card.print 
            |> ignore 
            |> Console.WriteLine
            |> Console.WriteLine

        if (checkBustLow state.PlayerHand)
            then 
                do Console.WriteLine $"The player busted!"
                do deal { state with Bankroll = state.Bankroll - state.BetSize }
            else ()

        do Console.WriteLine "Dealer's face-up card:"
        do state.DealerHand.Head |> Card.print |> Console.WriteLine |> Console.WriteLine
        do Console.WriteLine "hit/stay/double?"

        let rec prompt() =
            match Console.ReadLine() with
            |"hit" -> 
                let shoe', playerHand' = Card.drawCard state.Shoe state.PlayerHand
                do playerTurn {                    
                    state with 
                        Shoe = shoe'
                        PlayerHand = playerHand' }
                
            |"stay" -> 
                do dealerTurn state

            |"double" ->
                let shoe', playerHand' = Card.drawCard state.Shoe state.PlayerHand
                if(checkBustLow playerHand')
                    then 
                        do Console.Write $"The player busted! ( "
                        do Card.printHand playerHand'
                        do Console.WriteLine ")"
                        do deal { state with
                                    Shoe = shoe'
                                    PlayerHand = playerHand'
                                    Bankroll = state.Bankroll - (2*state.BetSize)}
                    else
                do Console.WriteLine $"Your hand: {Card.toStringHand playerHand'}"
                do dealerTurn { state with 
                                    Shoe = shoe'
                                    PlayerHand = playerHand'
                                    BetSize = 2*state.BetSize}

            |_ -> 
                prompt()

        prompt()

    and dealerTurn (state :GameState) =
        
        do Console.WriteLine()
        do Console.WriteLine "Dealer's Hand"
        do state.DealerHand |> List.map Card.print |> ignore
        do Console.ReadLine() |> ignore

        match evaluateHandLow state.DealerHand with
        |handValue when handValue > 21
            ->  do Console.WriteLine $"The House busted!"
                do deal { state with Bankroll = state.Bankroll + state.BetSize }

        |handValue when handValue > 16 ->
            do resolution state

        |_  -> 
            let Shoe', DealerHand' = Card.drawCard state.Shoe state.DealerHand
            dealerTurn 
                { state with 
                    Shoe        = Shoe'
                    DealerHand  = DealerHand' }

    and resolution (state :GameState) =
        let playerScore = 
            if checkBustHigh state.PlayerHand then evaluateHandLow state.PlayerHand else evaluateHandHigh state.PlayerHand

        let dealerScore =
            if checkBustHigh state.DealerHand then evaluateHandLow state.DealerHand else evaluateHandHigh state.DealerHand

        match playerScore, dealerScore with
        |p, d when p > d -> 
            do Console.WriteLine $"You win the hand! Player ({p}) vs House ({d})"
            do Console.WriteLine $"You win {state.BetSize}"
            deal { state with Bankroll = state.Bankroll + state.BetSize }

        |p, d when p < d -> 
            do Console.WriteLine $"The house wins the hand! Player ({p}) vs House ({d})"
            deal { state with Bankroll = state.Bankroll - state.BetSize }

        |p, d when p = d -> 
            do Console.WriteLine $"It's a tie! Player ({p}) vs House ({d})"
            deal state

        |_  -> failwith "Unexpected case in pattern match"
    
    do deal {
        Shoe = Card.randomShoePermutation()
        PlayerHand  = []
        DealerHand  = []
        Bankroll    = startingBankroll
        BetSize     = 0<Dollars>
        }

    0
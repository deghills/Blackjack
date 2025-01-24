open System
open Card
open GameState

[<EntryPoint>]
let Game _ =

    let startingBankroll = 200<Dollars>

    let rec deal (state :GameState) =

        if Option.isSome state.InactiveHand then
            let inactiveHand, inactiveBet = match state.InactiveHand with Some x -> x |_ -> failwith "unexpected case in pattern match"
            let dealerHand, shoe' = Card.drawCards state.Shoe [] 2
            playerTurn { state with
                            PlayerHand      = [inactiveHand]
                            DealerHand      = dealerHand
                            Shoe            = shoe'
                            InactiveHand    = None
                            BetSize         = inactiveBet
                            IsInitialHand   = false }

        do Console.WriteLine "//////"
        do Console.WriteLine "////// !NEW ROUND!"
        do Console.WriteLine "//////"
        do Console.WriteLine $"Your bankroll: {state.Bankroll}"

        do Console.WriteLine "What is your bet?"
        let rec promptBet() = 
            match Console.ReadLine() |> Int32.TryParse with
            |true, result -> result * 1<Dollars>
            |false, _ -> promptBet()

        let bet' = promptBet()

        let shoe', playerHand = Card.drawCards state.Shoe [] 2
        let shoe'', dealerHand = Card.drawCards shoe' [] 2

        if evaluate playerHand = 21
            then
                let threeToTwo = bet' + bet'/2
                do Console.WriteLine $"Blackjack for the player! ({Card.toStringHand playerHand})"
                do Console.WriteLine $"You profit {threeToTwo}"
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
            InactiveHand = state.InactiveHand
            IsInitialHand = true
        }

    and playerTurn (state :GameState) =
        
        do Console.WriteLine()
        do Console.WriteLine "Your hand:"
        do Card.printHand state.PlayerHand
        do () |> Console.WriteLine |> Console.WriteLine

        if (checkBust state.PlayerHand)
            then 
                do Console.WriteLine "The player busted!"
                do deal { state with Bankroll = state.Bankroll - state.BetSize }
            else

        do Console.WriteLine "Dealer's face-up card:"
        do state.DealerHand.Head |> Card.println |> Console.WriteLine

        if (state.IsInitialHand && match state.PlayerHand with 
                                    |a :: b :: [] when a = b -> true 
                                    |_ -> false) 
            then
                do Console.WriteLine "Split your pair?"
                let playerResponse = Console.ReadLine()
                (if playerResponse = "yes" || playerResponse = "y" || playerResponse = "split" then
                    let halfBet = state.BetSize / 2
                    playerTurn { state with 
                                    PlayerHand = [state.PlayerHand.Head]
                                    BetSize = halfBet
                                    InactiveHand = Some (state.PlayerHand.Tail.Head, halfBet)
                                    IsInitialHand = false }
                    )

        do Console.WriteLine (if state.IsInitialHand then "hit/stay/double?" else "hit or stay?")

        let rec promptAction() =
            match Console.ReadLine() with
            |"hit" -> 
                let shoe', playerHand' = Card.drawCard state.Shoe state.PlayerHand
                do playerTurn { state with 
                                    Shoe = shoe'
                                    PlayerHand = playerHand' 
                                    IsInitialHand = false}
                
            |"stay" -> 
                do dealerTurn state

            |"double" when state.IsInitialHand ->
                let shoe', playerHand' = Card.drawCard state.Shoe state.PlayerHand

                if(checkBust playerHand')
                    then 
                        do Console.Write $"The player busted! ( {Card.toStringHand playerHand'})"
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
                promptAction()

        promptAction()

    and dealerTurn (state :GameState) =
        
        do Console.WriteLine()
        do Console.WriteLine "Dealer's Hand"
        do Card.printHand state.DealerHand
        do Console.ReadLine() |> ignore

        match evaluate state.DealerHand with
        |handValue when handValue > 21 ->
            do Console.WriteLine $"The House busted!"
            do Console.WriteLine $"You profit {state.BetSize}"
            do deal { state with Bankroll = state.Bankroll + state.BetSize }

        |handValue when handValue > 16 ->
            do resolution state

        |_  -> 
            let Shoe', DealerHand' = Card.drawCard state.Shoe state.DealerHand
            dealerTurn { state with 
                            Shoe        = Shoe'
                            DealerHand  = DealerHand' }

    and resolution (state :GameState) =

        match evaluate state.PlayerHand, evaluate state.DealerHand with
        |p, d when p > d -> 
            do Console.WriteLine $"You win the hand! Player ({p}) vs House ({d})"
            do Console.WriteLine $"You profit {state.BetSize}"
            deal { state with Bankroll = state.Bankroll + state.BetSize }

        |p, d when p < d -> 
            do Console.WriteLine $"The house wins the hand! Player ({p}) vs House ({d})"
            deal { state with Bankroll = state.Bankroll - state.BetSize }

        |p, d when p = d -> 
            do Console.WriteLine $"It's a tie! Player ({p}) vs House ({d})"
            deal state

        |_  -> failwith "Unexpected case in pattern match"
    
    do deal {
        Shoe = Two :: Two :: Card.randomShoePermutation()
        PlayerHand  = []
        DealerHand  = []
        Bankroll    = startingBankroll
        BetSize     = 0<Dollars>
        InactiveHand = None
        IsInitialHand = false
        }

    0
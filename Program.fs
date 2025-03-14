﻿open System
open Card
open GameState

[<EntryPoint>]
let Game _ =

    let startingBankroll = 200<Dollars>

    let rec deal (state :GameState) =

        match state.InactiveHand with
        |Some (inactiveHand, inactiveBet) ->
            let shoe', dealerHand = Card.drawCards state.Shoe [] 2
            do playerTurn { state with
                                PlayerHand      = [inactiveHand]
                                DealerHand      = dealerHand
                                Shoe            = shoe'
                                InactiveHand    = None
                                BetSize         = inactiveBet
                                IsInitialHand   = false }
        |None ->

        do 
            Console.WriteLine "//////"
            Console.WriteLine "////// !NEW ROUND!"
            Console.WriteLine "//////"
            Console.WriteLine $"Your bankroll: {state.Bankroll}"

        let rec promptBet() = 
            do Console.WriteLine "What is your bet?"
            
            match Console.ReadLine() |> Int32.TryParse with
            |true, result -> result * 1<Dollars>
            |false, _ -> promptBet()

        let bet = promptBet()
        let shoe', playerHand = Card.drawCards state.Shoe [] 2
        let shoe'', dealerHand = Card.drawCards shoe' [] 2

        match evaluate playerHand with
        |score when score = 21 ->
            let threeToTwo = bet + bet/2
            do 
                Console.WriteLine $"Blackjack for the player! ({Card.toStringHand playerHand})"
                Console.WriteLine $"You profit {threeToTwo}"
                deal 
                    { state with 
                        Shoe = shoe''
                        Bankroll = state.Bankroll + threeToTwo
                    }
        |_ ->
            
        do playerTurn {
            PlayerHand  = playerHand
            DealerHand  = dealerHand
            Shoe        = shoe''
            Bankroll    = state.Bankroll
            BetSize     = bet
            InactiveHand = state.InactiveHand
            IsInitialHand = true
        }

    and playerTurn (state :GameState) =
        
        do 
            Console.WriteLine()
            Console.WriteLine "Your hand:"
            Card.printHand state.PlayerHand
            () |> Console.WriteLine |> Console.WriteLine

        match checkBust state.PlayerHand with
        | true ->
            do 
                Console.WriteLine "The player busted!"
                deal { state with Bankroll = state.Bankroll - state.BetSize }
        | false ->

        do 
            Console.WriteLine "Dealer's face-up card:"
            state.DealerHand.Head |> Card.println |> Console.WriteLine

        let rec promptSplit() =
            do Console.WriteLine "Split your pair?"

            match Console.ReadLine() with
            |"yes" |"y" |"split" -> 
                true

            |"no" |"n" -> 
                false

            |_ -> 
                promptSplit()

        match state.PlayerHand with 
        |a :: b :: [] when (a = b && state.IsInitialHand && promptSplit()) ->

            let halfBet = state.BetSize / 2
            do playerTurn { state with 
                                PlayerHand = [state.PlayerHand.Head]
                                BetSize = halfBet
                                InactiveHand = Some (state.PlayerHand.Tail.Head, halfBet)
                                IsInitialHand = false }
        |_ ->

        let rec promptAction() =
            do Console.WriteLine (if state.IsInitialHand then "hit/stay/double?" else "hit or stay?")

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

                match checkBust playerHand' with
                |true ->
                    do 
                        Console.Write $"The player busted! ( {Card.toStringHand playerHand'})"
                        deal { state with
                                Shoe = shoe'
                                PlayerHand = playerHand'
                                Bankroll = state.Bankroll - (2*state.BetSize)}
                |false ->

                do 
                    Console.WriteLine $"Your hand: {Card.toStringHand playerHand'}"
                    dealerTurn { state with 
                                    Shoe = shoe'
                                    PlayerHand = playerHand'
                                    BetSize = 2*state.BetSize}

            |_ -> 
                promptAction()

        do promptAction()

    and dealerTurn (state :GameState) =
        
        do 
            Console.WriteLine()
            Console.WriteLine "Dealer's Hand:"
            Card.printHand state.DealerHand
            Console.ReadLine() |> ignore

        match evaluate state.DealerHand with
        |handValue when handValue > 21 ->
            do 
                Console.WriteLine $"The House busted!"
                Console.WriteLine $"You profit {state.BetSize}"
                deal { state with Bankroll = state.Bankroll + state.BetSize }

        |handValue when handValue > 16 ->
            do resolution state

        |_  -> 
            let shoe', dealerHand' = Card.drawCard state.Shoe state.DealerHand
            dealerTurn { state with 
                            Shoe        = shoe'
                            DealerHand  = dealerHand' }

    and resolution (state :GameState) =

        match evaluate state.PlayerHand, evaluate state.DealerHand with
        |p, d when p > d -> 
            do 
                Console.WriteLine $"You win the hand! Player ({p}) vs House ({d})"
                Console.WriteLine $"You profit {state.BetSize}"
                deal { state with Bankroll = state.Bankroll + state.BetSize }

        |p, d when p < d -> 
            do 
                Console.WriteLine $"The house wins the hand. Player ({p}) vs House ({d})"
                deal { state with Bankroll = state.Bankroll - state.BetSize }

        |p, d when p = d -> 
            do 
                Console.WriteLine $"It's a tie! Player ({p}) vs House ({d})"
                deal state

        |_ ->  
            do failwith "Unexpected case in pattern match"
    
    do deal {
        Shoe = Card.randomShoePermutation()
        PlayerHand      = []
        DealerHand      = []
        Bankroll        = startingBankroll
        BetSize         = 0<Dollars>
        InactiveHand    = None
        IsInitialHand   = false
    }

    0
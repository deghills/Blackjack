open System
open Card
open Game

let startingBankroll = 200<Dollars>

[<EntryPoint>]
let Game _ =
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
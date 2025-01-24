module GameState
    open Card
    [<Measure>] type Dollars
    type GameState = {
        PlayerHand      :Card list
        InactiveHand    :(Card*int<Dollars>) option
        DealerHand      :Card list
        Shoe            :Card list
        IsInitialHand   :bool
        Bankroll    :int<Dollars>
        BetSize     :int<Dollars>
    }
module GameState
    open Card
    [<Measure>] type Dollars
    type GameState = {
        PlayerHand  :Card list
        DealerHand  :Card list
        Shoe        :Card list
        Bankroll    :int<Dollars>
        BetSize     :int<Dollars>
    }
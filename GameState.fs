module GameState
    open Card
    type GameState = {
        PlayerHand  :Card list
        DealerHand  :Card list
        Shoe        :Card list
    }
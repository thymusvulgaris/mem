module AppState exposing (Binary(..), Card(..), Clock, GameState(..), State(..), Variant(..), binaryToInt, binaryToWord, board, cardBinary, cardInt, cardInteger, cardWord, completed, deckShow, initRolls, instructions, intToBinary, landing, neutralCards, over, toBoard, toCompleted, toDeckShow, toInstructions, toLanding, toOver, toggleCardVariant, variantToString)

type Allowed
    = Allowed

type State t m
    = State m

type Card
    = Neutral Binary
    | Correct Binary
    | Incorrect Binary


type Variant
    = Alpha
    | Beta


type Binary
    = One
    | Zero


type alias Clock =
    { remaining : Int
    , cumulative : Int
    }


variantToString : Variant -> String
variantToString variant =
    case variant of
        Alpha ->
            "alpha"

        Beta ->
            "beta"


cardBinary : Card -> Binary
cardBinary card =
    case card of
        Neutral b ->
            b

        Correct b ->
            b

        Incorrect b ->
            b


cardWord : Card -> String
cardWord card =
    case card of
        Neutral b ->
            binaryToWord b

        Correct b ->
            binaryToWord b

        Incorrect b ->
            binaryToWord b


cardInt : Card -> Int
cardInt card =
    case card of
        Neutral b ->
            binaryToInt b

        Correct b ->
            binaryToInt b

        Incorrect b ->
            binaryToInt b


binaryToWord : Binary -> String
binaryToWord binary =
    case binary of
        One ->
            "one"

        Zero ->
            "zero"


binaryToInt : Binary -> Int
binaryToInt binary =
    case binary of
        One ->
            1

        Zero ->
            0


intToBinary : Int -> Binary
intToBinary integer =
    case integer of
        0 ->
            Zero

        _ ->
            One


toggleCardVariant : Variant -> Variant
toggleCardVariant variant =
    case variant of
        Alpha ->
            Beta

        Beta ->
            Alpha


neutralCards : List Card
neutralCards =
    [ Neutral One, Neutral Zero ]


cardInteger : Card -> Int
cardInteger card =
    case card of
        Neutral b ->
            binaryToInt b

        Correct b ->
            binaryToInt b

        Incorrect b ->
            binaryToInt b


initRolls : List Int
initRolls =
    [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 ]


type GameState
    = Landing (State { deckShow : Allowed } {})
    | DeckShow (State { board : Allowed, landing : Allowed } { clock : Clock, floor : Int, cardVariant : Variant, sequence : List Binary, rolls : List Int })
    | Board (State { deckShow : Allowed, landing : Allowed, completed : Allowed } { clock : Clock, floor : Int, cardVariant : Variant, sequence : List Binary, rolls : List Int, cards : List Card })
    | Over (State { deckShow : Allowed } { clock : Clock, floor : Int })
    | Completed (State { landing : Allowed } { clock : Clock, floor : Int })
    | Instructions (State { landing : Allowed } {})


landing : GameState
landing =
    State {} |> Landing


toLanding : State { a | landing : Allowed } m -> GameState
toLanding (State model) =
    landing


deckShow : Clock -> Int -> Variant -> List Binary -> List Int -> GameState
deckShow clock floor cardVariant sequence rolls =
    State { clock = clock, floor = floor, cardVariant = cardVariant, sequence = sequence, rolls = rolls } |> DeckShow


toDeckShow : Clock -> Int -> Variant -> List Binary -> List Int -> State { a | deckShow : Allowed } m -> GameState
toDeckShow clock floor cardVariant sequence rolls (State model) =
    deckShow clock floor cardVariant sequence rolls


board : Clock -> Int -> Variant -> List Binary -> List Int -> List Card -> GameState
board clock floor cardVariant sequence rolls cards =
    State { clock = clock, floor = floor, cardVariant = cardVariant, sequence = sequence, rolls = rolls, cards = cards } |> Board


toBoard : Clock -> Int -> Variant -> List Binary -> List Int -> List Card -> State { a | board : Allowed } m -> GameState
toBoard clock floor cardVariant sequence rolls cards (State model) =
    board clock floor cardVariant sequence rolls cards


over : Clock -> Int -> GameState
over clock floor =
    State { clock = clock, floor = floor } |> Over


toOver : Clock -> Int -> State { a | over : Allowed } m -> GameState
toOver clock floor (State model) =
    over clock floor


completed : Int -> Clock -> GameState
completed floor clock =
    State { floor = floor, clock = clock } |> Completed


toCompleted : Int -> Clock -> State { a | completed : Allowed } m -> GameState
toCompleted floor clock (State model) =
    completed floor clock


instructions : GameState
instructions =
    Instructions (State {})


toInstructions : State { a | instructions : Allowed } m -> GameState
toInstructions (State model) =
    instructions

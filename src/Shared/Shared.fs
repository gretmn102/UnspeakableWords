namespace Shared

type UserId = string

type LetterId = int
type LetterValue = char
type LetterUniq = LetterValue * LetterId
type Word = LetterId list

type LoginError =
    | YouAlreadyLogin
    | PlayersRecruited
type GetStateResult<'GameResponse, 'ClientGameState> =
    | WaitPlayers of int
    | PlayerJoined of UserId
    | PlayerLeaved of UserId
    | GameStarted of 'ClientGameState
    | GameEnded
    | GameResponse of 'GameResponse
type GetStateError =
    | YouAreNotLogin
type MovError =
    | LetterError
    | WordIsEmpty
    | WordWasUsed
type MoveError =
    | GetStateError of GetStateError
    | GameHasNotStartedYet
    | GameEndedError
    | NotYourMove
    | MovError of MovError

type PlayerId = string

type 'Hand Player = {
    Name: PlayerId
    Hand: 'Hand
    Points: int
    SanityPoints: int
}
module Client =
    type OtherPlayer =
        {
            PlayerId: UserId
            SanityPoints: int
            Points: int
            Hand: int
        }
    type MoveStage =
        | HasNotYourMoveYet
        | StartingMove
        | ApprovingWord
        | GameEnd
    type GameState =
        {
            OtherPlayers: Map<UserId, OtherPlayer>
            CurrentPlayerMove: UserId
            ClientPlayer: Player<LetterId list>
            PlayedWords: (Word * UserId) list
            Discard: LetterId list
            MoveStage: MoveStage
        }

type SanityCheckResult =
    | Pass
    | NotPass
type InsaneCheck =
    | Insane
    | NotInsane
type GameResponse =
    | TakeLetters of LetterId list
    | OtherTakeLetters of int

    | DiscardToDeck
    | RemovePlayerBecauseCardsIsLeft

    | NowTurn of UserId
    | WordSucc of Word
    | SanityCheck of points:int * throwResult:int * SanityCheckResult
    | InsaneCheck of InsaneCheck
    | Discard of LetterId list

module Init =
    type Letter = { Name:LetterValue; Points:int; Count:int }

    let letters =
        [('A', 5, 10); ('B', 5, 2); ('C', 0, 2); ('D', 2, 3); ('E', 4, 10);
         ('F', 3, 2);  ('G', 2, 2); ('H', 4, 3); ('I', 4, 9); ('J', 2, 1);
         ('K', 3, 1);  ('L', 1, 5); ('M', 3, 3); ('N', 2, 5); ('O', 0, 8);
         ('P', 3, 2);  ('Q', 2, 1); ('R', 4, 5); ('S', 0, 5); ('T', 2, 5);
         ('U', 0, 4);  ('V', 1, 2); ('W', 3, 2); ('X', 4, 1); ('Y', 3, 2);
         ('Z', 2, 1)]
        |> List.map (fun (name, points, count) ->
            let name = System.Char.ToLower name
            name, { Name = name; Points = points; Count = count})

    let allLetters =
        letters
        |> List.mapFold (fun i (c, l) ->
            let count = l.Count
            List.init count (fun i' -> i + i', l), i + count
        ) 0
        |> fst
        |> List.concat

    let sanityPoints = 5

type Color =
    | Red
    | Green
    | Blue
    | Black

type User = { Name : string; Color: Color }
type Message = {Time : System.DateTime; Content: string}

type Msgs =
    | ClientMsg of string * Message
    | SysMsg of Message

type RemoteClientMsg =
    | QueryConnected
    | GetUsers of User list

    | AddUser of User
    | RemoveUser of string
    | AddMsg of Msgs
    | AddMsgs of Msgs list

    | GameMsgs of GetStateResult<GameResponse, Client.GameState> list
    | LoginResult of Result<Client.GameState option, LoginError>
    | MoveResult of Result<unit, MoveError>

type RemoteServerMsg =
    | SetUser of User
    | SendMsg of string
    | UsersConnected
    | Move of Word

module Remote =
    let socketPath = "/socket"
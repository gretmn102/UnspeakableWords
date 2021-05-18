namespace Shared

type UserId = string

type Language =
    | English
    | Russian

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
            DeckCount: int
            MoveStage: MoveStage
            Language: Language
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
    | OtherDiscardHand of LetterId list
    | DiscardToDeck
    | RemovePlayerBecauseCardsIsLeft

    | NowTurn of UserId
    | WordSucc of Word
    | WordNotExist of Word

    | SanityCheck of points:int * throwResult:int * SanityCheckResult
    | InsaneCheck of InsaneCheck
    | Discard of LetterId list

module Init =
    type Letter = { Name:LetterValue; Points:int; Count:int }

    let englishLetters =
        [('A', 5, 10); ('B', 5, 2); ('C', 0, 2); ('D', 2, 3); ('E', 4, 10);
         ('F', 3, 2);  ('G', 2, 2); ('H', 4, 3); ('I', 4, 9); ('J', 2, 1);
         ('K', 3, 1);  ('L', 1, 5); ('M', 3, 3); ('N', 2, 5); ('O', 0, 8);
         ('P', 3, 2);  ('Q', 2, 1); ('R', 4, 5); ('S', 0, 5); ('T', 2, 5);
         ('U', 0, 4);  ('V', 1, 2); ('W', 3, 2); ('X', 4, 1); ('Y', 3, 2);
         ('Z', 2, 1)]
        |> List.map (fun (name, points, count) ->
            let name = System.Char.ToLower name
            name, { Name = name; Points = points; Count = count})

    let allEnglishLetters =
        englishLetters
        |> List.mapFold (fun i (c, l) ->
            let count = l.Count
            List.init count (fun i' -> i + i', l), i + count
        ) 0
        |> fst
        |> List.concat

    let russianLetters =
        // [('А', 5, 8); ('У', 2, 4); ('Д', 8, 4); ('П', 2, 4); ('В', 4, 4); ('Н', 4, 5); ('Е', 4, 6);
        //  ('Ь', 3, 2); ('Я', 4, 2); ('С', 0, 5); ('З', 1, 2); ('Б', 4, 2); ('В', 5, 4); ('Г', 1, 3);
        //  ('И', 2, 6); ('Г', 1, 3); ('И', 2, 6); ('Т', 2, 5); ('М', 3, 3); ('О', 0, 6); ('Й', 2, 1);
        //  ('Л', 2, 4); ('К', 3, 4); ('Ю', 2, 1); ('Э', 2, 1); ('Ф', 4, 1); ('Щ', 3, 1); ('И', 2, 1);
        //  ('Ж', 4, 1); ('Ы', 2, 1); ('Ш', 3, 1); ('З', 1, 1); ('Х', 4, 1); ('Ц', 2, 1); ] // ('Ё', 1)
        // TODO: define points through corners of letters
        [('А', 1, 8); ('Б', 3, 2); ('В', 1, 4); ('Г', 3, 2); ('Д', 2, 4);
         ('Е', 1, 8); ('Ё', 3, 1); ('Ж', 5, 1); ('З', 5, 2); ('И', 1, 5);
         ('Й', 4, 1); ('К', 2, 4); ('Л', 2, 4); ('М', 2, 3); ('Н', 1, 5);
         ('О', 1, 10); ('П', 2, 4); ('Р', 1, 5); ('С', 1, 5); ('Т', 1, 5);
         ('У', 2, 4); ('Х', 5, 1); ('Ц', 5, 1); ('Ч', 5, 1); ('Ш', 8, 1);
         ('Ф', 10, 1); ('Щ', 10, 1); ('Ъ', 10, 1); ('Ы', 4, 2); ('Ь', 3, 2);
         ('Э', 8, 1); ('Ю', 8, 1); ('Я', 3, 2)]
        |> List.map (fun (name, points, count) ->
            let name = System.Char.ToLower name
            name, { Name = name; Points = points; Count = count})

    let allRussianLetters =
        russianLetters
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
    | DiscardHand
module Remote =
    let socketPath = "/socket"
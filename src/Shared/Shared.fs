namespace Shared

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type UserId = string

type LoginError =
    | YouAlreadyLogin
    | PlayersRecruited
type 'GameResponse GetStateResult =
    | WaitPlayers of int
    | PlayerJoined of UserId
    | PlayerLeaved of UserId
    | GameStarted
    | GameResponse of 'GameResponse
type GetStateError =
    | YouAreNotLogin
type MoveError =
    | GetStateError of GetStateError
    | GameHasNotStartedYet

type GameResponse =
    | NowTurn of UserId

type IApi =
    {
        login: UserId -> Async<Result<unit, LoginError>>
        getState: UserId -> Async<Result<GameResponse GetStateResult list, GetStateError>>
        move: UserId -> Async<Result<unit, MoveError>>
    }

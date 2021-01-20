namespace Shared

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IApi =
    {
        sendMessage : unit -> Async<string>
    }

namespace Elmish.Debug
open Fable.Import.RemoteDev
open Fable.Core.JsInterop
open Fable.Core
open Fable.Import

[<RequireQualifiedAccess>]
module Debugger =
    open FSharp.Reflection
    open Fable.Import

    let inline private duName (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let inline private getCase cmd : obj =
        let cmdobj = toJsonWithTypeInfo cmd |> JS.JSON.parse
        [
            yield "type" ==> duName cmd
            // copy the action object's properties into the created object 
            // to flatten the action hierarchy displayed in RemoteDev.
            for prop in JS.Object.getOwnPropertyNames cmdobj do
                yield prop ==> cmdobj?(prop)
        ] |> createObj

    type ConnectionOptions =
        | ViaExtension
        | RemoteDevIo
        | Remote of address:string * port:int
        | Secure of address:string * port:int

    let [<Literal>] private ViaExtensionWarning =
        "ConnectionOptions.ViaExtension: Using the Chrome extension RemoteDev DevTools does not work with or without an external server. Use the Chrome app RemoteDev instead with an external server or RemoteDevIo"

    let connect =
        // when does this get used? remotedev.io only?
        // Can't find any use of a "serializer" property of the config being used in remotedev source.
        let serialize = createObj ["replacer" ==> fun _ v -> deflate v ]

        let fallback = { Options.remote = true; hostname = Some "remotedev.io"; port = Some 443; secure = true; getActionType = Some getCase; serialize = serialize }

        function
        | RemoteDevIo -> fallback
        | ViaExtension -> 
            Browser.console.error(ViaExtensionWarning)
            { fallback with remote = false; hostname = None; port = None; secure = false}
        | Remote (address,port) -> { fallback with hostname = Some address; port = Some port; secure = false }
        | Secure (address,port) -> { fallback with hostname = Some address; port = Some port;  }
        >> connectViaExtension

[<RequireQualifiedAccess>]
module Program =
    open Elmish

    let inline private deflateObj (o:obj) = 
       toJsonWithTypeInfo o |> JS.JSON.parse

    let inline private inflateObj<'model> (s:string) =
        ofJsonWithTypeInfo<'model> s

    [<PassGenericsAttribute>]
    let withDebuggerUsing (connection:Connection) (program : Program<'a,'model,'msg,'view>) : Program<'a,'model,'msg,'view> =
        let init a =
            let (model,cmd) = program.init a
            connection.init (deflateObj model, None)
            model,cmd

        let update msg model : 'model * Cmd<'msg> =
            let (model',cmd) = program.update msg model
   
            connection.send (msg, deflateObj model')
            (model',cmd)

        let subscribe model = 
            let sub dispatch =
                function
                | (msg:Msg) when msg.``type`` = MsgTypes.Dispatch ->
                    try
                        match msg.payload.``type`` with
                        | PayloadTypes.JumpToAction
                        | PayloadTypes.JumpToState ->
                            // Chrome App causes the JSON from the server to be re-encoded as a JSON string.
                            // We actually want JSON instead of a decoded plain object so we can use
                            // ofJsonWithTypeInfo. If it's double encoded we just use the result from
                            // exractState. If it's not double encoded (we get a plain JS obj), encode it to
                            // JSON for use with ofJsonWithTypeInfo.
                            let stateString = 
                                match extractState msg with
                                | :? string as s when s.Length > 0  && s.[0] = '"' || s.[0] = '{' -> 
                                    s
                                | o ->
                                    JS.JSON.stringify o
                            let state = inflateObj<'model> (stateString)
                            program.setState state dispatch
                        | PayloadTypes.ImportState ->
                            let state = msg.payload.nextLiftedState.computedStates |> Array.last
                            program.setState (inflate<'model> state?state) dispatch
                            connection.send(null, msg.payload.nextLiftedState)
                        | _ -> ()
                    with ex ->
                        Fable.Import.Browser.console.error ("Unable to process monitor command", msg, ex)
                | _ -> ()
                |> connection.subscribe
                |> ignore

            Cmd.batch
                [ [sub]
                  program.subscribe model ]

        let onError (text,ex) =
            connection.error (text,ex)

        { program with 
                    init = init
                    update = update
                    subscribe = subscribe
                    onError = onError }


    [<PassGenericsAttribute>]
    let withDebuggerAt options program : Program<'a,'model,'msg,'view> = 
        try
            (Debugger.connect options, program)
            ||> withDebuggerUsing
        with ex -> 
            Fable.Import.Browser.console.error ("Unable to connect to the monitor, continuing w/o debugger", ex)
            program

    
    [<PassGenericsAttribute>]
    let withDebugger (program : Program<'a,'model,'msg,'view>) : Program<'a,'model,'msg,'view> =
        try
            ((Debugger.connect Debugger.ViaExtension),program)
            ||> withDebuggerUsing
        with ex -> 
            Fable.Import.Browser.console.error ("Unable to connect to the monitor, continuing w/o debugger", ex)
            program

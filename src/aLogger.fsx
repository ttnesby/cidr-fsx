#load @"./logSolution.fsx"

namespace ALogger

type Logger = string -> unit

type ALog = {
    Inf: Logger
    Err: Logger
    Wrn: Logger
    Dbg: Logger
    Ftl: Logger
}

type GetLogs = unit -> ALog

[<RequireQualifiedAccess>]
module ALog =

    let getLogs : GetLogs = 
        fun () ->
            {
                ALog.Inf = (NLog.LogManager.GetLogger "Console").Info
                Err = (NLog.LogManager.GetLogger "Console").Error
                Wrn = (NLog.LogManager.GetLogger "Console").Warn
                Dbg = (NLog.LogManager.GetLogger "Console").Debug
                Ftl = (NLog.LogManager.GetLogger "Console").Fatal
            }

    let inf : Logger = (NLog.LogManager.GetLogger "Console").Info
    let err : Logger = (NLog.LogManager.GetLogger "Console").Error
    let wrn : Logger = (NLog.LogManager.GetLogger "Console").Warn
    let dbg : Logger = (NLog.LogManager.GetLogger "Console").Debug
    let ftl : Logger = (NLog.LogManager.GetLogger "Console").Fatal    
    
    let logPassThroughResult (l:ALog) (okMsg, errMsg) r = 
        match r with | Ok _ ->  l.Dbg okMsg | Error _ -> l.Err errMsg
        r

    let logPassThroughBool (l:ALog) (tMsg,fMsg) b  =
        if b then l.Inf tMsg else l.Err fMsg
        b

    let logPassThroughStr l msg = l msg; msg
    let logPassThroughX l msg x = l msg; x
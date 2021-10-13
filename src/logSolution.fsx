#load @"./../.paket/load/NLog.fsx"

open NLog

(
    NLog.Config.LoggingConfiguration(),
    new NLog.Targets.ColoredConsoleTarget("Console")
)
|> fun (conf, target) ->
        //target.Layout <- @""
        conf.AddRule(LogLevel.Debug, LogLevel.Fatal, target)
        LogManager.Configuration <- conf

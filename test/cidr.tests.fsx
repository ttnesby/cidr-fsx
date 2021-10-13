#load @"./../.paket/load/FsCheck.fsx"
#load @"./../src/aLogger.fsx"
#load @"./../src/cidr.fsx"

open FsCheck
open ALogger
open ClasslessInterDomainRouting

let shouldBeError r = match r with | Error _-> true | Ok _ -> false
let shouldBeOk r = match r with | Error _-> false | Ok _ -> true

let test2ExitCode (f: unit -> unit) = try f(); 0 with | _ -> 1

let reportExitCode ec =
    let output = $"Test end: exit code [{ec}]"
    match ec with
    | 0 -> ALog.inf output; 0
    | n -> ALog.err output; 1


type CIDRCreate =

    static member ``No network width must be Error `` () =
        "10.0.0.0" |> CIDR.create |> shouldBeError ||
        "10.0.0.0/" |> CIDR.create |> shouldBeError

    static member ``Invalid network width must be Error `` () =
        "10.0.0.0/500" |> CIDR.create |> shouldBeError ||
        "10.0.0.0/33" |> CIDR.create |> shouldBeError

    static member ``Valid CIDR must be Ok `` () =
        "10.0.0.0/8" |> CIDR.create |> shouldBeOk &&
        "192.168.0.0/16" |> CIDR.create |> shouldBeOk &&
        "172.16.0.0/12" |> CIDR.create |> shouldBeOk &&
        "172.16.0.1/32" |> CIDR.create |> shouldBeOk

ALog.inf $"Test start: {fsi.CommandLineArgs.[0]} "
[
    fun () ->
        Check.All<CIDRCreate>({ Config.QuickThrowOnFailure with MaxTest = 6; QuietOnSuccess=false })
]
|> List.sumBy test2ExitCode
|> reportExitCode
|> exit
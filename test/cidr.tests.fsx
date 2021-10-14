#load @"./../.paket/load/FsCheck.fsx"
#load @"./../src/aLogger.fsx"
#load @"./../src/cidr.fsx"

open FsCheck
open ALogger
open ClasslessInterDomainRouting

let shouldBeError r = match r with | Error _-> true | Ok _ -> false
let shouldBeOk r = match r with | Error _-> false | Ok _ -> true

let shouldBe e r = match r with | Error _-> false | Ok ip -> ip = e

let shouldBeTrue r = match r with | Error _-> false | Ok f -> f
let shouldBeFalse r = match r with | Error _-> false | Ok f -> not f

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

type CIDRIPv4StartIP =

    static member ``Start IP for common CIDR ranges must be correct`` () =
      "10.0.0.0/8" |>  CIDR.IPv4StartIP |> shouldBe "10.0.0.1" &&
      "172.16.0.0/12" |>  CIDR.IPv4StartIP |> shouldBe "172.16.0.1" &&
      "192.168.0.0/16" |>  CIDR.IPv4StartIP |> shouldBe "192.168.0.1" &&
      "10.78.32.0/24" |>  CIDR.IPv4StartIP |> shouldBe "10.78.32.1"

    static member ``Start IP for smal CIDR ranges must be correct`` () =
      "10.78.32.0/25" |>  CIDR.IPv4StartIP |> shouldBe "10.78.32.1" &&
      "10.78.32.128/25" |>  CIDR.IPv4StartIP |> shouldBe "10.78.32.129"

type CIDRIPv4EndIP =

    static member ``End IP for common CIDR ranges must be correct`` () =
      "10.0.0.0/8" |>  CIDR.IPv4EndIP |> shouldBe "10.255.255.255" &&
      "172.16.0.0/12" |>  CIDR.IPv4EndIP |> shouldBe "172.31.255.255" &&
      "192.168.0.0/16" |>  CIDR.IPv4EndIP |> shouldBe "192.168.255.255" &&
      "10.78.32.0/24" |>  CIDR.IPv4EndIP |> shouldBe "10.78.32.255"

    static member ``End IP for smal CIDR ranges must be correct`` () =
      "10.78.32.0/25" |>  CIDR.IPv4EndIP |> shouldBe "10.78.32.127" &&
      "10.78.32.128/25" |>  CIDR.IPv4EndIP |> shouldBe "10.78.32.255"

type CIDROverlappingRanges =

    static member ``fully overlapping ranges must return true`` () =
        CIDR.overlappingRanges "192.168.1.0/23" "192.168.1.0/23" |> shouldBeTrue &&
        CIDR.overlappingRanges "192.168.1.0/23" "192.168.1.0/24" |> shouldBeTrue

    static member ``order of overlapping ranges does not matter`` () =
        CIDR.overlappingRanges "192.168.1.0/24" "192.168.1.0/23" |> shouldBeTrue

    static member ``partial overlapping ranges must return true`` () =
        CIDR.overlappingRanges "192.168.1.0/26" "192.168.1.32/27" |> shouldBeTrue

    static member ``non-overlapping ranges must return false`` () =
        CIDR.overlappingRanges "192.168.1.0/24" "192.168.2.0/24" |> shouldBeFalse  &&
        CIDR.overlappingRanges "192.168.1.0/23" "192.168.3.0/24" |> shouldBeFalse &&
        CIDR.overlappingRanges "192.168.1.64/26" "192.168.1.128/25" |> shouldBeFalse

ALog.inf $"Test start: {fsi.CommandLineArgs.[0]} "
[
    fun () ->
        Check.All<CIDRCreate>({ Config.QuickThrowOnFailure with MaxTest = 6; QuietOnSuccess=false })
    fun () ->
        Check.All<CIDRIPv4StartIP>({ Config.QuickThrowOnFailure with MaxTest = 6; QuietOnSuccess=false })
    fun () ->
        Check.All<CIDRIPv4EndIP>({ Config.QuickThrowOnFailure with MaxTest = 6; QuietOnSuccess=false })
    fun () ->
        Check.All<CIDROverlappingRanges>({ Config.QuickThrowOnFailure with MaxTest = 6; QuietOnSuccess=false })
]
|> List.sumBy test2ExitCode
|> reportExitCode
|> exit
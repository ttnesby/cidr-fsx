#load @"./../.paket/load/FsCheck.fsx"
#load @"./../src/aLogger.fsx"
#load @"./../src/cidr.fsx"

open FsCheck
open ALogger
open ClasslessInterDomainRouting

// assertions

let shouldBeErrorType et (r: Result<_,CIDRError>) = r |> function | Error e when e = et -> true | _ -> false

let shouldBe e r = match r with | Error _-> false | Ok ip -> ip = e

let shouldOverlap r = match r with | Error _-> false | Ok f -> f
let shouldNotOverlap r = match r with | Error _-> false | Ok f -> not f

// helpers

let test2ExitCode (f: unit -> unit) = try f(); 0 with | _ -> 1

let reportErrors ec =
    let output = $"Test end: errors [{ec}]"
    match ec with
    | 0 -> ALog.inf output; 0
    | _ -> ALog.err output; 1

// test specifications
type CIDRNotation =

    static member ``Invalid notation must be Error InvalidNotation `` () =
        let folder acc e = acc && (CIDR.IPv4Range e |> shouldBeErrorType InvalidNotation)

        ["10.0.0.0";"10.0.0.0/";"10.0.0.0/33";"10.0.256.0/32"] |> List.fold folder true
type CIDRAddressRange =

    static member ``Outside address range must be Error OutsideAddressRange `` () =
        let folder acc e = acc && (CIDR.IPv4Range e |> shouldBeErrorType OutsideAddressRange)

        ["0.0.0.1/0";"255.255.255.250/29"] |> List.fold folder true

type CIDRIPv4StartIP =

    static member ``Start IP for CIDR ranges must be correct `` () =
        let folder acc e = acc && (CIDR.IPv4StartIP (fst e) |> shouldBe (snd e))

        [
            ("10.0.0.0/8","10.0.0.0")
            ("172.16.0.0/12","172.16.0.0")
            ("192.168.0.0/16","192.168.0.0")
            ("10.78.32.0/24","10.78.32.0")
            ("10.78.32.0/25","10.78.32.0")
            ("10.78.32.128/25","10.78.32.128")
            ("10.78.32.128/32","10.78.32.128")
        ]
        |> List.fold folder true
type CIDRIPv4EndIP =

    static member ``End IP for CIDR ranges must be correct `` () =
        let folder acc e = acc && (CIDR.IPv4EndIP (fst e) |> shouldBe (snd e))

        [
            ("10.0.0.0/8","10.255.255.255")
            ("172.16.0.0/12","172.31.255.255")
            ("192.168.0.0/16","192.168.255.255")
            ("10.78.32.0/24","10.78.32.255")
            ("10.78.32.0/25","10.78.32.127")
            ("10.78.32.128/25","10.78.32.255")
            ("10.78.32.128/32","10.78.32.128")
        ]
        |> List.fold folder true
type CIDRIPv4HasOverlap =

    static member ``Overlapping ranges must return true `` () =
        let folder acc e =
            let r1,r2 = fst e
            acc && (CIDR.IPv4HasOverlap r1 r2 |> (snd e))

        [
            (("192.168.1.0/23", "192.168.1.0/23"), shouldOverlap)
            (("192.168.1.0/23", "192.168.1.0/24"), shouldOverlap)
            (("192.168.1.0/24", "192.168.1.0/23"), shouldOverlap)
            (("192.168.1.0/26", "192.168.1.32/27"), shouldOverlap)
            (("192.168.1.0/24", "192.168.2.0/24"), shouldNotOverlap)
            (("192.168.1.0/23", "192.168.3.0/24"), shouldNotOverlap)
            (("192.168.1.64/26", "192.168.1.128/25"), shouldNotOverlap)
        ]
        |> List.fold folder true

// execute test specification

ALog.inf $"Test start: {fsi.CommandLineArgs.[0]} "

let defultConfig = { Config.QuickThrowOnFailure with MaxTest = 1; QuietOnSuccess=false }
[
    fun () -> Check.All<CIDRNotation>(defultConfig)
    fun () -> Check.All<CIDRAddressRange>(defultConfig)
    fun () -> Check.All<CIDRIPv4StartIP>(defultConfig)
    fun () -> Check.All<CIDRIPv4EndIP>(defultConfig)
    fun () -> Check.All<CIDRIPv4HasOverlap>(defultConfig)
]
|> List.sumBy test2ExitCode
|> reportErrors
|> exit
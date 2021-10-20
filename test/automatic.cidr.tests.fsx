#load @"./../.paket/load/FsCheck.fsx"
#load @"./../src/aLogger.fsx"
#load @"./../src/cidr.fsx"

open FsCheck
open ALogger
open ClasslessInterDomainRouting

// define relevant types and generators in CIDR domain

type NetworkWidth = NetworkWidth of byte with
    member this.Value = let (NetworkWidth nw) = this in (nw)

type MyGenerators =
  static member NetworkWidt() =
      {new Arbitrary<NetworkWidth>() with
          override x.Generator = Gen.elements ([0uy..32uy] |> List.map NetworkWidth)
          override x.Shrinker t = Seq.empty }

Arb.register<MyGenerators>()

type CIDR = CIDR of byte * byte * byte * byte * NetworkWidth with
    member this.Value = let (CIDR (a,b,c,d,nw)) = this in (a,b,c,d,nw)
    override this.ToString() = this.Value |> fun (a,b,c,d,nw) -> $"{a}.{b}.{c}.{d}/{nw.Value}"

// assertions and helpers

let shouldBeErrorType et (r: Result<_,CIDRError>) = r |> function | Error e when e = et -> true | _ -> false

let test2ExitCode (f: unit -> unit) = try f(); 0 with | _ -> 1

let reportErrors ec =
    let output = $"Test end: errors [{ec}]"
    match ec with
    | 0 -> ALog.inf output; 0
    | _ -> ALog.err output; 1

// test specification, each member will get automatic generated parametes

type CIDRAPI =

    // Assuming random generated string never will be a CIDR notation
    static member ``Arbitrary string is Error InvalidNotation `` s =
        CIDR.IPv4Range s |> shouldBeErrorType InvalidNotation

    static member ``Valid CIDR is Ok or Error OutsideAddressRange `` (cidr: CIDR) =
        let cidrUniverse = "0.0.0.0/0"
        let cidrCurrent = cidr.ToString()

        cidr.ToString() |> CIDR.IPv4Range
        |> function
        | Ok (fip,lip) ->
            let cidrFip = fip + "/32"
            let cidrLip = lip + "/32"
            let folder acc (r1,r2) = acc && (CIDR.IPv4HasOverlap r1 r2 |> function | Error _ -> false | Ok v -> v)

            [
                (cidrFip,cidrUniverse)
                (cidrUniverse,cidrLip)
                (cidrCurrent,cidrUniverse)
                (cidrCurrent,cidrFip)
                (cidrLip,cidrCurrent)
            ]
            |> List.fold folder true

        | Error e -> e |> function |OutsideAddressRange -> true | _ -> false

// execute test specification

ALog.inf $"Test start: {fsi.CommandLineArgs.[0]} "

let defultConfig = { Config.QuickThrowOnFailure with MaxTest = 50_000; MaxFail = 1; QuietOnSuccess=false }
[fun () -> Check.All<CIDRAPI>(defultConfig)]
|> List.sumBy test2ExitCode
|> reportErrors
|> exit
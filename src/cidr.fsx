namespace ClasslessInterDomainRouting

type CIDRNotation = string
type IPv4Format = string
type SubnetMask = string

type CIDRError =
    | InvalidNotation
    | OutsideAddressRange

type IPv4Range = CIDRNotation -> Result<(IPv4Format * IPv4Format) * SubnetMask, CIDRError>
type IPv4StartIP = CIDRNotation -> Result<IPv4Format, CIDRError>
type IPv4EndIP = CIDRNotation -> Result<IPv4Format, CIDRError>
type IPv4HasOverlap = CIDRNotation -> CIDRNotation -> Result<bool,CIDRError>

module CIDR =

    type CIDR = private CIDR of (uint32 * uint32) * uint32 with
        member this.Value = let (CIDR ((f,l),m)) = this in ((f,l),m)
        member this.X1 = (fst >> fst) this.Value
        member this.X2 = (fst >> snd) this.Value
        member this.M  = snd this.Value

    module Helpers =

        open System.Net

        let private baToUInt ba = System.BitConverter.ToUInt32(ba,0)
        let private toIPString (ba: byte array) = (IPAddress ba).ToString()

        let toUInt (ba: byte array) = ba |> (Array.rev >> baToUInt)
        let toIPv4String (ui: uint32) = ui |> (System.BitConverter.GetBytes >> Array.rev >> toIPString)

    open System.Text.RegularExpressions

    [<LiteralAttribute>]
    let private Pattern = """^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\/([0-9]{1,2})$"""

    let private (|CIDR|_|) input =
        try
            let m = Regex.Match(input, Pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None
        with | _ -> None

    let private toHostRange (nw: string) =
        let p2 e = 2.0 ** e
        let m1 x = x - 1.0
        nw |> (byte >> ((-) 32uy) >> float >> p2 >> m1 >> uint32)

    let private create s =
        let lt256 l = List.fold (fun acc e -> acc && (int e < 256)) true l
        match s with
        | CIDR [a;b;c;d;ntwWidth] when lt256 [a;b;c;d] && (byte ntwWidth) < 33uy ->

            let fip = Helpers.toUInt [|byte a;byte b;byte c;byte d|]
            let hostRange = toHostRange ntwWidth

            match hostRange with
            | 0u -> ((fip,fip),System.UInt32.MaxValue) |> CIDR |> Ok
            | n when fip + n > fip -> ((fip, fip + n),System.UInt32.MaxValue - n) |> CIDR |> Ok
            | _ -> Error OutsideAddressRange

        | _ -> Error InvalidNotation

    let IPv4Range : IPv4Range = fun s ->
        let toTuples (cidr: CIDR) =
            ((Helpers.toIPv4String cidr.X1, Helpers.toIPv4String cidr.X2),Helpers.toIPv4String cidr.M)
        s |> (create >> Result.map toTuples)

    let IPv4StartIP : IPv4StartIP = fun s -> s |> (IPv4Range >> Result.map (fst >> fst))
    let IPv4EndIP : IPv4EndIP = fun s -> s |> (IPv4Range >> Result.map (fst >> snd))

    let IPv4HasOverlap : IPv4HasOverlap = fun s1 s2 ->
        match (create s1, create s2) with
        | Error e, _ | _, Error e -> Error e
        | (Ok cidr1, Ok cidr2) ->  (cidr1.X1 <= cidr2.X2 && cidr2.X1 <= cidr1.X2) |> Ok
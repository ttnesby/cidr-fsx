namespace ClasslessInterDomainRouting

type IPFirstAsUInt = uint32
type IPLastAsUInt = uint32

type CIDR = private CIDR of IPFirstAsUInt*IPLastAsUInt with
    member this.Value = let (CIDR (f,l)) = this in (f,l)

module CIDR =

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
        let w = 32uy - (byte nw) |> float 
        let p2 e = 2.0 ** e
        let m1 x = x - 1.0
        w |> (p2 >> m1 >> uint32)      


    let private create s =
        let lt256 l = List.fold (fun acc e -> acc && (int e < 256)) true l
        match s with
        | CIDR [a;b;c;d;ntwWidth] when lt256 [a;b;c;d] && (byte ntwWidth) < 33uy ->

            let fip = Helpers.toUInt [|byte a;byte b;byte c;byte d|] 
            let hostRange = toHostRange ntwWidth

            match hostRange with
            | 0u -> (fip,fip) |> CIDR |> Ok
            | n when fip + n > fip -> (fip, fip + n) |> CIDR |> Ok
            | _ -> Error $"CIDR {s} is out of range"  

        | _ -> Error $"{s} has invalid CIDR notation 'a.b.c.d/networkWidth'"

    let asValue (cidr: CIDR) = cidr.Value      

    let IPv4StartIP s = s |> (create >> Result.map (asValue >> fst >> Helpers.toIPv4String))
    let IPv4EndIP s = s |> (create >> Result.map (asValue >> snd >> Helpers.toIPv4String))

    let IPv4Range s =
        s |> create |> Result.map (fun cidr -> 
            let (x1,x2) = cidr.Value 
            (Helpers.toIPv4String x1, Helpers.toIPv4String x2)
        )

    let IPv4HasOverlap r1 r2 =
        match (create r1, create r2) with
        | Error e, _ | _, Error e -> Error e
        | (Ok cidr1, Ok cidr2) ->  
            let (x1,x2) = cidr1.Value
            let (y1,y2) = cidr2.Value
            (x1 <= y2 && y1 <= x2) |> Ok
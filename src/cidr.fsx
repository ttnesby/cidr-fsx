namespace ClasslessInterDomainRouting
type NetworkWidth = byte

type CIDR = private CIDR of byte*byte*byte*byte*NetworkWidth with
    member this.Value = let (CIDR (a,b,c,d,nw)) = this in (a,b,c,d,nw)

module CIDR =

    module IPv4 =

        open System.Net

        let private baToUInt ba = System.BitConverter.ToUInt32(ba,0)

        let toUInt (ba: byte array) =
            let toIPv4Bytes (ba: byte array) = (IPAddress ba).GetAddressBytes()
            ba |> (toIPv4Bytes >> Array.rev >> baToUInt)

        let toString (ui: uint) =
            let toIPv4String (ba: byte array) = (IPAddress ba).ToString()
            ui |> (System.BitConverter.GetBytes >> Array.rev >> toIPv4String)

        let strToUInt s =
            let toIPv4Bytes (s: string) = (IPAddress.Parse s).GetAddressBytes()
            s |> (toIPv4Bytes >> Array.rev >> baToUInt )

    open System.Text.RegularExpressions

    [<LiteralAttribute>]
    let private Pattern = """^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\/([0-9]{1,2})$"""

    let private (|CIDR|_|) input =
        let m = Regex.Match(input, Pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

    let create s =
        match s with
        | CIDR [a;b;c;d;ntwWidth] when (int ntwWidth) < 33 -> (byte a,byte b,byte c,byte d,byte ntwWidth) |> CIDR |> Ok
        | _ -> Error $"{s} has invalid CIDR notation 'a.b.c.d/networkWidth'"

    let private toByteArray (cidr:CIDR) = cidr.Value |> fun (a,b,c,d,_) -> [|a;b;c;d|]

    let private toHostRange (cidr:CIDR) =
        let nw (_,_,_,_,w) = 32 - (int w)
        let p2 e = 2.0 ** e
        let m1 x = x - 1.0
        cidr.Value |> (nw >> float >> p2 >> m1 >> uint32)

    let private ipV4Addx add cidr =
        cidr |> (toByteArray >> IPv4.toUInt >> add >> IPv4.toString)

    let IPv4StartIP s =
        let addZero = ipV4Addx id
        s |> (create >> Result.map addZero)

    let IPv4EndIP s =
        let addHostRange cidr = ipV4Addx (fun ui -> ui + (toHostRange cidr)) cidr
        s |> (create >> Result.map addHostRange)

    let overlappingRanges r1 r2 =
        let toUInt f r = r |> f |> Result.map IPv4.strToUInt
        let rt1 = (toUInt IPv4StartIP r1, toUInt IPv4EndIP r1)
        let rt2 = (toUInt IPv4StartIP r2, toUInt IPv4EndIP r2)

        match (rt1,rt2) with
        | (Error _,_),_ | _,(Error _,_) -> Error "At least one CIDR is invalid"
        | (x1,x2),(y1,y2) ->  (x1 <= y2 && y1 <= x2) |> Ok
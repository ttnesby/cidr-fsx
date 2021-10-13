namespace ClasslessInterDomainRouting
type NetworkWidth = byte

type CIDR = private CIDR of byte*byte*byte*byte*NetworkWidth with
    member this.Value = let (CIDR (a,b,c,d,nw)) = this in (a,b,c,d,nw)

module CIDR =

    open System.Text.RegularExpressions
    open System.Net

    [<LiteralAttribute>]
    let Pattern = """^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\/([0-9]{1,2})$"""

    let private (|CIDR|_|) input =
        let m = Regex.Match(input, Pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

    let create s =
        match s with
        | CIDR [a;b;c;d;ntwWidth] when (int ntwWidth) < 33 -> (byte a,byte b,byte c,byte d,byte ntwWidth) |> CIDR |> Ok
        | _ -> Error "Invalid CIDR notation [a.b.c.d/networkWidth]"

    let private toByteArray (cidr:CIDR) = cidr.Value |> fun (a,b,c,d,_) -> [|a;b;c;d|]

    let IPv4StartIP s =
        let toIPv4Bytes (ba: byte array) = (IPAddress ba).MapToIPv4().GetAddressBytes()
        let toIPv4String (ba: byte array) = (IPAddress ba).MapToIPv4().ToString()
        let addOne (ba: byte array) =
            let add1 ui = ui + 1u
            let bConv ba = System.BitConverter.ToUInt32(ba,0)

            ba |> (Array.rev >> bConv >> add1 >> System.BitConverter.GetBytes >> Array.rev)

        s |> (create >> Result.map (toByteArray >> toIPv4Bytes >> addOne >> toIPv4String))




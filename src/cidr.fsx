namespace ClasslessInterDomainRouting
type NetworkWidth = byte

type CIDR = private CIDR of byte*byte*byte*byte*NetworkWidth with
    member this.Value = let (CIDR (a,b,c,d,nw)) = this in (a,b,c,d,nw)

module CIDR =

    open System.Text.RegularExpressions
    open System.Net

    module IPv4 = 

        open System.Net

        let toUInt (ba: byte array) = 
            let toIPv4Bytes (ba: byte array) = (IPAddress ba).GetAddressBytes() 
            let toUInt ba = System.BitConverter.ToUInt32(ba,0)
            
            ba |> (toIPv4Bytes >> Array.rev >> toUInt)
            
        let toString (i: uint) =
            let toIPv4String (ba: byte array) = (IPAddress ba).ToString()

            i |> (System.BitConverter.GetBytes >> Array.rev >> toIPv4String)


        


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

    let private toHostRange (cidr:CIDR) =
        let nw (_,_,_,_,w) = 32 - (int w)
        let p2 e = 2.0 ** e
        let m1 x = x - 1.0
        cidr.Value |> (nw >> float >> p2 >> m1 >> uint)

    let private ipV4Addx x cidr =
        let toIPv4Bytes (ba: byte array) = (IPAddress ba).GetAddressBytes()
        let toIPv4String (ba: byte array) = (IPAddress ba).ToString()
        let adder (ba: byte array) =
            let add (ui:uint) = ui + x
            let toUInt ba = System.BitConverter.ToUInt32(ba,0)

            ba |> (Array.rev >> toUInt >> add >> System.BitConverter.GetBytes >> Array.rev)

        cidr |> (toByteArray >> toIPv4Bytes >> adder >> toIPv4String)

    let IPv4StartIP s =
        let addOne cidr = cidr |> (ipV4Addx 1u)
        s |> (create >> Result.map addOne)

    let IPv4EndIP s =
        let addHostRange cidr = cidr |> (ipV4Addx (toHostRange cidr))
        s |> (create >> Result.map addHostRange)

    // let overlappingRanges r1 r2 =
    //     let ipToUInt ip = IPAddress.Parse
    //     let toTupleOfUInt r = (IPv4StartIP r, ) 
    //     let rt1 = (IPv4StartIP r1, IPv4EndIP r1)




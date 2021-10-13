namespace CIDR
type NetworkWidth = byte

type CIDR = private CIDR of (byte*byte*byte*byte*NetworkWidth)

module CIDR = 

    open System.Text.RegularExpressions

    [<LiteralAttribute>]
    let CIDRPattern = """^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\/([0-3]{1,2})$"""

    let (|CIDR|_|) input = 
        let m = Regex.Match(input, CIDRPattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

    let create s = 
        match s with
        | CIDR [a;b;c;d;ntwWidth] when (int ntwWidth) < 33 -> (byte a,byte b,byte c,byte d,byte ntwWidth) |> CIDR |> Ok
        | _ -> Error "Invalid CIDR notation [a.b.c.d/networkWidth]"


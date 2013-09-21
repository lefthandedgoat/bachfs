module sc

open Bespoke.Common.Osc
open System.Net
open System

OscPacket.LittleEndianByteOrder <- false

let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)

let id = ref 1
let nextId = (fun () -> id := !id + 1; !id)
    
let stop () =
    let msg = new OscMessage(superCollider, "/n_free")
    msg.Append(!id) |> ignore
    msg.Send(superCollider) |> ignore

let sinOsc freq =
    let freq =
        match box freq with
        | :? int as i -> i
        | :? float as f -> Convert.ToInt32(f)
        | _ -> 440
            
    let id = nextId()

    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(0) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(freq) |> ignore

    msg.Send(superCollider)


module sc

open Bespoke.Common.Osc
open System.Net
open System

OscPacket.LittleEndianByteOrder <- false

let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)

let id = ref 1
let nextId = (fun () -> id := !id + 1; !id)
    
let mutable playing : int list = []

let stop () =
    playing
    |> List.iter (fun i ->
        let msg = new OscMessage(superCollider, "/n_free")
        msg.Append(i) |> ignore
        msg.Send(superCollider) |> ignore)
    playing <- []

let sinOsc freq =
    let frq =
        match box freq with
        | :? int as i -> Convert.ToSingle(i)
        | :? float32 as f -> f
        | :? float as f -> Convert.ToSingle(f)
        | _ -> 440.0f
            
    let id = nextId()

    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(0) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(frq) |> ignore

    msg.Send(superCollider)
    playing <- playing @ [id]
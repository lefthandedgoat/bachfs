module sc

open Bespoke.Common.Osc
open System.Net
open System

OscPacket.LittleEndianByteOrder <- false

let nextId =
    let id = ref 1
    (fun () -> id := !id + 1; !id)

let sinOsc freq =
    let freq =
        match box freq with
        | :? int as i -> i
        | :? float as f -> Convert.ToInt32(f)
        | _ -> 440

    let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)
    let id = nextId()

    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(0) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(freq) |> ignore

    msg.Send(superCollider)

    System.Threading.Thread.Sleep(1000)

    let msg2 = new OscMessage(superCollider, "/n_free")
    msg2.Append(id) |> ignore
    msg2.Send(superCollider) |> ignore


module sc

open Bespoke.Common.Osc
open System.Net
open System

type command =
    | SinOsc of float
    | Envelope of int * int * float * command
    
OscPacket.LittleEndianByteOrder <- false

let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)
let superColliderLanguage = new IPEndPoint(IPAddress.Loopback, 57120)

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

let private sinOsc (freq : float) =                
    let id = nextId()
    let freq = freq |> Convert.ToSingle

    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(1) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(freq) |> ignore
        
    playing <- playing @ [id]
    msg.Send(superCollider)
    
let private envelope start stop (duration : float) command =
    let envelopedSinOsc (freq : float) =
        let freq = freq |> Convert.ToSingle
        let duration = duration |> Convert.ToSingle
        let msg = new OscMessage(superCollider, "/envelopedSinOsc")    
        msg.Append(start) |> ignore
        msg.Append(stop) |> ignore
        msg.Append(duration) |> ignore
        msg.Append(freq) |> ignore        
        msg.Send(superColliderLanguage)

    match command with
    | SinOsc(freq) -> envelopedSinOsc freq
    | _ -> failwith "does not compute!"
    
let play command =
    match command with
    | SinOsc(freq) -> sinOsc freq
    | Envelope(start, stop, duration, command) -> envelope start stop duration command
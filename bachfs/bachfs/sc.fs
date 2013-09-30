module sc

open Bespoke.Common.Osc
open System.Net
open System

type level = float
type attack = float
type release = float
type curve = float
type proportion = float

type command =
    | SinOsc of float
    | Envelope of level * level * attack * command
    | PercEnvelope of attack * release * level * curve * proportion * command
    | Mix of command list
    | DetectSilence of command
    
OscPacket.LittleEndianByteOrder <- false

let superCollider = new IPEndPoint(IPAddress.Loopback, 57110)
let superColliderLanguage = new IPEndPoint(IPAddress.Loopback, 57120)

let id = ref 1
let nextId = (fun () -> id := !id + 1; !id)

let private tofloat32 (f : float) = f |> Convert.ToSingle

let stop () =
    let msg = new OscMessage(superCollider, "/stopAll")
    msg.Send(superColliderLanguage) |> ignore

let private sinOsc (freq : float) =                
    let id = nextId()
    
    let msg = new OscMessage(superCollider, "/s_new")
    msg.Append("default") |> ignore
    msg.Append(id) |> ignore
    msg.Append(1) |> ignore
    msg.Append(0) |> ignore
    msg.Append("freq") |> ignore
    msg.Append(tofloat32 freq) |> ignore        
    msg.Send(superCollider)
    
let private envelope start stop duration command =
    let envelopedSinOsc (freq : float) =        
        let msg = new OscMessage(superCollider, "/envelopedSinOsc")    
        msg.Append(tofloat32 start) |> ignore
        msg.Append(tofloat32 stop) |> ignore
        msg.Append(tofloat32 duration) |> ignore
        msg.Append(tofloat32 freq) |> ignore        
        msg.Send(superColliderLanguage)

    match command with
    | SinOsc(freq) -> envelopedSinOsc freq
    | _ -> failwith "does not compute!"

let private detectSilence command =
    let matchPercEnvelope (commands : command list) (msg : OscMessage) =
        let addPercEnvelop perc =
            match perc with
            | PercEnvelope(attack, release, level, curve, proportion, SinOsc(freq)) ->
                msg.Append(tofloat32 attack) |> ignore
                msg.Append(tofloat32 release) |> ignore
                msg.Append(tofloat32 level) |> ignore
                msg.Append(tofloat32 curve) |> ignore
                msg.Append(tofloat32 freq) |> ignore
                msg.Append(tofloat32 proportion) |> ignore                
        
        let count = List.length commands
        msg.Append(count) |> ignore
        commands |> List.iter addPercEnvelop
        
    let msg = new OscMessage(superCollider, "/mixedPercEnvelopes")

    match command with
    | Mix(commands) ->
        match commands with
        | PercEnvelope(_,_,_,_,_,_) :: _ -> matchPercEnvelope commands msg
        | _ -> failwith "boom"
    | _ -> failwith "booooom"

    msg.Send(superColliderLanguage)
    
let private mix commands =
    match commands with
    | SinOsc(freq1) :: SinOsc(freq2) :: [] -> 
        let msg = new OscMessage(superCollider, "/mixedSinOsc")
        msg.Append(tofloat32 freq1) |> ignore
        msg.Append(tofloat32 freq2) |> ignore
        msg.Send(superColliderLanguage)
    | _ -> failwith "bo00om"

let play command =
    match command with
    | SinOsc(freq) -> sinOsc freq
    | Envelope(start, stop, duration, command) -> envelope start stop duration command
    | DetectSilence(command) -> detectSilence command
    | Mix(commands) -> mix commands
    | _ -> failwith "demoware goes boom"
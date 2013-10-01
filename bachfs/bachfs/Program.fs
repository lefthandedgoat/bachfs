module main

///////////////////////////////////////////////////////////////
// Originally by:                                            //
// Functional Composition by Chris Ford (@ctford)            //
// ThoughtWorks Uganda                                       //
//                                                           //
// http://github.com/ctford/functional-composition           //
// http://github.com/ctford/leipzig                          //
// http://github.com/overtone/overtone                       //
//                                                           //
//sheet music:                                               //
//http://www.ibiblio.org/mutopia/ftp/BachJS/BWV988/bwv-988-v12/bwv-988-v12-a4.pdf//
//                                                           //
// Converted to f# by Chris Holt                             //
// @lefthandedgoat                                           //
// http://github.com/lefthandedgoat/                         //
///////////////////////////////////////////////////////////////

//#r @"C:\projects\bachfs\bachfs\packages\Bespoke-OSC-Library.1.0.0\lib\Bespoke.Common.dll"
//#r @"C:\projects\bachfs\bachfs\packages\Bespoke-OSC-Library.1.0.0\lib\Bespoke.Common.Osc.dll"

open System
open sc

let SKIP = -100
let mapcat f list = 
    List.map f list
    |> List.fold (fun acc list -> acc @ list) []









///////////////////////////////////////////////////////////////
// Sine waves                                                //
///////////////////////////////////////////////////////////////

let tone freq = SinOsc freq |> play

let doubleTone freq1 freq2 = Mix [SinOsc freq1; SinOsc freq2] |> play

let beep freq duration = Envelope(1.0, 0.0, duration, (SinOsc freq)) |> play

(*
tone 300.0
doubleTone 300.0 300.0
beep 300.0 1.0
stop()
*)





///////////////////////////////////////////////////////////////
// Harmonics                                                 //
///////////////////////////////////////////////////////////////
//
let bells freqs durations harmonics =
    let defaultHarmonics = [1.0; 0.6; 0.4; 0.25; 0.2; 0.15]
    let harmonicSeries = [1.0; 2.0; 3.0; 4.2; 5.4; 6.8] //[1.0; 2.0; 3.0; 4.2; 5.4; 6.8] //[1.0; 2.0; 3.0; 4.0; 5.0; 6.0]
    let proportions = 
        harmonics @ (Seq.skip (List.length harmonics) defaultHarmonics |> List.ofSeq)
    let component' freqDuration harmonic proportion =
        let freq, duration = freqDuration
        PercEnvelope(0.01, (proportion * duration), 1.0, -4.0, proportion, (SinOsc (harmonic * freq)))
    let freqDurs = List.map2 (fun freq dur -> (freq, dur)) freqs durations
    let whole =         
        Mix (mapcat (fun freqDur -> List.map2 (fun harmonic proportion -> component' freqDur harmonic proportion) harmonicSeries proportions) freqDurs)
    DetectSilence whole |> play

let bell freq duration harmonics = bells [freq] [duration] harmonics
    
(*
beep 300.0 1.0
bell 300.0 10.0 []
*)

/////////////////////////////////////////////////////
// Psycho-acoustics                                //
/////////////////////////////////////////////////////

(*
bell 600.0 10.0 []
bell 500.0 10.0 [0.0]
bell 400.0 10.0 [0.0; 0.0]
*)

///////////////////////////////////////////////////////////////
// Equal temperament                                         //
///////////////////////////////////////////////////////////////

let midi2hertz (midi : int) = 8.1757989156 * (System.Math.Pow(2.0, (Convert.ToDouble(midi) / 12.0)))

(*
midi2hertz 69
*)

let ding midi = if midi <> SKIP then bell (midi2hertz midi) 3.0 []
(*
ding 69
*)

///////////////////////////////////////////////////////////////
// Musical events                                            //
///////////////////////////////////////////////////////////////

type note = { time : float; pitch : int}
let note time pitch = {time = time; pitch = pitch}

//note 3.0 4
//{time = 3.0; pitch = 4}

let play notes =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rec play ns =
        match ns with
        | [] -> ()
        | (time, notes) :: tail -> 
            if Convert.ToDouble(sw.ElapsedMilliseconds) >= time then 
                let notes = notes |> List.ofSeq |> List.filter (fun note -> note.pitch <> SKIP)                
                let freqs = notes |> List.map (fun note -> midi2hertz note.pitch)
                let durs = notes |> List.map (fun note -> 3.0)
                bells freqs durs []
                play tail
            else
                play ns
    let notes =                
        notes
        |> List.sortBy (fun note -> note.time)
        |> Seq.groupBy (fun note -> note.time)
        |> List.ofSeq

    play notes
    
    sw.Stop()

let evenMelody pitches =
    let times = [1 .. (List.length pitches)] |> List.mapi (fun index _ -> Convert.ToDouble(index) * (1000.0/3.0))
    let notes = List.map2 (fun time pitch -> note time pitch) times pitches
    play notes

(*
evenMelody [70 .. 80]
*)

///////////////////////////////////////////////////////////////
// Scale                                                     //
///////////////////////////////////////////////////////////////

let comp note scale = scale >> note
let compp note sharpOrFlat scale = scale >> note >> sharpOrFlat
let inc number = number + 1
let dec number = number - 1

let from offset partial = offset + partial

let scale intervals =
    let offset = ref 0
    let scale intervals = intervals |> List.map (fun i -> offset := !offset + i; !offset)    
    let reversescale intervals = intervals |> List.map (fun i -> offset := !offset - i; !offset)

    //cheese: generate three sets so we have plenty of notes to play with
    let scaled = [0] @ (scale intervals) @ (scale intervals) @ (scale intervals)
    offset := 0
    let reversescaled = [0] @ (reversescale intervals) @ (reversescale intervals) @ (reversescale intervals)
    
    (fun position -> 
        if position = SKIP then SKIP
        elif position < 0 then reversescaled.[(position * -1)]
        else scaled.[position])
   
let major = scale [2; 2; 1; 2; 2; 2; 1]

let C = from 60

//(comp C major) 0

let D = from 62
let E = from 64
let F = from 65
let G = from 67
let A = from 69
let B = from 71

let sharp = inc
let flat = dec

//(compp C flat major) 3

//alternative scales
let minor = scale [2; 1; 2; 2; 1; 2; 2]
let blues = scale [3; 2; 1; 1; 3; 2]
let pentatonic = scale [3; 2; 2; 3; 2]
let chromatic = scale [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;]

(*
[0 .. 6]  //major/minor 6, blues 5, pent 4, chromatic 11
|> List.rev
|> List.append [0 .. 7]
|> List.map (comp C major)
|> evenMelody

//FRERE JACQUES
[0; 1; 2; 0; 0; 1; 2; 0; 2; 3; 4; SKIP; 2; 3; 4; SKIP; ]
|> List.map (comp D major)
|> evenMelody 
*)

///////////////////////////////////////////////////////////////
// Melody                                                    //
///////////////////////////////////////////////////////////////

let alterTime f n = note (f n.time) n.pitch
let alterPitch f n = note n.time (f n.pitch)
let duration2Times durations start =
        let strt = ref start
        //start at 0, then map how long each subsequent note should wait to play, and drop the last note, (thats the rev/tail/rev)
        [!strt] @ (durations |> List.rev |> List.tail |> List.rev |> List.map (fun duration -> strt := !strt + duration; !strt))

let rowRowRowYourBoat =
    let pitches = 
        [0; 0; 0; 1; 2;
        // Row, row, row your boat,
        2; 1; 2; 3; 4; 
        // Gently down the stream,
        7; 7; 7; 4; 4; 4; 2; 2; 2; 0; 0; 0; 
        // (take 4 (repeat "merrily"))
        4; 3; 2; 1; 0]
        // Life is but a dream!
    let durations =
        [1.0; 1.0; 2.0/3.0; 1.0/3.0; 1.0;
        2.0/3.0; 1.0/3.0; 2.0/3.0; 1.0/3.0; 2.0;
        1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0; 1.0/3.0;
        2.0/3.0; 1.0/3.0; 2.0/3.0; 1.0/3.0; 2.0;]
    let times = duration2Times durations 0.0

    List.map2 note times pitches

let bpm beats = fun beat -> (beat * 60.0 * 1000.0) / beats
//(bpm 120.0) 4.0

(*
rowRowRowYourBoat
|> List.map (alterTime (bpm 90.0))
|> List.map (alterPitch (comp C major))
|> play
*)

//runs

let run fromsAndTos =
    let rec run fromsAndTos accum =
        match fromsAndTos with
        | [] -> accum
        | x :: [] -> accum
        | x :: y :: _ when x <= y && accum = [] -> run fromsAndTos.Tail (accum @ [x .. y])
        | x :: y :: _ when x <= y -> run fromsAndTos.Tail (accum @ [x + 1 .. y])        
        | x :: y :: _ when accum = [] -> run fromsAndTos.Tail (accum @ (List.rev [y .. x]))
        | x :: y :: _ -> run fromsAndTos.Tail (accum @ (List.rev [y .. x - 1]))

    match fromsAndTos with
    | x :: [] -> [x]
    | _ -> run fromsAndTos []

(*
run [0; 4; -1; 1; 0]
|> List.map (comp G major)
|> evenMelody
*)

//bach

let repeats timeDuration = timeDuration |> mapcat (fun (time, duration) -> [for x in [1 .. time] do yield duration])
let runs runs = runs |> mapcat run

let melody = 
    let call =
        let pitches = runs [[0; -1; 3; 0]; [4]; [1; 8]]
        let durations = repeats [(2, 1.0/4.0); (1, 1.0/2.0); (14, 1.0/4.0); (1, 3.0/2.0)]
        List.map2 (fun pitch duration -> (pitch, duration)) pitches durations
    let response =
        let pitches = runs [[7; -1; 0]; [0; -3]]
        let durations = repeats [(10, 1.0/4.0); (1, 1.0/2.0); (2, 1.0/4.0); (1, 9.0/4.0);]            
        List.map2 (fun pitch duration -> (pitch, duration)) pitches durations
    let development =
        let pitches = runs [[4]; [4]; [2; -3]; [-1; -2]; [0]; [3; 5]; [1]; [1]; [1; 2]; [-1; 1; -1]; [5; 0]]
        let durations = repeats [(1, 3.0/4.0); (12, 1.0/4.0); (1, 1.0/2.0); (1, 1.0); (1, 1.0/2.0); (12, 1.0/4.0); (1, 3.0)]
        List.map2 (fun pitch duration -> (pitch, duration)) pitches durations    
    let whole = call @ response @ development
    let pitches = whole |> List.map(fun (pitch, duration) -> pitch)
    let durations = whole |> List.map(fun (pitch, duration) -> duration)
    let times = duration2Times durations 0.5
    List.map2 note times pitches
    
let bass =
    let triples notes = notes |> mapcat (fun x -> [x; x ;x]) 
    let pitches = (run [-7; -10] |> triples) @ (run [-12; -10] |> triples) @ run [5; 0] @ run [6; 0]
    let durations = repeats [(21, 1.0); (13, 1.0/4.0)]
    let times = duration2Times durations 0.0
    List.map2 note times pitches 

(*
bass
|> List.map (alterTime (bpm 90.0))
|> List.map (alterPitch (comp G major))
|> play
*)

///////////////////////////////////////////////////////////////
// Canon                                                     //
///////////////////////////////////////////////////////////////

let canon f notes = notes @ (f notes)

//varieties  of canon
let simple wait = List.map (fun note -> { note with time = note.time + wait})

let interval interval = List.map (fun note -> { note with pitch = note.pitch + interval})

let mirror = List.map (fun note -> { note with pitch = note.pitch * -1 })
let crab = List.map (fun note -> { note with time = note.time * -1.0 }) //broken
let table = mirror >> crab

(*
rowRowRowYourBoat
|> (canon (simple 4.0))
|> List.map (alterTime (bpm 90.0))
|> List.map (alterPitch (comp C major))
|> play
*)

let canoneAllaQuarta = canon ((interval -3) >> mirror >> (simple 3.0))

(*
melody
|> canoneAllaQuarta
|> List.append bass
|> List.map (alterTime (bpm 90.0))
|> List.map (alterPitch (comp G major))
|> play
*)

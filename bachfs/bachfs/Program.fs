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
// Converted to f# by Chris Holt                             //
// @lefthandedgoat                                           //
// http://github.com/lefthandedgoat/                         //
///////////////////////////////////////////////////////////////

//#r @"C:\projects\bachfs\bachfs\packages\Bespoke-OSC-Library.1.0.0\lib\Bespoke.Common.dll"
//#r @"C:\projects\bachfs\bachfs\packages\Bespoke-OSC-Library.1.0.0\lib\Bespoke.Common.Osc.dll"

open System
open sc

let SKIP = -100










///////////////////////////////////////////////////////////////
// Sine waves                                                //
///////////////////////////////////////////////////////////////

let tone freq = SinOsc freq |> play

let doubleTone freq1 freq2 = Mix [SinOsc freq1; SinOsc freq2] |> play

let beep freq duration = Envelope(1.0, 0.0, duration, (SinOsc freq)) |> play

tone 300.0
doubleTone 300.0 300.5
beep 300.0 1.0
stop()






///////////////////////////////////////////////////////////////
// Harmonics                                                 //
///////////////////////////////////////////////////////////////

let bell freq duration harmonics =
    let defaultHarmonics = [1.0; 0.6; 0.4; 0.25; 0.2; 0.15]
    let harmonicSeries = [1.0; 2.0; 3.0; 4.2; 5.4; 6.8] //[1.0; 2.0; 3.0; 4.2; 5.4; 6.8] //[1.0; 2.0; 3.0; 4.0; 5.0; 6.0]
    let proportions = 
        harmonics @ (Seq.skip (List.length harmonics) defaultHarmonics |> List.ofSeq)
    let component' harmonic proportion =
        PercEnvelope(0.01, (proportion * duration), 1.0, -4.0, proportion, (SinOsc (harmonic * freq)))
    let whole = 
        Mix (List.map2 component' harmonicSeries proportions)
    DetectSilence whole |> play


beep 300.0 1.0
bell 300.0 10.0 []


/////////////////////////////////////////////////////
// Psycho-acoustics                                //
/////////////////////////////////////////////////////

bell 600.0 10.0 []
bell 500.0 10.0 [0.0]
bell 400.0 10.0 [0.0; 0.0]


///////////////////////////////////////////////////////////////
// Equal temperament                                         //
///////////////////////////////////////////////////////////////

let midi2hertz (midi : int) = 8.1757989156 * (System.Math.Pow(2.0, (Convert.ToDouble(midi) / 12.0)))

midi2hertz 69

let ding midi = if midi <> SKIP then bell (midi2hertz midi) 3.0 []

ding 69


///////////////////////////////////////////////////////////////
// Musical events                                            //
///////////////////////////////////////////////////////////////

type note = { time : float; pitch : int}
let note time pitch = {time = time; pitch = pitch}

note 3.0 4
{time = 3.0; pitch = 4}

let play notes =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rec play notes =
        match notes with
        | [] -> ()
        | note :: _ -> 
            if Convert.ToDouble(sw.ElapsedMilliseconds) >= note.time then 
                ding note.pitch
                play notes.Tail
            else
                play notes

    play notes
    sw.Stop()

let evenMelody (pitches : int list) =
    let times = [1 .. pitches.Length] |> List.mapi (fun index _ -> Convert.ToDouble(index) * (1000.0/3.0))
    let notes = List.map2 (fun time pitch -> note time pitch) times pitches
    play notes

evenMelody [70 .. 80]


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
    let scaled = [!offset] @ (intervals |> List.map (fun i -> offset := !offset + i; !offset))
    (fun position -> 
        if position = SKIP then SKIP
        else scaled.[position])
    

let major = scale [2; 2; 1; 2; 2; 2; 1]

let C = from 60
let D = from 62
let E = from 64
let F = from 65
let G = from 67
let A = from 69
let B = from 71

//(comp C major) 0

let sharp = inc
let flat = dec

//(compp C flat major) 3

//alternative scales
let minor = scale [2; 1; 2; 2; 1; 2; 2]
let blues = scale [3; 2; 1; 1; 3; 2]
let pentatonic = scale [3; 2; 2; 3; 2]
let chromatic = scale [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;]

[0 .. 5]  //major/minor 6, blues 5, pent 4, chromatic 11
|> List.rev
|> List.append [0 .. 6]
|> List.map (comp C blues)
|> evenMelody

//FRERE JACQUES
[0; 1; 2; 0; 0; 1; 2; 0; 2; 3; 4; SKIP; 2; 3; 4; SKIP; ]
|> List.map (comp D major)
|> evenMelody 

///////////////////////////////////////////////////////////////
// Melody                                                    //
///////////////////////////////////////////////////////////////

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

    let times = 
        let start = ref 0.0
        //start at 0, then map how long each subsequent note should wait to play, and drop the last note, (thats the rev/tail/rev)
        [!start] @ (durations |> List.rev |> List.tail |> List.rev |> List.map (fun duration -> start := !start + duration; !start))

    List.map2 note times pitches

let bpm beats = fun beat -> (beat * 60.0 * 1000.0) / beats
//(bpm 120.0) 4.0

rowRowRowYourBoat
|> List.map (fun n -> note ((bpm 90.0) n.time) n.pitch )
|> List.map (fun n -> note n.time ((comp C major) n.pitch))
|> play
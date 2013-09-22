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












///////////////////////////////////////////////////////////////
// Sine waves                                                //
///////////////////////////////////////////////////////////////

let tone = sinOsc
let doubleTone freq1 freq2 =
    tone freq1
    tone freq2

tone 300
doubleTone 300 300
stop()

beep 300 1
System.Console.ReadKey()
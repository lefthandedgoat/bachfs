bachfs
======

ctford style function composition in f#

supercollider
=============

Install supercollider:
http://supercollider.sourceforge.net/downloads/

Open supercollider:
C:\Program Files (x86)\SuperCollider-3.6.5\scide.exe

Start Server: Ctrl+B OR right click in bottom right corner and click Boot Server

paste lines 4 to 56 into ide window, highlight and Shift+Enter:
https://github.com/lefthandedgoat/bachfs/blob/master/bachfs/scFiles/sc.txt

You should see 'OSCFunc(/mixedPercEnvelopes, nil, nil, nil)' in the bottom right.


f#
==
Send line 20 and 21 to interactive

Highlight all of sc.fs (except module definition) and send to interactive (Alt+Enter unless resharper has that binding, and you will need to rebind).

Highlight all of Program.fs (from 26 down) and send to interactive.

Now start at the top and play through the examples between the comments blocks 


`(*

tone 300.0

doubleTone 300.0 300.0

beep 300.0 1.0

stop()

*)`

For a more realistic bell sound change 

`let harmonicSeries = [1.0; 2.0; 3.0; 4.0; 5.0; 6.0] //[1.0; 2.0; 3.0; 4.2; 5.4; 6.8] //[1.0; 2.0; 3.0; 4.0; 5.0; 6.0]`

to use the first commented values and re-send everything to interactive.


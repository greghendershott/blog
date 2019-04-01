    Title: Clear interrupts
    Date: 2013-02-12T07:00:00
    Tags: life, nostalgia, technology

For some reason [CLI](https://en.wikipedia.org/wiki/Interrupt_flag)
popped into my head the other day.

`CLI` is the 808x instruction to clear maskable interrupts. If you're
writing a routine to service a hardware interrupt, you do a `CLI` early
in your routine — to prevent _another_ hardware interrupt
from causing your routine to be re-entered. Neglecting this invites
the most delightful form of bug, the intermittent bug.

![Roland MPU-401](https://upload.wikimedia.org/wikipedia/commons/0/06/Roland_MPU-401.jpg "Roland MPU-401")

<!-- more -->

I spent a lot of time struggling with this stuff early in my coding
career, writing a MIDI sequencer. The
[Roland MPU-401](https://en.wikipedia.org/wiki/MPU-401) MIDI interface
would hit IRQ 8 when some MIDI bytes arrived, or when a timer
ticked. I remember eagerly reading BYTE magazine articles and learning
how write ISRs (interrupt service routines) by trial and error. Many
trials and many errors. There was no internet search much less
StackOverflow.com back then. (Also, we lived in a shoebox in the
middle of the road.)

In real life we have interruptions. The worst is when you're
interrupted, then that interruption is interrupted. And so on. People
don't have stacks they can pop instantly to return to their prior
context. Instead of popping a stack, the process is akin to flailing
around with Google search, throwing around keywords and trying to
sniff out the right track.

It's too bad there's no `CLI` instruction in real life.

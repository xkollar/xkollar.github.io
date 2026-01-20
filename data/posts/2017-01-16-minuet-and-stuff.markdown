---
title: Minuet and Stuff…
author: xkollar
tags: Music
---
(Disclaimer: It is absolutely possible that I have no Idea what I am writing about…)

I still don't fully understand how but I have started to take piano lessons.
Several people were involved and I can not thank them enough for making my
childhood dream come true.

Music is fascinating on so many levels… exploring its world from (for me) new
perspective, I will now share several thoughts.

Notation
--------

Looking at musical notation as a computer scientist I see something like a
formalism that lets me express thoughts and then someone (or something)
interpret them. Single, unified form of communication, way to preserve and
transfer thoughts. Source code if you like.

But the world is little more complex than that. Analogy with source code goes a little
further than that. Just as there is no single programming language, everybody is free
do develop their own way of notation. And just like with programming languages,
once you know principles, you are (with some effort) able to understand most of them
(up to some esoteric stuff).

Of course the analogy is not perfect. For example beauty is often in
variations of interpretations (whereas interpretation of code is
usually more strict). Little (one might say) imperfections that make difference
between what would software generate and what would professional pianist
produce is what makes it unique and beautiful. (Even though software is probably
getting there.)

And speaking of variations of the notations. Computer scientists and/or
programmers are people who like formalisms and so they too took the challenge
and created several of their own. I used to find it hard not to try to create
some kind of formalism myself. Well, until I tried to typeset some (rather
uncomplicated) music using some of existing notations (LilyPond, ABC 2, even
some MIDI).

It helped me realize that the usual musical notation is optimized for reading.
There are some basic components (notes, rests, …) but then there is whole zoo
of high-level symbols expressing some kind of complex things. And that can be
binding. On the one hand, it is convenient for reading, but on the other hand,
you need special symbol for everything as they usually do not compose well.

Interesting thoughts related to this:

[Chiptune: Pushing the Limits Using Constraints](https://www.youtube.com/watch?v=_7k25pwNbj8)

Variations
----------

Let us have a look at [Minuet in G major, BWV Anh. 114][wiki:Minuet]. If you
don't know it:

[Minuet In G Major](https://www.youtube.com/watch?v=IzbJiz_DO7E)

The piece is simple enough to be recommended for beginners such as myself. Yet
trying to use ABC Notation I have very soon hit some hurdles (may have been
caused by my lack of familiarity with the notation and/or musical theory). Here
is my attempt that took some non-trivial time.

~~~ {.abc-render}
X:1
T:Minuet
C:Christian Petzold, from Notebook for Anna Magdalena Bach
B:Notebook for Anna Magdalena Bach
Z:abc-transcription xkollar <http://tenticle.dev/>
I:linebreak $
M:3/4
K:G
Q:"Moderato"
%%%%%%%%%%%%%%%%%%
%%score {Rh | Lh}
V:Rh clef=treble
L:1/4
!mf!
!5!D' (!1!G/2A/2B/2C'/2 |(D')!tenuto!G)!tenuto!G|!5!E'(!1!C'/2D'/2E'/2F'/2|(G')!tenuto!G)!tenuto!G|!4!C'(D'/2C'/2B/2A/2|B)(C'/2B/2A/2!1!G/2| $
V:Lh clef=bass
L:1/4
!5!!2!!1![G,B,D]2 !3!A,| !2!B,3| !1!C3| B,3| A,3| G,3|
%%%%%%%%%%%%%%%%%%
V:Rh
!2!F) (!1!G/2A/2B/2G/2|{B}A3)| !5!D'(G/2A/2B/2C'/2|(D')!tenuto!G)!tenuto!G|E'(!1!C'/2D'/2E'/2F'/2| $
V:Lh
DB,G,| ">)"!1!D(!5!D,/2!1!C/2B,/2A,/2|!2!B,2)A,|(G,B,)G,|C3|
%%%%%%%%%%%%%%%%%%
V:Rh
(G')!tenuto!G)!tenuto!G|!4!C'(D'/2C'/2B/2A/2|B) (C'/2B/2A/2G/2|A) (B/2A/2!1!G/2!2!F/2|G3) :| $
V:Lh
!2!A, (C/2B,/2A,/2G,/2|!1!A,2) (!3!F,|!2!G,2) (!1!B,|!2!C) !1!!tenuto!D !tenuto!!5!D, | ">)"!1!G,2!5!G,, :|
%%%%%%%%%%%%%%%%%%
V:Rh
|: !mp!
!5!B' (G'/2A'/2B'/2G'/2|A') (!1!D'/E'/F'/D'/|G') (E'/F'/!4!G'/D'/| !3!^C' B/C'/ !1!A)| (!<(! A/B/^C'/!1!D'/E'/!<)!F'/ | !mf! G') F' E'| $
%%setfont-4 _  10
V:Lh
|:!1!G,3|F,3|E,G,E,|A,2A,,| "^$41-4" A,3|B,D^C|
%%%%%%%%%%%%%%%%%%
%%score {(Rh Rh2) | (Lh2 Lh)}
V:Rh
(!5!F' !1!A) ^C' | D'3 | !5!D' (G/!2!F/ !1!G) | !5!E' (G/F/ G) | !5!D' C' B | $
V:Rh2
L:1/4
x3 | !>(!x x !>)!!p!x
V:Lh
(!1!D !4!F,) A, | !1!D !5!D, !1!=C | B,2B, | !2!C2 (!2!C | !3!B,) A, G, |
V:Lh2 clef=bass
L:1/4
x3 | x3 | zD2 | zE2 | x3 |
%%%%%%%%%%%%%%%%%%
%%score {Rh | Lh}
V:Rh
(!2!A/!1!G/!3!F/G/ A) | (!1!D/E/F/!1!G/A/B/ | C') B A | (B/D'/ !tenuto!!1!G) !tenuto!!2!F | !1!!2!!5![B,DG]3:| $
V:Lh
D2 z | !5!D,2 (!3!F, | E,) G, !3!F, | (!1!G, !5!B,,) !3!D,| ">)"!tenuto!G,">)"!tenuto!!2!D,!tenuto!!5!G,,:|
%%%%%%%%%%%%%%%%%%
~~~

Fingering and some notations are actually work of my teacher. She enhanced
the one that I have randomly downloaded from the internet and brought with me to the lesson.
Then I have learned that typesetting music can be as complex as with text.
When they are preparing a book with sheet music, editors
usually have to think hard to use current notation to express the piece
in "the right way", whatever that in particular context means.

Another funny thing is that quite a lot of notation in this transcription is somewhat
redundant once you know that it is [baroque][wiki:Baroque_music]. For example
most of slurs could have been left out as [legato][wiki:Legato]
was implied over [steps and skips][wiki:Steps_and_skips] in music from that era (and
probably would not be used in the original). Nevertheless, music
notations are made to be convenient for reading… so some extra symbols can
become handy to remind someone who does not happen to know this. And also with
annotation being more explicit it is much easier to interpret the music with
computer.

Fingering
---------

Another interesting thing is fingering. One might be inclined to think that
there is some canonical fingering, probably backed by a model of hands and
instrument or something… yet as it turns out, there is not. It is usually up to
editor to decide this. And then the interpreter can find out that he does not like it
and not use it at all.

To make things even more confusing, sometimes editor might use less natural
fingering to force interpreter to make slight pauses, somehow "helping" with
final impression.

Conclusion
----------

When learning music it is good to ask questions ("why is it this way?", …), but
always be prepared that the best answer you can get is "because it is more
convenient this way, …". And it is not necessarily a bad thing <abbr title="☺️ :smiling_face:">`:-)`</abbr>.

[wiki:Baroque_music]: https://en.wikipedia.org/wiki/Baroque_music
[wiki:Legato]: https://en.wikipedia.org/wiki/Legato
[wiki:Minuet]: https://en.wikipedia.org/wiki/Minuet_in_G_major,_BWV_Anh._114
[wiki:Steps_and_skips]: https://en.wikipedia.org/wiki/Steps_and_skips

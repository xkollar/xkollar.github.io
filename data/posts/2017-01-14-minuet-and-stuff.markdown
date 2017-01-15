---
title: Minuet and Stuff…
author: xkollar
tags: Music
---

(Disclaimer: It is absolutely possible that I have no Idea what I am writing about…)

I still don't fully understand how, but I have started to take piano lessons.
Several people were involved and I can not thank them enough.

One of things fascinating about music is its notation. As a computer scientist
I used to find it hard not to try to create some kind of formalism to describe
it in text form. Well, until I tried to typeset some (rather uncomplicated)
music using some of existing notations (LilyPond and ABC 2).

Let us have a look at [Minuet in G major, BWV Anh. 114][wiki:Minuet].

[Minuet In G Major](https://www.youtube.com/watch?v=IzbJiz_DO7E)

The piece is simple enough to be recommended for beginners. Yet trying to use
ABC Notation I have very soon hit some hurdles. Here is my attempt that took
some non-trivial time.

~~~ {.abc-render}
X:1
T:Minuet
C:Christian Petzold, from Notebook for Anna Magdalena Bach
B:Notebook for Anna Magdalena Bach
Z:abc-transcription xkollar <http://xkollar.github.io/>
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
DB,G,| ">)"!1!D(!1!D,/2!5!C/2B,/2A,/2|!2!B,2)A,|(G,B,)G,|C3|
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
|:!1!G,3|F,3|E,G,E,|A,2A,,|"^$41-4"A,3|B,D^C|
%%%%%%%%%%%%%%%%%%
%%score {(Rh Rh2) | (Lh2 Lh)}
V:Rh
(!5!F' !1!A) ^C' | D'3 | !5!D' (G/!2!F/ !1!G) | !5!E' (G/F/ G) | !5!D' C' B | $
V:Rh2
L:1/4
X1 | !>(!x x !>)!!p!x
V:Lh
(!1!D !4!F,) A, | !1!D !5!D, !1!=C | B,2B, | !2!C2 (!2!C | !3!B,) A, G, |
V:Lh2 clef=bass
L:1/4
X2 | zD2 | zE2 | X |
%%%%%%%%%%%%%%%%%%
%%score {Rh | Lh}
V:Rh
(!2!A/!1!G/!3!F/G/ A) | (!1!D/E/F/!1!G/A/B/ | C') B A | (B/D'/ !tenuto!!1!G) !tenuto!!2!F | !1!!2!!5![B,DG]3:| $
V:Lh
D2 z | !5!D,2 (!3!F, | E,) G, !3!F, | (!1!G, !5!B,,) !3!D,| ">)"!tenuto!G,">)"!tenuto!!2!D,!tenuto!!5!G,,:|
%%%%%%%%%%%%%%%%%%
~~~

Fingering and some notations are actually work of my teacher.

The funny thing is, that quite a lot of this notation is little redundant once
you know that the piece is [baroque][wiki:Baroque_music]. Then almost all
[legato][wiki:Legato] annotations become redundant (or at least probably would
not be used in original), as legato was implied over [steps and
skips][wiki:Steps_and_skips] in baroque music. On the other hand, music
notations are made to be convenient for reading… so some extra notations can
become handy to remind someone who does not happen to know this.

Another interesting thing is fingering. One might be inclined to think that
there is some canonical fingering, probably backed by some models of hands or
something… yet as it turns out, there is not. It is usually up to editor to
decide this. And then interpret can find out that he does not like it and not
use it at all.

[wiki:Baroque_music]: https://en.wikipedia.org/wiki/Baroque_music
[wiki:Legato]: https://en.wikipedia.org/wiki/Legato
[wiki:Minuet]: https://en.wikipedia.org/wiki/Minuet_in_G_major,_BWV_Anh._114
[wiki:Steps_and_skips]: https://en.wikipedia.org/wiki/Steps_and_skips

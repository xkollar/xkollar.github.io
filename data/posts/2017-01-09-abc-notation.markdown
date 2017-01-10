---
title: ABC Notation Support
author: xkollar
tags: Haskell, Hakyll, Music
---
Another day (or should I rather say night?) well spent. This time I have added
support for [ABC Notation](https://en.wikipedia.org/wiki/ABC_notation). From
now on I can use some reasonably nice musical notation. Next time I might
finally add support for [LilyPond](http://lilypond.org/) that was originally
planed.

Here is an example (part of my very first piano homework).

~~~ {.abc-render}
X:1
T:Etude
C:Dle C. Czerného upravil A.S.
M:3/4
K:C
Q:"Moderato"
V:1 name=Rh
L:1/4
!mf!
(!1!ceg|!4!f)d2|
(!3!eg3/2c/2|d3)|(!1!ceg|
fd2)|((ee/2)g/2 d/2g/2|c3)|
V:2 clef=bass name=Lh
L:1/16
!5!C,G,E,G, C,G,E,G, C,G,E,G,| !5!B,,!1!G,!3!D,G, B,,G,D,G, B,,G,!2!F,G,|
!5!C,G,E,G, C,G,E,G, C,G,E,G,| !5!B,,!1!G,!3!D,G, B,,G,D,G, B,,G,!2!F,G,| !5!C,G,E,G, C,G,E,G, C,G,E,G,|
!5!B,,G,D,G, B,,G,D,G, B,,G,F,G,| C,G,E,G, C,G,E,G, !5!B,,!1!G,!2!F,G,| !5!C,G,E,G, C,G,E,G, C,4|
~~~

In ideal case one day there will be also generated midi with embedded player.

For those interested (and for my future self) some related resources follow.

ABC Music Editors
-----------------

* <http://moinejf.free.fr/js/edit-1.xhtml>
* <https://abcjs.net/abcjs-editor.html>

Syntax and Examplex
-------------------

* <http://abcwiki.selfthinker.org/abc:syntax>
* <http://trillian.mit.edu/~jc/music/abc/doc/ABCtut.html>
* <http://abcnotation.com/examples>
* <http://www.ucolick.org/~sla/abcmusic/piano/piano.html>

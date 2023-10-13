---
title: "Monty Hall Problem"
author: xkollar
tags: Math
---

I'm writing this just to help myself compose my own thoughts on the topic.
The problem while seemingly simple is often source of confusion. I have
recently encountered fresh debate on the topic on [Hacker
News](https://news.ycombinator.com/item?id=37829926) that started quite
innocently (as conversations there often do).

For good (though somewhat lengthy) introduction to the problem (and all it's
possible variants) see
[Wikipedia](https://en.wikipedia.org/wiki/Monty_Hall_problem).

Here is re-creation of a scribble I made while thinking about the problem.

~~~ {.dot-render}
digraph G {
    graph [bgcolor=transparent];
    node [style=filled;fillcolor=white];
    edge [fontsize=10];
    ranksep=2.5;
    #splines=false;

    init [label="⊥"];

    # Lol, it needs to be called cluster
    subgraph cluster_1 {
        graph [bgcolor=lightblue;nodesep=2];
        CGG[label="🚘🐐🐐"]; GCG[label="🐐🚘🐐"]; GGC[label="🐐🐐🚘"];
        color=blue;
    };

    init -> CGG [label="1/3"];
    init -> GCG [label="1/3"];
    init -> GGC [label="1/3"];

    subgraph cluster_2_1 {
        graph [bgcolor=lightblue];
        "[C]GG"[label="[🚘]🐐🐐",color=red];
        "[G]CG"[label="[🐐]🚘🐐"];
        "[G]GC"[label="[🐐]🐐🚘"];
        color=blue;
    };

    subgraph cluster_2_2 {
        graph [bgcolor=lightblue];
        "C[G]G"[label="🚘[🐐]🐐"];
        "G[C]G"[label="🐐[🚘]🐐",color=red];
        "G[G]C"[label="🐐[🐐]🚘"];
        color=blue;
    };

    subgraph cluster_2_3 {
        graph [bgcolor=lightblue];
        "CG[G]"[label="🚘🐐[🐐]"];
        "GC[G]"[label="🐐🚘[🐐]"];
        "GG[C]"[label="🐐🐐[🚘]",color=red];
        color=blue;
    };

    CGG -> "[C]GG" [label="1/3"];
    CGG -> "C[G]G" [label="1/3"];
    CGG -> "CG[G]" [label="1/3"];

    GCG -> "[G]CG" [label="1/3"];
    GCG -> "G[C]G" [label="1/3"];
    GCG -> "GC[G]" [label="1/3"];

    GGC -> "[G]GC" [label="1/3"];
    GGC -> "G[G]C" [label="1/3"];
    GGC -> "GG[C]" [label="1/3"];

    subgraph cluster_3_1a {
        graph [bgcolor=lightblue];
        "[C]XG"[label="[🚘]🙅🐐",color=red];
        "[G]XC"[label="[🐐]🙅🚘",color=green];
        color=blue;
    };
    subgraph cluster_3_1b {
        graph [bgcolor=lightblue];
        "[C]GX"[label="[🚘]🐐🙅",color=red];
        "[G]CX"[label="[🐐]🚘🙅",color=green];
        color=blue;
    };
    subgraph cluster_3_2a {
        graph [bgcolor=lightblue];
        "X[C]G"[label="🙅[🚘]🐐",color=red];
        "X[G]C"[label="🙅[🐐]🚘",color=green];
        color=blue;
    };
    subgraph cluster_3_2b {
        graph [bgcolor=lightblue];
        "C[G]X"[label="🚘[🐐]🙅",color=green];
        "G[C]X"[label="🐐[🚘]🙅",color=red];
        color=blue;
    };
    subgraph cluster_3_3a {
        graph [bgcolor=lightblue];
        "XC[G]"[label="🙅🚘[🐐]",color=green];
        "XG[C]"[label="🙅🐐[🚘]",color=red];
        color=blue;
    };
    subgraph cluster_3_3b {
        graph [bgcolor=lightblue];
        "CX[G]"[label="🚘🙅[🐐]",color=green];
        "GX[C]"[label="🐐🙅[🚘]",color=red];
        color=blue;
    };

    "[C]GG" -> "[C]XG" [label="1/2"];
    "[C]GG" -> "[C]GX" [label="1/2"];
    "C[G]G" -> "C[G]X";
    "CG[G]" -> "CX[G]";

    "[G]CG" -> "[G]CX";
    "G[C]G" -> "X[C]G" [label="1/2"];
    "G[C]G" -> "G[C]X" [label="1/2"];
    "GC[G]" -> "XC[G]";

    "[G]GC" -> "[G]XC";
    "G[G]C" -> "X[G]C";
    "GG[C]" -> "XG[C]" [label="1/2"];
    "GG[C]" -> "GX[C]" [label="1/2"];

    subgraph cluster_4_1_1 {
        graph [bgcolor=lightblue];
        "[C]G"[label="[🚘]🐐",color=red];
        "[G]C"[label="[🐐]🚘",color=green];
        color=blue;
    };

    subgraph cluster_4_1_2 {
        graph [bgcolor=lightblue];
        "C[G]"[label="🚘[🐐]",color=green];
        "G[C]"[label="🐐[🚘]",color=red];
        color=blue;
    };

    "[C]XG" -> "[C]G";
    "[C]GX" -> "[C]G";
    "C[G]X" -> "C[G]";
    "CX[G]" -> "C[G]";

    "[G]CX" -> "[G]C";
    "X[C]G" -> "[C]G";
    "G[C]X" -> "G[C]";
    "XC[G]" -> "C[G]";

    "[G]XC" -> "[G]C";
    "X[G]C" -> "[G]C";
    "XG[C]" -> "G[C]";
    "GX[C]" -> "G[C]";
}
~~~

Each layer represents what we know about possible states of the universe
as we progress through the problem.

Obviously at the beginning (layer 0) we know nothing (with probability 1).

Layer 1 emerges as we are presented with tree doors, car behind one of them.
Each case has probability of 1/3.

Layer 2 represents our choice of a door. As we don't know anything, we choose
randomly. Now our multiverse has 9 possible states (still uniform probability,
now 1/9). If we ask what is the probability of us having chosen the correct
door, we just sum the probabilities of all matching states (red border) and
arrive at probability 1/3 (3 * 1/9). (As an exercise try thinking what
would have happened if we chose non-uniform strategy here.)

Alternative thinking about layer two is, that by choice we have collapsed
the multiverse (grouped by the collapse).

Layer 3 is when host opens one of remaining doors that has goat (there might
be one or two such doors, if there are two host chooses randomly... As an
exercise think about what would it mean if host did not choose uniformly). Red
states are the ones where we win by keeping our door, green ones where we win
by switching. Interesting thing to note that both at level of collapsed
multiverse and uncollapsed one (whole layer) probabilities are stay 1/3 win
and switch 2/3 with!

Last layer is an attempt at demonstration for one of cases where confusion is
coming from. The question whether the car is behind left or right door is
different from whether it is the door we choose or the other one.

If you look at uncollapsed multiverse and ask about probabilities for left
versus right, it is uniform (1/2 for each of remaining door). But once you
have chosen a door suddenly asymmetry emerges. Part of our multiverse have
collapsed. If our door is the left one of remaining ones, then it has only 1/3
probability of hiding a car.

The question is not whether it is behind left or right door (in the
uncollapsed multiverse), but whether is is behind left or right door given our
choice of door.

It almost feel like confusion here is similar one to
[Gambler's_fallacy](https://en.wikipedia.org/wiki/Gambler's_fallacy) where
people assume that after head flipping another head is less probable not
realising that by first flip part of multiverse available to them has
collapsed and now it is 1/2 again.

~~~ {.dot-render}
digraph G {
    graph [bgcolor=transparent];
    node [style=filled;fillcolor=white];
    edge [fontsize=10];
    ranksep=1;

    init[label="ε"];
    init -> H [label="1/2"];
    init -> T [label="1/2"];

    subgraph cluster_1_a {
        graph [bgcolor=lightblue];
        "HH"; "HT"
    };
    subgraph cluster_1_b {
        graph [bgcolor=lightblue];
        "TH"; "TT"
    };

    H -> HH [label="1/2"];
    H -> HT [label="1/2"];

    T -> TH [label="1/2"];
    T -> TT [label="1/2"];
}
~~~

Maybe it is about how the question is phrased? A nice and concise answer is
possible only because some symmetries present. If those were broken the answer
might not be as simple as "always switch for 2/3 chance to win car" but more
like (if you chose door 1 and host showed you door 2 then switch for chance P,
but if 3 then stay for chance Q, if you choose door 2…).

Interesting insights from the thread:

* Try thinking how would it work with 100 doors.
* By choosing the other door you are actually choosing two doors
  instead of one.

Ideas:

* Interactive/scriptable version where it would be possible
  to play with different strategies of both host and player
  and see how final probabilities change.

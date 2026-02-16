---
title: "Teaching Probability"
author: xkollar
tags: Math, Probability
---

Here is a crude representation of what I use as a visual aid for explaining probability: Unit Square.

<figure>
<svg viewBox="0 0 150 150" fill="white">
  <style>
  text {
    font: 13px sans-serif;
    fill: black;
  }
  </style>
  <!-- Background -->
  <rect x="0" y="0" width="150" height="150" fill="white" />
  <g transform="translate(25,25)">
      <!-- Border -->
      <rect x="0" y="0" width="100" height="100" fill="black" stroke="black" stroke-width="2" />
      <!-- Rect -->
      <rect x="0" y="0" width="100" height="100" fill="white" stroke-width="0" />
      <!-- Curly -->
      <g transform="">
        <text x="50" y="-8" text-anchor="middle">1</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 46.5 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 46.5 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <g transform="translate(100,0) rotate(90)">
        <text x="50" y="-8" text-anchor="middle">1</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 46.5 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 46.5 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
  </g>
</svg>
</figure>

Boringly simple, isn't it?

It has some nice properties. Let's have an event $A$ that will happen with probability $\mathbb{P}(A)$.

<svg viewBox="0 0 150 150" fill="white">
  <style>
  text {
    font: 13px sans-serif;
    fill: black;
  }
  </style>
  <!-- Background -->
  <rect x="0" y="0" width="150" height="150" fill="white" />
  <g transform="translate(25,25)">
      <!-- Border -->
      <rect x="0" y="0" width="100" height="100" fill="black" stroke="black" stroke-width="2" />
      <!-- Rect -->
      <rect x="0" y="0" width="100" height="100" fill="white" stroke-width="0" />
      <!-- Event A -->
      <rect x="0" y="0" width="100" height="35" fill="#fcc" stroke-width="0" />
      <!-- Curly -->
      <g transform="">
        <text x="50" y="-8" text-anchor="middle">1</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 46.5 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 46.5 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <g transform="translate(100,0) rotate(90)">
        <text x="16" y="-8" text-anchor="middle">ℙ(A)</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 13.75 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 13.75 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <text x="50" y="22" text-anchor="middle">ℙ(A)</text>
  </g>
</svg>

Notice that both the length of the shorter edge of the red region and the area
of the red region are equal to $\mathbb{P}(A)$. While nice and useful, this is
simultaneously horrible as it stretches your brain to think about
probability as a length (one-dimensional) and as an area (two-dimensional),
neither of which it actually is, since it is just a ratio (dimensionless). Anyway, if
you can stomach that, you can use it to draw pictures that can help you reason
about probabilities.

Let's look at an illustrative example in the following picture.

<svg viewBox="0 0 300 150" fill="white">
  <style>
  text {
    font: 11px sans-serif;
    fill: black;
  }
  </style>
  <!-- Background -->
  <rect x="0" y="0" width="300" height="150" fill="white" />
  <g transform="translate(25,25)">
      <!-- Border -->
      <rect x="0" y="0" width="100" height="100" fill="black" stroke="black" stroke-width="2" />
      <!-- Rect -->
      <rect x="0" y="0" width="100" height="100" fill="white" stroke-width="0" />
      <!-- Events -->
      <rect x="0" y="0" width="100" height="40" fill="#f006" stroke-width="0" />
      <rect x="40" y="0" width="60" height="40" fill="#00f6" stroke-width="0" />
      <rect x="70" y="40" width="30" height="60" fill="#00f6" stroke-width="0" />
      <!-- Curly -->
      <g transform="translate(40.5,0)">
        <text x="30" y="-8" text-anchor="middle">ℙ(B|A)</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 26.25 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 26.25 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <g transform="translate(100,0) rotate(90)">
        <text x="16" y="-8" text-anchor="middle">ℙ(A)</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 16.25 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 16.25 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <text x="70" y="23" text-anchor="middle">ℙ(A&B)</text>
  </g>
  <g transform="translate(175,25)">
      <!-- Border -->
      <rect x="0" y="0" width="100" height="100" fill="black" stroke="black" stroke-width="2" />
      <!-- Rect -->
      <rect x="0" y="0" width="100" height="100" fill="white" stroke-width="0" />
      <!-- Events -->
      <rect x="0" y="0" width="58" height="32.75862068965517" fill="#f006" stroke-width="0" />
      <rect x="58" y="0" width="42" height="45.23809523809524" fill="#f006" stroke-width="0" />
      <rect x="58" y="0" width="42" height="100" fill="#00f6" stroke-width="0" />
      <!-- Curly -->
      <g transform="translate(58.5,0)">
        <text x="21" y="-8" text-anchor="middle">ℙ(B)</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 17.25 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 17.25 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <g transform="translate(100,0) rotate(90)">
        <text x="22.6" y="-8" text-anchor="middle">ℙ(A|B)</text>
        <path d="M -0.5 -1.5 c 0 -1, 1 -2, 2 -2 l 18.75 0 c 1 0, 2 -1, 2 -2 c 0 1, 1 2, 2 2 l 18.75 0 c 1 0, 2 1, 2 2" fill="none" stroke="black" stroke-width="1" />
      </g>
      <text x="79" y="25" text-anchor="middle">ℙ(A&B)</text>
  </g>
</svg>

From that we can easily write down the following two formulas:

$$
\mathbb{P}(B|A) = \frac{\mathbb{P}(A\&B)}{\mathbb{P}(A)}
\quad
\mathbb{P}(A|B) = \frac{\mathbb{P}(A\&B)}{\mathbb{P}(B)}
$$

And from there through

$$
\mathbb{P}(A\&B) = \mathbb{P}(A|B)\cdot\mathbb{P}(B)
$$

we get straight to

$$
\mathbb{P}(B|A) = \frac{\mathbb{P}(A|B)\cdot\mathbb{P}(B)}{\mathbb{P}(A)}
$$

Boom: [Bayes' theorem](https://en.wikipedia.org/wiki/Bayes%27_theorem). 🖐️🎤💥

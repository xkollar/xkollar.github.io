---
title: Graphviz Support
author: xkollar
tags: Haskell, Hakyll
---

I have spent some time adding Grapviz support to my blog.
Now I am able to write graph descriptions directly in
markdown instead of separate `.dot` files.

I can write something like this:

~~~~
~~~ {.dot-render}
digraph G {
    graph [bgcolor=transparent];
    a -> c;
    b -> c;
    c -> d;
}
~~~
~~~~

And you will see something like this:

~~~ {.dot-render}
digraph G {
    graph [bgcolor=transparent];
    a -> c;
    b -> c;
    c -> d;
}
~~~

And while it could have been done in a simpler manner,
I hope to reuse the code for some other stuff...

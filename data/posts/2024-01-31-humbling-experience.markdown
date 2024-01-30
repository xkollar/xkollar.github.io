---
title: "Humbling Experience"
author: xkollar
tags: Bash, Fun
---

I'm relatively comfortable with Bash but when the other day I saw [this Stack
Overflow answer on how to decode URL-encoded string in
shell](https://stackoverflow.com/questions/6250698/how-to-decode-url-encoded-string-in-shell/37840948#37840948)
my brain took a moment to process it.

![What?](https://i.imgflip.com/h4gba.jpg)

Let me copy it here just to appreciate it.

```bash
function urldecode() { : "${*//+/ }"; echo -e "${_//%/\\x}"; }
```

Let's explore what is going on here, lookig at it in easier to digest(?) form:

```bash
function urldecode() {
    : "${*//+/ }"
    echo -e "${_//%/\\x}"
}
```

First part is using built-in `:`{.bash} and as the (single) argument expanding
all the arguments to the function into a single string `"${*}"`{.bash} while
replacing all occurrences of `+` with space (double forward slash pattern
substitution).

Confusing part is that `:`{.bash} build-in kinda does nothing (except for
exiting with 0, many of you probably invoked spell of shape `while :; do
domething; done` or as a part of comment/documentation combined with here
document). While this looks like
[`nop`](https://en.wikipedia.org/wiki/NOP_(code))/pass/… it is not.

The magic is revealed when we inspect the following line where special
variable `${_}`{.bash} is used. From [Bash
documentation](https://www.gnu.org/software/bash/manual/bash.html#index-_005f)
in this case it

> expands to the last argument to the previous simple command executed in the
> foreground, after expansion.

Therefore meaning of the previous line was "just" to set `${_}`. There all
percent signs get replaced with `\x` (double backslash for escaping in `"`) and
pass that to `echo -e` for evaluation.

Obviously, people who [read me](2020-11-23-bash-interview-echo.html) know
there is a bug: `urldecode -e` will return nothing. It is still very elegant way to
solve task at hand on command line.

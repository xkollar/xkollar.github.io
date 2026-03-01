---
title: "Humbling Experience"
author: xkollar
tags: Bash, Fun
---

I'm relatively comfortable with Bash but when the other day I saw [this Stack
Overflow answer on how to decode URL-encoded string in
shell](https://stackoverflow.com/questions/6250698/how-to-decode-url-encoded-string-in-shell/37840948#37840948)
my brain took a moment to process it.

![What?](https://i.imgflip.com/h4gba.jpg){onerror="this.src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOUAAADcAgMAAADy0o7gAAAAIGNIUk0AAHomAACAhAAA+gAAAIDoAAB1MAAA6mAAADqYAAAXcJy6UTwAAAAJUExURQcHB/v7+////zkIjLwAAAABYktHRAJmC3xkAAAFVElEQVRo3u3azW70KgwGYCPV+x6pvh+yyN6R/N7/rZwFmQx/JoRR1bM4WVT9vskzgAFDQgmrV6T/6WeUWCmsUKV0yWMaTzko16H0vh5Syyg/oppJv8Y9Wki/xh1aSTfGLTVaprGmPEubQt3GNjS2lOeottLrnpp2CiWaotaTTowr2i3UaWxJ+4U6jS1pv1CnsSWlZWoelVsa16kn+yHOqa1TdWm4o25T+yHOqF/ffpwmKf8a1QENaxROnDIavQL1lvrB7Yf4Tc3v0nhD+02NnD4a0m5Tg50V4hEd9OcSlddnYUBtNHLHVEdDKPY6dkR5ksbRTOv2zoAyJukwMdgTyvUqxi61cTKao93sN0edTB8mKJ5SHVP9HRrH1D6h5NK7nc4Ulf8QjX9C9RMqq9T+hOIDGn+X8kNqt1Q/oLxKlZYpufQuTLZOtZPaJyn9BTUiXaRKjEUaCdjuU3iXBkDXKDHJGjX6obBKlb5NVqiGSD8u1SEloi8s0UhEX+DHdAM0KIUJWtXLQqKC3aGdzY9e+yhjECZoNuSv/yDidiT6NOZPKwHAcUcDOt+lWKYQtOPfpa/6prGrJDCHoqVnP15iml7xelO5oVcD+OyTBQpE4u0MEZD/MqbKUCKO+b23NN2ggrhII4yJvr4W6IaISFR0CM/RHQGRuBh8u0NjOdXFBDFghcK4WaBmKSLXd3tUb7YhaOb6DS3643Co9Z7pysni0/SypaD7JOXUucEdP+ZQkBhiZ89g10+fglg7z0gZFY+abNajctXdo0FxwC0VNKDbAUbsLDrntJEB3SwNjD6Vzaf7dg4Mh1o9YSvaeY0WXl3MPt3PCVTeYP+84mw3VGuqx1VtHlHjZs98JKrXl3cjDGxbs1PfD0DOCHmlHgfAR/MubD+gARG2iUv1AHC07z2MgpJEavYwGVUAR/PyQhDPzOVTMwBHmrf1lD3Th0t/BNCa2jtb+hTCgAEtNbqjvAMQoSYLvpKlT/cNQJTY5F+6LfVQAChXKOyvQhkKbzRBTVoqZ5ACcPjUhAGwdevbScv5CxLZzyldTp180fUoK2BSpJEj1ZfTvtinmwm0SflnViceUUXEVk+tM9HpmJoYSXsDBQCh90n2D26DcaZIFRJ/qgNIA2mr6hsABNzQtLyEigoQ5Y4mRRUFLKSXRjKgJi1lgM63XCMKBvBVUoEKQCT++nql2x8pKBDOJrur+rU+lJShRJFI7ygYJgWVNOnYcEc3KCp6nkI1Dzo1tSoF6Wv8B3/flN3MRUZMxwXGU3Sv6yGps7enNM07nqEH2iu9u5mgPfwh3e+p/QFtdmXT1IJL+YbG7/YNxFyEjb7hhfiGEn2jH6fjhkaiH/Qbu4+pUqKdy2RIjXwax20lnxoPaXRp2sJFl557nJ9OjNKqXm0B28NxccZSSuNdej2rs0+pS6/nV9m9MV89P3Vey23OmLfqObNzUFGO4FBS4prmx8TqjPlYPZAnmp8WS7kWBjRxzGlxus0l7fx5QkatOirmHpVYHyBGqo+ZOWueyev0yNqz+kj1sTjni6HKGRZrD7EjKQ3odY6v7Wl9exIfiu3G+YynnZP+Hi3i1N7wClWXbhM09D4pNw3qUOIuzRvr0n4r8sY+olI01h5SrdPk5MVlY+MzirBO41qcAooa20Oa1/hhxxYT7yE1XgqUoFqbIhFRnKbvYgkABZ0v9V1s6q75jsX7hPjhSMz659H4D8hrvELPEfVowlKxPtIytSVK8rjQ95+cPb8EC1XNO/YDqgwjsiUqaUg9oufSK2nK2yI1eZ7YXrsIeZhPMxoe9nDqm/cX2FMq77K7Vf4XwIFK2i63DgAAAAAASUVORK5CYII='"}

Let me copy it here just to appreciate it.

```bash
function urldecode() { : "${*//+/ }"; echo -e "${_//%/\\x}"; }
```

Let's explore what is going on here, looking at it in an easier-to-digest(?) form:

```bash
function urldecode() {
    : "${*//+/ }"
    echo -e "${_//%/\\x}"
}
```

First part is using builtin `:`{.bash} with the (single) argument being
expansion of all the function's arguments into a single string
`"${*}"`{.bash} while replacing all the occurrences of `+` with space
(double-forward-slash pattern substitution).

Confusing part is that `:`{.bash} builtin kinda does nothing (except for
exiting with 0; many of you have probably invoked a spell of shape
`while :; do something; done` or used it as a part of comment/documentation
when combined with here document). While this looks like a
[`nop`](https://en.wikipedia.org/wiki/NOP_(code))/pass/… it is not.

The magic is revealed when we inspect the following line where special
variable `${_}`{.bash} is used. From
[Bash documentation](https://www.gnu.org/software/bash/manual/bash.html#index-_005f),
in this case it

> expands to the last argument to the previous simple command executed in the
> foreground, after expansion.

Therefore the purpose of the previous line was "just" to set `${_}`. There all
the percent signs get replaced with `\x` (double backslash
for escaping in `"`) and pass that to `echo -e` for evaluation.

Obviously, people who [read me](2020-11-23-bash-interview-echo.html) know
there is a bug: `urldecode -e` will return nothing. It is still very elegant way to
solve task at hand on command line. Easy fix with `printf '%b'`.

```bash
function urldecode() { : "${*//+/ }"; printf %b "${_//%/\\x}"; }
```

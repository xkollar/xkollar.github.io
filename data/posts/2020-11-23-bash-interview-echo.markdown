---
title: "Bash Interview: echo"
author: xkollar
tags: Linux, Bash, Fun, Interview
---
Interviews are funny. You have only very little time
to find out whether participating sides are a good match or not.
On many different levels: from personal to technical.
Traditionally it is established by series of questions
and answers (ideally not one sided...).

Good questions are not necessarily about correct answer but they will
let you see someone's thought process, will allow for a conversation about it,
and also will enable you to estimate candidate's experience level. (So the
information gained from the interaction is not just binary.) Bonus points
if the question will show the candidate that you are a cool place to be.

For example, interviewing someone for a position that involves Linux,
I like to ask:

> Using `echo`{.bash}, print `-e` (<abbr title="ASCII HEX 2D">minus</abbr> e) on a line.

Candidate would be ideally provided with a terminal to play with. Actually, try
it yourself before reading any further, it is quite fun (if you are into that
kind of thing <abbr title="😉 :wink:">`;-)`</abbr>).

```bash
echo -e
```

Lol. But no. Hopefully they are just trolling.
(I consider being confident enough to make a joke a good sign.)

```bash
echo -e -e
```

Nice try, candidate started to think, but no.

```bash
echo -- -e
```

Okay, candidate knows about `--`{.bash} convention, but no.

```bash
\echo -- -e
...
```

Trying some of previous variants with executable echo rather than builtin one,
but no.

```bash
echo "-e"
echo '-e'
echo \-e
echo \\-e
```

Candidate started to panic, interviewer needs to calm them down.

```bash
echo -n -; echo e
printf -- '-e\n'
cat <<< -e
echo –e
```

Fair enough, it is important skill to be able to realize that one is being
tricked (and try to trick in response). Interviewer needs to re-assure the
candidate that it is indeed possible, with single echo. And that they meant
ASCII character <abbr title="HEX 2D">-</abbr>.

```bash
echo $'\x2De'
```

Candidate realized that they need to be sneaky somehow, but
still no. Somewhat impressive though as not everyone knows `$''`{.bash}.

Hopefully from there it is not far to get to the following.

```bash
echo -e '\x2De'
```

If candidate on top of getting to the correct answer have
a big grin on their face (signifying they had fun), I'm happy.

## Alternatives/similar

> You run `echo *`{.bash} yet nothing gets printed. Can you explain?

Hint: directory is not empy. Hidden files are irrelevant.

> There is a directory called `~` in current directory. Remove it and
> all it contains.

Hint: maybe don't give them terminal to try this one.

Or slightly more obvious version:

> There is a file called `file; rm -rf ~` in current directory. Remove it.

Also fun thing to ponder:

```bash
touch '\e[31mhello'
touch '\e[33mworld'
touch ./-e
#clear
echo *
```

## Final note

If it is not clear from the tags, this article was not meant to be serious. If
you however decide to use this in a real interview, please don't make it the
first question. You can ask it once you think the candidate is good enough
and want to have a bit more of a interesting conversation with them.

If anything, I hope that reader got some intuition into why defensive
programming in Bash is important.

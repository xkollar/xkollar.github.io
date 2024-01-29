---
title: "Function pre- and post-hooks in Bash"
author: xkollar
tags: Bash
---
Perhaps you have also found yourself in a situation where you needed to run
something pre or post invocation of a function in Bash? (A pre- or post-hook if
you wish.)

Let me show you a trick:

```bash
function __wrap() {
    if [[ "$( type -t "${1}" )" != 'function' ]]; then
        echo "Function '${1}' does not exist." >&2
        return 1
    fi
    if [[ "$( type -t "${1}.wrapped" )" == 'function' ]]; then
        echo "Function '${1}.wrapped' already exists." >&2
        return 1
    fi
    source <( typeset -f "${1}" | sed '1s/^\([^ ]\+\)/\1.wrapped/' )
}

function omg() {
    echo omg: "${@}"
}

__wrap omg

# At this point we have two identical functions omg and omg.wrapped,
# former one to be re-defined next.

function omg() {
    echo "pre-hook"
    # manipulate arguments
    omg.wrapped extra_arg "${@}"
    local ret=${?}
    echo "post-hook"
    # and possibly change the result
    return "${ret}"
}

omg 1 2 3
```

On some level we are getting closer to metaprogramming as we are manipulatig
the code that is running/being executed.

Obviously `__wrap` can be extended further, for example to allow for custom
name transformation in complicated and diffult to navigate environments.

Now go and have fun!

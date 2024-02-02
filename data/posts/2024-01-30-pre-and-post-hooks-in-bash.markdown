---
title: "Pre-hooks and post-hooks for Bash functions"
author: xkollar
tags: Bash
---
Perhaps you have also found yourself in a situation where you needed to run
something pre or post invocation of a function in Bash? (A pre-hook or post-hook if
you wish.)

Let me show you a trick:

```bash
function __wrap() {
    local -r from="${1?FUNCTION_NAME}"
    local -r to="${2-${from}.wrapped}"
    if [[ "$( type -t "${from}" )" != 'function' ]]; then
        echo "Function '${from}' does not exist." >&2
        return 1
    fi
    if [[ "$( type -t "${to}" )" == 'function' ]]; then
        echo "Function '${to}' already exists." >&2
        return 1
    fi
    source <( typeset -f "${1}" | awk -v "to=${to}" 'NR==1{$1=to}{print}' )
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

Now go and have fun!

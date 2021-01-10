---
title: "Environment variables"
author: xkollar
tags: Linux, Bash, Fun
---

```bash
A=$'TEST\nSNEAKY=EVIL'
```

```bash
env --null | sed --null-data 's/=.*//' | tr '\0' '\n'
```

```bash
python -c 'import os; print("\n".join(os.environ.keys()))'
```

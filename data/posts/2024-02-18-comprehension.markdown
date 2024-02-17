---
title: "Comprehensions in Python"
author: xkollar
tags: Python, Haskell
---

Python's comprehensions (list, generator, dictionarry) are neat,
however coming from Haskell I miss `let`.

```haskell
funItems = [ item | itemId <- ids, let item = getItem itemId, isFun item ]
```

In the meantime, I have seen in python

```python
fun_items = [ get_item(item_id) for item_id in ids if is_fun(get_item(item_id)) ]
```

Needless to say, even without `get_item` being an expensive operation,
this feels bad. After thinking about it for a while I came up with and
alternative.

```python
fun_items = [ item for item_id in ids for item in (get_item(item_id),) if is_fun(item) ]
```

Though it is a bit longer 🤔. You win some, you lose some, I guess. 🤷

# testi

## A dynamically typed toy programming language with unmutable values but mutable variables

---

'+' operator is overloaded for `Map` and `List` types. The operator doesn't mutate the existing values insted returns a new one.

```testi
let list = [1, 2]
let new_list = list + 3
// list = [1, 2]
// new_list = [1, 2, 3]
```

```testi
let map = #["1": 1]
let new_map = map + ["2", 2] // 2-lenght lists can be added to `Map`.
// map = #["1": 1]
// new_map = #["1": 1, "2": 2]
```

Common pattern to update a collection is using composite assingment

```testi
let my_list = ["Hello", "World"]
my_list += "!" // Creates a new list and assigns it to `my_list`.
// my_list = ["Hello", "World", "!"]
```

---

### For examples see [here](examples)

| Run              |
|------------------|
| `./testi <file>` |

---

### Hello, World

```testi
print("Hello, World!") 
```

---

### Recursive Fibanocci

```testi
fun fib(n)
    if n <= 2 then
        return 1
    end
    fib(n-1) + fib(n-2)
end
```

---

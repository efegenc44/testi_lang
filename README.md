# testi

## A dynamically typed toy programming language

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
    recur(n-1) + recur(n-2)
end
```

---

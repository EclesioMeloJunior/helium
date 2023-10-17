# Helium compiler

A simple compiler

> This is a toy project where I am trying to learn LLVM/compilers

The syntax is really simple (C-like), everything start with the `main` function that
should return an integer (like a signal, 0 means success)

```
func main() : i32 {
    return 0
}
```

Currently, the language supports 2 types `i32` (integer 32) and `f32` (float point numbers), the last one needs improvements

```
func main() : i32 {
    let a : i32 = 1;
    let b : i32 = 1;
    let c : i32 = a + b;
    return c
}
```

##### Next steps

- [ ] Conditional flow
- [ ] Recursivity
- [ ] Implementing fibonacci algo
- [ ] Introduce modules and imports (starting of std lib)

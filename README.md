# Imperia

A simple imperative programming language.

---

## How to install

    $ git clone https://github.com/thomasbrus/imperia.git
    $ cd imperia
    $ cabal install --prefix=$HOME --user
    
## How to run

    $ ~/bin/imperia examples/power.im 
    16    

## Examples

Calculates 2^4. Prints 16 since any program returns the expression that was last evaluated.

```ruby
a = 2
n = 4
power = 1

while n > 0
    n = n - 1
    power = a * power
```

See [src/examples](https://github.com/thomasbrus/imperia/tree/master/examples) for more.

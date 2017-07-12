# RFun: A reversible functional programming language

Some text describing the basics of the language

## Simple Example

...

## History

..

## Syntax

```
q ::= d*                                              (program)
d ::= t                                               (type signature)
    | f l = e                                         (definition)
t ::= b -> t
    | b => b
b ::= Nat
    | [b]
    | {b_1,...,b_m}
    | a
    | (t)
l ::= x                                               (variable)
    | c(l_1,...,l_m)                                  (constructor)
    | n                                               (integer)
    | l_1:l_2                                         (list-constructor)
    | [l_1,...,l_m]                                   (empty and generic list)
    | {l_1,...,l_m}                                   (tuples)    | f                                               (function name)
    | |l|                                             (duplication/equality)e ::= l                                               (left-expression)
    | let  [l_out = f_x l_a* l_in]+ in e end          (let-expression)
    | rlet [l_out = f_x l_a* l_in]+ in e end          (rlet-expression)
    | case l of [l -> e]+                             (case-expression)    | case f_x l_a+ l of [l -> e]+                    (application case-expression)f =:: f                                               (application expression)
    | x                                               (function reference)
```
```Syntax domains:q ∈ Programsd ∈ Definitionsf ∈ Functionsl ∈ Left-expressions e ∈ Expressionsx ∈ Variablesc ∈ Constructorsn ∈ Integers
```

## Important concepts

### Linearity

### First match policy

### Ancillae

## Examples
Let's look at some examples to get a better understanding of RFun.

### Natural number arithmetic

Natural numbers encoded at Peano numbers are a built-in type of RFun. Natural numbers will here have the standard definition:

```
data Nat = Z | S(Nat)
```

Specific natural numbers can also be written with the relating integer value.

The first interesting operation we can define over our natural number are an increment function. First we define the data type

```
inc :: Nat => Nat
```

which defines `inc` to be a function that given a `Nat` returns a `Nat`. Though RFun have many similarities with other functional languages (the type signature is inspired by Haskell) the usage of `=>` and not `->` is important. By `f :: a => b` we define `f` to be a function that consumes an input of type `a` and returns an output of type `b`. Here consumes should be take literally; to ensure reversibility, all information of `a` must be transformed and _conserved_ into `b`.

We can now move onto defining the incremental function as

```
inc :: Nat => Nat
inc n = S(n)
```

We see here that our left-hand-side variable `n` occurs once on the right-hand-side, which means that linearity is upheld.

As we now have in incrementing function we can move onto defining a decrementing function. In a normal language we would do this in the standard was as

```
dec :: Nat => Nat
dec S(n) = n
```

However, we know that decrementing is the symmetrical inverse of incrementing, so given that we have a reversible language we can define this using a reverse interpretation of the forward call, thus

```
dec :: Nat => Nat
dec n = 
  rlet n' = inc n
  in   n'
```

Granted, these are not the most interesting functions, but we now have a first grasp of the language. From this we can now move onto more interesting function, so let's look at addition. Firstly, it is important to remember that addition in reversible computation is not defined as the normal `+(a, b) = a + b`, but instead is embedded as `+(a, b) = (a, a + b)`. In other words, not only do we need to calculate the sum of the two numbers, we must also leave one of the numbers unchanged.

So let's define the type for our `plus` function. A first attempt could be

```
plus :: {Nat, Nat} => {Nat, Nat}
```

Note that we here use braces `{` and `}` to denote tuples, which is a  second build-in type of RFun.

The above matches perfectly the above understanding, that plus takes a pair of `Nat`'s and returns another pair of `Nat`'s. However, we do actually have more information than this: we also know that the value of one of the numbers is unchanged. This information we can include in our type signatures as

```
plus :: Nat -> Nat => Nat
```

Here `plus` is defined as a function that take a `Nat` that must be unchanged over the computation (ancillae) and another `Nat` that is transformed into a `Nat`. That the first `Nat` is unchanged is something that we must ensure in our computation. Note that `=>` binds stronger than `->`. So how can we define such a function? Well, looks much like the normal implementation

```
plus :: Nat -> Nat => Nat
plus Z    x = x
plus S(y) x =
  let x' = plus y x
  in  S(x')
  end
```

So how do we know that the first argument is unchanged? That actually follows from simple structural induction. In the base clause we know the first argument is a basic constructor term (you can look at is as constant) and it is therefore guaranteed to be unchanged.
In the recursive clause we have (by induction hypothesis) that `y` is unchanged over the recursive call and as this is the only usage we have of `y` we can always reconstruct the input `S(y)` and thus the first argument is unchanged. 

This arguments here was a bit handwaving, but given the type signature and the abstract syntact, we can easily check this construction. 

We can actually with simple program transformation transform our `plus` function into a paired version of addition that matches out initial type signature. 

```
plusP :: {Nat, Nat} => {Nat, Nat}
plusP {Z,    x} = {Z, x}
plusP {S(y), x} =
  let {y, x'} = plusP {y, x}
  in  {S(y), S(x')}
  end
```

Here we only

  * wrap our input into a tuple,
  * add the ancillae arguments to all output leaves,
  * wrap all function calls into touples,
  * add the ancillae inputs to function calls to the output.

Note that the copying of ancillae arguments seemingly destroys referential transparency, however, due to linearity the reintroductions of `y` does not overwrite the value in `y` as the value was previously consumed. 

### List functions

Lists is the third and final build-in data type of RFun and from a functional perspective very interesting. We will start by defining the favourite `map` function.

```
map :: (a => b) -> [a] => [b]
```

The type signature tells us that `map` is a function that given a function (which will not be changed) that transforms an input of type `a` to an output of type `b` will tranform a list of `a`s to a list of `b`s. 

```
map :: (a => b) -> [a] => [b]
map fun     []   = []
map fun (x : xs) =
  let x'  = fun x
    xs' = map fun xs
  in  (x' : xs')
  end
```

Given that application of the function referenced in `fun` it is clear (by induction) that `fun` is unchanged and only the input is transformed to the output. Personally, I find it surprising how close it models the normal definition.

The next function we can look at is the lenght function, which returns the lenght of a list. Before starting an implementation, we actually are doing. The lenght of a list is one of several pieces of information that held in a list: other ones are the element values and the order of the elements. It is therefore obvious that we cannot make a function transforms a list of some type into its lenght. The approach that we will use here is instead that the lenght is a property of the list that will will extract, while keeping the list unchanged.

```
length :: [a] -> {} => Nat
```

The `length` function is therefore defined as a function that given a list containing element of any type, transforms no information (here represented by the empty tuple `{}`) into a `Nat`. It is from this obvious that the information contained if the `Nat` has been copied form the list. This type also comprises what we would call a _Bennett embedding_ of the normal lenght function.

```
length :: [a] -> {} => Nat
length []       {} = Z
length (x : xs) {} =
  let  s = length xs {}
  in   S(s)
  end
```

Based on this type, we have an implementation of `length` that closely resembles the normal implemantation.


```
scanl :: (a -> b => a) -> a -> [b] => [a]
scanl fun i []     = []
scanl fun i (x:xs) =
  let x' = fun i x
    xs' = scanl fun x' xs
  in  (x' : xs')
  end
```

```
foldl :: (b -> a => a) -> [b] -> a => a
foldl fun []     a = a
foldl fun (x:xs) a =
  let a' = fun x a
  in  foldl fun xs a'
  end
```

```
foldr :: (b -> a => a) -> [b] -> a => a
foldr fun []     a = a
foldr fun (x:xs) a =
  let a'  = foldr fun xs a
  in  fun x a'
  end
```

```
reverse :: [a] => [a]
reverse xs =
  let  xs_s = length xs {}
       {[], ys} = move xs_s {xs, []}
  rlet {} = length ys xs_s
  in   ys
  end

move :: Nat -> ([a], [a]) => ([a], [a])
move Z    {   x  , l} = {x, l}
move S(s) {(x:xs), l} =
  let  xs' = move s {xs, (x:l)}
  in   xs'
  end
```

## Semantics
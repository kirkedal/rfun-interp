# RFun: A reversible functional programming language

This is a tutorial that introduces the reversible functional RFun. More specifically, this tutorial describes RFun version 2. It is intended to give a good overview of the language and make the reader able to start writing his/hers own programs. 

It is not intended to give a detailed formalisation RFun, and for this we refer to the references. Also we do not give any background of reversible computations nor any motivation for looking it at the first place; for this we will refer to [...].

Note also that RFun is being actively developed. It is therefore expected that the language will change over time. If you find the language interesting, you can also participate in the further development.

## History

RFun was first introduces in 2013 by Holger Bock Axelsen, Tetsuo Yokoyama and Robert Glück [1]. It was introduced as a simple reversible untyped first-order functional language, and focused on the theoretical foundation and formal semantics. It is also noteworthy to add that the language is R-Turing complete; i.e. Turing complete with the restriction to reversible programs.

Later work by Michael Kirkedal Thomsen and Holger Bock Axelsen [2] detailed how implemented RFun and what to consider when developing programs. This work also added much syntactic sugar to make the program easier to program. Finally we added function pointers such that functions like `map` could be implemented.

This second version of RFun can be credited to two event. Firstly, follows much from the work in [2]. It was the first time that larger programs was developed in RFun and it gave much insight to what needed to be improved. Secondly, it can be credited to the Training School on Reversible Computations held in Torún. When teaching the a language to 20+ students you have to move it from a simple proof-of-concept language to consider one that can be easily understood.

The first version of RFun is very basic and properties of the languages have been (more easily) shown. It is therefore used as the target language for the later version of RFun. We will, thus, refer to the first version as RFun\_core which the later is just called RFun.


## Running RFun programs
You get the source code for the latest version of the RFun interpreter by contacting Michael Kirkedal Thomsen (<kirkedal@acm.org>). It is currently held at a private repository.

The source code for an RFun\_core interpreter (developed for [2]) can be found at: 

* <https://github.com/kirkedal/rfun.git>

Alternatively, you can also use our online interpreter, which is available at:

* <http://topps.diku.dk/pirc>


## Simple Example

For historical reasons the Fibonacci function have always been used as a _Hello World_ program for reversible language. There is no reason to change this so here it is:

```
fib :: Nat <-> (Nat, Nat)
fib Z     = ((S Z),(S Z))
fib (S m) =
  let (x,y) = fib m
      y' = plus x y
  in (y',x)
```

Now you have seen some code. For functional programmers, they will look familiar (have drawn inspiration from Haskell), but some parts are new. We will explain some details of the language below.

## Important concepts
Before we understand how to make programs, we need to understand a few central concepts.

### Linearity
The theory behind linear types stems back from linear logic. It is basically a way to ensure that a resource are only used once. There we will stretch it even further to make sure that resources are used exactly one.

In the Fibonacci function, for example, you can see that the variable `m` is introduced on the left-hand-side and used only (and precisely) in the recursive call to `fib`. This is the same for `y` and `y'`; you should also consider the return of `y'` as a usage.

### Ancillae
Ancillae (or an ancilla variable) is a term that have been adapted from physics and covers a state in which the entropy is unchanged. Here we specifically use it for variables for which we can _guarantee_ that the value is unchanged over a function call. We cannot put too little emphasis on the _guarantee_, because we have taken a conservative approach and will only use it when we statically can endure that it is upheld.

You might have noted that I left out the `x` variable of the Fibonacci function. If you look close you can see that (in opposition to the other variables) is occurs three times. First, it is introduces by the recursive call to `fib`, then it is used by the plus function, and, finally, it is returned. Of these `plus` is actually using it as an ancillae; it has read and used the value for something, but we are guaranteed that the values is restored the call. This is not the same for `y`, which is the reason that we introduce the new `y'`.

### First-match policy
The last concept we will introduce is the first-match policy, which is important to guarantee injectivity of the functions. Conceptually is states that a result of a function must not be able to be the result of any previous branches; knowing nothing about the possible content of variables.

Often you can check this statically based on the type definitions (which we currently does not do), but is some cases we can only to it at run-time. In the Fibonacci example there is on simple analysis that can check if it is upheld; we need a side-condition referring to the development if Fibonacci numbers. Thus, we can only rely on the run-time check. We will soon see examples where is have a static guarantee.

## Examples
Let's look at some examples to get a better understanding of RFun.

### Natural number arithmetic

Natural numbers encoded at Peano numbers are a built-in type of RFun. Natural numbers will here have the standard definition:

```
data Nat = Z | (S Nat)
```

Specific natural numbers can also be written with the relating integer value.

The first interesting operation we can define over our natural number are an increment function. First we define the data type

```
inc :: Nat <-> Nat
```

which defines `inc` to be a function that given a `Nat` returns a `Nat`. Though RFun have many similarities with other functional languages (the type signature is inspired by Haskell) the usage of `<->` and not `->` is important. By `f :: a <-> b` we define `f` to be a function that consumes an input of type `a` and returns an output of type `b`. Here consumes should be take literally; to ensure reversibility, all information of `a` must be transformed and _conserved_ into `b`.

We can now move onto defining the incremental function as

```
inc :: Nat <-> Nat
inc n = (S n)
```

We see here that our left-hand-side variable `n` occurs once on the right-hand-side, which means that linearity is upheld.

As we now have in incrementing function we can move onto defining a decrementing function. In a normal language we would do this in the standard was as

```
dec :: Nat <-> Nat
dec (S n) = n
```

However, we know that decrementing is the symmetrical inverse of incrementing, so given that we have a reversible language we can define this using a reverse interpretation of the forward call, thus

```
dec :: Nat <-> Nat
dec n = inc! n
```
Note that the "!" at the end of the function indicates that the function is reverse executed.

Granted, these are not the most interesting functions, but we now have a first grasp of the language. From this we can now move onto more interesting function, so let's look at addition. Firstly, it is important to remember that addition in reversible computation is not defined as the normal `+(a, b) = a + b`, but instead is embedded as `+(a, b) = (a, a + b)`. In other words, not only do we need to calculate the sum of the two numbers, we must also leave one of the numbers unchanged.

So let's define the type for our `plus` function. A first attempt could be

```
plus :: (Nat, Nat) <-> (Nat, Nat)
```

The above matches perfectly the above understanding, that plus takes a pair of `Nat`'s and returns another pair of `Nat`'s. However, we do actually have more information than this: we also know that the value of one of the numbers is unchanged. This information we can include in our type signatures as

```
plus :: Nat -> Nat <-> Nat
```

Here `plus` is defined as a function that takes a `Nat` that must be unchanged over the computation (ancillae) and another `Nat` that is transformed into a `Nat`. That the first `Nat` is unchanged is something that we must ensure in our computation. Note that `<->` binds stronger than `->`. So how can we define such a function? Well, looks much like the normal implementation

```
plus :: Nat -> Nat <-> Nat
plus Z    x = x
plus (S y) x =
  let x' = plus y x
  in  (S x')
```

So how do we know that the first argument is unchanged? That actually follows from simple structural induction. In the base clause we know the first argument is a basic constructor term (you can look at is as constant) and it is therefore guaranteed to be unchanged.
In the recursive clause we have (by induction hypothesis) that `y` is unchanged over the recursive call and as this is the only usage we have of `y` we can always reconstruct the input `(S y)` and thus the first argument is unchanged. 

This arguments here was a bit handwaving, but given the type signature and the abstract syntax, we can easily check this construction. 

We can actually with simple program transformation transform our `plus` function into a paired version of addition that matches out initial type signature. 

```
plusP :: (Nat, Nat) <-> (Nat, Nat)
plusP (Z,    x) = (Z, x)
plusP (S(y), x) =
  let (y, x') = plusP (y, x)
  in  (S(y), S(x'))
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
map :: (a <-> b) -> [a] <-> [b]
```

The type signature tells us that `map` is a function that given a function (which will not be changed) that transforms an input of type `a` to an output of type `b` will tranform a list of `a`s to a list of `b`s. 

```
map :: (a <-> b) -> [a] <-> [b]
map fun     []   = []
map fun (x : xs) =
  let x'  = fun x
    xs' = map fun xs
  in  (x' : xs')
```

Given that application of the function referenced in `fun` it is clear (by induction) that `fun` is unchanged and only the input is transformed to the output. Personally, I find it surprising how close it models the normal definition.

The next function we can look at is the length function, which returns the length of a list. Before starting an implementation, we actually are doing. The length of a list is one of several pieces of information that held in a list: other ones are the element values and the order of the elements. It is therefore obvious that we cannot make a function transforms a list of some type into its length. The approach that we will use here is instead that the length is a property of the list that will will extract, while keeping the list unchanged.

```
length :: [a] -> {} <-> Nat
```

The `length` function is therefore defined as a function that given a list containing element of any type, transforms no information (here represented by the empty tuple `{}`) into a `Nat`. It is from this obvious that the information contained if the `Nat` has been copied form the list. This type also comprises what we would call a _Bennett embedding_ of the normal length function.

```
length :: [a] -> () <-> Nat
length []       () = Z
length (x : xs) () =
  let  n = length xs ()
  in   (S n)
```

Based on this type, we have an implementation of `length` that closely resembles the normal implementation.

Finally we would like to implement a efficient (linear run-time) version of list reversal. We have a reversible language, so that should be easy. Right!

The simple normal version would use an append function to move the first element to the last. This is however not efficient. A better implementation would use a helper function that have an accumulator to which one element is moved at the time. This is the version we would implement.

```
reverse :: [a] <-> [a]
reverse xs =
  let  xs_s = length xs ()
       ([], ys) = move xs_s (xs, [])
       () = length! ys xs_s
  in   ys
  end

move :: Nat -> ([a], [a]) <-> ([a], [a])
move Z    (   x  , l) = (x, l)
move (S s) ((x:xs), l) =
  let  xs' = move s (xs, (x:l))
  in   xs'
  end
```
However, the first-match policy complicated matters a bit. Here the accumulator version is implemented as the `move` function. This take a `Nat` and move this this many elements from one list to the other. Note, the difference to the conventional accumulator version that just moves all.

Reverse is them implemented by finding the length of the list, moving this many element from one list to the other, and uncomputing the the length again.

## Equality and Duplication

Equality (and duplication) have a special place in FRun. First, there exist a predefined data constructor for, equality-testing, which have the following definition.

```
data EQ = Eq | Neq a
```

Furthermore, there exist a predefined function to perform
equality-testing. It is a special function with the
following data type.

```
eq :: a -> a <-> EQ
```

Given two values, say `eq x y`, the first argument (`x`) is ancillae, so the know that it will be unchanged. The second argument (`y`), however, will be transformed into a `EQ` type, where the result is

 * `Eq` if `x` is equal to `y`
 * `Neq y` if `x` is different from `y`.

Note, thus, that the equality can remove one copy of the two values. I will not give an implementation of `eq` as it is not possible in RFun.

Based on `eq` we can then make a duplication function by inverse execution 

```
dup :: a -> () <-> a
dup x () =  eq! x Eq
```

## Semantics
The semantics of RFun is defines over a translation to RFun\_core. There is no formal definition of the semantics and I will here not go into details with the translation. Most translations are quite straight forward. _Be warned_ as the semantics is defines over a translation to RFun\_core, you can see quite obscure run-time errors returns from the interpretation of RFun\_core. The will be updated in the future.

RFun\_core does have a formal semantics, which can be found in [1]. 


# References

[1] T. Yokoyama and H. B. Axelsen and R. Gluck, Towards a reversible functional language, Reversible Computation, RC '11, 7165 14--29 (2012)

[2] M. K. Thomsen and H. B. Axelsen, Interpretation and Programming of the Reversible Functional Language, Proceedings of the 27th Symposium on the Implementation and Application of Functional Programming Languages, 8:1--8:13 (2016)



# RFun: A reversible functional programming language

This is a tutorial that introduces the reversible functional RFun. More specifically, this tutorial describes RFun version 2. It is intended to give a good overview of the language and make the reader able to start writing their own programs.

It is not intended to give a detailed formalisation of RFun, and for this we refer to the references. Also we do not give any background of reversible computations nor any motivation for looking it at the first place; for this we will refer to [...].

**Note** also that RFun is being actively developed. It is therefore expected that the language will change over time. Also, though the type checker is (mostly) sound, it is by fra complete. The interpretation, however, is both which makes is a langauge you can play with. If you find the language interesting, you can also participate in further development.

## History

RFun was first introduces in 2013 by Holger Bock Axelsen, Tetsuo Yokoyama and Robert Glück [1]. It was introduced as a simple reversible untyped first-order functional language, and focused on the theoretical foundation and formal semantics. It is also noteworthy to add that the language is R-Turing complete; i.e. Turing complete with the restriction to reversible programs.

Later work by Michael Kirkedal Thomsen and Holger Bock Axelsen [2] detailed how RFun was implemented and what to consider when developing programs. This work also added much syntactic sugar to make it easier to program. Finally we added higher-order functions so that functions like `map` could be implemented.

This second version of RFun can be credited to two events. Firstly, much follows from the work in [2]. It was the first time that larger programs were developed in RFun and it gave much insight to what needed to be improved. Secondly, it can be credited to the Training School on Reversible Computation held in Toruń. When teaching a language to 20+ students you have to move it from a simple proof-of-concept to one that can be easily understood.

The first version of RFun is very basic and properties of the languages have been (more easily) shown. It is therefore used as the target language for the later version of RFun. We will, thus, refer to the first version as RFun\_core while the later is just called RFun.


## Running RFun programs
You get the source code for the latest version of the RFun interpreter by contacting Michael Kirkedal Thomsen (<kirkedal@acm.org>). It is currently held at a private repository.

The source code for an RFun\_core interpreter (developed for [2]) can be found at:

* <https://github.com/kirkedal/rfun.git>

Alternatively, you can also use our online interpreter, which is available at:

* <http://topps.diku.dk/pirc>


## Simple Example

For historical reasons the Fibonacci function has always been used as a _Hello World_ program for reversible languages. There is no reason to change this so here it is:

```haskell
fib :: Nat <-> (Nat, Nat)
fib Z     = ((S Z),(S Z))
fib (S m) =
  let (x,y) = fib m
      y' = plus x y
  in (y',x)
```

Now you have seen some code. For functional programmers, it will look familiar (have drawn inspiration from Haskell), but some parts are new. We will explain some details of the language below.

## Important concepts
Before we understand how to make programs, we need to understand a few central concepts.

### Linearity
The theory behind linear types stems back from linear logic. It is basically a way to ensure that resources are only used once. Then we will stretch it even further to make sure that resources are used exactly once.

In the Fibonacci function, for example, you can see that the variable `m` is introduced on the left-hand side and used only (and precisely) in the recursive call to `fib`. This is the same for `y` and `y'`; you should also consider returning `y'` as a usage.

### Ancillae
Ancillae (or ancillary variables) is a term that has been adapted from physics and describes a state in which entropy is unchanged. Here we specifically use it for variables for which we can _guarantee_ that their values are unchanged over a function call. We cannot put too little emphasis on the _guarantee_, because we have taken a conservative approach and will only use it when we statically can ensure that it is upheld.

You might have noted that I left out the `x` variable of the Fibonacci function. If you look closely you can see that (in opposition to the other variables) it occurs three times. First, it is introduced by the recursive call to `fib`, then it is used by the plus function, and, finally, it is returned. Of these `plus` is actually using it as an ancilla; it has read and used the value for something, but we are guaranteed that the same value is restored after the call. This is not the case for `y`, which is the reason that we introduce the new `y'`.

### First-match policy
The last concept we will introduce is the first-match policy (FMP), which is necessary to guarantee injectivity of functions. Conceptually it states that the result of a function must not be the result of any previous branches; knowing nothing about the possible content of variables.

Often you can check this statically based on type definitions (which we currently do not do), but in some cases we can only do it at run-time. In the Fibonacci example there is no simple analysis that can check if the FMP is upheld; we would need a side-condition referring to the development of Fibonacci numbers. Thus, we can only rely on a run-time check. We will soon see examples where the FMP has a static guarantee.

## Examples
Let's look at some examples to get a better understanding of RFun.

### Natural number arithmetic

Natural numbers encoded as Peano numbers are a built-in type of RFun. Natural numbers will here have the standard definition:

```
data Nat = Z | (S Nat)
```

Specific natural numbers can also be written as literals.

The first interesting operation we can define over our natural numbers is the increment function. First we declare its type:

```
inc :: Nat <-> Nat
```

This declares `inc` to be a function that given a `Nat` returns a `Nat`. Though RFun has many similarities with other functional languages (the type signature is inspired by Haskell) the usage of `<->` and not `->` is important. Using `f :: a <-> b` we define `f` to be a function that consumes an input of type `a` and returns an output of type `b`. Here "consumes" should be taken literally: to ensure reversibility, all information of `a` must be transformed and _conserved_ into `b`.

We can now move onto defining the increment function as

```
inc :: Nat <-> Nat
inc n = (S n)
```

We see here that our left-hand-side variable `n` occurs once on the right-hand side, which means that linearity is upheld.

As we now have the increment function we can move onto defining the decrement function. In a normal language we would do this in the standard way as

```
dec :: Nat <-> Nat
dec (S n) = n
```

However, we know that decrementing is the symmetrical inverse of incrementing, so given that we have a reversible language we can define this using a reverse interpretation of the forward call, thus

```
dec :: Nat <-> Nat
dec n = inc! n
```
Note that the "!" at the end of the function indicates that the function is executed in reverse.

Granted, these are not the most interesting functions, but we now have a first grasp of the language. From this we can now move onto more interesting functions, so let's look at addition. Firstly, it is important to remember that addition in reversible computation is not defined as the normal `+(a, b) = a + b`, but instead is embedded as `+(a, b) = (a, a + b)`. In other words, not only do we need to calculate the sum of the two numbers, we must also leave one of the numbers unchanged.

So let's define the type for our `plus` function. A first attempt could be

```
plus :: (Nat, Nat) <-> (Nat, Nat)
```

The above matches perfectly the above understanding, that plus takes a pair of `Nat`s and returns another pair of `Nat`s. However, we do actually have more information than this: we also know that the value of one of the numbers is unchanged. We can include this information in our type signatures as

```
plus :: Nat -> Nat <-> Nat
```

Here `plus` is defined as a function that takes one `Nat` that must be unchanged over the computation (ancillary) and one `Nat` that is going to be transformed into another `Nat`. That the first `Nat` is unchanged is something we must ensure in our computation. Note that `<->` binds stronger than `->`. So how can we define such a function? Well, it looks much like the normal implementation:

```
plus :: Nat -> Nat <-> Nat
plus Z    x = x
plus (S y) x =
  let x' = plus y x
  in  (S x')
```

So how do we know that the first argument is unchanged? This actually follows from simple structural induction. In the base clause we know that the first argument is a basic constructor term (you can look at it as constant) and it is therefore guaranteed to be unchanged.
In the recursive clause we have (by the induction hypothesis) that `y` is unchanged over the recursive call and as this is the only usage of `y` we can always reconstruct the input `(S y)` and thus the first argument is unchanged.

This argument here was a bit handwaving, but given the type signature and the abstract syntax, we can easily check this construction.

Using a simple program transformation we can actually transform our `plus` function into a paired version of addition that matches out initial type signature.

```
plusP :: (Nat, Nat) <-> (Nat, Nat)
plusP (Z,     x) = (Z, x)
plusP ((S y), x) =
  let (y', x') = plusP (y, x)
  in  ((S y'), (S x'))
```

Here we only

  * wrap our input into a tuple,
  * add the ancillary arguments to all output leaves,
  * wrap all function calls into tuples,
  * add the ancillary inputs to function calls to the output.

Note that copying of ancillary arguments seemingly destroys referential transparency, however, due to linearity the reintroduction of `y` does not overwrite the value in `y` as the value was previously consumed.

### List functions

Lists are the third and final built-in data type of RFun and from a functional perspective very interesting. We will start by defining the favourite `map` function.

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

Given the application of the function referenced by `fun` it is clear (by induction) that `fun` is unchanged and only the input is transformed to the output. Personally, I find it surprising how closely this models the normal definition.

The next function we can look at is `length`, which returns the length of a list. Before starting an implementation, we should think about what we are doing. The length of a list is one of several pieces of information that is held in a list: the other ones are the element values and the order of the elements. It is therefore obvious that we cannot make a function which merely transforms a list of some type into its length. The approach that we will use here is instead that the length is a property of the list that we will extract, while keeping the list unchanged.

```
length :: [a] -> () <-> Nat
```

The `length` function is therefore defined as a function that given a list containing elements of any type, transforms no information (here represented by the empty tuple `()`) into a `Nat`. From this it is obvious that the information contained in the `Nat` has been copied from the list. This type also comprises what we would call a _Bennett embedding_ of the normal `length` function.

```
length :: [a] -> () <-> Nat
length []       () = Z
length (x : xs) () =
  let  n = length xs ()
  in   (S n)
```

Based on this type, we have an implementation of `length` that closely resembles the normal implementation.

Finally we would like to implement an efficient (linear run-time) version of list reversal. We have a reversible language, so that should be easy. Right!

The simple normal version would use an `append` function to move the first element to the last. This is however not efficient. A better implementation would use a helper function with an accumulator to which one element is moved at a time. This is the version we would implement.

```
reverse :: [a] <-> [a]
reverse xs =
  let  xs_s = length xs ()
       ([], ys) = move xs_s (xs, [])
       () = length! ys xs_s
  in   ys
  end

move :: Nat -> ([a], [a]) <-> ([a], [a])
move Z     (x,      l) = (x, l)
move (S s) ((x:xs), l) =
  let  xs' = move s (xs, (x:l))
  in   xs'
  end
```
However, the first-match policy complicates matters a bit. Here the accumulator version is implemented as the `move` function, which takes a `Nat` and moves this many elements from one list to another. Note, the difference to the conventional accumulator version that moves all elements.

`reverse` is them implemented by finding the length of a list, moving this many elements from the input list to a list that will serve as the output, then uncomputing the length again.

## Equality and Duplication

Equality (and duplication) have a special place in FRun. First, there exists a predefined data constructor for equality testing, which has the following definition:

```
data EQ = Eq | Neq a
```

Furthermore, there exists a predefined function that performs equality testing. It is a special function with the following type:

```
eq :: a -> a <-> EQ
```

Given two values, say `eq x y`, the first argument (`x`) is ancillary, so we know that it will be unchanged. The second argument (`y`), however, will be transformed into a value of the `EQ` type, where the result is

 * `Eq` if `x` is equal to `y`
 * `Neq y` if `x` is different from `y`.

Note, thus, that testing for equality can destroy one copy of the two values. I will not give an implementation of `eq` as it is not possible in RFun.

Based on `eq` we can then make a duplication function by inverse execution:

```
dup :: a -> () <-> a
dup x () =  eq! x Eq
```

## Semantics
The semantics of RFun is defined over a translation to RFun\_core. There is no formal definition of the semantics and I will here not go into details about the translation. Most translations are quite straightforward. _Be warned_ that since the semantics is defined over a translation to RFun\_core, you can see quite obscure run-time errors returned from the interpretation of RFun\_core. This will be improved in the future.

RFun\_core does have a formal semantics, which can be found in [1].


# References

[1] T. Yokoyama and H. B. Axelsen and R. Gluck, Towards a reversible functional language, Reversible Computation, RC '11, 7165 14--29 (2012)

[2] M. K. Thomsen and H. B. Axelsen, Interpretation and Programming of the Reversible Functional Language RFUN, Proceedings of the 27th Symposium on the Implementation and Application of Functional Programming Languages, 8:1--8:13 (2016)

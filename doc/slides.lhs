Lambda World 2017
=================

- Teaching functional programming 
- The `optics` abstraction for immutable data structures
- Category Theory in Life
- Mathematics as a common language for programming, fun and creativity
- Compile-time Search Engine Optimization with dependent types languages
- Introduction to Lambda Calculus
- State management in pure functional programs
- Side-effects managements in user interfaces 
- Functional programming in Kotlin
- Recursive schemes
- Type-driven programming applied to array languages

Semantic of programming languages
=================================

- Operational semantic
    - Simulating an abstract machine

- Denotational semantic
    - Mathematical interpretation
    - Equational reasoning
    - Theorems and proofs

Functional programming patterns\footnote{[6] T. Williams, "Recursion Schemes by Example"}
===============================

- *Recursion Schemes* are essentially programming patterns
- By structuring our programs in a well-defined way we can:
    - communicate and reason about our programs
    - reuse both code and ideas
    - use a catalogue of theorems to optimise or prove properties
    - identify and exploit opportunities for parallelism

Haskell - Functions
===================

> twice x = x + x

~~~
 > twice 2
 4
~~~

~~~
 > :t twice
 twice :: Num a => a -> a
~~~

Haskell - Data types
====================

> data Bool = False | True  

~~~
 > a = False
 > :t a
 a :: Bool
~~~

> data Person = Person String Int

~~~
 > bob = Person "Bob" 42
~~~

Haskell - Records syntax
========================

> data Person = Person { name :: String, age :: Int }

~~~
 > bob = Person "Bob" 42
 > name bob
 "Bob"
 > age bob
 42
~~~

Haskell - Type variables
========================

> data Maybe a = Nothing | Just a

~~~
 > x = Nothing
 > y = Just bob
 > :t y
 y :: Maybe Person
~~~

~~~
 > :k Maybe
 Maybe :: * -> *
~~~

`Maybe` is a type constructor. Given a type parameter it produces a type.

Haskell - Type classes
======================

A *type class* is a way to control _ad hoc_ polymorphism (aka. overloading) in Haskell. 

> class Num a where
>   (+) :: a -> a -> a
> -- ...

> class Eq a where
>   (==) :: a -> a -> Bool

> instance Eq Person where
>   (Person a b) == (Person c d) = a == c && b == d 

~~~
 > bob == bob
 True
~~~

Domain Specific Language
========================

Minimalist expression language for integer computations:

~~~{.haskell}
data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
~~~

`Expr` is a recursive algebraic data type.

> -- | 1 * 2 + 0
> e = Add (Mul (Const 1) (Const 2)) (Const 0)

Evaluation
==========

~~~{.haskell}
eval :: Expr -> Int
eval (Const a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
~~~

~~~
 > eval e
 2
~~~

Optimization
============

~~~{.haskell}
opt1 :: Expr -> Expr
opt1 = o where
  o (Add a (Const 0)) = o a
  o (Add (Const 0) a) = o a
  o (Mul a b) = Mul (o a) (o b)
  o e = e

opt2 :: Expr -> Expr
opt2 = o where
  o (Mul a (Const 1)) = o a
  o (Mul (Const 1) a) = o a
  o (Add a b) = Add (o a) (o b)
  o e = e
~~~

Composition
===========

~~~{.haskell}
opt :: Expr -> Expr
opt = opt2 . opt1
~~~

~~~
 > opt e
 Const 2
~~~

F-Algebra
=========

Let's abstract away the recursion from our algebraic data type.

~~~{.haskell}
data ExprF r = ConstF Int
             | AddF r r
             | MulF r r
~~~

We would like evaluation to look something like:

~~~{.haskell}
g :: ExprF Int -> Int
g (ConstF a) = a
g (AddF a b) = a + b
g (MulF a b) = a * b
~~~

`g` is an evaluation function for an *F-Algebra* over the *endofunctor* `ExprF` and the *carrier type* `Int`.

Category
========

The mathematics of mathematics. Applied to reasoning about composition in programming.

- Objects (types)
- Morphisms between objects (functions)
- Associative composition of morphisms (functions composition)
- Identity morphisms on each object, as a unit of composition (identity function)

\vspace{.2in}
\centerline{\resizebox{2in}{!}{%
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  a \arrow[loop left, "id_a"] \arrow[r, "f"] \arrow[rd, "g \circ f"] \arrow[d, "(h \circ g) \circ f = h \circ (g \circ f)"'] \&
    b \arrow[loop right, "id_b"] \arrow[d, "g"] \\
  d \arrow[loop left, "id_d"] \&
    c \arrow[loop right, "id_c"] \arrow[l, "h"]
\end{tikzcd}
}}
\vspace{0.2in}

Functor
=======

A functor is a mapping between two categories, so that:

- Every morphism $f : a \rightarrow b$ is mapped to a morphism $F f : F a \rightarrow F b$.

- The image under $F$ of a composition $h = g \circ f$ is a composition of images $F h = F g \circ F f$.

- The identity morphism $id_a$ on object $a$ is mapped to an identity morphism $id_{Fa}$ on the image $Fa$ of $a$.

An *endofunctor* is a functor over the same category. We will use the category of Haskell types.

Functor diagram
===============

\vspace{.2in}
\centerline{\resizebox{3in}{!}{%
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  a \arrow[loop left, "id_a"] \arrow[d, "f"] \arrow[dd, bend right, "h = g \circ f"'] \arrow[r, blue] \&
    Fa \arrow[loop right, "id_{Fa} = F\ id_a"] \arrow[d, "Ff"'] \arrow[dd, bend left, "Fh = Fg \circ Ff"] \\
  b \arrow[d, "g"] \arrow[r, blue] \&
    Fb \arrow[d, "Fg"'] \\
  c \arrow[r, blue] \&
    Fc
\end{tikzcd}
}}
\vspace{0.2in}

Haskell - Functor
=================

In Haskell, functors are defined as a type class:

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

`fmap` is the mapping of morphisms $a \rightarrow b$ to $F a \rightarrow F b$.

The type variable `f` takes a type parameter. It is a _type constructor_ mapping objects $a$ to $F a$.

The `Maybe` type is functorial:

> data Maybe a = Nothing | Just a

> instance Functor Maybe where
>   fmap _ Nothing  = Nothing
>   fmap f (Just a) = Just (f a)


Maybe functor diagram
=====================

\vspace{.2in}
\begin{columns}
\begin{column}{0.5\textwidth}
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  a \arrow[d, "f"] \arrow[r, "Maybe", blue] \& Maybe\ a \arrow[d, "fmap\ f"'] \\
  b \arrow[r, "Maybe", blue] \& Maybe\ b
\end{tikzcd}
\end{column}
\begin{column}{0.5\textwidth}
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  Person \arrow[d, "age"] \arrow[r, "Maybe", blue] \& Maybe\ Person \arrow[d, "fmap\ age"'] \\
  Int \arrow[r, "Maybe", blue] \& Maybe\ Int
\end{tikzcd}
\end{column}
\end{columns}
\vspace{0.2in}

~~~
> x = Maybe bob
> y = Nothing
> fmap age x
Just 42
> fmap age y
Nothing
~~~

F-Algebra commuting diagram
===========================

An F-Algebra $(a, f)$ over an endofunctor $F$ is defined by:

- a carrier object $a$
- an evaluation morphism $f : F\ a \rightarrow a$

An initial algebra $(i, j)$ over $F$ is such that, if it exists,
there is a unique morphism $m$ so that $m \circ j = f \circ F\ m$ for any other F-Algebra $(a, f)$.
We say that the diagram commutes:

\vspace{.2in}
\centerline{\resizebox{1.4in}{!}{%
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  F\ i \arrow[d, "j"] \arrow[r, "F\ m"] \& F\ a \arrow[d, "f"] \\
  i \arrow[r, "m"]                      \& a
\end{tikzcd}
}}
\vspace{0.2in}

Initial algebra
===============

- Lambek’s Lemma:

If there exists a initial algebra for an endfunctor $F$, then it is a fixed point of $F$.

- Adámek’s theorem:

A set functor has an initial algebra if and only if it has a fixed point.

Recursion
=========

> fact n = if n == 0 then 1 else n * fact (n - 1)

Fixed point combinator
======================

~~~{.haskell}
fix f = let x = f x in x

fact' n = fix f n where f x m = if m == 0 then 1 else m * x (m - 1)

fact' 4 = fix f 4 = let x = f x in x 4
                  = let x = f x in f x 4
                  = let x = f x in 4 * x (4 - 1)
                  = let x = f x in 4 * f x 3
                  = let x = f x in 4 * 3 * x (3 - 1)
                  = let x = f x in 4 * 3 * f x 2
                  = let x = f x in 4 * 3 * 2 * x (2 - 1)
                  = let x = f x in 4 * 3 * 2 * f x 1
                  = let x = f x in 4 * 3 * 2 * 1 * x (1 - 1)
                  = let x = f x in 4 * 3 * 2 * 1 * f x 0
                  = let x = f x in 4 * 3 * 2 * 1 * 1
                  = 24
~~~

Fixed point of a functor
========================

Let's define our new expression type `Expr'` as the fixed point of the endofunctor `ExprF`:

~~~{.haskell}
newtype Fix f = Fix { unFix :: f (Fix f) }

type Expr' = Fix ExprF
~~~

And few _smart_ constructors to make it easier to construct expressions:

~~~{.haskell}
cst a   = Fix (ConstF a)
add a b = Fix (AddF a b)
mul a b = Fix (MulF a b)
~~~

> -- | 1 * 2 + 0
> e' = add (mul (cst 1) (cst 2)) (cst 0)

ExprF-Algebra commuting diagram
===============================

\vspace{.2in}
\centerline{\resizebox{4in}{!}{%
\begin{tikzcd}[ampersand replacement=\&,column sep=6em,row sep=6em]
  ExprF\ Expr' \arrow[d, "Fix", shift left, red] \arrow[r, "fmap\ g"] \& ExprF\ a \arrow[d, "alg"] \\
  Expr' \arrow[r, blue, "g" blue] \arrow[u, "unFix", shift left]              \& a
\end{tikzcd}
}}
\vspace{0.2in}

> g = alg . fmap g . unFix

Catamorphism
============

For any evaluation function `alg` of type:

> type Algebra a = ExprF a -> a

We can define a **catamorphism** `cata alg` to _fold_ the expression `Expr'` to the carrier type `a`:

~~~{.haskell}
cata :: Algebra a -> Expr' -> a
cata alg = alg . fmap (cata alg) . unFix
~~~

The evaluation function `alg` is not recursive.
The recursion has been abstracted away in the initial algebra, e.g. in the fixed point of the endofunctor.

Evaluation (bis)
=================

~~~{.haskell}
eval' :: Expr' -> Int
eval' = cata alg where
  alg :: ExprF Int -> Int
  alg (ConstF a) = a
  alg (AddF a b) = a + b
  alg (MulF a b) = a * b
~~~

~~~
 > eval' e'
 2
~~~

Optimization (bis)
==================

~~~{.haskell}
opt1' :: ExprF Expr' -> Expr'
opt1' (AddF a (Fix (ConstF 0))) = a
opt1' (AddF (Fix (ConstF 0)) a) = a
opt1' e = Fix e

opt2' :: ExprF Expr' -> Expr'
opt2' (MulF a (Fix (ConstF 1))) = a
opt2' (MulF (Fix (ConstF 1)) a) = a
opt2' e = Fix e
~~~

Composition & short-cut fusion
==============================

Naive composition of catamorphisms:

> optSlow :: Expr' -> Expr'
> optSlow = cata opt2' . cata opt1'

Can now be replaced by *short-cut fusion*, avoiding iterating twice over the data structure:

> optFast :: Expr' -> Expr'
> optFast = cata (opt2' . unFix . opt1')

~~~
 > optFast ef
 Fix {unFix = ConstF 2}
~~~

References
==========

[1] Lambda World, \href{http://www.lambda.world/}{Website}, \href{https://www.youtube.com/channel/UCEBcDOjv-bhAmLavY71RMHA/videos}{Videos}

[2] B. Milewski, "Category Theory for Programmers", \href{https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/}{Blog}, \href{https://github.com/hmemcpy/milewski-ctfp-pdf}{PDF}, \href{https://youtu.be/I8LbkfSSR58}{Videos}

[3] M. Lipovaca, "Learn You a Haskell for Great Good!", \href{http://learnyouahaskell.com/}{Website}

[4] C. Allen and J. Moronuki, "Haskell Programming from first principles", \href{http://haskellbook.com/}{Website}

[5] E. Cheng, "How to Bake Pi", Basic Books, 2015

[6] T. Williams, "Recursion Schemes by Example", \href{https://github.com/willtim/recursion-schemes}{Github}, \href{https://www.youtube.com/watch?v=Zw9KeP3OzpU}{Video}

[7] "Collaborative work on Mathematics, Physics and Philosophy", \href{https://ncatlab.org/nlab/show/HomePage}{nCatLab}

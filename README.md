Argo
====

Pragmatic functional programming language

Here are some of my thought about possible language design.
There is no language yet, but The journey is the destination.

Roadmap
-------

 * Become self-hosted as fast as possible to have first-hand experience with language.

 * First write parser and transpiler into Haskell without any type-checking or processing.

 * Implement parser and transpiler in language itself.

 * Add type-checking.

 * Compile to STG-language.
   We can generate Rust source code to build in-memory STG-expressions.
   This source code will serve as an external format
   for compiled code.
   This will allow to avoid complications with
   definition of some independent external format like it's serialization and validation.

 * Write STG-interpreter in Rust.

 * Experiment with STG-interpreter to finally define
   external format for compiled code that will allow validation
   and possible alternative implementations.

 * Try to provide IDE support with syntax-highlighting and rename refactoring

 * Implement profiling/hot-spot detection in interpreter.

 * Implement Just-in-time optimization (Just-in-time supercompilation).

 * Implement Just-in-time compilation into native language.

Design
------

### General design guidelines ###

Tooling and libraries proved to be critical components of practical language success.
Tooling and libraries need language stability. Java is prime example of highly developed ecosystem.
And it's firmly based on promise of backward compatibility and gradual changes.

Good language should be simple and minimalistic. Language shouldn't have features that can be removed.
Minimalistic language is easy to learn and it puts little mental burden during programming.
Many successful languages are really minimalistic.
Language simplicity is still subjective, so it comes down to personal judgment and taste.
I consider Scheme, C, Standard ML to be really minimalistic languages.
Java, Haskell 98 and Javascript and, maybe Python and Rust are more complex, but reasonably minimal.
OCaml, GHC Haskell, Command Lisp and Ruby are reasonably complex.
And at last C++, in my opinion, is unreasonably complex.

But language shouldn't stagnate. Language changes should be possible.
This means that language's changes should be proactively planned for.
Every feature should be small and simple.
Every feature should be as general as possible when simplicity is not sacrificed.
But most importantly language features shouldn't block future extensions.
A language feature should be allowed to become more general later.
A language feature shouldn't steal syntax that can be used for some future extension.
The problem of identifying set of possible extensions is ill defined, but nevertheless
some analysis should be performed anyway following personal judgment, taste and experience.

Readability should be valued more than expressiveness, because,
you know, code is read more frequently than written.

### Purity, lazy evaluation and non-strict semantics ###

It seems that laziness is the main corner stone when discussing functional languages design.
Main counter point seems to be memory-leaks.
As I see it laziness' discussions lack balanced comparison of trade-offs and benefits.

Purity on the other hand seems to be universally prised and accepted.
Modularity and referential transparency are highly valued and desired.

As been [stated](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf) laziness is the only practical mechanism that paved a way to purity.

Laziness is [required](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) to achieve really high level of modularity
and referential transparency.

Moreover laziness is one of main mechanisms that allowed to simplify language (Haskell)
to unprecedented minimalism. Laziness allows to

 * Get rid of functions of `() -> a` (unit to some other type) type.
   There is no need for distinct values and "parameterless" functions.

 * Get rid of special treatment of recursion/co-data etc.

 * Use `undefined` values to test and gradually define program on the course of program development.

 * Laziness allowed to move really far without using macros (or other meta-programming tools).
   Most needs filled by common macros' usage in other languages are [filled](https://www.google.com/search?q=macros+vs+lazy+evaluation&ie=utf-8&oe=utf-8) by most basic tools of
   lazy language.

Memory-leaks on are not eradicated when strict language is used! *They can happen anyway.*
Languages deal with memory-leaks by providing profilers and other tools to investigate leaks' causes.
Haskell provide similar memory profiling tools as other strict languages.

All in all I think that laziness' benefits outweigh it's problems. Practical functional language should be

 * Pure
 * Non-strict *by default*
 * Modular

### Hindley-Milner type-system ###

Hindley-Milner type-system is [prised](citation-needed) as a sweet-spot of practical type-systems.
On the other hand pure Hindley-Milner type-system is never used.

It should be noted what makes Hindley-Milner so good not from theoretical type-systems' point of view, but
from user's interface and user's experience point of view.

Hindley-Milner provides two separate languages:

 1. expression language
 2. type language

Expression language fully defines program behavior itself, but type language complements expressions
to help to get rid of erroneous programs.

 * Type language in Hindley-Milner is really minimal and lightweight.

 * Hindley-Milner type language defines *statically enforcible* contracts for program behavior.

 * Type language is fully separate from expression language.

Type language separation allows type-level reasoning for programmers.
They can fully ignore expressions and reason about program behavior using type-level language only.
Programmers can specify high level program behavior and properties using type-language only.
Type language only can be used as a design tool (like UML) without actual code (implementation).

This two-level structure with two separate languages seems really valuable.
This structure is severely damaged in dependently-typed languages.
But it seems that nevertheless dependently-typed languages tries to preserve it.
My position is that type-expression language separation is really valuable from user experience point of view.

Controversial topic here is Haskell's scoped type variables (ScopedTypeVariables extension).
At first Haskell assumes that type signature and value-expression
are totally separate for every declared name, but with scoped type variables they are not separate at all.
You can reference type-variables from type signature in value-expression however type signature and expression
are clearly syntacticly separated without any hint about their connection.
Nevertheless ScopedTypeVariables are seldom used but are really important when they are used.
We may provide some kind of special syntax to use when scoped type variables are needed.

````
mkpair1 :: <a b> a -> b -> (a, b) where
    mkpair1 aa bb = (ida aa, bb) where {
        ida :: a -> a;
        ida = id;
    };
````

### Type system extensions ###

Lots of very practical type-system extensions are provided by most functional languages.
This process seems to be constant boundary pushing trying to provide most practical benefits without
sacrificing type-system usability.

Haskell is positioned as an academic playground in this process.
Type-system extension trade-offs and benefits are not easily envisioned.
Haskell provides an environment to test and find best trade-offs.

Haskell-prime was created to provide stable Haskell-flavor, but it seems [abandoned](citation needed) by it's community.
It seems that Haskell community fully embraced the stream of minor backward incompatible changes.

Type-system extensions should be adopted when they seem to be fully understood and
tested through really explicit opt-in mechanisms that will prevent them to be generally adopted.
Adoption of experimental type-extensions creates language dialects and
poses real difficulties for novices trying to learn language.
Language extensions as present in GHC create large and complex language and
remove possibility to analyze language as a whole.
These distract investments into ecosystem and prevents fast ecosystem growth.

Type system extension mechanism can be provided for language but it should bring the promise of breakage.
Your library is guaranteed not to work with next release of a compiler if you use type-system extension.
This promise of instability should prevent general adoption of extensions.
When extension proves to be useful it will stimulate a community backed movement to adopt and stabilize
extension as a language feature.

Rust language provides another way to ensure that experimental features are not adopted.
Rust provides both stable compiler and unstable nightly builds.
Experimental language features are not accessible from stable compiler.
The only way to get experimental language features is to become compiler's tester which seams a reasonable contract.

We can bind experimental language features to specific compiler version to ensure breakage.
If we do this than library that uses experimental feature will be unstable.
It will surely break with next compiler version.
Rust-way seems to be better as it requires some special compiler version for unstable libraries and thus
clearly marks unstable libraries.

### Modules ###

Inner modules should be provided. By inner modules I mean modules enclosed into parent module
and defined in the same file alone with parent-module.
Haskell decision to have top-level modules only leads to bad program structure.
Short modules are awkward to use since it leads to too many short files.
Big modules leads to loose structure.

Inner modules bring the problem of file/compilation-unit lookup. Where module should be defined?
In the same file as parent-module or in subdirectory named as parent-module?
What if both are present? Case when both definitions are present should be compilation error,
but how can it be detected? Well working solution can be found in Java as it's separation between
packages and classes. Same separation can be introduced.
Every module should be defined in some package and name clash between module and package should be
compilation error.

Module exports should always have explicit type-signatures.
This seems counter intuitive since there is that sentiment that Hindley-Milner-based type systems are cool since
most types can be inferred by the compiler, so your are not required to provide type signatures.

There are counter-points so.
Providing type-signatures for exported symbols is established Haskell-practice.
Having type-signatures allows circular module dependencies without any special mechanisms.
And circular module dependencies are used in Haskell anyway with some obscure `.hs-boot` files.
Complex types seem to be a design problem.
You should probably not export symbol with overly complicated type or
you should export some specialization of it, which will allow feature refinements.

Qualified imports should be default.
Qualified imports are established Haskell practice.
Qualified imports greatly enhance code readability.
Unqualified imports should be restricted to save readability.
For example we can restrict unqualified import to single module.
This will allow to easily identify this module when we see unqualified name.
Some mechanism should be provided to make qualified imports easy to use.
Java-like imports can be adopted as a starting point. This means that

````
    import util.data.Map
````

will import names from util.data.Map module, that can be referenced with `Map.` prefix

````
    Map.lookup "key1" mymap
````

Haskell qualified imports are bad in this respect since it's usually a burden to type

````
    import qualified Data.Map as Map
````

### Infix operators ###

Infix operators should be restricted. There are kind of two extremes with this.
Haskell provides full custom operators. Java provides no operator overloading at all.
Java's position is that alphabetical names are always more descriptive than operators,
hence they should increase readability.
But counter point is that operators greatly enhance readability at the point when
you are familiar with them.
Haskell's problem is that it is hard to become familiar with operators.
And this is a real problem for language beginners.
It is hard to learn all relative operators' priorities.
When you see `Data.Lens` source first time it's a shock, even
if you know Haskell as a language reasonably well.

I propose to provide special `operatorset` files that will concisely define all
operators with their types and relative priorities (and associativity).

With C-family languages you can inspect documentation to find nice table of operators with their relative precedence
and quickly become fluent with complex expressions like if-conditions.

`operatorset` files should serve this operator-table purpose.
You can see it and learn it and than you can easily read code with these operators.

This means that every compilation-unit (module) can explicitly import single `operatorset`
and use operators from this set.

When multiple `operatorsets` are required this should hint that maybe it's time to split module
to reduce cognitive load.

Some standard `operatorset` can be defined to be implicitly available.
But custom operators should require explicit import of `operatorset` and
examining `operatorset` source should be enough to learn all operators and their precedence.

### Type classes ###

Type classes bring a can of worms with them.
Orphan instances should be eradicated, but it's already a stable Haskell practice not to define
orphan instances. The way to codify already established Haskell-practice is to only allow
instance declarations along with data-type declarations. This by definition rules out many
multi-parameter type-classes definitions. You can't have class `Convert a b` to allow conversions
from some type to another, since it's impossible to define where instance declarations are allowed
for such type-class. Should you define `Convert Int Text` along with `Int` type or along with `String` type.

Here is imaginary syntax to implement Haskell's Ord type-class.
Meanwhile `class` keyword seems too confusing from other languages' perspective and it may be better to use
`interface` or `trait`

````
    interface Ord extends Eq {
        compare :: self -> self -> Order;

        equals a b =
            case compare a b:
                EQ -> True
                _ -> False;
    }
````

We can provide multi-parameter type classes when other parameters depends on main *self* parameter

````
    interface <k v> Map k v {
        lookup :: k -> self -> Maybe v;
    }
````

Here implicit functional dependency is present. Haskell equivalent is

````
class Map self k v | self -> k, self -> v where
    lookup :: self -> k -> Maybe v
````

Haskell type-classes have one more flaw. They are not extensible.
It's not possible to slip in `Applicative` class as a super class for `Monad` without breaking all the code.
And there are possibilities of type-class hierarchy evolution.
This is known as [Numeric tower](https://en.wikipedia.org/wiki/Numerical_tower) in Lisp world and
Haskell has it's own Monad tower.
Monad tower evolution seems inevitable (see [The Other Prelude](https://wiki.haskell.org/The_Other_Prelude) and [AMP](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal) Haskell-proposals).
Type classes should be extensible without client code breakage.
Extensibility of type-classes requires constraints on type class definitions and implementations.

As a start we can allow single parameter type-classes only.
Type-class instances to be defined with it's data-type only.
As an extension mechanism we can allow type-class to provide implementation for required type-classes.
Another mechanism is direct extension. Extension means that type class is not only required, but
makes all methods of extended class accessible as if they are members of current class.

With Monad-Applicative example you can make `Monad` extend `Applicative`
In such setting you can move `return` method from `Monad` to `Applicative` without client code breakage.

Before making `Applicative` superclass of `Monad`:

```
    interface Applicative extends Functor {
        apply :: <a b> self (a -> b) -> self a -> self b;
        pure :: <a> a -> self a;
        then :: <a b> self a -> self b -> self b;

        then a b = apply (map (const id) a) b
    }

    interface Monad {
        return :: <a> a -> self a;
        bind :: <a b> self a -> (a -> self b) -> self b;
        then :: <a b> self a -> self b -> self b;

        then a b = bind a (\_ -> b)
    }
```

After making `Applicative` superclass of `Monad`:

```
    interface Applicative extends Functor {
        apply :: <a b> self (a -> b) -> self a -> self b;
        pure :: <a> a -> self a;
        return :: <a> a -> self a;
        then :: <a b> self a -> self b -> self b;

        then a b = apply (map (const id) a) b
        pure = return
        return = pure
    }

    interface Monad extends Applicative {
        bind :: <a b> self a -> (a -> self b) -> self b;
        join :: <a> self (self a) -> self a

        bind a f = join (fmap f a)
        join a = bind a id
    }
```

Every implementation(instance) that defined `Monad` before introducing `Applicative` as it's *extended* type-class
will work after the change and will automatically implement Applicative.

### Data types ###

There are list of problems with data-types syntax
(see [here](http://www.the-magus.in/Publications/notation.pdf) for example).
Moreover GADT's proves to be universally [accepted](OCaml GADT) type-system feature.
It's seems reasonable to always use GADT-syntax (as provided by GHC) even if GADTs are not allowed
as a type-level feature, because GADT-syntax is more explicit and clear than legacy grammar-like declaration.

Another problem is data-type namespaces. It use usually stated as a problem that Haskell's
records can define conflicting accessors.

Having inner-modules we can always provide new namespace for data-types. List module can be defined like this:

````
module A:
    module PersonUtil:
        data Person:
            public constructor person :: String -> String -> Person
        public name :: Person -> String
        name (person? n a) = n

    module CityUtil:
        data City:
            public constructor city :: String -> (Int, Int) -> City

        public name :: City -> String
        name (city? n coords) = n
````

It is already an established Haskell practice to name types with the same name as module. Like here

````
module Data.Text (...) where
    data Text = ...
    ...
````

It is [recommended](https://github.com/chrisdone/haskell-style-guide) to import this module like this

````
import qualified Data.Text as Text
import Data.Text (Text)
````

We can and probably should build it right into the language and make it possible to define data types
with their namespaces.

````
module A:
    data module Person:
        public constructor person :: String -> String -> Person
        public name :: Person -> String
        name (person? n a) = n

    data module City:
        public constructor city :: String -> (Int, Int) -> City

        public name :: City -> String
        name (city? n coords) = n
````

So when we import such a module:

````
    import pkg.A.Person
````

we can reference `Person` type without any additional ceremony, and we can reference all values
from Person module with `Person.` prefix. Like this:

````
    hello :: Person -> String
    hello p = "Hello, " ++ Person.name p
````

### Dependent types ###

Haskell has proved that people will push for dependent types.
Haskell proved that people like Haskell and try to bolt dependent types on it.
We can see two sides of it.

First is that new dependently typed languages are created
directly inspired by Haskell: Agda, Idris.

Second is that GHC provides type-system extensions to bring type-system closer to dependently-typed language.
We now have type-level literals and automatic promotion of data constructors into type level.

With this extensions GHC has now two complex and distinct computational languages.
One for computations with values and another as powerful as first for computations with types.

But do we really need dependent types at all?
There are [some](http://www.brics.dk/RS/01/10/BRICS-RS-01-10.pdf)
[evidence](http://jadpole.github.io/rust/typechecked-matrix/) that most benefits are provided
by type-level (string and numeric) literals and not by dependent types per se.

It seems still a research topic.
And research topic is something you should try to avoid when designing industrial programming language...

Still we can follow Haskell now and try to leave space for future addition of dependent-types.
This is still a problem and should be solved for successful language design.

One low hanging fruit on this path is language identifiers.
We should probably not make any assumption about identifier character case
and treat upper-case and lower-case identifiers without any prejudices
from the start to avoid many Haskell's problems that lead to awkward syntax.

### Metaprogramming ###

It is still an open question whether non-strict functional language needs
real metaprogramming.

Metaprogramming is easy in Lisp since Lisp has little syntax, but
it is definitly trickier in language like Haskell.

Metaprogramming brings quasi-quotation to the table.
After being watching for Yesod project I would really like to avoid
any quasi-quotation abuse and not provide any form of quasi-quotation
altogether.

My feeling is that metaprogramming shouldn't be easy.
We should really try to explore language limitations and
try to design language around them and
not drink metaprogramming kool-aid.

What I see as a real need for metaprogramming is compile-time
processing of some externally provided data. For example,
web-development requires HTML-code generation.
HTML-code is usually developed externally to provide
easy sharing and possible independent/design-driven development.
When HTML-code is developed independently we would like
to statically check and generate code to render application data
into given HTML-templates.

Another example is generation interoperability routines
from some standard interface-description language,
for instance generation of Document Object Model (DOM) implementation
using IDL-description provided by W3C.

This can be achieved with metaprogramming facilities.
Considering above examples we can conclude that
metaprogramming needs IO or at least an ability to read file.

Metaprogramming facilities needs to define language
that can be used to generate expressions/declarations.
My point is that this language should not be
full-fledged mirror of original language.
Generated language doesn't need syntactic-sugar.
There is simply no need for it since you can always implement
any sugar you want in host-language.

Even if metaprogramming is not implemented at first
we must leave space for it to be possibly bolted on later.
Therefore it is better to have two defined language levels from the start:
kernel-language, full-language.

Kernel-language is a language without syntactic-sugar.
Haskell doesn't have it's variant of kernel language.
GHC has *core*-language, but it is too low level and
Haskell to *core* translation is [really](GHC implementation) not obvious.

### Compilation and Run-time ###

This topic is akin discussion of lazyness.
What I want to state is that VM is needed for language, similar to JVM.
VM is the only known way to get both modularity and performance.

VM provides:

 * Fast compilation times.
   Go language proves that this may be very important to some people.

 * Fast execution of modular code.
   VM has no limitations on optimizing little functions spread out across bunch of modules.

Optimizing compilers can be used like GHC, but they has costs

 * Much longer compilation times.
   Compiler needs to optimize everything unlike VM that can optimize hot spots only.

 * True separate compilation can't be implemented because
   cross-module inlining prevents it.
   You need to recompile every dependent module even if module interface doesn't change
   to prevent inlining of old (bad) version of some function.

To get true benefits from VM. VM (byte-)code should be best suited for optimization.
This means that code should be high-level enough.
GHC Core language should be VM's "byte-code" if we going to build VM for GHC-Haskell,
because GHC performs most of it's optimizations on Core-level.

I'd like to use supercompilation as an optimization technique, since it get some
[good results](citation needed). If we what to use supercompilation as a VM's
optimization technique, we should use STG language as a VM's byte-code.

This brings us to "Just use JVM" sentiment.
The problem with JVM is that if we really need speed then JVM will be a limitation in the end.
JVM will not be able to optimize things that we really think needs optimizing for our language,
list deforestarization for example.
If we don't need speed we can easily write simple interpreter and be done.
If we write simple interpreter we can later implement optimization and
just-in-time compilation. If we choose JVM we are stuck with the choices of JVM-developers.

"Just use JVM" sentiment has another side.
It references that Java has lots of libraries and tools built for it and
we can just reuse them.
But Java-ecosystem is not unique, Python has lots of high-quality libraries,
GNU ecosystem built around C-programming language has a lot to offer.
We may be better with some flexible interoperability scheme than to tie our selfs with
one particular ecosystem.

Another point to remind is that lots of Java-tools are not tied to Java at all.
You can use Java-IDEs and Java build-tools for languages other than Java.

### Layout rule and curly braces ###

I have no strong opinions about layout rule. But lately I've stated to think that it brings more
complications than benefits. Modern languages like rust and ruby seems to get away without layout processing.
Haskell's layout rule was one of the obstacles when I've been learning language.
Even now the fact that Haskell's parser fixes parsing errors by automatically inserting closing curly braces
makes me uncomfortable.

### Numbers and literals ###

Haskell as it is has two simple problems with it's built in syntax and types.

First is it's reliance on `Integer`-type. Integer is unbounded integer type.
There reality is that `Integer` is not usually useful as it is for many programs, but
nevertheless it is used by almost all Haskell code, since any numeric literal
implicitly creates `Integer` and then convert `Integer` to some actually used type
(fast system dependent `Int`). Fast `Integer` implementation is not trivial and is not
easily found on many platforms (Javascript for instance).

It seems to me that large numeric literals are almost never used in actual code.
It may be better to get rid of reliance on `Integer`-type.
Go-language has polymorphic numeric literals like Haskell do, but doesn't rely on some
unbounded integer type. Another problem with `Integer` is overflow.
What if literal is too large for required type? With Haskell's `Num` class we get run-time error
when calling `fromInteger` method. It should be better to always get compile-time errors on such occasions.

We can solve these problems by introducing hierarchy of classes instead of simple `fromInteger` method.
We can introduce `FromWord8`, `FromWord16`, `FromWord32` classes and choose least required class
depending on actual literal value.

`8` is a syntactic-sugar for `fromWord8 (8::Word8)`. `300` is a syntactic sugar for `fromWord16 (300::Word16)`.

With such class hierarchy we can get rid of unbounded `Integer` type on platforms where it is problematic.
We get compile-time error `Word8 doen't implement FromWord16 interface` for expression `300::Word8`
with such class hierarchy.

Another Haskell pain-point is negative numbers. Should minus operator be unary operator?
Should minus operator be part of number syntax? I'm inclined to cut this knot and say that
minus is *always* infix operator and there will be no negative number syntax.

You can always write `0 - 5` or `negative 5` to denote negative number.
And this is an actual expression and not a single token.
Such expressions can be optimized away to behave like compile-time constants.
But in the end we get simple and uniform language without strange dark corners.

### Final syntax examples ###

````
package argo.util

import argo.util.Functor

data module List a implements Functor:
    public constructor cons :: <a> a -> List a
    public constructor nil :: <a> List a

    public append :: List a -> List a -> List a
    append (cons? x xs) ys = cons x (append xs ys)
    append nil? ys = ys

    public reverse :: <a> List a -> List a
    reverse (cons? x xs) = append (reverse xs) (cons x nil)
    reverse nil? = nil

    Functor.fmap f (cons? x xs) = cons (f x) (Functor.fmap f xs)
    Functor.fmap f nil? = nil
````

We may possibly leave out layout rules...

````
package ru.mobico.sviperll.test;

import argo.lang.System;
import argo.lang.Unit;
import argo.lang.IO;

module Main {
    public main1 :: IO Unit;
    main1 = do {
        name <- System.readLine;
        System.putStrLn message;
    } where {
        message = "Hello, World!";
    }

    public main2 :: IO Unit;
    main2 =
        do {
            name <- System.readLine;
            System.putStrLn message;
        } where {
            message = "Hello, World!";
        }

    public main3 :: IO Unit;
    main3 = do {
        name <- System.readLine;
        System.putStrLn message;
    } where {
        message = "Hello, World!";
    }

    public map :: <a b> (a -> b) - > List a -> List b;
    map f (cons? x xs) = cons (f x) (map f xs);
    map f nil? = nil;

    fibs :: List Int;
    fibs = cons 1 $ cons 1 $ zipWith (+) fibs (tail fibs);

    qsort :: <a> List a -> List a;
    qsort nil? = nil;
    qsort (cons? x xs) = append (qsort ls) (cons x $ qsort rs)
        where (ls, rs) = partition (< x) xs;

    fact :: <a> a -> a where {a implements Num};
    fact x
        if (x <= 0) = 1
        else = x * fact (x - 1)
}

````



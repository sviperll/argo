Argo
====

Pragmatic functional programming language

Here are some of my thought about possible language design.
There is no language yet, but The journey is the destination.

Goal
----

The goal is Golden Fleece.
Seriously the goal is to create simple and stable language from ground up that
can hopefully be proved more practical than Haskell or Idris.
Haskell and Idris should be treated as an inspiration, but their design decisions
should be suspiciously scrutinized to reevaluate them in context of modern programming.
Haskell and Idris shouldn't be treated as some kind of holy cow and
should be turned into laboratory rabbit instead.

Design
------

### General design guidelines ###

Tooling and libraries proved to be critical components of practical language success.
Tooling and libraries needs language stability. Java is a prime example of highly developed ecosystem.
And it's firmly based on promise of backward compatibility and gradual changes.

Good language should be simple and minimal. Language shouldn't have features that can be removed.
Minimal language is easy to learn and it puts little mental burden during programming.
Many successful languages are really minimal.
Language simplicity is still subjective, so it comes down to personal judgment and taste.

But language shouldn't stagnate. Language changes should be possible.
This means that language's changes should be proactively planned for.
Every feature should be small and simple.
Every feature should be as general as possible when simplicity is not sacrificed.
But most importantly language features shouldn't block future extensions.
A language feature should be allowed to become more general later.
A language feature shouldn't steal syntax that can be used for some feature extension.
The problem of identifying set of possible extensions is ill defined, but nevertheless
some analysis should be performed anyway following personal judgment, taste and experience.

Readability should be valued more than expressiveness, as, you know, code is read more frequently than written.

### Purity, lazy evaluation and non-strict semantics ###

It seems that laziness is the main corner stone when discussing functional languages design.
Main counter point seems to be memory-leaks.
As I see it laziness discussions seem to lack balanced comparison of trade-offs and benefits.

Purity on the other hand seems to be universally prised and accepted.
Modularity and referential transparency are highly valued and desired.

As been [stated](citation-needed) laziness is the only practical mechanism that paved a way to purity.

Laziness is [required](Why functional programming matters) to achieve really high level of modularity
and referential transparency.

Moreover laziness is one of the main mechanisms that allowed to simplify language (Haskell)
to unprecedented minimalism. Laziness allows to

 * Get rid of functions of `() -> a` (unit to some other type) type.
   There is no need to separate values and "parameterless" functions.

 * Get rid of special treatment of recursion/co-data etc.

 * Use `undefined` values as stubs to test and gradually define program on the course of program development.

 * Laziness allowed to move really far without using macros (or other meta-programming tools).
   Most needs filled by common macros' usage in other languages are [filled](example needed) by most basic tools of lazy language.

Lets look into memory-leaks problem.
Memory-leaks on are not eradicated when strict language is used! *They can happen anyway.*
Languages deal with memory-leaks by providing profilers and other tools to investigate leaks' causes.
Haskell provide similar memory profiling tools as other strict languages.

All in all I think that laziness benefits outweigh it's problems. Practical functional language should be

 * Pure
 * Non-strict *by default*
 * Modular

### Hindley-Milner type-system ###

Hindley-Milner type-system is [prised](citation-needed) as a sweet-spot of practical type-systems.
On the other hand pure Hindley-Milner type-system is never used.

It should be noted what makes Hindley-Milner so good not from theoretical type-systems point of view, but
from user interface, user experience point of view.

Hindley-Milner unlike System F provides two separate languages:

 * expression language
 * type language

Expression language is what CS in general is about, but type language is what really interesting about Hindley-Milner.

 * Type language in Hindley-Milner is really minimal and lightweight.

 * Hindley-Milner type language defines *statically enforcible* contracts for program behavior.

 * Type language is fully separate from expression language.

Type language separation allows type-level reasoning for programmers.
They can fully ignore expressions and reason about program behavior using type-level language only.
Programmers can specify high level program behavior and properties using type-language only.
Type language only can be used as a design tool (like UML, but better) without actual code (implementation).

This two-level structure with two separate languages seems really valuable.
This structure is severely damaged in dependently-typed languages.
But it seems that nevertheless dependently-typed languages tries to preserve it.
My position is that it is not a coincidence.
Type-expression language separation is really valuable from user experience point of view.

This separations should be preserved as much as possible.

### Type system extensions ###

Lots of very practical type-system extensions are provided by most functional languages.
This process seems to be constant boundary pushing trying to provide most practical benefits without
sacrificing type-system usability.

Haskell is positioned as an academic playground in this process.
Type-system extension trade-offs and benefits are not easily envisioned.
Haskell provides an environment to test and find best trade-offs.
From the other side this seems like a constant fight of the whole ecosystem with it own energy source.
Life has flourished around a bright star, but constant threat is present to be wiped out by it's super-nova explosion.

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
extension as language feature.

### Module structure ###

Inner modules should be provided.
Haskell decision to have top-level modules only leads to bad program structure.
Short modules are awkward to use since it leads to too many short files.
Big modules leads to loose structure.

Inner modules leads to problem of file/compilation-unit lookup. Where module should be defined?
In the same file as parent-module or in subdirectory named as parent-module?
What if both are present? Case when both definitions are present should be compilation error,
but how can it be detected? Well working solution can be found in Java as it's separation between
packages and classes. Same separation can be introduced.
Every module should be defined in some package and name clash between module and package should be
compilation error.

### Module exports ###

Module exports should always have explicit type-signatures.
This seems counter intuitive since there is that sentiment that Hindley-Milner type systems are cool since
most types can be inferred by the compiler, so your are not required to provide type signatures.

There are counter-points so.
Providing type-signatures for exported symbols is established Haskell-practice.
Having type-signatures allows circular module dependencies without any special mechanisms.
And circular module dependencies are used in Haskell anyway with some obscure `.hs-boot` files.
Complex type seems to be a design problem.
You should probably not export symbol with overly complicated type or
you should export some specialization of it, which will allow feature extension.

### Module imports ###

Qualified imports should be default. Qualified imports are frequently used by Haskell community.
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

Infix operators should be restricted. There is some kind of two extremes with this.
Haskell provides full custom operators. Java provides no operator overloading at all.
Java's position is that alphabetical names are always more descriptive than operators,
hence they should increase readability.
But counter point is that operators greatly enhance readability at the point when
you are familiar with them.
Haskell problem is that it is hard to become familiar with operators.
And this is real problem for language beginners.
It is hard to lean all relative operators' priorities.
When you see `Data.Lens` source it's a shock, even if you know Haskell as a language.

I propose to provide special `operatorset` files that will concisely define all
operators with their types and relative priorities.

As with C-family languages you can inspect operator-table several times and
become fluent with complex expressions like if conditions.

`operatorset` files should serve this operator-table purpose.
You can see it and learn it and than you can easily read code with this operators.

This means that every compilation-unit (module) can explicitly import single `operatorset`
and use operators from this set.

When multiple operatorsets are required this should hint that maybe module should be split
to reduce cognitive load.

Some standard operatorset can be defined to be implicitly available.
But custom operators should require explicit import of operatorset and
examining operatorset source should be enough to learn all operators and their precedence.

### Type classes ###

Type classes bring a can of worms with them.
Orphan instances should be eradicated, but it's already a stable Haskell practice not to define
orphan instances. Haskell type-classes have one more fatal flaw. They are not extensible.
It's not possible to slip in `Applicative` class as a super class for `Monad` without breaking all the code.
And there are possibilities of classes evolution.
This is known as [Numeric tower](https://en.wikipedia.org/wiki/Numerical_tower) in Lisp world and
Haskell has it's own Monad tower.
Monad tower evolution is required (where is my `Pointed` class?).
Type classes should be extensible without client code breakage.
Extensibility of type-classes requires constraints on type class definitions and implementations.

As a start we can allow single parameter type-classes only.
Type-class instances to be defined with it's data-type only.
As an extension mechanism we can allow type-class to extend another type-class and
to provide implementation for extended type class. So `Monad` will not only *require*
`Applicative`, but will *extends* it and provide it's implementation.

Every implementation(instance) that defined `Monad` before introducing `Applicative` as it's *extended* type-class
will work after the change and will automatically implement Applicative.

Meanwhile `class` keyword seems to confusing from other language perspective and it may be better to use
`interface` or `trait`

Here is imaginary syntax to implement Haskell's Ord type-class:

````
    interface Ord extends Eq:
        compare :: self -> self -> Order

        equals a b =
            case compare a b:
                EQ -> True
                _ -> False
````

We can provide multi-parameter type classes when other parameters depends on main *self* parameter

````
    interface Map k v:
        lookup :: self -> k -> Maybe v
````

Here implicit functional dependency is present. Haskell equivalent is

````
class Map self k v | self -> k, self -> v where
    lookup :: self -> k -> Maybe v
````

### Data types ###

There are list of problems with data-types syntax
(see [here](http://www.the-magus.in/Publications/notation.pdf) for example).
Moreover GADT's proves to be universally accepted type-system feature.
It's seems reasonable to always use GADT-syntax (as provided by GHC) even if GADTs are not allowed
as a type-level feature.

Another problem is data-types namespaces. For example Haskell records can define conflicting accessors.

Having inner-modules we can always provide new namespace for data-types. List module can be defined like this:

````
data module List a:
    public constructor cons :: a -> List a
    public constructor nil :: List a

    public map :: (a -> b) -> List a -> List b
    map f (cons? x xs) = cons (f x) (map f xs)
    map f nil? = nil
````

List module will provide List type and will export cons, nil and map symbols.

### Dependent types ###

### Final syntax examples ###

````
data module List a implements Functor:
    public constructor cons :: <a> a -> List a
    public constructor nil :: <a> List a

    Functor.fmap f (cons? x xs) = cons (f x) (Functor.fmap f xs)
    Functor.fmap f nil? = nil
````

### Compilation and Run-time ###

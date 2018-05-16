# LM Language
LM is a statically typed, multi-paradigm language language with focus on:

* First class tooling and IDE integration backed by the compiler. This includes:
  * Error highlighting in the editor
  * Auto completion (intellisense)
  * Showing types on hover
  * Renaming
  * Refactoring

* Ease of use; It shouldn't take more than a few commands to download
  the toolchain and create a new project.
  
* A rich type system
  * First class modules
  * Algebraic types with pattern matching
  * Generic types (Universal and existential quantification)
  * Higher kinded types
  * Higher rank types
  * Scala like implicits
  
* A good deal of type safety

* Modern yet familiar syntax.

* Small core language; Most syntax should be sugar for a handful of core
  concepts.
  
* Support for multiple backends

## Non goals
In order to focus on the aforementioned goals, I've decided to give up on the following things:
* Inheritance: This is a controversial one and mostly a matter of taste. I think most of
  the things enabled by inheritance can be achieved by using interfaces. LM interfaces are way
  more powerful than those you might be familiar with. They're more like Haskell type classes.

* Global type inference: This is more of a tooling issue. LM requires type annotations on top level
  function definitions. While it is certainly possible (albeit tricky) to infer function parameters
  in some places, editor tools such as auto completion don't play well with that. Moreover, it can
  lead to misleading type errors.
  
## Quick start
https://github.com/lm-language/lmc/wiki/Quickstart

## Development
Development is on going and right now, only the frontend of the compiler is implemented.
This means you can't run any LM programs. You can only type check.
You need to install sbt to build this.

```
git clone https://github.com/lm-language/lmc
cd lmc
sbt test
```

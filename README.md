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


## Trying it out
Right now, you need to build it from source.
You need Java and [sbt](https://www.scala-sbt.org/1.0/docs/Setup.html) installed.

```bash
cd ~
mkdir .lmc
cd .lmc

# clone compiler
git clone https://github.com/lm-language/lmc
cd lmc

# build
sbt pack
```
Building will take a while (10-15 minutes) when doing it
for the first time, so have a cup of coffee or something.

Once the build is complete, you can add `~/.lmc/target/pack/bin`
to your PATH so that you can run `lmc` from command line.

Right now, lmc has only one command (`server`) which starts
the IDE server.

It is recommended that you install VS Code plugin for lmc
for editor integration. Right now, it's not published so
you have to manually check it out.

```bash
cd ~/.vscode/extensions
# or ~/.vscode-insiders for insiders release

git clone https://github.com/lm-language/lmc-vscode
cd lmc-vscode
# requires nodejs to be installed
npm install
```

Now, when you open any `.lm` file in VS code, you'll
get error highlighting and hover info.
  
## Quick start
https://github.com/lm-language/lmc/wiki/Quickstart

## Implemented features
If this list is out of sync, `.lm` files in test/compiler will
contain all the features implemented (except for IDE ones).

* Basic let declarations
* Lambdas, calls (including labeled arguments), blocks
* if expressions
* Modules
* Generics with universal quantification (forall types)
* Type aliases
* Higher kinded types
* Rank-N types
* Extern declarations for foreign function interface.
* IDE features (they're a bit rough though; Types and errors aren't pretty printed
  but still usable.)
  * Show type on hover
  * Error highlighting (showing exact errors as the command line compiler)
  * Go to definition

## To be implemented
* imports
* Syntax sugar for declaring records (they're just modules).
* Syntax sugar for interfaces (again, they're just modules).
* Algebraic data types
* Pattern matching over ADTs
* Implicits
* fn declarations (for recursive functions)
* module declarations (
  modules can be declared using let declarations; module expressions
  are done).
* A backend so that something can actually run.
* IDE features
  * Auto complete
  

## Development
Development is on going and right now, only the frontend of the compiler is implemented.
This means you can't run any LM programs. You can only type check.
You need to install sbt to build this.

```
git clone https://github.com/lm-language/lmc
cd lmc
sbt test
```

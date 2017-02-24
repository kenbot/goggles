# Contributor guidelines
If you are considering contributing - thank you! Your time is much appreciated.

## Features
New features have a good chance of being rejected; the limited scope of 
the library is one of its strengths. Before submitting a PR, please read 
the "Design principles" section below, and raise an issue for discussion.

## Bugs
Patches welcome! Unhelpful compilation error messages are bugs, 
and will be given high priority.

## PRs
The usual hygiene stuff. Keep your commits focused on a single task, 
avoid messing up whitespace, keep the imports clean, run the tests.

## Design principles
Please try to align your feature requests and PRs with
these general principles.

### "You already know how to use it" 
Ease-of-use as a "missing piece of the puzzle" in lens libraries is 
a central motivation for Goggles. The syntax used in the 
string format should be familiar to developers, and preferably 
attested in other popular languages or technologies. 

### Incomplete is fine
Goggles does not aspire to provide syntax to cover Monocle's 
entire surface area; features should be chosen based on their
utility and whether they can be represented with familiar syntax
attested elsewhere. For additional features, Monocle can be used 
in the usual way.

### First class error messages
Helpful, detailed error messages are a first-class requirement
of Goggles functionality. The messages should help beginners
understand and discover optics concepts, while not hindering expert users.

Because we know a lot more about what the user is trying to
achieve here than scalac does, we can construct much better
messages using the OpticInfo data that the macro accumulates
internally.

### Anti-magic
The behaviour of the DSL should not be surprising, nor feel "magic".
In particular, this means that Scala language elements must behave like
normal Scala; the macro should not pun on code-tree structure for semantics.

### Compile-time over run-time complexity
Where a tradeoff is required, it is preferable that the macro code accrue
complexity than the code that is generated to be run by the user. For example,
compile-time reflection is acceptable, but runtime reflection is not.

### Tests
As many cases as possible should be tested, using Scalacheck where sensible
invariants can be discovered. Compilation errors should be directly
tested, using the `testdsl` front end. End-to-end tests (ie actually running
the macro) are preferred, but unit tests can be added where they clarify
local functionality.
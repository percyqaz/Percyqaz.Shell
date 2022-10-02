This is the follow up file to Syntax.md which explains the shell syntax and concept

It doesn't act as documentation but lets you (and also me) see where I'm going with this

### No side-effects

The shell on its own has no way to produce side effects other than printing to console with the `@:` operator

The only way to give it side effects is to provide the environment with functions that give side effects

Todo: write up documentation on the API from F# that lets you do this (it's still subject to major change)

This means there will be standard libraries providing side effect functions:
- For more complex data manipulation on objects and lists
- For file I/O
- For for importing other libraries/scripts dynamically

All optional if not needed - Your program may have its own specific API and not use any standard libraries

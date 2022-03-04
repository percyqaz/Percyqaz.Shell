# Percyqaz.Shell
Toolkit to add command line/IPC capabilities to your F# application

So far, just me fooling around mostly

KEY AIMS/FEATURES:
- The basic building block of this shell is "Values" - structured like JSON, and "Commands" - an action that takes a list of required arguments, optional arguments and optional flags, and return a "Value".
- There is a type system (very typescript-like) that verifies all programs before any execution
- Steal some ideas from functional programming/unix shells (like piping arguments being the main way to do productive things)

- Ability to embed/interop shell engine with .NET code
- Ability to use the shell as a general purpose shell (not my use case at the moment so batteries will not be included)

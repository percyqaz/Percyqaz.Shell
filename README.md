# Percyqaz.Shell
Toolkit to add command line/IPC capabilities to your F# application

Mostly just a simplistic POC/toy at the moment

KEY CONCEPT/FEATURES:
- The basic building block of this shell is "Values" - structured like JSON, and "Commands" - an action that takes a list of arguments (they are values) and returns another value
- Commands/variables/scripts/etc can be provided by the host process (in F#)
- Commands/variables/scripts/etc can be provided by the user

OTHER AIMS:
- Shell interface can be exposed to other processes for inter-process communication (make your commands threadsafe yourself)
- Ability to use the shell as a general purpose shell (not my use case at the moment so batteries will not be included)

This will eventually become my goto for whenever I need a basic "menu" for a several-purpose console tool, or remote-control capabilities for an application

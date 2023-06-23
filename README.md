# Percyqaz.Shell

Toolkit for creating a list of available "commands" and then providing a CLI/REPL for executing those commands

Older versions were slightly more geared towards a scripting language with variables and expressions, this has been put away for now

All planned features:
- Developer can create a "shell context" and define available commands for that context
- An underlying simplified scripting language (I have designed it but not implemented it)
	- Data types are very simple, if you want more power use an actual programming language
	- Scripts are oriented around what I like to call "data shoveling" - Taking many inputs and returning an output
	- You can give scripts a standard library/access to F# code as you like as a developer to make the scripts as powerful as desired
- Commands can be implemented in F# code and converted to a form callable from the scripting language
- Commands can be implemented in the scripting language
- Both implementations of commands can be called from a CLI/REPL
- The CLI/REPL can be repurposed for inter-process communication

What currently exists:
- Developer can create a "shell context" and define available commands for that context
- Commands can be implemented in F# code
- Commands can be called from a CLI/REPL
- The CLI/REPL can be repurposed for inter-process communication
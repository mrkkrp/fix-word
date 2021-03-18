# Fix Word

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/fix-word-badge.svg)](https://melpa.org/#/fix-word)
![CI](https://github.com/mrkkrp/fix-word/workflows/CI/badge.svg?branch=master)

This is a package that allows us to transform words intelligently. It
provides the function `fix-word` that lifts functions that do string
transformation into commands with interesting behavior. There are also some
built-in commands built on top of `fix-word`.

## Installation

The package is available via MELPA, so you can just type `M-x
package-install RET fix-word RET`.

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`. Then you can require it in your init file like
this:

```emacs-lisp
(require 'fix-word)
```

## API description

```
fix-word fnc
```

Lift the function `fnc` into a command that operates on words and regions.

The following behaviors are implemented:

1. If the point is placed outside of a word, apply `fnc` to the previous
   word. When the command is invoked repeatedly, every its invocation
   transforms one more word moving from right to left. For example
   (upcasing, `^` shows the position of the point):

   ```
   The quick brown fox jumps over the lazy dog.^
   The quick brown fox jumps over the lazy DOG.^
   The quick brown fox jumps over the LAZY DOG.^
   The quick brown fox jumps over THE LAZY DOG.^
   ```

   The point doesn't move, this allows us to fix recently entered words and
   continue typing.

2. If the point is placed inside of a word, the entire word is transformed.
   The point is moved to the first character of the next word. This allows
   us to transform several words by invoking the command repeatedly.

   ```
   ^The quick brown fox jumps over the lazy dog.
   THE ^quick brown fox jumps over the lazy dog.
   THE QUICK ^brown fox jumps over the lazy dog.
   THE QUICK BROWN ^fox jumps over the lazy dog.
   ```

3. If there is an active region, all words in that region are transformed.

Use `fix-word` to create new commands like this:

```emacs-lisp
(defalias 'command-name (fix-word #'upcase)
  "Description of the command.")
```

There is also a macro that defines such commands for you:
`fix-word-define-command`.

----

```
fix-word-define-command name fnc &optional doc
```

Define a `fix-word`-based command named `name`. `fnc` is the processing
function and `doc` is the documentation string.

## Built-in commands

The default commands to upcase/downcase/capitalize words are not convenient,
for the following reasons:

1. There are three different commands for upcaseing, for example. The user
   needs to remember the three commands, their key bindings, and when to use
   each of them. There should be one command per action: one for upcasing,
   one for downcasing, and one for capitalizing.

2. The commands on regions don't have dedicated key bindings and are
   disabled by default.

3. The commands like `upcase-word` depend on the position of pointer inside
   of the word, so that the result of upcasing `"fo^o"`is `"foO"`. This
   packages assumes that you want `"FOO"`.

4. One needs to use arguments for commands like `upcase-word` to make them
   correct the words that one has just written and only one word can be
   adjusted in this way.

Here are the commands that try to fix these flaws:

* `fix-word-upcase`
* `fix-word-downcase`
* `fix-word-capitalize`

I propose replacing of the built-ins with these new commands:

```emacs-lisp
(global-set-key (kbd "M-u") #'fix-word-upcase)
(global-set-key (kbd "M-l") #'fix-word-downcase)
(global-set-key (kbd "M-c") #'fix-word-capitalize)
```

## License

Copyright © 2015–present Mark Karpov

Distributed under GNU GPL, version 3.

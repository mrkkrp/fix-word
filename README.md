# Fix Word

This is a package that allows to transform words intelligently. Fix Word
provides function `fw/fix-word` that lifts functions that do string
transformation into commands with interesting behavior. There are also some
built-in command based on `fw/fix-word`, see below.

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'fix-word)` into your configuration file. Done!

## API Description

```
fw/fix-word fnc
```

Lift function `fnc` into command that operates on words and regions.

The following behaviors are implemented:

If the point is placed outside of a word, apply `fnc` to previous word. If
the command is invoked repeatedly, every its invocation transforms one more
word moving from right to left. For example (upcasing, `^` shows position of
point/cursor):

```
The quick brown fox jumps over the lazy dog.^
The quick brown fox jumps over the lazy DOG.^
The quick brown fox jumps over the LAZY DOG.^
The quick brown fox jumps over THE LAZY DOG.^
```

The point doesn't move, this allows user to fix recently entered words and
continue typing.

If the point is placed inside any part of a word, the whole word is
transformed. The point is moved to first character of the next word. This
allows to transform words repeatedly pressing dedicated key binding.

```
^The quick brown fox jumps over the lazy dog.
THE ^quick brown fox jumps over the lazy dog.
THE QUICK ^brown fox jumps over the lazy dog.
THE QUICK BROWN ^fox jumps over the lazy dog.
```

If there is an active region, all words in that region are transformed.

Use `fw/fix-word` to create new commands like this:


```emacs-lisp
(defun command-name ()
  "Description of the command."
  (interactive)
  (fw/fix-word #'upcase))
```

There is also a macro that defines such commands for you:
`fw/define-command`.

## Built-in Commands

So, here I describe built-in commands based on `fw/fix-word`.

Default commands to upcase/downcase/capitalize stuff are not very convenient
to say the least, for the following reasons:

1. There are three different commands to upcase thing, for example. User
   needs to remember three commands, their key bindings, and when to use
   each of them. There should be one command per action: one for upcasing,
   one for downcasing, and one for capitalizing.

2. Commands on regions don't have dedicated key bindings and what's even
   worse: they are disabled by default!

3. Commands like `upcase-word` depend on position of pointer inside word,
   now tell me what's expected result of upcase command here: `"fo^o"`? I
   bet you want `"FOO"`, not `"foO"`. The same applies to capitalization,
   etc. These commands should work on entire word no matter what.

4. You need to use arguments for commands like `upcase-word` to make them
   correct words that you've just written. What's even worse, you can fix
   only one last word this way. What if you want to fix a couple of them?

Here are commands that are trying to fix all these flaws:

* `fw/upcase`
* `fw/downcase`
* `fw/capitalize`

I propose replacing of old functions with these, you can do it this way:

```emacs-lisp
(global-set-key (kbd "M-u") #'fw/upcase)
(global-set-key (kbd "M-l") #'fw/downcase)
(global-set-key (kbd "M-c") #'fw/capitalize)
```

There are many ways `fw/fix-word` can be used. For example, if you have a
function `translate` that translates some text, you can get a command that
can translate words. It will work in all those modes described here.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.

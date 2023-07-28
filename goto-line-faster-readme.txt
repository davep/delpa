
goto-line-faster.el provides a utility function that binds M-g followed
by a number (1 through 9) to an invocation of goto-line with the number
pre-populated in the input in the minibuffer.

I wrote this because I've never liked 'M-g g' as a way to go to a line
number, but I'd like to use some of the other M-g-prefixed key bindings
too. This pretty much gives me the best of both worlds.

Please note that this version of the code is very different from the
original version, with <URL:https://github.com/phil-s> providing a much
cleaner approach.
#!/usr/bin/env bash

cd ~/.emacs.d/lib/meow
exec emacs -q -L . -l meow.el -l ../../meow-start.el

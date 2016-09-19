# Suggestion-box.el

This package is more or less for major-mode maintainers who want to
show type information on the cursor and currently only tested on
nim-mode (https://github.com/nim-lang/nim-mode).

The tooltip will be placed above on the current cursor, so most of
the time, the tooltip doesn't destruct your auto-completion result.

![suggestion-box](https://cloud.githubusercontent.com/assets/1082473/18650134/3246c024-7e78-11e6-8e8b-4fb7d832495f.gif)

## How to use (implement):

If you just want to show type information after company-mode's
:post-completion or :exit-function for `completion-at-point',
you may implement something like this:

``` lisp

   (defun xxx-completion-at-point ()
      ... ; do something
     :exit-function (lambda (string status)
                      (if STRING-IS-FUNCTION
                         (insert "()")
                         (backward-char 1)
                         (suggestion-box TYPE-INFO)))
     )

```

## License
GPLv3 License

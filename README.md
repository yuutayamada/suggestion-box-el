# Suggestion-box.el

Show tooltip on the cursor with convenient information

Note: this package is still early stage. I'm going to
support [nim-mode](https://github.com/nim-lang/nim-mode) first and
then other programming major-modes.

## GIF

The tooltip will be placed above on the current cursor, so most of
the time, the tooltip doesn't destruct your auto-completion
result. (hopefully)

![suggestion-box](https://cloud.githubusercontent.com/assets/1082473/18650134/3246c024-7e78-11e6-8e8b-4fb7d832495f.gif)

## How to implement (for package maintainers):

If you want to show type information after company-mode's
:post-completion or :exit-function for `completion-at-point',
you may implement something like this:

``` lisp

   (defun xxx-completion-at-point ()
      ... ; do something
     ;; you can find information about :exit-function by `completion-extra-properties'
     :exit-function (lambda (string status)
                      (if STRING-IS-FUNCTION ; predicate whether check the string is function
                         (insert "()")
                         (backward-char 1)
                         ;; the TYPE-INFO is string, which you want to show it
                         ;; on the cursor
                         (suggestion-box TYPE-INFO)))
     )

```

As a side note, you might want to use this package without :exit-function or
company-mode's :post-completion, but I didn't do it because showing
popup unconditionally were too annoying. So I don't recommend.

## License
GPLv3 License

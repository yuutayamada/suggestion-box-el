;;; test.el --- test for suggestion-box -*- lexical-binding: t; -*-

;;; Code:
(require 'suggestion-box)
(require 'cl-lib)
(require 'ert)

(defvar test-nth nil)

(cl-defmethod suggestion-box-normalize ((_backend (eql test)) raw-str)
  "Return normalized string."
  (suggestion-box-h-filter
   :content    (suggestion-box-h-trim raw-str "(" ")")
   :split-func (lambda (content) (split-string content ", "))
   :nth-arg    test-nth
   :sep "" :mask1 "" :mask2 ""))

(defconst test-arg "-- (arg1: string, arg2: int) -- this is blah blah blah")

(cl-defun test-sb (test-arg arg
                   &key no-text-property backend inside-other-paren)
  (setq test-nth arg)
  (unless no-text-property
    (suggestion-box-put test-arg :backend 'test))
  (save-excursion
    (with-temp-buffer
      (erase-buffer)
      (when inside-other-paren
        (insert "\n\n(string)")
        (backward-char 3))
      (suggestion-box-normalize
       backend
       (if no-text-property
           (substring-no-properties test-arg)
         test-arg)))))

(ert-deftest test1-1 ()
  "suggestion-box without text-property"
  (should (equal "arg1: string"
                 (test-sb test-arg 1 :backend 'test :no-text-property t))))

(ert-deftest test1-2 ()
  "suggestion-box without text-property"
  (should (equal "arg2: int"
                 (test-sb test-arg 2 :backend 'test :no-text-property t))))

(ert-deftest test1-3 ()
  "suggestion-box without text-property"
  (should (equal "too many arguments?"
                 (test-sb test-arg 3 :backend 'test :no-text-property t))))

(ert-deftest test1-4 ()
  "suggestion-box without text-property"
  (should (equal 'ignore
                 (test-sb test-arg 1 :backend 'test :no-text-property t :inside-other-paren t))))

;;
(ert-deftest test2-1 ()
  "suggestion-box with text-property"
  (should (equal
           '(:backend test :content "arg1: string")
           (test-sb test-arg 1 :backend nil))))

(ert-deftest test2-2 ()
  "suggestion-box with text-property"
  (should (equal
           '(:backend test :content "arg2: int")
           (test-sb test-arg 2 :backend nil))))

(ert-deftest test2-3 ()
  "suggestion-box with text-property"
  (should (equal
           '(:backend test :content "too many arguments?" )
           (test-sb test-arg 3 :backend nil))))

(ert-deftest test2-4 ()
  "suggestion-box without text-property"
  (should (equal '(:backend test :content ignore)
                 (test-sb test-arg 1 :backend nil :inside-other-paren t))))

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile: t
;; End:

;;; test.el ends here

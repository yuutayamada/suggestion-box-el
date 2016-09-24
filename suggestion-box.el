;;; suggestion-box.el --- show tooltip on the cursor -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (popup "0.5.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Note: this package is still early stage. I'm going to
;; support [nim-mode](https://github.com/nim-lang/nim-mode) first and
;; then other programming major-modes.
;;
;; If your nim-mode merged this PR (https://github.com/nim-lang/nim-mode/pull/138)
;; and you already configured nimsuggest, you can use suggestion-box
;; without configuration.
;;
;;
;; This package is more or less for major-mode maintainers who want to
;; show type information on the cursor and currently only tested on
;; nim-mode (https://github.com/nim-lang/nim-mode).

;; The tooltip will be placed above on the current cursor, so most of
;; the time, the tooltip doesn't destruct your auto-completion result.

;; How to implement:
;;
;; if you want to show type information after company-mode's
;; :post-completion or :exit-function for `completion-at-point',
;; you may implement something like this:
;;
;;   (defun xxx-completion-at-point ()
;;      ... ; do something
;;     :exit-function (lambda (string status)
;;                      (if STRING-IS-FUNCTION
;;                         (insert "()")
;;                         (backward-char 1)
;;                         (suggestion-box TYPE-INFO)))
;;     )
;;
;;
;; But, I might change API to reduce defgeneric/defmethod stuff, so
;; please keep in mind this package isn't stable yet.
;;
;;; Code:

(require 'popup)
(require 'cl-lib)
(require 'eieio)
(require 'rx)
(require 'subr-x) ; need Emacs 25.1 or later for `when-let', `if-let'
                  ; and also `alist-get' in subr.el


(defgroup suggestion-box nil
  "Show information on the cursor."
  :link '(url-link "https://github.com/yuutayamada/suggestion-box-el")
  :group 'suggestion-box)

(defface suggestion-box-face
  '((((class color) (background dark))
     (:background "#00ffff" :foreground "black"))
    (((class color) (background light))
     (:background "#000087"  :foreground "white"))
    (t (:inverse-video t)))
  "Face for suggestion-box's tooltip."
  :group 'suggestion-box)

(defclass suggestion-box-data ()
  ((bound   :initarg :bound)  ; you can store any data
   (popup   :initarg :popup)  ; popup object or nil
   (content :initarg :content
            :type string)
   (ppss    :initarg :ppss)   ; `syntax-ppss'
   (backend :initarg :backend))
  :documentation "wip")

(defclass suggestion-box-embed-data ()
  ((backend :initarg :backend)
   (handler :initarg :handler)
   (data    :initarg :data
            :allow-nil-initform t))
  :documentation "wip")

(defun suggestion-box-embed-p (text-obj)
  (when-let ((obj (suggestion-box-get-embed-text text-obj)))
    (eq 'suggestion-box-embed-data (eieio-object-class obj))))

(defun suggestion-box-get-embed-text (text-obj)
  (get-text-property 0 :suggestion-box text-obj))

(cl-deftype suggestion-box-embed ()
  '(satisfies suggestion-box-embed-p))

(defvar suggestion-box-obj nil
  "Internal variable to store popup object and other properties.")



;;; API

(defvar suggestion-box-backend-functions nil
  "Special hook to find the suggestion-box backend for the current context.
Each function on this hook is called in turn with no arguments,
and should return either nil to mean that it is not applicable,
or an suggestion-box backend, which is a value to be used to dispatch the
generic functions.")

;;;###autoload
(defun suggestion-box-find-backend ()
  (run-hook-with-args-until-success 'suggestion-box-backend-functions))

(defun suggestion-box-get (name)
  (when suggestion-box-obj
    (slot-value suggestion-box-obj name)))


(cl-defgeneric suggestion-box-normalize (_backend string)
  "Return normalized string."
  (cl-typecase string
    (suggestion-box-embed
     (suggestion-box-h-embed-normalize string))))

;; Those generic functions can be optional to implement
(cl-defgeneric suggestion-box-save-boundary (_backend)
  "Return something to indicate boundary to delete suggestion-box later.
This function is called only one time when you make a suggestion-box and
the returning value is used until the suggestion-box is deleted.

If the returning value was `paren', which is default value,
`suggestion-box-close-predicate' will adhere to syntax table's
parenthesis to close the suggestion-box meaning when the cursor
is outside of original (nth 1 ppss) the suggestion-box will be
closed immediately.

Note that if you want to use `paren', you must start
`suggestion-box' inside parenthesis, otherwise this program can
not calculate correct point."
  'paren)

(cl-defgeneric suggestion-box-close-predicate (backend bound)
  "Predicate function that returns non-nil if suggestion-box needs to close.
The value of BOUND is that you will be implemented at `suggestion-box-save-boundary'.")


;; For less configuration
(cl-defmethod suggestion-box-close-predicate (_backend (_bound (eql paren)))
  "Return non-nil if current cursor is outside of parenthesis.
In here, the parenthesis means syntax table's.
See also https://www.emacswiki.org/emacs/EmacsSyntaxTable.
The point of parenthesis is registered when you invoke
`suggestion-box' at once and reuse them til suggestion-box is disappeared."
  (not (suggestion-box-h-inside-paren-p)))



;;; nim-mode backend

;; Note: below nim backend stuff may be moved to nim-mode
;; repository. (after this package registered MELPA).

(cl-defmethod suggestion-box-normalize ((_backend (eql nim)) raw-str)
  "Return normalized string."
  (suggestion-box-h-filter
   :content    (suggestion-box-h-trim raw-str "(" ")")
   :split-func (lambda (content) (split-string content ", "))
   :nth-arg    (suggestion-box-h-compute-nth "," 'paren)
   :sep "" :mask1 "" :mask2 ""))



;; Helper functions
(defun suggestion-box-h-inside-paren-p ()
  (memq (nth 1 (suggestion-box-get 'ppss)) (nth 9 (syntax-ppss))))

(defun suggestion-box-h-trim (string opener closer)
  (substring string
             (when-let ((start (cl-search opener string)))
               (1+ start))
             (when-let (end (cl-search closer string :from-end t))
               end)))

(defun suggestion-box-h-compute-nth (sep start-pos)
  (save-excursion
    (when-let ((start (if (eq 'paren start-pos)
                          (nth 1 (suggestion-box-get 'ppss))
                        start-pos))
               (r (apply `((lambda () (rx (or (eval (list 'syntax ?\))) ,sep))))))
               (count 1))
      (while (re-search-backward r start t)
        (let ((ppss (syntax-ppss)))
          (if (eq ?\) (char-syntax (char-after (point))))
              ;; 8th of ppss is start position of comment or string.
              ;; comment would be rare case, but maybe it's
              ;; beneficial for languages like haskell, which has
              ;; multi-comment.
              (goto-char (or (nth 8 ppss) (nth 1 ppss) (point)))
            (when (not (nth 8 ppss))
              (setq count (1+ count))))))
      count)))

(cl-defun suggestion-box-h-filter
    (&key content split-func nth-arg sep mask1 mask2 many-arg &aux strs max)
  (setq strs (delq nil (funcall split-func content))
        max (length strs))
  (cond
   ((suggestion-box--inside-paren-p)
    'ignore)
   ((< max nth-arg)
    (or many-arg "too many arguments?"))
   (t
    (cl-loop with count = 0
             for s in strs
             do (setq count (1+ count))
             if (eq count nth-arg)
             collect s into result
             else if (<= max count)
             collect (or mask2 s) into result
             else collect (or mask1 s) into result
             finally return (mapconcat 'identity result sep)))))

(defun suggestion-box-h-embed-normalize (text-obj)
  (with-slots (backend handler data) (suggestion-box-get-embed-text text-obj)
    (cond ((and (functionp handler) data)
           (funcall handler data))
          (backend
           (list :backend backend
                 :content (suggestion-box-normalize backend (or data text-obj))))
          (t (error "suggestion-box-h-embed-normalize: something wrong")))))



;; Core

;;;###autoload
(defun suggestion-box (string)
  "Show convenience information on the cursor."
  (let ((backend (suggestion-box-find-backend)))
    (and string (suggestion-box--core string backend))))

;;;###autoload
(cl-defun suggestion-box-put (text &key backend handler data)
  (put-text-property
   0 1
   :suggestion-box
   (suggestion-box-embed-data
    :backend backend :handler handler :data data)
   text))

(cl-defun suggestion-box--core (string backend &key still-inside &aux embed-str)
  (when-let ((res (suggestion-box-normalize backend string)))
    (when (listp res)
      (setq backend (plist-get res :backend)
            embed-str (plist-get res :content)))
    (suggestion-box--init
     :string string
     :res (or embed-str res)
     :backend backend
     :still-inside still-inside)))

(cl-defun suggestion-box--init (&key string res still-inside backend)
  (suggestion-box--delete)
  (suggestion-box--set-obj
   (unless (eq 'ignore res)
     (suggestion-box--tip res :truncate t))
   string
   (or (car still-inside)
       (suggestion-box-save-boundary backend))
   (or (cdr still-inside)
       (syntax-ppss))
   backend)
  (add-hook 'post-command-hook 'suggestion-box--update nil t))

(defun suggestion-box--inside-paren-p ()
  (not (eq (nth 1 (syntax-ppss))
           (nth 1 (suggestion-box-get 'ppss)))))

(defun suggestion-box--set-obj (popup-obj string boundary ppss backend)
  (setq suggestion-box-obj
        (suggestion-box-data
         :bound boundary
         :popup popup-obj
         :content string
         :ppss ppss
         :backend backend)))

(defun suggestion-box--update ()
  "Update suggestion-box.
This function is registered to `post-command-hook' and used to
update suggestion-box. If `suggestion-box-close-predicate'
returns non-nil, delete current suggestion-box and registered
function in `post-command-hook'."
  (when-let ((backend (suggestion-box-get 'backend)))
    (let ((bound (suggestion-box-get 'bound)))
      (cond
       ;; Close suggestion-box
       ((or (suggestion-box-close-predicate backend bound)
            (eq 'keyboard-quit this-command))
        (suggestion-box--reset))
       ;; Update suggestion-box
       (t (suggestion-box--core
           (suggestion-box-get 'content)
           backend
           :still-inside
           (cons bound
                 (and (suggestion-box--inside-paren-p)
                      (suggestion-box-get 'ppss)))))))))

(defun suggestion-box--reset ()
  (suggestion-box--delete)
  (setq suggestion-box-obj nil)
  (remove-hook 'post-command-hook 'suggestion-box--update t))

(defun suggestion-box--delete ()
  "Delete suggestion-box."
  (when-let ((p (suggestion-box-get 'popup)))
    (popup-delete p)))

(cl-defun suggestion-box--tip (str &key truncate &aux tip width lines)
  (when (< 1 (line-number-at-pos))
    (cl-letf* (((symbol-function 'popup-calculate-direction)
                (lambda (&rest _r) -1)))
      (let ((s (substring str 0 (min (- (window-width) (current-column))
                                     (length str)))))
        (let ((it (popup-fill-string s nil popup-tip-max-width)))
          (setq width (car it))
          (setq lines (cdr it)))
        (setq tip (popup-create nil width 1
                                :min-height nil
                                :max-width nil
                                :around t
                                :margin-left nil
                                :margin-right nil
                                :scroll-bar nil
                                :face 'suggestion-box-face
                                :parent nil
                                :parent-offset nil))
        (unwind-protect
            (when (> (popup-width tip) 0)                   ; not to be corrupted
              (when (and (not (eq width (popup-width tip))) ; truncated
                         (not truncate))
                ;; Refill once again to lines be fitted to popup width
                (setq width (popup-width tip))
                (setq lines (cdr (popup-fill-string s width width))))
              (popup-set-list tip lines)
              (popup-draw tip)
              tip))))))


(provide 'suggestion-box)
;;; suggestion-box.el ends here

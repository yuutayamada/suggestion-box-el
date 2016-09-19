;;; suggestion-box.el --- show tooltip on the cursor -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Keywords: convenience
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

;; This package is more or less for major-mode maintainers who want to
;; show type information on the cursor and currently only tested on
;; nim-mode (https://github.com/nim-lang/nim-mode).

;; The tooltip will be placed above on the current cursor, so most of
;; the time, the tooltip doesn't destruct your auto-completion result.

;; How to use:
;;
;; if you just want to show type information after company-mode's
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

;;; Code:

;; TODO:
;;  - add gif https://github.com/Malabarba/camcorder.el
;;  - escape comma inside strings
;;  - minor-mode?

(require 'popup)
(require 'cl-lib)
(require 'cl-generic)
(require 'subr-x) ; need Emacs 25.1 or later for `when-let' and `if-let'

(defgroup suggestion-box nil
  "Show information on the cursor."
  ;; :link '(url-link "http://")
  :group 'suggestion-box)

(defvar suggestion-box-data nil
  "Will be stored popup object and other properties.")

(defface suggestion-box-face
  '((((class color) (background dark))
     (:background "#00ffff" :foreground "black"))
    (((class color) (background light))
     (:background "#000087"  :foreground "white"))
    (t (:inverse-video t)))
  "Face for suggestion-box's tooltip."
  :group 'suggestion-box)

;;;;;;;;;;;;;;;;;;;;;;;
;; Generic functions
;; (see also `cl-defmethod')

(cl-defgeneric suggestion-box-set-obj (popup-obj original-string)
  "Set data to `suggestion-box-data'.
The `suggestion-box-data' has to include POPUP-OBJ and able to get
popup object from `suggestion-box-get-popup-obj'."
  (setq suggestion-box-data
        (list :pos (nth 1 (syntax-ppss)) ; at "(" after function
              :popup popup-obj
              :string original-string)))

(cl-defgeneric suggestion-box-get-popup-obj ()
  "Return suggestion-box's popup object."
  (plist-get suggestion-box-data :popup))

(cl-defgeneric suggestion-box-get-str ()
  "Return string, which is used first completion."
  (plist-get suggestion-box-data :string))

(cl-defgeneric suggestion-box-close-predicate (data)
  "Predicate function.
Return non-nil if suggestion-box need to close."
  (not (eq (plist-get data :pos) (nth 1 (syntax-ppss)))))

(cl-defgeneric suggestion-box-trim (string)
  "Trim STRING."
  (substring string
             (cl-search "(" string)
             (1+ (cl-search ")" string :from-end t))))

(cl-defgeneric suggestion-box-split (string)
  "Return list of string."
  (split-string string ", "))

(cl-defgeneric suggestion-box-get-nth ()
  "Return a number, which represent Nth's arg."
  (let ((start (nth 1 (syntax-ppss))))
    (length (split-string (buffer-substring start (point)) ",") )))

(cl-defgeneric suggestion-box-highlight (string)
  "WIP"
  (cl-loop with nth-arg = (suggestion-box-get-nth)
           with count = 0
           with strs = (delq nil (suggestion-box-split (suggestion-box-trim string)))
           for s in strs
           do (setq count (1+ count))
           if (eq count nth-arg)
           collect s into result
           else if (<= (length strs) count)
           collect "?" into result
           else collect "." into result
           finally return (mapconcat 'identity result ", ")))


;;;###autoload
(defun suggestion-box (string)
  "Show STRING on the cursor."
  (when-let ((str (and string (suggestion-box-highlight string))))
    (suggestion-box-delete)
    (suggestion-box-set-obj (suggestion-box--tip str :truncate t) string)
    (add-hook 'post-command-hook 'suggestion-box--update nil t)))

(defun suggestion-box--update ()
  "Delete existing popup object inside `suggestion-box-data'."
  (when-let ((data suggestion-box-data))
    (if (not (or (suggestion-box-close-predicate data)
                 (eq 'keyboard-quit this-command)))
        ;; TODO: add highlight current argument
        (suggestion-box (suggestion-box-get-str))
      ;; Delete popup obj
      (suggestion-box-delete)
      (remove-hook 'post-command-hook 'suggestion-box--update t))))

(defun suggestion-box-delete ()
  "Delete suggestion-box."
  (when-let ((p (suggestion-box-get-popup-obj)))
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

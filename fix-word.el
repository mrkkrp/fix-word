;;; fix-word.el --- Convenient word transformation -*- lexical-binding: t; -*-
;;;
;;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;;
;;; Author: Mark Karpov <markkarpov@openmailbox.org>
;;; URL: https://github.com/mrkkrp/fix-word
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "24.1"))
;;; Keywords: word, convenience
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This package provides means of lifting functions that transform strings
;;; into commands that have different behavior depending on active region,
;;; position of point, etc.

;;; Code:

;;;###autoload
(defun fw/fix-word (fnc)
  "Lift function FNC into command that operates on words and regions.

The following behaviors are implemented:

If the point is placed outside of a word, apply FNC to previous
word.  If the command is invoked repeatedly, every its invocation
transforms one more word moving from right to left.  For
example (upcasing, ^ shows position of point/cursor):

  The quick brown fox jumps over the lazy dog.^
  The quick brown fox jumps over the lazy DOG.^
  The quick brown fox jumps over the LAZY DOG.^
  The quick brown fox jumps over THE LAZY DOG.^

The point doesn't move, this allows user to fix recently entered
words and continue typing.

If the point is placed inside any part of a word, the whole word
is transformed.  The point is moved to first character of the
next word.  This allows to transform words repeatedly pressing
dedicated key binding.

  ^The quick brown fox jumps over the lazy dog.
  THE ^quick brown fox jumps over the lazy dog.
  THE QUICK ^brown fox jumps over the lazy dog.
  THE QUICK BROWN ^fox jumps over the lazy dog.

If there is an active region, all words in that region are
transformed.

Use `fw/fix-word' to create new commands like this:

\(defun command-name ()
  \"Description of the command.\"
  (interactive)
  (fw/fix-word #'upcase))

There is also a macro that defines such commands for you:
`fw/define-command'."
  (funcall
   (if (region-active-p)
       #'fw/-fix-region
     (if (looking-at "\\w+\\>")
         #'fw/-fix-and-move
       #'fw/-fix-quickly))
   fnc))

(defun fw/-fix-region (fnc)
  "Transform active region with function FNC."
  (let* ((from (point))
         (to   (mark))
         (str  (buffer-substring-no-properties from to)))
    (delete-region from to)
    (insert (funcall fnc str))
    (goto-char from)))

(defun fw/-fix-and-move (fnc)
  "Transform current word with function FNC and move to the next word."
  (fw/-transform-word fnc)
  (forward-word 2)
  (backward-word))

(defvar fw/-quick-fix-times 1
  "How many times `fw/-fix-quickly' has been invoked consequently.")

(defun fw/-fix-quickly (fnc)
  "Transform previous word with function FNC.
If this function is invoked repeatedly, transform more words
moving from right to left."
  (interactive)
  (let* ((origin (point))
         (i (if (eq last-command this-command)
                (setf fw/-quick-fix-times
                      (1+ fw/-quick-fix-times))
              (setf fw/-quick-fix-times 1))))
    (backward-word i)
    (fw/-transform-word fnc)
    (goto-char origin)))

(defun fw/-transform-word (fnc)
  "Transform word at point with function FNC."
  (let* ((origin (point))
         (from   (re-search-backward "\\<\\|\\W" nil t))
         (to     (progn
                   (goto-char origin)
                   (re-search-forward  "\\>" nil t))))
    (when (and from to)
      (let ((str (buffer-substring-no-properties from to)))
        (delete-region from to)
        (insert (funcall fnc str))
        (goto-char origin)))))

;;;###autoload
(defmacro fw/define-command (name fnc doc)
  "Define `fw/fix-word'-based command named NAME.
FNC is the processing function and DOC is documentation string."
  (declare (indent defun))
  `(defun ,name ()
     (interactive)
     ,(concat doc "\n\nSee function `fw/fix-word' for more information.")
     (fw/fix-word ,fnc)))

;; Here are some default commands implemented with `fw/fix-word'.

;;;###autoload
(fw/define-command fw/upcase #'upcase
  "Upcase word intelligently.")
;;;###autoload
(fw/define-command fw/downcase #'downcase
  "Downcase word intelligently.")
;;;###autoload
(fw/define-command fw/capitalize #'capitalize
  "Capitalize word intelligently.")

(provide 'fix-word)

;;; fix-word.el ends here

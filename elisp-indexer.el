;;; elisp-indexer.el --- indexing utils for emacslisp  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-depends: ((dash "2.9.0"))

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

;; When developing packages of EmacsLisp one often has the package
;; installed.  If you use the Emacs help system to find functions or
;; variables this will mostly take you to the packaged version. Often,
;; this is not what you want. Once I spent a whole day tracing a bug
;; because I had edited the packaged version of a function instead of
;; the source controlled version.

;; When you look up a function or variable Emacs should know that you
;; have a source controlled version of the same code and it should be
;; able to show you links to that. That's what this code does.

;; An etags based indexer is used, after every elisp file is saved the
;; index is updated. There is also a shell script to batch create (or
;; recreate) the entire index. Whenever a function is looked up in
;; help the results are emended with anything discovered in the index.

;;; Code:

(require 'cl)
(require 'dash)
(require 'rx)

(defmacro cond-re (expression &rest clauses)
  "Evaluate EXPRESSION and then match regex CLAUSES against it.

Each clause looks like (REGEX BODY ...) where REGEX is matched
against EXPRESSION and, if true, BODY is then evaluated.

The match data is saved around the whole thing and restored."
  (declare (indent 1))
  (let ((expr (make-symbol "exprv"))
        (md (make-symbol "mdv")))
    `(let ((,expr ,expression)
           (,md (match-data)))
       (unwind-protect
            (cond 
              ,@(loop for form in clauses
                   collect `((string-match ,(car form) ,expr)
                             ,@(cdr form))))
         (set-match-data ,md)))))

(defun cond-re-test ()
  "A test for `cond-re'."
  (let ((str "hello nic"))
    (cond-re str
      ("hello \\([a-z]+\\)"
       (message "got %s" (match-string 1 str)))
      ("bye" (message "said godbye")))))

(defun proc-shell-promise (command &optional receiver)
  "Do shell COMMAND calling RECEIVER when it's done.

The RECEIVER is called with the numeric completion status and a
list of lines of the output.  A default RECEIVER is supplied if
none is given.

A promise function is returned.  `funcall' the promise function
to wait on the completion of the data.  The promise function
returns whatever the RECEIVER returns.  The default RECEIVER
simply returns what it was passed as a list."
  (let (res
        (rfunc (or receiver (lambda (&rest lst) lst)))
        (proc (start-process-shell-command
               "proc" (generate-new-buffer "*proc*") command)))
    (set-process-sentinel
     proc (lambda (p status)
            (setq res
                  (funcall rfunc
                           (cond-re status
                             ("finished\n" 0)
                             ("exited abnormally with code \\([0-9]+\\)\n"
                              (string-to-int (match-string 1 status))))
                           (split-string
                            (with-current-buffer (process-buffer p)
                              (buffer-string)) "\n")))))
    (lambda (&optional millis)
      (while (not res)
        (accept-process-output proc 0 (or millis 100)))
      res)))

(defun elispindex/find (symbol-name)
  "Find SYMBOL-NAME in the Emacs-Lisp tags.

Return the buffer to the source file."
  (let ((tags-file-name
         (expand-file-name
          ".elisptags"
          (file-name-directory user-init-file))))
    (condition-case err
        (find-tag-noselect (symbol-name symbol-name))
      (user-error nil))))

(defun elispindex/do-file (filename)
  "Index FILENAME in the Emacs-Lisp tags."
  (proc-shell-promise
   (format
    "~/emacs/bin/etags -a -o ~/.emacs.d/.elisptags %s"
    filename)))

(defun elispindex/after-save ()
  "Index the current buffer, if it's an Elisp file."
  (let ((filename (buffer-file-name)))
    (when (string-match-p ".*\\.el$" filename)
      (elispindex/do-file filename))))


(defun elispindex-do-init ()
  "Initialize elispindex.

Puts itself in the after-save hook and so forth."
  (interactive)
  (add-hook 'after-save-hook 'elispindex/after-save))


(defun elispindex/help-hack ()
  ;; Hack help buffers to add index info
  ;;
  ;; this doesn't work, it doesn't seem possible to hack help buffers.
  (progn 
    (describe-function 'elnode-docroot-for)
    (with-selected-window (get-buffer-window (get-buffer "*Help*"))
      (with-current-buffer (get-buffer "*Help*")
        (save-excursion
          (goto-char (point-min))
          (let* ((func-sym
                  (save-match-data
                    (when (re-search-forward "^\\([^ ]+\\) " nil t)
                      (intern (match-string 1)))))
                 (indexed (elispindex/find func-sym)))
            (when indexed
              (message "hacking help!")
              (let ((buffer-read-only nil))
                (goto-char (point-max))
                (insert "\nIndexed at ")
                (let ((start (point))
                      (file (file-name-base (buffer-file-name indexed))))
                  (insert (buffer-file-name indexed))
                  (let ((end (point)))
                    ;;(make-text-button start end)
                  ))))))))))

(defun elispindex/make-text-link (link-to filename-or-buffer)
  "Make a hypertext link target LINK-TO in FILENAME-OR-BUFFER.

Returns the correctly propertized string."
  (let* ((index-fname (if (bufferp filename-or-buffer)
                          (buffer-file-name filename-or-buffer)
                          filename-or-buffer))
         (short-name (file-name-nondirectory index-fname))
         (str short-name))
    (make-text-button
     str nil
     'type 'help-function-def
     'help-args (list link-to index-fname))
    str))

(defun elispindex/what-is-this-function (this)
  "Return a description of what THIS function is."
  (let* ((def (symbol-function this))
         (file-name (find-lisp-object-file-name this def))
         (beg (if (and (or (byte-code-function-p def)
                           (keymapp def)
                           (memq (car-safe def) '(macro lambda closure)))
                       file-name
                       (help-fns--autoloaded-p this file-name))
                  (if (commandp def)
                      "an interactive autoloaded "
                      "an autoloaded ")
                 (if (commandp def) "an interactive " "a "))))
    (cond ((or (stringp def) (vectorp def))
           "a keyboard macro")
          ((subrp def)
           (if (eq 'unevalled (cdr (subr-arity def)))
               (concat beg "special form")
               (concat beg "built-in function")))
          ((byte-code-function-p def)
           (concat beg "compiled Lisp function"))
          ;; (aliased
          ;;  (format "an alias for `%s'" real-def))
          ((eq (car-safe def) 'lambda)
           (concat beg "Lisp function"))
          ((eq (car-safe def) 'macro)
           (concat beg "Lisp macro"))
          ((eq (car-safe def) 'closure)
           (concat beg "Lisp closure"))
          ((autoloadp def)
           (format "%s autoloaded %s"
                   (if (commandp def) "an interactive" "an")
                   (if (eq (nth 4 def) 'keymap) "keymap"
                       (if (nth 4 def) "Lisp macro" "Lisp function"))))
          ((keymapp def)
           (let ((is-full nil)
                 (elts (cdr-safe def)))
             (while elts
               (if (char-table-p (car-safe elts))
                   (setq is-full t
                         elts nil))
               (setq elts (cdr-safe elts)))
             (concat beg (if is-full "keymap" "sparse keymap"))))
          (t ""))))

;; (elispindex/what-is-this-function 'elnode-docroot-for) ; => "a Lisp macro"
;; (elispindex/what-is-this-function 'if) ; => "a special form"
;; (elispindex/what-is-this-function 'symbol-function) ; => "a built in function"

(defun elispindex/lookup-function ()
  ;; This is ripped from the elisp in `describe-function'
  (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
    (setq val (completing-read
               (if fn
                   (format "Describe function (default %s): " fn)
                   "Describe function: ")
               obarray 'fboundp t nil nil
               (and fn (symbol-name fn))))
    (list (if (equal val "")
              fn
              (intern val)))))

(defun elispindex/fundoc (sym)
  "Homogenize documentation for SYM.

Documentation comes in 2 kinds, extended doc has the call args
described at the end in something like:

 (fn ARG1 ARG2 &rest ARG3)

this documentation seems to be generated by `defmacro*' and
`defun*'.  The other kind of documentation has no arg description
embedded, it has to be obtained separately with
`help-function-arglist'.

This function homogenizes both types of doc to the first kind.
This is done so we can pass anything to `help-split-fundoc'."
  (let ((doc (documentation sym)))
    (if
     (string-match-p 
      (rx (and (* anything) "\n(fn" (1+ anything) ")" string-end))
      doc)
     doc
     ;; Else append the arg list correctly to the end of the doc
     (format "%s\n\n%s"
             doc
             (cons
              'fn
              (--map
               (if (equal (elt it 0) ?\&) it (upcase it))
               (--map
                (symbol-name it)
                (help-function-arglist sym t))))))))

(defun elispindex/describe-function (symbol)
  "An alternative `describe-function' with index info.

Current *Help* buffers can't be post edited and there aren't any
hooks to alter the documentation."
  (interactive (elispindex/lookup-function))
  (let* ((symbol-fn (symbol-function symbol))
         (indexed (elispindex/find symbol))
         (file-name (find-lisp-object-file-name symbol symbol-fn))
         (doc (help-split-fundoc (elispindex/fundoc symbol) symbol)))
    (with-current-buffer (get-buffer-create "*Doc*")
      (erase-buffer)
      (insert
       (concat
        (symbol-name symbol)
        " is "
        (elispindex/what-is-this-function symbol)
        (format 
         " in `%s'"
         (elispindex/make-text-link symbol file-name))
        (if (bufferp indexed)
            (format
             " indexed in `%s'"
             (elispindex/make-text-link symbol indexed))
            "")
        "\n"))
      (insert "\n" (car doc) "\n\n" (cdr doc) "\n")
      (save-excursion
        (when (re-search-backward "`\\([^`']+\\)'" nil t)
          (help-xref-button 1 'help-function-def function file-name)))
      (switch-to-buffer (current-buffer)))))


(provide 'elisp-indexer)

;;; elisp-indexer.el ends here

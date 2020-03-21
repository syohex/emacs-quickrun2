;;; quickrun2.el --- Run commands quickly -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-quickrun2
;; Version: 2.2.8
;; Package-Requires: ((emacs "26.1"))

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

;; quickrun2.el executes editing buffer. quickrun2.el selects commands to execute
;; buffer automatically. Please see https://github.com/syohex/emacs-quickrun2
;; for more information.
;;
;; This package respects `quickrun.vim' developed by thinca
;;   - https://github.com/thinca/vim-quickrun
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'quickrun2)
;;
;; And you call 'M-x quickrun2'.
;;

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'em-banner)
(require 'eshell)

;; for warnings of byte-compile
(declare-function anything "anything")
(declare-function helm "helm")
(declare-function tramp-dissect-file-name "tramp")

(defgroup quickrun2 nil
  "Execute buffer quickly"
  :group 'processes
  :prefix 'quickrun2)

(defcustom quickrun2-timeout-seconds 10
  "Timeout seconds for running too long process"
  :type 'integer)

(defcustom quickrun2-focus-p t
  "If this value is `nil`, quickrun2.el does not move focus to output buffer."
  :type 'boolean)

(defcustom quickrun2-input-file-extension ".qrinput"
  "Extension of input file name"
  :type '(choice (string :tag "Extension of quickrun2 input file")
                 (boolean :tag "Not use input file" nil)))

(defcustom quickrun2-debug nil
  "Enable debug message"
  :type 'boolean)

(defconst quickrun2--buffer-name "*quickrun2*")
(defvar quickrun2--executed-file nil)
(defvar quickrun2--remove-files nil)
(defvar quickrun2--compile-only-flag nil)
(defvar quickrun2--original-buffer nil)
(defvar quickrun2--original-outputter nil)

(defmacro quickrun2--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun quickrun2--mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defsubst quickrun2--log (fmt &rest args)
  (when quickrun2-debug
    (apply 'message fmt args)))

(defsubst quickrun2--windows-p ()
  (memq system-type '(ms-dos windows-nt cygwin)))

;;
;; file local variable
;; Based on shadow.el. https://raw.github.com/mooz/shadow.el/master/shadow.el
;;
(defmacro quickrun2--defvar (name &optional value safep doc)
  "Define buffer-local and safe-local variable."
  (declare (indent defun))
  `(progn
     (defvar ,name ,value ,doc)
     (make-variable-buffer-local (quote ,name))
     ;; Suppress file local variable warning
     ,(when safep
        `(put (quote ,name) 'safe-local-variable (quote ,safep)))))

(quickrun2--defvar quickrun2-option-cmd-alist
                   nil listp
                   "Specify command alist directly as file local variable")

(quickrun2--defvar quickrun2-option-command
                   nil stringp
                   "Specify command directly as file local variable")

(quickrun2--defvar quickrun2-option-cmdkey
                   nil stringp
                   "Specify language key directly as file local variable")

(quickrun2--defvar quickrun2-option-cmdopt
                   nil stringp
                   "Specify command option directly as file local variable")

(quickrun2--defvar quickrun2-option-args
                   nil stringp
                   "Specify command argument directly as file local variable")

(defun quickrun2--outputter-p (_x)
  (lambda (x)
    (or (functionp x) (symbolp x) (stringp x)
        (quickrun2--outputter-multi-p x))))

(quickrun2--defvar quickrun2-option-outputter
                   nil quickrun2--outputter-p
                   "Specify format function output buffer as file local variable")

(quickrun2--defvar quickrun2-option-shebang
                   t booleanp
                   "Select using command from schebang as file local variable")

(quickrun2--defvar quickrun2-option-timeout-seconds
                   nil integerp
                   "Timeout seconds as file local variable")

(quickrun2--defvar quickrun2-option-default-directory
                   nil file-directory-p
                   "Default directory where command is executed")

;; hooks
(defvar quickrun2-after-run-hook nil
  "Run hook after execute quickrun2")

(defvar quickrun2--temporary-file nil)

;;
;; language command parameters
;;

(defvar quickrun2--language-alist
  '(("c/gcc" . ((:command . "gcc")
                (:exec    . ("%c -x c %o -o %e %s" "%e %a"))
                (:compile-only . "%c -Wall -Werror %o -o %e %s")
                (:remove . ("%e"))
                (:description . "Compile C file with gcc and execute")))

    ("c/clang" . ((:command . "clang")
                  (:exec    . ("%c -x c %o -o %e %s" "%e %a"))
                  (:compile-only . "%c -Wall -Werror %o -o %e %s")
                  (:remove  . ("%e"))
                  (:description . "Compile C file with llvm/clang and execute")))

    ("c++/g++" . ((:command . "g++")
                  (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
                  (:compile-only . "%c -Wall -Werror %o -o %e %s")
                  (:remove  . ("%e"))
                  (:description . "Compile C++ file with g++ and execute")))

    ("c++/clang++" . ((:command . "clang++")
                      (:exec    . ("%c -x c++ %o -o %e %s" "%e %a"))
                      (:compile-only . "%c -Wall -Werror %o -o %e %s")
                      (:remove  . ("%e"))
                      (:description . "Compile C++ file with llvm/clang++ and execute")))

    ("perl" . ((:command . "perl") (:compile-only . "%c -wc %s")
               (:description . "Run Perl script")))
    ("ruby" . ((:command . "ruby") (:compile-only . "%c -wc %s")
               (:description . "Run Ruby script")))
    ("python" . ((:command . "python") (:compile-only . "pyflakes %s")
                 (:description . "Run Python script")))

    ("javascript" . ((:command . "node")
                     (:description . "Run Javascript file with node.js")))

    ("go"  .  ((:command . "go")
               (:exec    . ((lambda ()
                              (if (string-match-p "_test\\.go\\'" (buffer-name))
                                  "%c test %o"
                                "%c run %o %s %a"))))
               (:compile-only . "%c build -o /dev/null %s %o %a")
               (:tempfile . nil)
               (:description . "Compile go file and execute with 'go'")))
    )

  "List of each programming languages information.
Parameter form is (\"language\" . parameter-alist). parameter-alist has
5 keys and those values , :command, :exec, :remove.
:command pair is mandatory, other pairs are optional. Associated value
should be string or a function which returns a string object.

Assosiated values are
:command = Program name which is used compiled or executed source code.
:exec    = Exec command template. If you omit this parameter, quickrun2
           use default parameter \"%c %o %s %a\".
:remove  = Remove files or directories templates.
           Compiler or executor generates temporary files,
           you should specified this parameter.
           If value is List, quickrun2 removes each element.
Every pair should be dot-pair.

See explanation of quickrun2--template-place-holders
if you set your own language configuration.
")

(defvar quickrun2-file-alist
  '(("\\.c\\'" . "c")
    ("\\.\\(cpp\\|cxx\\|C\\|cc\\)\\'" . "c++")
    ("\\.\\(pl\\|pm\\)\\'" . "perl")
    ("\\.rb\\'" . "ruby")
    ("\\.py\\'" . "python")
    ("\\.js\\'" . "javascript")
    ("\\.go\\'" . "go"))
  "Alist of (file-regexp . key)")

(defvar quickrun2--major-mode-alist
  '((c-mode . "c")
    (c++-mode . "c++")
    ((perl-mode cperl-mode) . "perl")
    (ruby-mode . "ruby")
    (python-mode . "python")
    ((javascript-mode js-mode js2-mode) . "javascript")
    (go-mode . "go"))
  "Alist of major-mode and langkey")

(defun quickrun2--decide-file-type (filename)
  ;; First search by file extension, Second search by major-mode
  (or (assoc-default filename quickrun2-file-alist 'string-match)
      (quickrun2--find-from-major-mode-alist)))

(defun quickrun2--find-from-major-mode-alist ()
  (cl-loop for (lang . lang-info) in quickrun2--major-mode-alist
           for lang-lst = (quickrun2--mklist lang)
           when (memq major-mode lang-lst)
           return lang-info))

(defun quickrun2--command-info (lang)
  (or quickrun2-option-cmd-alist
      (assoc-default lang quickrun2--language-alist)
      (throw 'quickrun2
             (format "not found [%s] language information" lang))))

;;
;; Compile Only
;;
(defun quickrun2--check-using-compilation-mode (compile-conf)
  (if (not compile-conf)
      t
    (let ((compilation-mode (assoc :compilation-mode compile-conf)))
      (if (not compilation-mode)
          t
        (cdr compilation-mode)))))

(defun quickrun2--pop-to-buffer (buf cb)
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    (funcall cb)
    (unless quickrun2-focus-p
      (select-window win))))

(defun quickrun2--compilation-start (cmd compile-conf)
  (let ((use-compile (quickrun2--check-using-compilation-mode compile-conf)))
    (cond (use-compile
           (setq compilation-finish-functions 'quickrun2--compilation-finish-func)
           (compilation-start cmd t (lambda (_x) quickrun2--buffer-name)))
          (t
           (with-current-buffer (get-buffer-create quickrun2--buffer-name)
             (read-only-mode -1)
             (erase-buffer)
             (process-file-shell-command cmd nil t)
             (goto-char (point-min))
             (quickrun2--awhen (assoc-default :mode compile-conf)
                               (funcall it)
                               (quickrun2--pop-to-buffer
                                (current-buffer) (lambda () (read-only-mode +1)))
                               (read-only-mode +1)))
           (quickrun2--remove-temp-files)))))

(defun quickrun2--compilation-finish-func (_buffer _str)
  (quickrun2--remove-temp-files))

;;
;; Execute
;;
(defvar quickrun2--timeout-timer nil)
(defvar quickrun2--run-in-shell nil)

(defsubst quickrun2--concat-commands (cmd-lst)
  (mapconcat 'identity cmd-lst " && "))

(defsubst quickrun2--stdin-file-name ()
  (concat quickrun2--executed-file quickrun2-input-file-extension))

(defsubst quickrun2--stdin-file-regexp ()
  (concat quickrun2-input-file-extension "\\'"))

(defsubst quickrun2--use-stdin-file-p ()
  (string-match-p (quickrun2--stdin-file-regexp)
                  (or (buffer-file-name) (buffer-name))))

(defun quickrun2--send-file-as-stdin (process file)
  (let ((open-buf-func (cond ((file-exists-p file) 'find-file-noselect)
                             ((get-buffer file) 'get-buffer))))
    (when open-buf-func
      (quickrun2--log "Send '%s' to STDIN of %s" file (process-name process))
      (with-current-buffer (funcall open-buf-func file)
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))))

(defun quickrun2--default-filter (proc output)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (goto-char (point-max))
    (let ((start (point)))
      (insert output)
      (ansi-color-apply-on-region start (point)))))

(defun quickrun2--exec (cmd-lst src mode)
  (if quickrun2--run-in-shell
      (quickrun2--send-to-shell cmd-lst)
    (ignore-errors
      (let* ((next-cmd  (car cmd-lst))
             (rest-cmds (cdr cmd-lst))
             (process (quickrun2--exec-cmd next-cmd))
             (outputter (or quickrun2-option-outputter
                            'quickrun2--default-outputter)))
        (when (and (null rest-cmds) quickrun2-input-file-extension)
          (let ((file (quickrun2--stdin-file-name)))
            (quickrun2--send-file-as-stdin process file)))
        (when (eq outputter 'quickrun2--default-outputter)
          (set-process-filter process #'quickrun2--default-filter))
        (set-process-sentinel process
                              (quickrun2--make-sentinel rest-cmds outputter src mode))))))

(defvar quickrun2--eshell-buffer-name "*eshell-quickrun2*")
(defvar quickrun2--shell-last-command)

(defun quickrun2--eshell-finish ()
  (quickrun2--remove-temp-files)
  (remove-hook 'eshell-post-command-hook 'quickrun2--eshell-post-hook))

(defun quickrun2--eshell-window-restore ()
  (interactive)
  (jump-to-register :quickrun2-shell))

(defun quickrun2--eshell-post-hook ()
  (let ((rerun-p nil)
        (prompt "Press 'r' to run again, any other key to finish"))
    (unwind-protect
        (ignore-errors
          (let ((input (read-char prompt)))
            (when (char-equal input ?r)
              (quickrun2--insert-command quickrun2--shell-last-command)
              (setq rerun-p t))))
      (unless rerun-p
        (quickrun2--eshell-finish)
        (read-only-mode +1)
        (local-set-key (kbd "q") 'quickrun2--eshell-window-restore)))))

(defun quickrun2--insert-command (cmd-str)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert cmd-str)
  (eshell-send-input))

(defun quickrun2--send-to-shell (cmd-lst)
  (window-configuration-to-register :quickrun2-shell)
  (let ((buf (get-buffer quickrun2--buffer-name))
        (win (selected-window)))
    (pop-to-buffer buf)
    (let ((cmd-str (quickrun2--concat-commands cmd-lst))
          (eshell-buf (get-buffer quickrun2--eshell-buffer-name))
          (eshell-buffer-name quickrun2--eshell-buffer-name)
          (eshell-banner-message ""))
      (when eshell-buf
        (kill-buffer eshell-buf))
      (eshell)
      (kill-buffer quickrun2--buffer-name)
      (setq-local quickrun2--shell-last-command cmd-str)
      (add-hook 'eshell-post-command-hook 'quickrun2--eshell-post-hook)
      (quickrun2--insert-command cmd-str)
      (unless quickrun2-focus-p
        (select-window win)))))

(defsubst quickrun2--default-directory ()
  (or quickrun2-option-default-directory default-directory))

(defun quickrun2--set-default-directory (cmd-key)
  (let ((cmd-info (quickrun2--command-info cmd-key)))
    (quickrun2--awhen (assoc-default :default-directory cmd-info)
                      (let ((formatted (file-name-as-directory it)))
                        (unless (file-directory-p formatted)
                          (throw 'quickrun2
                                 (format "'%s' is not existed directory" it)))
                        (setq quickrun2-option-default-directory formatted)))))

(defsubst quickrun2--process-connection-type (cmd)
  ;; for suppressing 'carriage return'(^M)
  (not (string-match-p "\\`php" cmd)))

(defun quickrun2--exec-cmd (cmd)
  (let ((program (car (split-string cmd)))
        (buf (get-buffer quickrun2--buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer))
    (let ((proc-name (format "quickrun2-process-%s" program))
          (process-connection-type (quickrun2--process-connection-type program))
          (default-directory (quickrun2--default-directory)))
      (quickrun2--log "Quickrun2 Execute: %s at %s" cmd default-directory)
      (let ((process (start-file-process-shell-command proc-name buf cmd)))
        (when (>= quickrun2-timeout-seconds 0)
          (setq quickrun2--timeout-timer
                (run-at-time quickrun2-timeout-seconds nil
                             'quickrun2--kill-process process)))
        process))))

(defun quickrun2--kill-process (process)
  (when (eq (process-status process) 'run)
    (kill-process process))
  (let ((buf (get-buffer quickrun2--buffer-name)))
    (with-current-buffer buf
      (insert (format "\nTime out %s(running over %d second)"
                      (process-name process)
                      quickrun2-timeout-seconds)))
    (quickrun2--remove-temp-files)
    (quickrun2--pop-to-buffer buf (lambda () (read-only-mode +1)))))

(defun quickrun2--remove-temp-files ()
  (quickrun2--log "Quickrun2 remove %s" quickrun2--remove-files)
  (dolist (file quickrun2--remove-files)
    (cond
     ((file-directory-p file) (delete-directory file t))
     ((file-exists-p file) (delete-file file))))
  (setq quickrun2--remove-files nil))

(defun quickrun2--kill-running-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (message "No Process!!")
      (message "Kill process: %s" (process-name proc))
      (kill-process proc))))

(defvar quickrun2--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "C-c C-c") 'quickrun2--kill-running-process)
    map))

(define-derived-mode quickrun2--mode nil "Quickrun2"
  ""
  (read-only-mode +1)
  (use-local-map quickrun2--mode-map))

;;
;; Predefined outputter
;;

(defvar quickrun2--defined-outputter-symbol
  '(
    (message  . quickrun2--outputter-message)
    (browser  . quickrun2--outputter-browser)
    (null     . quickrun2--outputter-null)
    (replace  . quickrun2--outputter-replace-region)
    (eval-print . quickrun2--outputter-eval-print)
    ))

(defvar quickrun2--defined-outputter-symbol-with-arg
  '(
    ("^file:"     . quickrun2--outputter-file)
    ("^buffer:"   . quickrun2--outputter-buffer)
    ("^variable:" . quickrun2--outputter-variable)
    ))

(defun quickrun2--recenter (arg)
  (with-selected-window (get-buffer-window quickrun2--buffer-name)
    (recenter arg)))

(defun quickrun2--default-outputter ()
  (quickrun2--recenter -1))

(defun quickrun2--outputter-multi-p (outputter)
  (and (not (functionp outputter)) (listp outputter)
       (eq (car outputter) 'multi)))

(defun quickrun2--defined-outputter-p (outputter)
  (cond ((quickrun2--outputter-multi-p outputter) t)
        ((or (symbolp outputter) (stringp outputter))
         (let ((name (or (and (symbolp outputter) (symbol-name outputter))
                         outputter)))
           (or (assoc outputter quickrun2--defined-outputter-symbol)
               (assoc-default name
                              quickrun2--defined-outputter-symbol-with-arg
                              'string-match))))))

(defun quickrun2--outputter-file (file)
  (write-region (point-min) (point-max) file))

(defun quickrun2--outputter-message ()
  (message "%s" (buffer-substring-no-properties (point-min) (point-max))))

(defun quickrun2--outputter-browser ()
  (browse-url-of-region (point-min) (point-max)))

(defun quickrun2--outputter-null ()
  (delete-region (point-min) (point-max))
  (kill-buffer (get-buffer quickrun2--buffer-name)))

(defun quickrun2--outputter-replace-region ()
  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer quickrun2--original-buffer
      (delete-region (region-beginning) (region-end))
      (insert output)
      (setq quickrun2-option-outputter quickrun2--original-outputter))))

(defun quickrun2--outputter-eval-print ()
  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer quickrun2--original-buffer
      (forward-line 1)
      (let ((start (point)))
        (insert output)
        (comment-region start (point))
        (setq quickrun2-option-outputter quickrun2--original-outputter)))))

(defun quickrun2--outputter-buffer (bufname)
  (let ((str (buffer-substring (point-min) (point-max))))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert str))))

(defun quickrun2--outputter-variable (varname)
  (let ((symbol (intern varname)))
    (set symbol (buffer-substring (point-min) (point-max)))))

(defun quickrun2--apply-outputter (op)
  (let ((buf (get-buffer quickrun2--buffer-name))
        (origbuf (current-buffer))
        (outputters (or (and (quickrun2--outputter-multi-p op) (cdr op))
                        (list op)))
        (outputter-func nil))
    (dolist (outputter outputters)
      (setq outputter-func outputter)
      (when (symbolp outputter)
        (let* ((name (symbol-name outputter))
               (func (assoc-default outputter
                                    quickrun2--defined-outputter-symbol))
               (func-with-arg
                (assoc-default name
                               quickrun2--defined-outputter-symbol-with-arg
                               'string-match)))
          (cond (func (setq outputter-func func))
                (func-with-arg
                 (when (string-match ":\\(.*\\)\\'" name)
                   (setq outputter-func
                         (lambda ()
                           (funcall func-with-arg
                                    (match-string 1 name)))))))))
      (with-current-buffer buf
        (let ((quickrun2--original-buffer origbuf))
          (read-only-mode -1)
          (funcall outputter-func)
          (read-only-mode +1))))))

(defun quickrun2--apply-compilation-mode (input-file mode)
  (when (not (string= input-file quickrun2--executed-file))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (search-forward input-file nil t)
          (replace-match quickrun2--executed-file)))))
  (compilation-mode mode))

(defun quickrun2--apply-colorizing (input-file mode)
  (with-current-buffer (get-buffer quickrun2--buffer-name)
    (read-only-mode -1)
    (when (and quickrun2--executed-file input-file)
      (quickrun2--apply-compilation-mode input-file mode)
      (read-only-mode -1))
    (quickrun2--default-outputter)
    (goto-char (point-min))
    (read-only-mode +1)))

(defun quickrun2--make-sentinel (rest-commands outputter-func input orig-mode)
  (lambda (process _event)
    ;; XXX Why reset `quickrun2-option-outputter' ??
    (setq quickrun2-option-outputter outputter-func)
    (when (memq (process-status process) '(exit signal))
      (and quickrun2--timeout-timer (cancel-timer quickrun2--timeout-timer))
      (delete-process process)
      (let* ((exit-status (process-exit-status process))
             (is-success (zerop exit-status)))
        (cond ((and is-success rest-commands)
               (quickrun2--exec rest-commands input orig-mode))
              (t
               (if (not is-success)
                   (if (eq quickrun2-option-outputter #'quickrun2--default-outputter)
                       (quickrun2--apply-colorizing input orig-mode)
                     (message "Failed: Exit Status=%d" exit-status))
                 (quickrun2--apply-outputter outputter-func)
                 (run-hooks 'quickrun2-after-run-hook))
               (when (eq outputter-func 'quickrun2--default-outputter)
                 (cond ((> scroll-conservatively 0) (quickrun2--recenter nil))
                       ((/= scroll-step 0) (quickrun2--recenter -1))))
               (quickrun2--remove-temp-files)))))))

;;
;; Composing command
;;
(defconst quickrun2--template-place-holders
  '("%c" "%o" "%s" "%S" "%a" "%d" "%n" "%N" "%e" "%E")
  "A list of place holders of each language parameter.
Place holders are beginning with '%' and replaced by:
%c: :command parameter
%o: command options
%s: source code name
%S: source code name without extension
%a: program argument
%d: directory name
%n: absolute path of source code without extension
%N: source code path without extension
%e: absolute path of source code with executable extension(.exe, .out, .class)
%E: source code name with executable extension
")

(defun quickrun2--executable-suffix (command)
  (cond ((string= command "java") ".class")
        ((quickrun2--windows-p) ".exe")
        (t ".out")))

(defun quickrun2--real-file-name (src)
  (let ((buffile (buffer-file-name)))
    (if (not (and buffile (file-remote-p buffile)))
        src
      (aref (tramp-dissect-file-name (buffer-file-name)) 3))))

(defun quickrun2--place-holder-info (cmd cmdopt source args)
  (let* ((src (quickrun2--real-file-name source))
         (without-extension (file-name-sans-extension src))
         (dirname (file-name-directory (expand-file-name src)))
         (directory (substring dirname 0 (- (length dirname) 1)))
         (executable-suffix (quickrun2--executable-suffix cmd))
         (executable-name (concat without-extension executable-suffix)))
    `(("%c" . ,cmd)
      ("%o" . ,cmdopt)
      ("%s" . ,(file-name-nondirectory src))
      ("%S" . ,(file-name-nondirectory without-extension))
      ("%n" . ,(expand-file-name without-extension))
      ("%N" . ,without-extension)
      ("%d" . ,directory)
      ("%e" . ,(expand-file-name executable-name))
      ("%E" . ,executable-name)
      ("%a" . ,args))))

(defconst quickrun2--default-tmpl-alist
  '((:exec . "%c %o %s %a")))

(defun quickrun2--extract-template (key cmd-info &optional take-list)
  (let ((tmpl (or (assoc-default key cmd-info)
                  (assoc-default key quickrun2--default-tmpl-alist))))
    (when tmpl
      (if take-list
          (mapcar 'quickrun2--eval-parameter (quickrun2--mklist tmpl))
        (quickrun2--eval-parameter tmpl)))))

(defun quickrun2--eval-parameter (param)
  (cond ((functionp param)
         (let* ((default-directory (quickrun2--default-directory))
                (ret (funcall param)))
           (cond ((stringp ret) ret)
                 ((symbolp ret) (symbol-name ret))
                 (t
                  (throw 'quickrun2
                         "template function should return symbol or string")))))
        (t param)))

(defun quickrun2--get-shebang ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#![ \t]*\\(.*\\)$")
      (match-string-no-properties 1))))

(defun quickrun2--template-argument (cmd-info src)
  (let ((cmd (or quickrun2-option-command
                 (and quickrun2-option-shebang (quickrun2--get-shebang))
                 (quickrun2--eval-parameter (assoc-default :command cmd-info))
                 (throw 'quickrun2 "Not found :command parameter")))
        (cmd-opt (or quickrun2-option-cmdopt
                     (quickrun2--extract-template :cmdopt cmd-info) ""))
        (arg (or quickrun2-option-args
                 (quickrun2--extract-template :args cmd-info) "")))
    (quickrun2--place-holder-info cmd cmd-opt src arg)))

(defun quickrun2--fill-templates (cmd-key src)
  (let* ((cmd-info (quickrun2--command-info cmd-key))
         (tmpl-arg (quickrun2--template-argument cmd-info src))
         (info (make-hash-table)))
    ;; take one parameter
    (cl-loop for key in '(:compile-only)
             when (quickrun2--extract-template key cmd-info)
             do (puthash key (quickrun2--fill-template it tmpl-arg) info))
    ;; take one or more parameters
    (cl-loop for key in '(:exec :remove)
             when (quickrun2--extract-template key cmd-info t)
             do
             (let ((filled-tmpls (mapcar (lambda (x)
                                           (quickrun2--fill-template x tmpl-arg))
                                         it)))
               (puthash key filled-tmpls info)))
    ;; function parameter
    (dolist (key '(:outputter))
      (let ((func (assoc-default :outputter cmd-info)))
        (when (and func (or (functionp func) (symbolp func)))
          (puthash key func info))))
    info))

(defun quickrun2--fill-template (tmpl info)
  (let ((place-holders quickrun2--template-place-holders)
        (str tmpl)
        (case-fold-search nil))
    (dolist (holder place-holders str)
      (let ((rep (assoc-default holder info)))
        (setq str (replace-regexp-in-string holder rep str t))))))

;;
;; initialize
;;

(defconst quickrun2--support-languages
  '("c" "c++" "perl" "ruby" "python" "javascript" "go")
  "Programming languages and Markup languages supported as default
by quickrun2.el. But you can register your own command for some languages")

(defvar quickrun2--command-key-table
  (make-hash-table :test 'equal))

;;;###autoload
(defun quickrun2-set-default (lang key)
  "Set `key' as default key in programing language `lang'"
  (unless (assoc key quickrun2--language-alist)
    (error "%s is not registered." key))
  (puthash lang key quickrun2--command-key-table))

(defun quickrun2--override-command (cmdkey cmd-alist)
  (let ((registered (assoc-default cmdkey quickrun2--language-alist)))
    (unless registered
      (error (format "'%s' is not registered" cmdkey)))
    (cl-loop for old-param in registered
             do
             (let ((new-value (assoc-default (car old-param) cmd-alist)))
               (when new-value
                 (setcdr old-param new-value))))))

;;;###autoload
(cl-defun quickrun2-add-command (key alist &key default mode override)
  (declare (indent defun))
  (cond ((not key) (error "Undefined 1st argument 'key'"))
        ((not alist) (error "Undefined 2nd argument 'command alist'")))
  (if override
      (quickrun2--override-command key (copy-alist alist))
    (if (not (assoc :command alist))
        (error "not found :command parameter in language alist")
      (push (cons key (copy-alist alist)) quickrun2--language-alist)))
  (let ((cmd-key (or default key)))
    (when default
      (puthash cmd-key key quickrun2--command-key-table))
    (when mode
      (push (cons mode cmd-key) quickrun2--major-mode-alist))
    key))

(defun quickrun2--find-executable (candidates)
  (cl-loop for candidate in candidates
           when (executable-find candidate)
           return candidate))

(defun quickrun2--set-command-key (lang candidates)
  (quickrun2--awhen (quickrun2--find-executable candidates)
                    (puthash lang (format "%s/%s" lang it) quickrun2--command-key-table)))

(defsubst quickrun2--c-compiler ()
  (cond((eq system-type 'darwin) '("clang" "gcc"))
       (t '("gcc" "clang"))))

(defsubst quickrun2--c++-compiler ()
  (cond((eq system-type 'darwin) '("clang++" "g++"))
       (t '("g++" "clang++"))))

(defconst quicklang/lang-candidates
  `(("c" . ,(quickrun2--c-compiler))
    ("c++" . ,(quickrun2--c++-compiler)))
  "Candidates of language which has some compilers or interpreters")

(defun quickrun2--init-command-key-table ()
  "Decide command for programing language which has multiple candidates"
  (dolist (lang quickrun2--support-languages)
    (puthash lang lang quickrun2--command-key-table))
  (cl-loop for (lang . candidates) in quicklang/lang-candidates
           do
           (quickrun2--set-command-key lang candidates)))

(quickrun2--init-command-key-table)

(defun quickrun2--set-executed-file ()
  (let* ((buffer-file (buffer-file-name))
         (name (or buffer-file (buffer-name)))
         (use-stdin-file-p (quickrun2--use-stdin-file-p))
         orig-file)
    (when (string-match (concat "\\(.+\\)" (quickrun2--stdin-file-regexp)) name)
      (setq orig-file (match-string 1 name)))
    (if (and (not buffer-file) (not use-stdin-file-p))
        (setq quickrun2--executed-file nil)
      (setq quickrun2--executed-file
            (if use-stdin-file-p
                (if (not (file-exists-p orig-file))
                    (error "Can't find %s" orig-file)
                  orig-file)
              (file-name-nondirectory buffer-file))))))

;;
;; main
;;
;;;###autoload
(defun quickrun2 (&rest plist)
  "Run commands quickly for current buffer
   With universal prefix argument(C-u), select command-key,
   With double prefix argument(C-u C-u), run in compile-only-mode"
  (interactive)
  (quickrun2--set-executed-file)
  (let ((beg (or (plist-get plist :start) (point-min)))
        (end (or (plist-get plist :end) (point-max)))
        (quickrun2-option-cmd-alist (or quickrun2-option-cmd-alist
                                        (plist-get plist :source)))
        (quickrun2-timeout-seconds (or quickrun2-option-timeout-seconds
                                       quickrun2-timeout-seconds))
        (quickrun2--compile-only-flag (or quickrun2--compile-only-flag
                                          (and (consp current-prefix-arg)
                                               (= (car current-prefix-arg) 16)))))
    (let ((has-error (catch 'quickrun2
                       (quickrun2--common beg end)
                       nil)))
      (when has-error
        (message "%s" has-error)
        (quickrun2--remove-temp-files)))))

(defvar quickrun2--with-arg--history nil)

;;;###autoload
(defun quickrun2-with-arg (arg)
  "Run commands quickly for current buffer with arguments"
  (interactive
   (list (read-string "Quickrun2 Arg: " nil 'quickrun2--with-arg--history)))
  (let ((quickrun2-option-args arg))
    (quickrun2)))

(defvar quickrun2--last-cmd-key nil)

(defun quickrun2--prompt ()
  (let* ((default (or quickrun2-option-cmdkey quickrun2--last-cmd-key))
         (prompt (format "Quickrun2 Lang%s: "(if default
                                                 (format "[Default: %s]" default)
                                               ""))))
    (completing-read prompt quickrun2--language-alist nil nil nil nil default)))

(defun quickrun2--region-command-common (start end)
  (deactivate-mark)
  (quickrun2 :start start :end end))

;;;###autoload
(defun quickrun2-region (start end)
  "Run commands with specified region"
  (interactive "r")
  (quickrun2--region-command-common start end))

;;;###autoload
(defun quickrun2-replace-region (start end)
  "Run commands with specified region and replace"
  (interactive "r")
  (setq quickrun2--original-outputter quickrun2-option-outputter)
  (let ((quickrun2-option-outputter 'replace))
    (quickrun2--region-command-common start end)))

;;;###autoload
(defun quickrun2-eval-print (start end)
  "Run commands with specified region and replace"
  (interactive "r")
  (setq quickrun2--original-outputter quickrun2-option-outputter)
  (let ((quickrun2-option-outputter 'eval-print))
    (quickrun2--region-command-common start end)))

;;;###autoload
(defun quickrun2-compile-only ()
  "Exec only compilation"
  (interactive)
  (let ((quickrun2--compile-only-flag t))
    (quickrun2)))

;;;###autoload
(defun quickrun2-shell ()
  "Run commands in shell for interactive programs"
  (interactive)
  (let ((quickrun2--run-in-shell t)
        (quickrun2-timeout-seconds nil))
    (quickrun2)))

(defun quickrun2--add-remove-files (removed-files)
  (let ((abs-paths (mapcar 'expand-file-name (quickrun2--mklist removed-files))))
    (setq quickrun2--remove-files (append abs-paths quickrun2--remove-files))))

(defun quickrun2--temp-name (src)
  (let* ((extension (file-name-extension src))
         (suffix (or (and extension (concat "." extension)) ""))
         (dir (quickrun2--default-directory)))
    (expand-file-name (concat dir (make-temp-name "qr_") suffix))))

(defun quickrun2--command-key (src)
  (let ((file-type (and src (quickrun2--decide-file-type src)))
        (use-prefix-p (and (consp current-prefix-arg)
                           (= (car current-prefix-arg) 4))))
    (or (and use-prefix-p (quickrun2--prompt))
        (and quickrun2-option-cmd-alist "_user_defined") ;; setting dummy value
        quickrun2-option-cmdkey
        (and (not src) (quickrun2--prompt))
        (gethash file-type quickrun2--command-key-table)
        file-type
        (quickrun2--prompt))))

(defun quickrun2--get-content (start end)
  (if (quickrun2--use-stdin-file-p)
      (with-current-buffer (find-file-noselect quickrun2--executed-file)
        (buffer-substring-no-properties (point-min) (point-max)))
    (buffer-substring-no-properties start end)))

(defun quickrun2--copy-region-to-tempfile (start end dst)
  ;; Suppress write file message
  (let ((content (quickrun2--get-content start end))
        (codec buffer-file-coding-system))
    (with-temp-file dst
      (set-buffer-file-coding-system codec)
      (insert content))
    (quickrun2--add-remove-files dst)))

(defun quickrun2--kill-quickrun2-buffer ()
  (when (get-buffer quickrun2--buffer-name)
    (kill-buffer quickrun2--buffer-name)))

(defun quickrun2--setup-exec-buffer (buf)
  (let ((default-dir (quickrun2--default-directory)))
    (with-current-buffer buf
      (setq quickrun2-option-default-directory default-dir))))

(defun quickrun2--use-tempfile-p (cmd-key)
  (let ((buffile (buffer-file-name)))
    (unless (or quickrun2--compile-only-flag (and buffile (file-remote-p buffile)))
      (let* ((cmdinfo (quickrun2--command-info cmd-key))
             (tempfile-param (assoc :tempfile cmdinfo)))
        (if tempfile-param
            (cdr tempfile-param)
          t)))))

(defsubst quickrun2--buffer-popup-p ()
  (and (not (quickrun2--defined-outputter-p quickrun2-option-outputter))
       (not quickrun2--run-in-shell)))

(defun quickrun2--common (start end)
  (let* ((orig-src quickrun2--executed-file)
         (cmd-key (quickrun2--command-key orig-src)))
    (quickrun2--set-default-directory cmd-key)
    (quickrun2--kill-quickrun2-buffer)
    (unless (local-variable-p 'quickrun2--last-cmd-key)
      (make-local-variable 'quickrun2--last-cmd-key))
    (setq quickrun2--last-cmd-key cmd-key)

    (let ((src (quickrun2--temp-name (or orig-src ""))))
      (if (quickrun2--use-tempfile-p cmd-key)
          (quickrun2--copy-region-to-tempfile start end src)
        (setq src orig-src))
      (let ((cmd-info-hash (quickrun2--fill-templates cmd-key src)))
        (quickrun2--add-remove-files (gethash :remove cmd-info-hash))
        (unless quickrun2-option-outputter
          (setq quickrun2-option-outputter (gethash :outputter cmd-info-hash)))
        (cond (quickrun2--compile-only-flag
               (let* ((cmd (gethash :compile-only cmd-info-hash))
                      (cmd-info (quickrun2--command-info cmd-key))
                      (compile-conf (assoc-default :compile-conf cmd-info)))
                 (unless cmd
                   (throw 'quickrun2
                          (format "%s does not support quickrun2-compile-only"
                                  cmd-key)))
                 (quickrun2--compilation-start cmd compile-conf)))
              (t
               (let ((buf (get-buffer-create quickrun2--buffer-name)))
                 (quickrun2--setup-exec-buffer buf)
                 (quickrun2--exec (gethash :exec cmd-info-hash)
                                  (file-name-nondirectory src) major-mode)
                 (when (quickrun2--buffer-popup-p)
                   (quickrun2--pop-to-buffer buf 'quickrun2--mode)))))))))

(provide 'quickrun2)
;;; quickrun2.el ends here

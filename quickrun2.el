;;; quickrun2.el --- Run commands quickly -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-quickrun2
;; Version: 2.2.8
;; Package-Requires: ((emacs "26.3"))

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

(defconst quickrun2--buffer-name "*quickrun2*")
(defvar quickrun2--remove-files nil)

(defvar quickrun2--base-sources nil)
(defvar quickrun2--sources nil)
(defvar quickrun2--timeout-timer nil)

(defsubst quickrun2--windows-p ()
  (memq system-type '(ms-dos windows-nt cygwin)))

(defun quickrun2--pop-to-buffer (buf cb)
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    (funcall cb)
    (unless quickrun2-focus-p
      (select-window win))))

(defun quickrun2--colorize-filter (proc output)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (goto-char (point-max))
    (let ((start (point)))
      (insert output)
      (ansi-color-apply-on-region start (point)))))

(defun quickrun2--kill-process (process timeout)
  (when (eq (process-status process) 'run)
    (kill-process process))
  (let ((buf (get-buffer quickrun2--buffer-name)))
    (with-current-buffer buf
      (insert (format "\nTime out %s(running over %d second)"
                      (process-name process) timeout)))
    (quickrun2--remove-temp-files)
    (quickrun2--pop-to-buffer buf (lambda () (read-only-mode +1)))))

(defun quickrun2--start-process (cmd timeout)
  (let ((buf (get-buffer quickrun2--buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer))
    (let ((proc (condition-case nil
                    (apply #'start-file-process (concat "quickrun2-proc-" (car cmd)) buf cmd)
                  (error nil))))
      (when (and proc (>= timeout 0))
        (setq quickrun2--timeout-timer
              (run-at-time quickrun2-timeout-seconds nil
                           #'quickrun2--kill-process proc timeout)))
      proc)))

(defun quickrun2--recenter (arg)
  (with-selected-window (get-buffer-window quickrun2--buffer-name)
    (recenter arg)))

(defun quickrun2--set-output-buffer (after-fn)
  (let ((buf (get-buffer quickrun2--buffer-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (read-only-mode -1)
      (quickrun2--recenter -1)
      (when after-fn
        (funcall after-fn))
      (read-only-mode +1))))

(defun quickrun2--set-error-buffer (orig-file orig-mode use-tempfile)
  (with-current-buffer (get-buffer quickrun2--buffer-name)
    (read-only-mode -1)
    ;; replace file name with original name
    (when use-tempfile
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (search-forward orig-file nil t)
            (replace-match orig-file)))))
    ;; enable compilation-mode for error jump
    (compilation-mode orig-mode)
    (read-only-mode -1)
    (quickrun2--recenter -1)
    (goto-char (point-min))
    (read-only-mode +1)))

(defun quickrun2--execute (commands orig-name orig-mode use-tempfile timeout after-fn)
  (let* ((next-command  (car commands))
         (rest-commands (cdr commands))
         (process (quickrun2--start-process next-command timeout)))
    (if (not process)
        (progn
          (message "Failed to execute '%s'" next-command)
          (quickrun2--remove-temp-files))
      (set-process-filter process #'quickrun2--colorize-filter)
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (when quickrun2--timeout-timer
             (cancel-timer quickrun2--timeout-timer))
           (delete-process proc)
           (let* ((exit-status (process-exit-status proc))
                  (succeeded (zerop exit-status)))
             (if (and succeeded (not (null rest-commands)))
                 (quickrun2--execute
                  rest-commands orig-name orig-mode use-tempfile timeout after-fn)
               (if succeeded
                   (quickrun2--set-output-buffer after-fn)
                 (quickrun2--set-error-buffer orig-name orig-mode use-tempfile))
               (cond ((> scroll-conservatively 0) (quickrun2--recenter nil))
                     ((/= scroll-step 0) (quickrun2--recenter -1)))
               (quickrun2--remove-temp-files)))))))))

(defun quickrun2--remove-temp-files ()
  (dolist (file quickrun2--remove-files)
    (cond ((file-directory-p file) (delete-directory file t))
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

(defun quickrun2--add-remove-files (files)
  (let* ((files (if (listp files) files (list files)))
         (abs-paths (mapcar #'expand-file-name files)))
    (setq quickrun2--remove-files (append abs-paths quickrun2--remove-files))))

(defun quickrun2--temp-name (src)
  (let* ((extension (file-name-extension src))
         (suffix (or (and extension (concat "." extension)) ""))
         (dir default-directory))
    (expand-file-name (concat dir (make-temp-name "qr_") suffix))))

(defun quickrun2--find-source-from-pattern (filename)
  (cl-loop for lang-source in quickrun2--sources
           for source = (plist-get lang-source :source)
           when (string-match-p (plist-get source :pattern) filename)
           return source))

(defun quickrun2--find-source-from-major-mode (mode)
  (cl-loop for lang-source in quickrun2--sources
           for source = (plist-get lang-source :source)
           for modes = (plist-get source :major-mode)
           when (or (and (listp modes) (memq mode modes))
                    (eq mode modes))
           return source))

(defun quickrun2--prompt ()
  (let* ((candidates (cl-loop for source in quickrun2--sources
                              collect (plist-get source :name)))
         (lang (completing-read "Quickrun2 Lang: " candidates nil t)))
    (cl-loop for source in quickrun2--sources
             when (string= lang (symbol-name (plist-get source :name)))
             return (plist-get source :source))))

(defun quickrun2--find-language-source (filename)
  (or (and current-prefix-arg (quickrun2--prompt))
      (quickrun2--find-source-from-pattern filename)
      (quickrun2--find-source-from-major-mode major-mode)
      (quickrun2--prompt)))

(defun quickrun2--copy-region-to-tempfile (start end dst)
  ;; Suppress write file message
  (let ((content (buffer-substring-no-properties start end))
        (codec buffer-file-coding-system))
    (with-temp-file dst
      (set-buffer-file-coding-system codec)
      (insert content))
    (quickrun2--add-remove-files dst)))

(defun quickrun2--replace-param (elem source file)
  (cond ((stringp elem) elem)
        ((symbolp elem)
         (if (eq elem 'source)
             file
           (let* ((key (intern (format ":%s" elem)))
                  (replaced (plist-get source key)))
             (unless replaced
               (error "parameter %s is not found in source. %s" elem source))
             (if (functionp replaced)
                 (funcall replaced file)
               replaced))))
        (t (error "invalid type parameter %s is found in source. %s" elem source))))

(defun quickrun2--fill-param (prop source file)
  (cl-loop for param in (plist-get source prop)
           if (and (listp param) (not (null param))) ;; :exec param
           collect
           (progn
             (when (functionp param)
               (setq param (funcall param file)))
             (cl-loop for elem in param
                      for replaced = (quickrun2--replace-param elem source file)
                      if (listp replaced)
                      append replaced
                      else
                      collect replaced))
           else
           collect (quickrun2--replace-param param source file)))

;;;###autoload
(defun quickrun2 ()
  "Run commands quickly for current buffer"
  (interactive)
  (unless (buffer-file-name)
    (user-error "quickrun2 does not support buffer which is not visited file"))
  (when (get-buffer quickrun2--buffer-name)
    (kill-buffer quickrun2--buffer-name))
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (basename (file-name-nondirectory (buffer-file-name)))
         (src (quickrun2--temp-name (or basename "")))
         (lang-source (quickrun2--find-language-source basename)))
    (unless lang-source
      (error "Could not find language source for this file"))
    (let* ((tempfile-param (plist-member lang-source :tempfile))
           (use-tempfile (if tempfile-param (cadr tempfile-param) t))
           process-start)
      (unwind-protect
          (progn
            (if use-tempfile
                (quickrun2--copy-region-to-tempfile beg end src)
              (setq src basename))
            (quickrun2--add-remove-files (quickrun2--fill-param :remove lang-source src))
            (let ((buf (get-buffer-create quickrun2--buffer-name))
                  (commands (quickrun2--fill-param :exec lang-source src))
                  (timeout (or (plist-get lang-source :timeout) quickrun2-timeout-seconds))
                  (after-fn (plist-get lang-source :after)))
              (unless (listp (car commands))
                (setq commands (list commands)))
              (quickrun2--execute commands basename major-mode use-tempfile timeout after-fn)
              (quickrun2--pop-to-buffer buf 'quickrun2--mode)
              (setq process-start t)))
        (unless process-start
          (quickrun2--remove-temp-files))))))

(defun quickrun2--set-base-source (name &rest args)
  (let ((registered (assoc-default name quickrun2--base-sources)))
    (unless registered
      (add-to-list 'quickrun2--base-sources (cons name args)))))

(defmacro quickrun2-define-base-source (name &rest args)
  (declare (indent 1))
  `(quickrun2--set-base-source ',name ,@args))

(defun quickrun2--validate-source (name source)
  (let ((mode (plist-get source :major-mode))
        (pattern (plist-get source :pattern))
        (timeout (plist-get source :timeout))
        (exec (plist-get source :exec)))
    (unless exec
      (user-error "[%s] missing `:exec' parameter" name))
    (when (and pattern (not (stringp pattern)))
      (user-error "[%s] `:pattern' parameter must be string" name))
    (when (and mode (not (or (and (listp mode) (cl-every #'symbolp mode))
                             (symbolp mode))))
      (user-error "[%s] `:major-mode' parameter must be symbol or symbol list" name))
    (when (and timeout (not (numberp timeout)))
      (user-error "[%s] `:timeout' parameter must be number" name))))

(defun quickrun2--set-source (name &rest args)
  (declare (indent 1))
  (let* ((inherit (plist-get args :inherit))
         (command (plist-get args :command)))
    (when inherit
      (let ((parent (assoc-default inherit quickrun2--base-sources)))
        (setq args (append parent args))))
    (unless command
      (plist-put args :command (symbol-name name)))
    (quickrun2--validate-source name args)
    (let ((lang-source (cl-loop for source in quickrun2--sources
                                when (eq (plist-get source :name) name)
                                return source)))
      (if lang-source
          (plist-put lang-source :source args)
        (add-to-list 'quickrun2--sources (list :name name :source args))))))

(defmacro quickrun2-define-source (name &rest args)
  (declare (indent 1))
  `(quickrun2--set-source ',name ,@args))

;;
;; Language setting helpers
;;

(defun quickrun2--exe-output (filename)
  (let ((noext (file-name-sans-extension filename))
        (exe-ext (if (quickrun2--windows-p) ".exe" ".out")))
    (concat noext exe-ext)))

(defun quickrun2--c-link-option (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (link-flags)
      (when (re-search-forward "^#\\s-*include\\s-*<\\(?:pthread\\.h\\|thread>\\)"nil t)
        (push "-lpthread" link-flags))
      (when (re-search-forward "^#\\s-*include\\s-*<\\(?:math\\.h\\|cmath>\\)"nil t)
        (push "-lm" link-flags))
      link-flags)))

;;
;; Language Settings
;;

(let* ((has-gcc-only (or (not (executable-find "clang")) (executable-find "gcc")))
       (c-compiler (if has-gcc-only "gcc" "clang"))
       (cpp-compiler (if has-gcc-only "g++" "clang++")))
  (quickrun2-define-source c
    :major-mode 'c-mode
    :pattern "\\.c\\'"
    :output #'quickrun2--exe-output
    :exec '((compiler "-x" "c" "-std=gnu99" "-o" output source link-option) (output))
    :compiler c-compiler
    :link-option #'quickrun2--c-link-option
    :remove '(output))

  (quickrun2-define-source c++
    :inherit 'c++-base
    :major-mode 'c++-mode
    :pattern "\\.\\(cpp\\|cc\\|cxx\\)\\'"
    :output #'quickrun2--exe-output
    :exec '((compiler "-x" "c++" "-std=c++17" "-o" output source link-option) (output))
    :compiler cpp-compiler
    :link-option #'quickrun2--c-link-option
    :remove '(output)))

(quickrun2-define-base-source interpreter-base
  :exec '(command source))

(quickrun2-define-source perl
  :inherit 'interpreter-base
  :major-mode '(perl-mode cperl-mode)
  :pattern "\\.\\(?:pl\\|pm\\)\\'")

(quickrun2-define-source python
  :inherit 'interpreter-base
  :major-mode '(python-mode)
  :pattern "\\.py\\'"
  :command (if (executable-find "python3") "python3" "python"))

(quickrun2-define-source ruby
  :inherit 'interpreter-base
  :major-mode '(ruby-mode)
  :pattern "\\.rb\\'")

(quickrun2-define-source javascript
  :inherit 'interpreter-base
  :major-mode '(js-mode js2-mode)
  :pattern "\\.js\\'"
  :command "node")

(let ((has-deno (executable-find "deno")))
  (quickrun2-define-source typescript
    :major-mode '(typescript-mode)
    :exec (if has-deno
              '("deno" "run" "--allow-all" source)
            '(command source))
    :pattern "\\.tsx?\\'"
    :command (if has-deno "deno" "ts-node")))

(quickrun2-define-source go
  :major-mode '(go-mode)
  :pattern "\\.go\\'"
  :exec '((lambda (name)
            (if (string-match-p "_test\\.go\\'" name)
                '("go" "test")
              (list "go" "run" name))))
  :tempfile nil)

(quickrun2-define-source rust
  :major-mode '(rust-mode)
  :pattern "\\.rs\\'"
  :output #'quickrun2--exe-output
  :exec '(("rustc" "-o" output source) (output))
  :remove '(output))

(quickrun2-define-source lua
  :inherit 'interpreter-base
  :major-mode '(lua-mode)
  :pattern "\\.lua\\'")

(quickrun2-define-source julia
  :inherit 'interpreter-base
  :major-mode '(julia-mode)
  :pattern "\\.jl\\'")

(provide 'quickrun2)
;;; quickrun2.el ends here

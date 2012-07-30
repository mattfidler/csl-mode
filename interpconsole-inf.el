;;; interpconsole-inf.el --- Inferior InterpConsole
;;
;; Filename: interpconsole-inf.el
;; Description:  Inferior InterpConsole
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Jul 24 14:55:54 2012 (-0500)
;; Version:  0.01
;; Last-Updated: Wed Jul 25 10:14:35 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 110
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'octave-inf)
(defgroup interpconsole-inferior nil
  "Running InterpConsole as an inferior Emacs process."
  :group 'm-mode)

(defcustom interpconsole-inferior-interp-console-program
  (let (ret)
    (if (and (eq system-type 'windows-nt)
             (progn
               (setq ret (format "%s/AEgis Technologies/acslX/InterpConsole.exe" (getenv "ProgramFiles")))
               (file-exists-p ret)))
        (expand-file-name ret)
      "InterpConsole"))
  "Program invoked by `interpconsole-inferior'."
  :type 'string
  :group 'interpconsole-inferior)

(defcustom interpconsole-inferior-buffer "*Acsl*"
  "Buffer for interactive InterpConsole."
  :type 'string
  :group 'interpconsole-inferior)

(defcustom interpconsole-inferior-startup-args nil
  "List of command line arguments for the inferior InterpConsole process."
  :type '(repeat string)
  :group 'interpconsole-inferior)

(defcustom interpconsole-inferior-startup-file nil
  "Name of the inferior InterpConsole startup file.
The contents of this file are sent to the inferior InterpConsole process on
startup."
  :type '(choice (const :tag "None" nil)
                 file)
  :group 'interpconsole-inferior)

(defcustom interpconsole-inferior-prompt
  "\\(^[ \t]*\\|^[ \t]*\\[\\)>+ "
  "Regexp to match prompts for the inferior InterpConsole process."
  :type 'regexp
  :group 'interpconsole-inferior)

(defvar interpconsole-inferior-font-lock-keywords
  (list
   (cons interpconsole-inferior-prompt 'font-lock-type-face))
  ;; Could certainly do more font locking in inferior Octave ...
  "Additional expressions to highlight in Inferior Octave mode.")

(define-derived-mode interpconsole-inferior-mode comint-mode "Inferior InterpConsole"
  "Major mode for interacting with an inferior InterpConsole process.
Runs M as a subprocess of Emacs, with M I/O through an Emacs
buffer.

Entry to this mode successively runs the hooks `comint-mode-hook' and
`interpconsole-inferior-mode-hook'."
  (setq comint-prompt-regexp interpconsole-inferior-prompt)
  
  (set (make-local-variable 'comment-start) octave-comment-start)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) octave-comment-start-skip)
  
  (set (make-local-variable 'font-lock-defaults)
       '(interpconsole-inferior-font-lock-keywords nil nil))
  
  (set (make-local-variable 'info-lookup-mode) 'm-mode)
  
  (setq comint-input-ring-file-name
        (or (getenv "INTERPCONSOLE_HISTFILE") "~/.interpconsole_hist")
        comint-input-ring-size (or (getenv "INTERPCONSOLE_HISTSIZE") 1024))
  (add-hook 'comint-input-filter-functions
            'interpconsole-inf-directory-tracker nil t)
  (comint-read-input-ring t))

;;;###autoload
(defun interpconsole-inf (&optional arg)
  "Run an inferior InterpConsole process, I/O via `interpconsole-inferior-buffer'.
This buffer is put in Inferior InterpConsole mode.  See `interpconsole-inferior-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `interpconsole-inferior-startup-args' are sent as
command line arguments to the inferior InterpConsole process on startup.

Additional commands to be executed on startup can be provided either in
the file specified by `interpconsole-inferior-startup-file' or by the default
startup file, `~/.emacs-interpconsole'."
  (interactive "P")
  (let ((buffer interpconsole-inferior-buffer)
        (orig-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (get-buffer-create buffer)
    (if (comint-check-proc buffer)
        ()
      (with-current-buffer buffer
        (comint-mode)
        (interpconsole-inferior-startup orig-dir)
        (interpconsole-inferior-mode)))
    (if (not arg)
        (pop-to-buffer buffer))))

(defalias 'acsl 'interpconsole-inf)

(defvar interpconsole-inferior-receive-in-progress nil)
(defvar interpconsole-inferior-output-list nil)
(defvar interpconsole-inferior-output-string nil)

(defun interpconsole-inferior-output-digest (_proc string)
  "Special output filter for the inferior InterpConsole process.
Save all output between newlines into `interpconsole-inferior-output-list', and
the rest to `interpconsole-inferior-output-string'."
  (setq string (concat interpconsole-inferior-output-string string))
  (while (string-match "\n" string)
    (setq interpconsole-inferior-output-list
          (append interpconsole-inferior-output-list
                  (list (substring string 0 (match-beginning 0))))
          string (substring string (match-end 0))))
  (if (string-match interpconsole-inferior-prompt string)
      (setq interpconsole-inferior-receive-in-progress nil))
  (setq interpconsole-inferior-output-string string))

(defun interpconsole-inf-directory-tracker (string)
  "Tracks `cd' commands issued to the inferior InterpConsole process.
Use \\[interpconsole-inf-resync-dirs] to resync if Emacs gets confused."
  (cond
   ((string-match "^[ \t]*cd[ \t]*%[ \t]*$" string)) ; Do nothing for echo
   ((string-match "^[ \t]*cd[ \t;]*$" string)) ; Also an echo in Acsl
   ((string-match "^[ \t]*cd[ \t]+[\"']*\\([^ \t\n;]*?\\)[\"']*[ \t\n;]*" string)
    (cd (substring string (match-beginning 1) (match-end 1))))))

(defun interpconsole-inf-resync-dirs ()
  "Resync the buffer's idea of the current directory.
This command queries the inferior InterpConsole process about its current
directory and makes this the current buffer's default directory."
  (interactive)
  (interpconsole-inferior-send-list-and-digest '("cd %\n"))
  (cd (car interpconsole-inferior-output-list)))

(defvar interpconsole-inferior-process nil)

(defvar interpconsole-inferior-receive-in-progress nil)

(defun interpconsole-inferior-send-list-and-digest (list)
  "Send LIST to the inferior Octave process and digest the output.
The elements of LIST have to be strings and are sent one by one.  All
output is passed to the filter `inferior-octave-output-digest'."
  (let* ((proc interpconsole-inferior-process)
         (filter (process-filter proc))
         string)
    (set-process-filter proc 'interpconsole-inferior-output-digest)
    (setq interpconsole-inferior-output-list nil)
    (unwind-protect
        (while (setq string (car list))
          (setq interpconsole-inferior-output-string nil
                interpconsole-inferior-receive-in-progress t)
          (comint-send-string proc string)
          (while interpconsole-inferior-receive-in-progress
            (accept-process-output proc))
          (setq list (cdr list)))
      (set-process-filter proc filter))))

(defvar interpconsole-startup-hook nil)

(defun interpconsole-inferior-output-filter (proc string)
  "Standard output filter for the inferior Octave process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'."
  (comint-output-filter proc (inferior-octave-strip-ctrl-g string)))

(defun interpconsole-inferior-startup (&optional dir)
  "Start Inferior InterpConsole"
  (let ((exec-path exec-path)
        (last-path (getenv "PATH"))
        (base-dir (if (eq system-type 'windows-nt)
                      (format "%s/AEgis Technologies/acslX/" (getenv "ProgramFiles"))
                    nil))
        (orig-dir (or dir default-directory)))
    (when base-dir
      (when (file-exists-p base-dir)
        (cd base-dir)
        (setq exe-path `(,(format "%s" base-dir)
                         ,(format "%smingw32/bin" base-dir)
                         ,(format "%smingw32/libexec/gcc/mingw32/3.4.2" base-dir)
                         ,@exec-path))
        (setenv "PATH" (replace-regexp-in-string "/" "\\\\"
                                                 (mapconcat
                                                  (lambda(x) x) exe-path ";")))))
    (let ((proc (comint-exec-1
                 (substring interpconsole-inferior-buffer 1 -1)
                 interpconsole-inferior-buffer
                 interpconsole-inferior-interp-console-program
                 interpconsole-inferior-startup-args)))
      (set-process-filter proc 'interpconsole-inferior-output-digest)
      
      (setq comint-ptyp process-connection-type
            interpconsole-inferior-process proc
            interpconsole-inferior-output-list nil
            interpconsole-inferior-output-string nil
            interpconsole-inferior-receive-in-progress t)
      
      ;; This may look complicated ... However, we need to make sure that
      ;; we additional startup code only AFTER InterpConsole is ready (otherwise,
      ;; output may be mixed up).  Hence, we need to digest the InterpConsole
      ;; output to see when it issues a prompt.
      (while interpconsole-inferior-receive-in-progress
        (accept-process-output interpconsole-inferior-process))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (insert-before-markers
       (concat
        (if (not (bobp)) "\n")
        (if interpconsole-inferior-output-list
            (concat (mapconcat
                     'identity interpconsole-inferior-output-list "\n")
                    "\n"))))
      
      ;; O.k., now we are ready for the Inferior InterpConsole startup commands.
      (let* (commands
             (program (file-name-nondirectory interpconsole-inferior-interp-console-program))
             (file (or interpconsole-inferior-startup-file
                       "~/.emacs-interpconsole")))
        (setq commands
              (list
               (concat "cd \"" orig-dir "\"\n")
               (if (file-exists-p file)
                   (format "source (\"%s\");\n" file))))
        (interpconsole-inferior-send-list-and-digest commands))
      
      (insert-before-markers
       (concat
        (if interpconsole-inferior-output-list
            (concat (mapconcat
                     'identity interpconsole-inferior-output-list "\n")
                    "\n"))
        interpconsole-inferior-output-string))
      ;; Next, we check whether InterpConsole supports `completion_matches' ...
      
      ;; And finally, everything is back to normal.
      (set-process-filter proc 'interpconsole-inferior-output-filter)
      (run-hooks 'interpconsole-inferior-startup-hook)
      ;; Just in case, to be sure a cd in the startup file
      ;; won't have detrimental effects.
      (interpconsole-inf-resync-dirs))
    (setenv "PATH" (replace-regexp-in-string
                    "/" "\\\\"
                    (mapconcat
                     (lambda(x) x) exe-path ";")))))
(provide 'interpconsole-inf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interpconsole-inf.el ends here

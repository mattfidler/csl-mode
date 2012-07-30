;;; csl-mode.el --- Edits CSL files for acslX
;; 
;; Filename: csl-mode.el
;; Description: Edits CSL files for acsIX
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri May 18 10:21:34 2012 (-0500)
;; Version: 0.01
;; Last-Updated: Wed Jul 25 11:01:56 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 275
;; URL: http://github.com/mlf176f2/csl-mode
;; Keywords: CSL, acslX
;; Compatibility: Emacs 24
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: csl-mode.
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

(require 'font-lock)
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'm-mode)
(require 'interpconsole-inf)

;;;###autoload
(setq auto-mode-alist (append '(("\\.\\([Cc][Ss][Ll]\\)$" .
                                 csl-mode)) auto-mode-alist))

(defgroup csl-mode nil
  "Major mode for editing AcslX csl-source files."
  :group 'languages)


(defvar csl-keywords nil
  "CSL Keywords")
(unless csl-keywords
  (setq csl-keywords
        '("PROGRAM"
          "INITIAL"
          "DYNAMIC"
          "DERIVATIVE"
          "TERMINAL"
          "IF"
          "ELSE"
          "DO"
          "CONTINUE"
          "THEN"
          "GO"
          "TO"
          "DISCRETE"
          "MACRO"
          "EXIT"
          "STANDVAL"
          "PROCEDURAL"
          "END")))

;; Constants
(defun csl-build-builtin ()
  "Build Builtin from command reference manual."
  (interactive)
  (let ((dir "C:/Program Files/Aegis Technologies/acslX/Documentation/Online Help/Language Reference Manual")
        (builtin '())
        (ops '())
        (case-fold-search nil)
        tmp)
    (mapc
     (lambda(file)
       (when (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (while (re-search-forward "<h[23] class=\"Heading[23]_inner\"><a name=\".*?\">\\(.*?\\)<" nil t)
             (setq tmp (match-string 1))
             (when (string-match " *$" tmp)
               (setq tmp (replace-match "" nil nil tmp)))
             (unless (string-match " " tmp)
               (unless (string-match "[a-z]" tmp)
                 (unless (member tmp csl-keywords)
                   (add-to-list 'builtin tmp)))))
           (goto-char (point-min))
           (while (re-search-forward "\\([.][A-Z]+[.]\\)" nil t)
             (add-to-list 'ops (match-string 1))))))
     (directory-files dir t ".*html$"))
    (with-temp-file "./csl-builtin.el"
      (insert ";;; This file is autogenerated.\n")
      (insert "(setq csl-builtin '(")
      (mapc (lambda(keyword)
              (insert (format "\n%s\"%s\"" " " keyword)))
            builtin)
      (insert "))\n")
      (insert "(setq csl-ops '(")
      (mapc (lambda(op)
              (insert (format "\n%s\"%s\"" " " op)))
            ops)
      (insert "))\n(provide 'csl-builtin)")
      (insert "\n;;; end of csl-builtin.el\n"))))

;; csl-builtin
(require 'csl-builtin)

(defvar csl-mode-syntax-table nil
  "`csl-mode' syntax table")
(unless csl-mode-syntax-table
  (setq csl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" csl-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" csl-mode-syntax-table)
  (modify-syntax-entry ?\( "()" csl-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" csl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" csl-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" csl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" csl-mode-syntax-table)
  (modify-syntax-entry ?\} "){" csl-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\% "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\& "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\* "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\- "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\/ ". 14"  csl-mode-syntax-table)
  ;;(modify-syntax-entry ?* ". 23" csl-mode-sytax-table)
  (modify-syntax-entry ?\< "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\= "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\> "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\| "."  csl-mode-syntax-table)
  (modify-syntax-entry ?. "."   csl-mode-syntax-table)
  (modify-syntax-entry ?\_ "_"  csl-mode-syntax-table)
  (modify-syntax-entry ?! "w"   csl-mode-syntax-table)
  (modify-syntax-entry ?\; "."  csl-mode-syntax-table)
  ;;(modify-syntax-entry ?. "w" csl-mode-syntax-table)
  ;;(modify-syntax-entry ?- "w" csl-mode-syntax-table)
  (modify-syntax-entry ?\\ "."  csl-mode-syntax-table)
  ;; No quoting possible without $\
  (modify-syntax-entry ?/ ".")
  ;; Single quote and double quote and back-quote are string delimiters
  (modify-syntax-entry ?\' "\"" csl-mode-syntax-table)
  (modify-syntax-entry ?\" "."  csl-mode-syntax-table)
  (modify-syntax-entry ?\` "."  csl-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\! "<"  csl-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  csl-mode-syntax-table))

(defun csl-font-lock-constant (limit)
  "Font lock mode to look for constant variable names to highlight."
  (interactive (list (point-max)))
  (let (ret)
    (cond
     ((and
       (not (save-match-data (looking-at "[ \t]*\\<constant[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)")))
       (save-match-data
         (save-excursion
           (beginning-of-line)
           (looking-at "[ \t]*\\<constant[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)")))
       (if (looking-back "\\<constant[ \t]*")
           (progn
             (setq ret (re-search-forward "[ \t]*\\([A-Za-z_][A-Za-z_0-9]*\\)" limit t))
             (symbol-value 'ret))
         
         (progn
           (setq ret (re-search-forward "\\=.*?[,][ \t]*\\([A-Za-z_][A-Za-z_0-9]*\\)" limit t))
           (symbol-value 'ret)))))
     ((progn
        (setq ret (re-search-forward "\\<constant[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)" limit t))
        (symbol-value 'ret))))
    (symbol-value 'ret)))

(defvar csl-font-lock-keywords nil
  "`csl-mode' font-lock keywords")
(unless csl-font-lock-keywords
  (setq csl-font-lock-keywords
        `(
          ("!.*" (0 font-lock-comment-face t))
          (,(eval-when-compile
              (regexp-opt csl-keywords 'words))
           (1 font-lock-keyword-face))
          (,(eval-when-compile
              (regexp-opt csl-builtin 'words))
           (1 font-lock-builtin-face))
          (,(eval-when-compile
              (regexp-opt csl-ops 't))
           (1 font-lock-constant-face))
          ("[ \t]*\\([A-Za-z_][A-Za-z_0-9]*\\)[ \t]*="
           (1 font-lock-variable-name-face))
          ("\\<discrete[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)"
           (1 font-lock-function-name-face))
          (csl-font-lock-constant
           (1 font-lock-variable-name-face)))))


(defvar csl-imenu-generic-expression
  `(("Sections" ,(regexp-opt
                  '("DISCRETE"
                    "INITIAL"
                    "PROGRAM"
                    "DERIVATIVE"
                    "TERMINAL"
                    "DYNAMIC")
                  'words) 1)
    ("Variables" "^[ \t]*\\([A-Za-z_][A-Za-z_0-9]*\\)[ \t]*=" 1))
  "`csl-mode' imenu expression.")

(defvar csl-mode-map nil
  "Keymap used in `csl-mode' buffers.")
(setq csl-mode-map nil)
(when (not csl-mode-map)
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" 'csl-mode-open-in-libero)
    (define-key map "\C-c\C-c" 'csl-mode-compile)
    (setq csl-mode-map map)))

(define-derived-mode csl-mode prog-mode "CSL"
  "Major mode for editing CSL files."
  (interactive)
  ;; set up local variables
  
  (set (make-local-variable 'font-lock-defaults)
       '(csl-font-lock-keywords nil t))
  
  (use-local-map csl-mode-map)
  (setq imenu-generic-expression csl-imenu-generic-expression)
  (imenu-add-menubar-index)
  ;; (make-local-variable 'paragraph-separate)
  ;; (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  ;; (make-local-variable 'comment-indent-function)
  ;; (make-local-variable 'indent-region-function)
  (set (make-local-variable 'indent-line-function) 'csl-indent-line-function)
  ;; (make-local-variable 'add-log-current-defun-function)
  ;;
  (set-syntax-table csl-mode-syntax-table)
  (setq paragraph-separate      "^[ \t]*$"
        paragraph-start         "^[ \t]*$"
        require-final-newline   t
        comment-start           "!"
        comment-end             ""
        comment-start-skip      "\\([!]+\\)\\s *"
        comment-column          40)
  ;;(csl-construct-faces)
  )


(defun csl-last-line-indent-line-p (&optional li-q deindent-q)
  "* Is the last line an indentation line?"
  (interactive)
  (save-excursion
    (let (ret (pt (point)))
      (beginning-of-line)
      (csl-goto-last-line li-q)
      (setq ret (looking-at
                 (eval-when-compile
                   (format "[ \t]*%s" csl-start-keywords))))
      (unless ret
        (setq ret (looking-at
                   (eval-when-compile
                     (format "[ \t]*%s"
                             csl-indent-keywords))))
        (unless ret
          (setq ret (looking-at
                     (eval-when-compile
                       (format "[ \t]*%s" csl-indent-deindent-keywords))))
          (when (and deindent-q
                     (save-excursion
                       (goto-char pt)
                       (beginning-of-line)
                       (looking-at
                        (eval-when-compile
                          (format "[ \t]*%s" csl-indent-deindent-keywords)))))
            (set deindent-q t))))
      (when (interactive-p)
        (message "Indentation: %s" ret))
      (symbol-value 'ret))))


(defvar csl-end-keywords nil
  "Deindent keywords for CSL")
(unless csl-end-keywords
  (setq csl-end-keywords
        (eval-when-compile
          (regexp-opt
           '("END"
             "MACRO END"
             "ENDIF") 'words))))

(defvar csl-indent-deindent-keywords nil
  "Keywords that deindent for a line and reindent for a line.  An ELSE statement.")
(unless csl-indent-deindent-keywords
  (setq csl-indent-deindent-keywords
        (eval-when-compile
          (replace-regexp-in-string
           " +" " +" (replace-regexp-in-string
                      "@" ".+?"
                      (regexp-opt
                       '("ELSE IF@THEN"
                         "ELSE") 'words))))))


(defvar csl-start-keywords nil
  "Keywords that when starting a line indents a line")
(unless csl-start-keywords
  (setq csl-start-keywords
        (eval-when-compile
          (regexp-opt
           '(
             "DISCRETE"
             )
           'words))))

(defvar csl-indent-keywords nil
  "Keywords that indent.")
(unless csl-indent-keywords
  (setq csl-indent-keywords
        (eval-when-compile
          (replace-regexp-in-string
           "@" ".*?"
           (regexp-opt
            '("PROGRAM"
              "INITIAL" ; Can nest in derivate section 
              "DYNAMIC"
              "DERIVATIVE" ; Should be in Dynamic Section
              "TERMINAL"
              "PROCEDURAL"
              "MACRO@)"
              "IF@THEN" ; Only indent if THEN is found at end of line
              ))))))

(defun csl-current-line-deindent-p ()
  "Current line a deindent?"
  (save-excursion
    (beginning-of-line)
    (or
     (looking-at (eval-when-compile (format "[ \t]*%s" csl-end-keywords)))
     (looking-at (eval-when-compile (format "[ \t]*%s" csl-indent-deindent-keywords))))))

(defun csl-goto-last-line (&optional li-q)
  "Go to the last line of code -- ignore continuation lines.  Set li-q to the current-indentation when requested."
  (interactive)
  (beginning-of-line )
  (while (re-search-backward "^[ \t]*\n\\=" nil t))
  (forward-line -1)
  (beginning-of-line)
  (while (looking-at "[ \t]*$")
    (forward-line -1)
    (beginning-of-line))
  (when li-q
    (set li-q (current-indentation))))

(defun csl-last-line-indentation (&optional ignore)
  "Last line's indentation"
  (let (ret)
    (save-excursion
      (csl-goto-last-line 'ret)
      (symbol-value 'ret))))

(defun csl-indent-line-function ()
  "Indent Line Function for CSL code."
  (interactive)
  (let ((case-fold-search t)
        (curi (current-indentation))
        li
        is-id
        fli)
    (save-excursion
      (cond
       ((save-excursion
          (beginning-of-line)
          (or (bobp)
              (progn
                (skip-chars-backward " \t\n")
                (bobp)))) ;; First line is indented to zero.
        (setq fli 0))
       ((and (csl-current-line-deindent-p)
             (csl-last-line-indent-line-p 'li 'is-id))
        ;;(message "Same Indentation.")
        (unless (= li curi)
          (setq fli li)))
       ((csl-current-line-deindent-p) ;; Deindentation is possible
        ;;(message "Deindentation")
        (setq li (csl-last-line-indentation t))
        (setq li (max 0 (- li 2)))
        (unless (= li curi)
          (setq fli li)))
       ((csl-last-line-indent-line-p 'li 'is-id)
        ;;(message "Indent-line")
        ;; Last line indicates we should indent
        (if (and is-id) ;; Actually indent/deindent line, keep same indentation.
            (unless (= li curi) ;; Change the indentation appropriately.
              (setq fli li))
          (unless (= (+ 2 li) curi) ;; Indent
            (setq fli (+ li 2)))))
       
       (t ;; Keep last indentation
        ;;(message "Keep last indentation")
        (setq li (csl-last-line-indentation))
        (unless (= li curi)
          (setq fli li)))))
    (when fli
      (indent-line-to fli)
      (when (looking-at "[ \t]*$")
        (goto-char (match-end 0))))))
;;; Useful commands
(defcustom csl-libero-path
  (let (ret)
    (if (and (eq system-type 'windows-nt)
             (progn
               (setq ret (format "%s/AEgis Technologies/acslX/Libero.exe" (getenv "ProgramFiles")))
               (file-exists-p ret)))
        (expand-file-name ret)
      "Libero"))
  "Program invoked by `csl-mode-open-in-libero'."
  :type 'string
  :group 'csl-mode)


(defun csl-mode-open-in-libero ()
  "Opens current buffer in Libero"
  (interactive)
  (when (eq system-type 'windows-nt)
    (let ((file-name (buffer-file-name))
          (exec-path exec-path)
          (base-dir (format "%s/AEgis Technologies/acslX/" (getenv "ProgramFiles")))
          (last-path (getenv "PATH")))
      (when (file-exists-p base-dir)
        (setq exe-path `(,(format "%s" base-dir)
                         ,(format "%smingw32/bin" base-dir)
                         ,(format "%smingw32/libexec/gcc/mingw32/3.4.2" base-dir)
                         ,@exec-path))
        (setenv "PATH" (replace-regexp-in-string "/" "\\\\"
                                                 (mapconcat
                                                  (lambda(x) x) exe-path ";")))
        (start-process "libero" " *Libero*"
                       csl-libero-path
                       file-name)))
    (setenv "PATH" (replace-regexp-in-string
                    "/" "\\\\"
                    (mapconcat
                     (lambda(x) x) exe-path ";")))))

(defun csl-mode-makefile ()
  "Creates Makefile based on current CSL file"
  (let ((file-name (buffer-file-name))
        (base-dir (format "%s/AEgis Technologies/acslX/" (getenv "ProgramFiles")))
        w32-base-dir)
    (setq w32-base-dir base-dir)
    (setq w32-file-name file-name)
    (when (eq system-type 'windows-nt)
      (setq w32-base-dir
            (replace-regexp-in-string
             "/" "\\\\"
             (w32-short-file-name base-dir)))
      (setq w32-file-name
            (replace-regexp-in-string
             "/" "\\\\"
             (w32-short-file-name file-name))))
    (with-temp-file "makefile"
      (insert (format "ACSLXTREME_SETUP = %ssetup.in\n" w32-base-dir))
      (insert (format "AXLITEPATH = %s\n" (substring w32-base-dir 0 -1)))
      (insert (format "MODEL_NAME = %s\n"
                      (file-name-sans-extension
                       (file-name-nondirectory w32-file-name))))
      (insert (format "MODEL_DIR = %s\n"
                      (substring
                       (replace-regexp-in-string
                        "/" "\\\\"
                        (file-name-directory w32-file-name)) 0 -1)))
      (insert (format "TARGET_FULL = %s.dll\n"
                      (file-name-sans-extension
                       (file-name-nondirectory file-name))))
      (insert (format "MODEL_RRR_FULL = %s.rrr\n"
                      (file-name-sans-extension
                       (file-name-nondirectory file-name))))
      (insert (format "include %smakedefs\n" w32-base-dir)))))

(defun csl-mode-compile (&optional force)
  "Compile CSL model"
  (interactive "P")
  (when (eq system-type 'windows-nt)
    (let ((file-name (buffer-file-name))
          (exec-path exec-path)
          (base-dir (format "%s/AEgis Technologies/acslX/" (getenv "ProgramFiles")))
          (last-path (getenv "PATH")))
      (when (file-exists-p base-dir)
        (setq base-dir (w32-short-file-name base-dir))
        (setq exe-path `(,(format "%s" base-dir)
                         ,(format "%smingw32/bin" base-dir)
                         ,(format "%smingw32/libexec/gcc/mingw32/3.4.2" base-dir)
                         ,@exec-path))
        (setenv "PATH" (replace-regexp-in-string "/" "\\\\"
                                                 (mapconcat
                                                  (lambda(x) x) exe-path ";")))
        (csl-mode-makefile)
        (get-buffer-create "*CSL-Make*")
        (switch-to-buffer-other-window "*CSL-Make*")
        (insert (format "Build %s%s\n" file-name (if force " (forced)" "")))
        (insert (make-string 80 ?=))
        (insert "\n")
        (cd (file-name-directory file-name))
        (when force
          (shell-command-to-string "make clean"))
        (start-process "csl-make" "*CSL-Make*"
                       "make")))
    (setenv "PATH" (replace-regexp-in-string
                    "/" "\\\\"
                    (mapconcat
                     (lambda(x) x) exe-path ";")))))

(provide 'csl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; csl-mode.el ends here

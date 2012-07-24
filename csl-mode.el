;;; csl-mode.el --- Edits CSL files for acslX
;; 
;; Filename: csl-mode.el
;; Description: Edits CSL files for acsIX
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri May 18 10:21:34 2012 (-0500)
;; Version: 0.01
;; Last-Updated: Mon Jul 23 20:20:25 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 181
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
;;;###autoload
(setq auto-mode-alist (append '(("\\.\\([Cc][Ss][Ll]\\)$" .
                                 csl-mode)) auto-mode-alist))

(defvar csl-keywords nil
  "CSL Keywords")
(setq csl-keywords nil)
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
(load (concat
       (expand-file-name "csl-builtin.el"
                         (file-name-directory (or load-file-name buffer-file-name)))))

(defvar csl-mode-syntax-table nil
  "`csl-mode' syntax table")
(setq csl-mode-syntax-table nil)
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
(setq csl-font-lock-keywords nil)
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

(defvar csl-mode-map nil
  "Keymap used in `csl-mode' buffers.")
(when (not csl-mode-map)
  (setq csl-mode-map (make-sparse-keymap)))

(defun csl-mode ()
  "Major mode for editing CSL files."
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  
  (set (make-local-variable 'font-lock-defaults)
       '(csl-font-lock-keywords nil t))
  
  ;;(setq imenu-generic-expression csl-imenu-generic-expression)
  (use-local-map csl-mode-map)
  ;; (imenu-add-menubar-index)
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
  (setq major-mode              'csl-mode
        mode-name               "CSL"
        paragraph-separate      "^[ \t]*$"
        paragraph-start         "^[ \t]*$"
        require-final-newline   t
        comment-start           "!"
        comment-end             ""
        comment-start-skip      "\\([!]+\\)\\s *"
        comment-column          40)
  (csl-construct-faces)
  (run-mode-hooks 'csl-mode-hook))


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
(setq csl-end-keywords nil)
(unless csl-end-keywords
  (setq csl-end-keywords
        (eval-when-compile
          (regexp-opt
           '("END"
             "ENDIF") 'words))))

(defvar csl-indent-deindent-keywords nil
  "Keywords that deindent for a line and reindent for a line.  An ELSE statement.")

(setq csl-indent-deindent-keywords nil)
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
(setq csl-start-keywords nil)
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

(setq csl-indent-keywords nil)
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

(provide 'csl-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; csl-mode.el ends here
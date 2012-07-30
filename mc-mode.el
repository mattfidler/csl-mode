;;; mc-mode.el --- Acsl MC Language Statement
;;
;; Filename: mc-mode.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Jul 25 11:08:18 2012 (-0500)
;; Version: 
;; Last-Updated: Mon Jul 30 09:49:18 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 137
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
(eval-when-compile
  (require 'cl))
(require 'octave-mod)
(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'm-mode)
(setq auto-mode-alist (append '(("\\.\\([Mm][Cc]\\)$" .
                                 mc-mode)) auto-mode-alist))

(defgroup mc-mode nil
  "Major mode for editing AcslX mc-source files."
  :group 'languages)

(defgroup mc-mode-sampler-settings nil
  "Default Sampler Settings"
  :group 'mc-mode)

(defgroup mc-mode-sampler-general-settings nil
  "General Sampler Settings"
  :group 'mc-mode-sampler-settings)

(defcustom mc-mode-isadaptive "True"
  "Determines whether or not the variance of the proposal distribution should adapt over the iterations"
  :type '(choice
          (const :tag "True" "True")
          (const :tag "False" "False"))
  :group 'mc-mode-sampler-general-settings)

(defcustom mc-mode-loggingperiod 50
  "Specifies the number of iterations between when chains are saved to file"
  :type 'integer
  :group 'mc-mode-sampler-general-settings)


(defcustom mc-mode-numchains 1
  "Number of chains to compute"
  :type 'integer
  :group 'mc-mode-sampler-general-settings)

(defcustom mc-mode-numiterations 5000
  "Number of Iterations"
  :type 'integer
  :group 'mc-mode-sampler-general-settings)

(defcustom mc-mode-thinningfactor 1
  "Factor to thin the chains"
  :type 'integer
  :group 'mc-mode-sampler-general-settings)

(defcustom mc-mode-updatemode "ComponentwiseRandomWalk"
  "Specifies the MCM Sampling mode to use"
  :type '(choice
          (const "UserDefinedComponentwise")
          (const "MultivariateNormal")
          (const "UserDefinedMultivariateNormal")
          (const "ComponentwiseRandomWalk")
          (const "DifferentialEvoultion"))
  :group 'mc-mode-sampler-general-settings)

(defcustom mc-mode-verboseoutput "False"
  "Should the output be verbose?"
  :type '(choice
          (const :tag "True" "True")
          (const :tag "False" "False"))
  :group 'mc-mode-sampler-general-settings)


(defgroup mc-mode-sampler-advanced-settings nil
  "Advanced Sampler Settings"
  :group 'mc-mode-sampler-settings)

(defcustom mc-mode-adaptcovarscale "True"
  "Should the adaptive covariance scale be adjusted when using MVN scaling?"
  :type '(choice
          (const :tag "True" "True")
          (const :tag "False" "False"))
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptlowerthresh 0.25
  "Threshold on acceptance rate below which the proposal variance will be decreased"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptlowerthresham 0.15
  "Threshold on acceptance rate below which the proposal variance will be decreased (Adaptive Metropolis)"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptlowerthreshdr 0.45
  "Threshold on acceptance rate below which the proposal variance will be decreased (Delayed Rejection)"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptupperthresh 0.45
  "Threshold on acceptance rate above which the proposal variance will be increased"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptupperthresham 0.3
  "Threshold on acceptance rate above which the proposal variance will be increased (Adaptive Metropolis)"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptupperthreshdr 0.65
  "Threshold on acceptance rate above which the proposal variance will be increased (Delayed Rejection)"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-adaptperiod 30
  "Specifies the number of iterations between adapting the proposal variances."
  :type 'integer
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-sigmadecreasefact 20
  "Factor which the covariance scale is decreased during adaptation for full MVN sampling."
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)


(defcustom mc-mode-sigmaincreasefact 20
  "Factor which the covariance scale is increased during adaptation for full MVN sampling."
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-drsigmareducefact 0.2
  "Factor which the covariance scale is decreased during adaptation for delayed rejection component wise sampling ."
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)


(defcustom mc-mode-drsigmareducefactam 0.1
  "Factor which the covariance scale is decreased during adaptation for delayed rejection component wise sampling (Adaptive Metropolis)."
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-ignorenansinlikelihood "True"
  "Ignore Not a Number in Liklihoods."
  :type '(choice
          (const :tag "True" "True")
          (const :tag "False" "False"))
  :group 'mc-mode-sampler-advanced-settings)


(defcustom mc-mode-isdelayedrejection "False"
  "Use delayed rejection algorithm.."
  :type '(choice
          (const :tag "True" "True")
          (const :tag "False" "False"))
  :group 'mc-mode-sampler-advanced-settings)


(defcustom mc-mode-sigmadecreasefactor 0.9
  "Decrease sigma by factor"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-sigmaincreasefactor 1.1
  "Increase sigma by factor"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defgroup mc-mode-sampler-demc-settings nil
  "DEMC Settings"
  :group 'mc-mode-sampler-settings)


(defcustom mc-mode-demcb 0.0001
  "Random offset added to DEMC proposal"
  :type 'number
  :group 'mc-mode-sampler-demc-settings)

(defcustom mc-mode-demcthinningfactor 10
  "Random offset added to DEMC proposal"
  :type 'number
  :group 'mc-mode-sampler-demc-settings)

(defcustom mc-mode-snookerfraction 0.1
  "Snooker Fraction"
  :type 'number
  :group 'mc-mode-sampler-demc-settings)

(defcustom mc-mode-numburniniterations 0
  "Number of Burn-in Iterations"
  :type 'integer
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-covarscaledecreasefact 20
  "Covariance scaled decrease factor"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defcustom mc-mode-covarscaleincreasefact 20
  "Covariance scaled increase factor"
  :type 'number
  :group 'mc-mode-sampler-advanced-settings)

(defvar mc-mode-mcx-tags-boolean
  (eval-when-compile
    (regexp-opt
     '("isadaptive"
       "verboseoutput"
       "adaptcovarscale"
       "ignorenansinlikelihood"
       "isdelayedrejection")))
  "mcx boolean tags")

(defvar mc-mode-mcx-tags nil
  "`mc-mode' tags for generating mcx files.")
(setq mc-mode-mcx-tags nil)
(unless mc-mode-mcx-tags
  (setq mc-mode-mcx-tags
        (eval-when-compile
          (replace-regexp-in-string
           "###" "[0-9][0-9.]*"
           (format "\\<%s[ \t=:]+[\"]?%s[\"]?"
                   (regexp-opt
                    '("numiterations" "numburniniterations" "thinningfactor" "numchains"
                      "loggingperiod" "verboseoutput" "updatemode" "isadaptive" "adaptperiod"
                      "adaptcovarscale" "isdelayedrejection" "ignorenansinlikelihood"
                      "adaptlowerthresh" "adaptupperthresh" "adaptlowerthreshdr"
                      "adaptupperthreshdr" "sigmadecreasefact" "sigmaincreasefact"
                      "drsigmareducefact" "drsigmareducefactam" "adaptlowerthresham"
                      "adaptupperthresham" "covarscaledecreasefact" "covarscaleincreasefact"
                      "snookerfraction" "demcthinningfactor" "demcb") t)
                   (regexp-opt
                    '("true" "false" "t" "f" "userdefinedcomponentwise"
                      "multivariatenormal" "userdefinedmultivariatenormal"
                      "componentwiserandomwalk" "differentialevoultion" "###")
                    't))))))

(defun mc-mode-set-mcx-args ()
  "Set mcx arguments in the .mc file"
  (let ((mcx-file (buffer-file-name))
        (case-fold-search t)
        (values '())
        (values-reg nil)
        (ret ""))
    (when mcx-file
      (setq mcx-file (concat (file-name-sans-extension mcx-file) ".mcx"))
      (when (file-readable-p mcx-file)
        ;; Find any values that are already listed in the mc file.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward mc-mode-mcx-tags nil t)
            (add-to-list 'values (match-string 1))))
        (unless (= 0 (length values))
          (setq values-reg (regexp-opt values 't)))
        
        ;; Find values different from default values.
        (with-temp-buffer
          (insert-file-contents mcx-file)
          (goto-char (point-min))
          (while (re-search-forward mc-mode-mcx-tags nil t)
            (unless
                (string= (match-string 2)
                         (format "%s"
                                 (save-match-data
                                   (with-temp-buffer
                                     (insert (format "mc-mode-%s" (downcase
                                                                   (match-string 1)))))
                                   (eval-buffer))))
              ;; Different value
              (when (or (not values-reg)
                        (not (and values-reg (save-match-data (string-match values-reg (match-string 1))))))
                ;; Not specified in mc buffer.
                (if (string= "" ret)
                    (setq ret (format "; %s = %s" (match-string 1) (match-string 2)))
                  (setq ret (format "%s\n; %s = %s" (match-string 1) (match-string 2))))))))
        (unless (string= "" ret)
          (goto-char (point-min))
          (insert ret)
          (insert "\n"))))))

(defun mc-mode-find-mcx-args ()
  "Find Sampler settings in comment lines, and then generate a mcx file"
  (interactive)
  (when (and mc-mode (not (minibufferp)))
    (let ((case-fold-search t)
          (options "")
          (file-name (buffer-file-name))
          (curr nil))
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward mc-mode-mcx-tags nil t)
            (when (save-match-data (looking-back "%.*" nil t))
              (cond
               ((string= "updatemode" (downcase (match-string 1)))
                (setq curr
                      (cond
                       ((string= "userdefinedcomponentwise" (downcase (match-string 2)))
                        "UserDefinedComponentwise")
                       ((string= "multivariatenormal" (downcase (match-string 2)))
                        "MultivariateNormal")
                       ((string= "userdefinedmultivariatenormal" (downcase (match-string 2)))
                        "UserDefinedMultivariateNormal")
                       ((string= "componentwiserandomwalk" (downcase (match-string 2)))
                        "ComponentwiseRandomWalk")
                       ((string= "differentialevoultion" (downcase (match-string 2)))
                        "DifferentialEvoultion")
                       (t nil)))
                (when curr
                  (setq options (format "%s :%s \"%s\""
                                        options (downcase (match-string 1)) curr))))
               ((save-match-data
                  (string-match mc-mode-mcx-tags-boolean
                                (match-string 1)))
                (setq curr (cond
                            ((or (string= (downcase (match-string 2)) "t")
                                 (string= (downcase (match-string 2)) "true"))
                             "True")
                            ((or (string= (downcase (match-string 2)) "f")
                                 (string= (downcase (match-string 2)) "false"))
                             "False")
                            (t nil)))
                (when curr
                  (setq options (format "%s :%s \"%s\""
                                        options (downcase (match-string 1)) curr))))
               ((save-match-data
                  (string-match "[0-9][0-9.]*" (match-string 2)))
                (setq options (format "%s :%s %s"
                                      options (downcase (match-string 1)) (match-string 2)))))))
          (with-temp-buffer
            (insert (format "(mc-mode-generate-mcx :file-name \"%s\" %s)" file-name options))
            (eval-buffer)))))))

(defun* mc-mode-generate-mcx
    (&optional
     &key
     file-name
     numiterations numburniniterations thinningfactor
     numchains loggingperiod verboseoutput
     updatemode isadaptive adaptperiod
     adaptcovarscale isdelayedrejection
     ignorenansinlikelihood adaptlowerthresh
     adaptupperthresh adaptlowerthreshdr
     adaptupperthreshdr sigmadecreasefact
     sigmaincreasefact drsigmareducefact
     drsigmareducefactam adaptlowerthresham
     adaptupperthresham covarscaledecreasefact
     covarscaleincreasefact snookerfraction
     demcthinningfactor demcb)
  "Generates a mcx file."
  (let ((file (or file-name (buffer-file-name)))
        (f-verboseoutput (or verboseoutput mc-mode-verboseoutput))
        (f-updatemode (or updatemode mc-mode-updatemode))
        (f-isadaptive (or isadaptive mc-mode-isadaptive))
        (f-ignorenansinlikelihood (or ignorenansinlikelihood mc-mode-ignorenansinlikelihood))
        (f-adaptcovarscale (or adaptcovarscale mc-mode-adaptcovarscale))
        (f-isdelayedrejection (or isdelayedrejection mc-mode-isdelayedrejection))
        
        (f-numiterations (or numiterations mc-mode-numiterations))
        (f-numburniniterations (or numburniniterations mc-mode-numburniniterations))
        (f-thinningfactor (or thinningfactor mc-mode-thinningfactor))
        (f-numchains (or numchains mc-mode-numchains))
        (f-loggingperiod (or loggingperiod mc-mode-loggingperiod))
        
        
        (f-adaptperiod (or adaptperiod mc-mode-adaptperiod))
        (f-adaptlowerthresh (or adaptlowerthresh mc-mode-adaptlowerthresh))
        (f-adaptupperthresh (or adaptupperthresh mc-mode-adaptupperthresh))
        (f-adaptlowerthreshdr (or adaptlowerthreshdr mc-mode-adaptlowerthreshdr))
        (f-adaptupperthreshdr (or adaptupperthreshdr mc-mode-adaptupperthreshdr))
        (f-sigmadecreasefact (or sigmadecreasefact mc-mode-sigmadecreasefact))
        (f-sigmaincreasefact (or sigmaincreasefact mc-mode-sigmaincreasefact))
        (f-drsigmareducefact (or drsigmareducefact mc-mode-drsigmareducefact))
        (f-drsigmareducefactam (or drsigmareducefactam mc-mode-drsigmareducefactam))
        (f-adaptlowerthresham (or adaptlowerthresham mc-mode-adaptlowerthresham))
        (f-adaptupperthresham (or adaptupperthresham mc-mode-adaptupperthresham))
        (f-covarscaledecreasefact (or covarscaledecreasefact mc-mode-covarscaledecreasefact))
        (f-covarscaleincreasefact (or covarscaleincreasefact mc-mode-covarscaleincreasefact))
        (f-snookerfraction (or snookerfraction mc-mode-snookerfraction))
        (f-demcthinningfactor (or demcthinningfactor mc-mode-demcthinningfactor))
        (f-demcb (or demcb mc-mode-demcb)))
    (if (not file)
        (message "Need to save file before generating mcx file.")
      (with-temp-file (concat (file-name-sans-extension file) ".mcx")
        (insert (format "<mcmodelersettings numiterations=\"%s\" numburniniterations=\"%s\" thinningfactor=\"%s\" numchains=\"%s\" loggingperiod=\"%s\" verboseoutput=\"%s\" updatemode=\"%s\" isadaptive=\"%s\" adaptperiod=\"%s\" adaptcovarscale=\"%s\" isdelayedrejection=\"%s\" ignorenansinlikelihood=\"%s\" adaptlowerthresh=\"%s\" adaptupperthresh=\"%s\" adaptlowerthreshdr=\"%s\" adaptupperthreshdr=\"%s\" sigmadecreasefact=\"%s\" sigmaincreasefact=\"%s\" drsigmareducefact=\"%s\" drsigmareducefactam=\"%s\" adaptlowerthresham=\"%s\" adaptupperthresham=\"%s\" covarscaledecreasefact=\"%s\" covarscaleincreasefact=\"%s\" snookerfraction=\"%s\" demcthinningfactor=\"%s\" demcb=\"%s\" />\n" f-numiterations f-numburniniterations f-thinningfactor f-numchains f-loggingperiod f-verboseoutput f-updatemode f-isadaptive f-adaptperiod f-adaptcovarscale f-isdelayedrejection f-ignorenansinlikelihood f-adaptlowerthresh f-adaptupperthresh f-adaptlowerthreshdr f-adaptupperthreshdr f-sigmadecreasefact f-sigmaincreasefact f-drsigmareducefact f-drsigmareducefactam f-adaptlowerthresham f-adaptupperthresham f-covarscaledecreasefact f-covarscaleincreasefact f-snookerfraction f-demcthinningfactor f-demcb))))))

(defvar mc-mode-known-keywords nil
  "`mc-mode' known keywords")

(unless mc-mode-known-keywords
  (setq mc-mode-known-keywords
        '("observed"
          "I"
          "dimension")))

(defvar mc-mode-distributions nil
  "`mc-mode' known distributions")

(unless mc-mode-distributions
  (setq mc-mode-distributions
        '("bern"
          "beta"
          "chi2"
          "disc"
          "exp"
          "f"
          "logn"
          "logu"
          "logist"
          "nbin"
          "norm"
          "poiss"
          "t"
          "tri"
          "unif"
          "weib"
          "mvnorm"
          "emp"
          "gam"
          "dirch"
          "multi"
          "igam"
          "wish")))

(defvar mc-font-lock-keywords nil
  "`mc-mode' font-lock definitions, different from `octave-mode'.")
(setq mc-font-lock-keywords nil)
(unless mc-font-lock-keywords
  (setq mc-font-lock-keywords
        `((,(eval-when-compile
              (regexp-opt (append mc-mode-known-keywords
                                  mc-mode-distributions) 'words))
           (1 font-lock-keyword-face))
          ,@m-font-lock-keywords)))

(defvar mc-completion-alist nil)

(defun mc-initialize-completions ()
  "Create an alist for Octave completions."
  (if mc-completion-alist
      ()
    (setq mc-completion-alist
          (append m-builtin
                  m-props
                  mc-mode-distributions
                  mc-mode-known-keywords
                  octave-reserved-words
                  octave-text-functions
                  octave-variables))))

(defun mc-completion-at-point-function ()
  "Find the text to complete and the corresponding table."
  (let* ((beg (save-excursion (skip-syntax-backward "w_") (skip-chars-backward "@") (point)))
         (end (point)))
    (if (< beg (point))
        ;; Extend region past point, if applicable.
        (save-excursion (skip-syntax-forward "w_")
                        (setq end (point))))
    (list beg end mc-completion-alist)))

(define-derived-mode mc-mode m-mode "MC"
  "M-mode for editing Acsl mc-files."
  (mc-initialize-completions)
  (set (make-local-variable 'font-lock-defaults)
       '(mc-font-lock-keywords))
  (add-hook 'completion-at-point-functions
            'mc-completion-at-point-function nil t)
  (mc-mode-set-mcx-args)
  (add-hook 'write-contents-hooks 'mc-mode-find-mcx-args t t))

(provide 'mc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mc-mode.el ends here

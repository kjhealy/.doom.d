;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kieran Healy"
      user-mail-address "kjhealy@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Berkeley Mono" :size 12 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "TT Supermolot Condensed" :size 14))

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-dracula)
;;(setq doom-dracula-brighter-comments t)
;;(setq doom-dracula-brighter-modeline t)


(with-eval-after-load 'doom-themes
 (doom-themes-treemacs-config)
 (doom-themes-org-config))

(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox \(Maestral\)/Org/")
;; (setq org-directory "~/Dropbox/Org/")

;; Choose either listings or minted for exporting source code blocks.
;; Using minted (as here) requires pygments be installed. To use the
;; default listings package instead, use
;; (setq org-latex-listings t)
;; and change references to "minted" below to "listings"
(setq org-latex-listings 'minted)

;; default settings for minted code blocks.
;; bg will need to be defined in the preamble of your document. It's defined in  org-preamble-xelatex.sty below.
(setq org-latex-minted-options
      '(;("frame" "single")
        ("bgcolor" "bg")
        ("fontsize" "\\small")
        ))

;; turn off the default toc behavior; deal with it properly in headers to files.
(defun org-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
            depth)))
(setq org-latex-format-toc-function 'org-latex-no-toc)


;; note the insertion of the \input statement for the vc information
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("memarticle"
                 "\\documentclass[11pt,oneside,article]{memoir}\n\\input{vc} % vc package"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("membook"
                 "\\documentclass[11pt,oneside]{memoir}\n\\input{vc} % vc package"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  ;; LaTeX compilation command. For orgmode docs we just always use xelatex for convenience.
  ;; You can change it to pdflatex if you like, just remember to make the adjustments to the packages-alist below.
  (setq org-latex-pdf-process '("latexmk -pdflatex='xelatex -synctex=1 --shell-escape' -pdf %f"))

  ;; Default packages included in the tex file. As before, org-preamble-xelatex is part of latex-custom-kjh.
  ;; There's org-preamble-pdflatex as well, if you wish to use that instead.
  (setq org-latex-default-packages-alist nil)
  (setq org-latex-packages-alist
        '(("minted" "org-preamble-xelatex" t)
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" )))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq kill-whole-line t)

(setq auto-save-default t
      make-backup-files t)

(setq confirm-kill-emacs nil)

(after! auth-source
  (setq auth-sources (nreverse auth-sources)))

(setq doom-modeline-enable-word-count t)

(use-package! switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

;;(map! "C-s" #'counsel-grep-or-swiper)
(map! "C-s" #'+default/search-buffer)

(use-package! visual-regexp-steroids
  :defer 3
  :config
  (require 'pcre2el)
  (setq vr/engine 'pcre2el)
  (map! "C-c s r" #'vr/replace)
  (map! "C-c s q" #'vr/query-replace))

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-x C-p") 'find-file-at-point)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file)


(global-set-key (kbd "C-x C-m") 'execute-extended-command)



(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))


;; avy
(map! "M-g g" #'avy-goto-line)
(map! "M-g M-g" #'avy-goto-line)
(map! "C-:" #'avy-goto-char)

;; comment region toggle
(map! "C-c ;" #'comment-or-uncomment-region)

;; Windows
(map! "C-S-<left>" #'shrink-window-horizontally)
(map! "C-S-<right>" #'enlarge-window-horizontally)
(map! "C-S-<down>" #'shrink-window)
(map! "C-S-<up>" #'enlarge-window)

;; Make window selection numbers easily visible
(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(defun kjh/rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(map! "C-x m" #'kjh/rotate-windows)

;; Reftex
;; Make RefTex able to find my local bib files
(setq reftex-bibpath-environment-variables
      '("/Users/kjhealy/Library/texmf/bibtex/bib"))

;; Default bibliography
(setq reftex-default-bibliography
      '("/Users/kjhealy/Documents/bibs/socbib.bib"))


;; citar
(setq! bibtex-completion-bibliography '("/Users/kjhealy/Documents/bibs/socbib.bib"))
(setq! citar-bibliography '("/Users/kjhealy/Documents/bibs/socbib.bib"))
(setq org-cite-csl-styles-dir "~/.pandoc/csl")

;; Flycheck
(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map))
  (setq! flycheck-lintr-linters "linters_with_defaults(line_length_linter = line_length_linter(120), trailing_whitespace_linter = NULL)")
  )


;; ESS
(use-package! ess-mode

  :config
  (add-hook! 'ess-mode-hook
    (setq! ess-use-flymake nil
           lsp-ui-doc-enable nil
           lsp-ui-doc-delay 1.5
           polymode-lsp-integration nil
           ess-style 'RStudio
           ess-offset-continued 2
           ess-expression-offset 0
           comint-scroll-to-bottom-on-output t)

  (setq! ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers      . t)
          (ess-fl-keyword:operators    . t)
          (ess-fl-keyword:delimiters) ; don't because of rainbow delimiters
          (ess-fl-keyword:=            . t)
          (ess-R-fl-keyword:F&T        . t)
          (ess-R-fl-keyword:%op%       . t)))
  )

  ;; ESS buffers should not be cleaned up automatically
  (add-hook 'inferior-ess-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Assignment
  (define-key ess-mode-map "_" #'ess-insert-assign)
  (define-key inferior-ess-mode-map "_" #'ess-insert-assign)

  (defun kjh/then-R-operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (define-key ess-mode-map (kbd "C-|") 'kjh/then_R_operator)
  (define-key inferior-ess-mode-map (kbd "C-|") 'kjh/then-R-operator)

  ;; mirror R-Studio's cmd-shift-M binding for %>%
  (map! "s-M" #'kjh/then-R-operator)

)


;; polymode
(use-package! poly-R
  :config
    (defun kjh/insert-r-chunk (header)
  "Insert an r-chunk in markdown mode."
  (interactive "sLabel: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

  (add-to-list 'auto-mode-alist '("\\.Rmarkdown" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))
  (map! (:localleader
         :map polymode-mode-map
         :desc "Export"   "e" 'polymode-export
         :desc "Errors" "$" 'polymode-show-process-buffer
         :desc "Eval region or chunk" "v" 'polymode-eval-region-or-chunk
         :desc "Eval from top" "v" 'polymode-eval-buffer-from-beg-to-point
         :desc "Weave" "w" 'polymode-weave
         :desc "New chunk" "c" 'kjh/insert-r-chunk
         :desc "Next" "n" 'polymode-next-chunk
         :desc "Previous" "p" 'polymode-previous-chunk
         ;; (:prefix ("c" . "Chunks")
         ;;   :desc "Narrow" "n" . 'polymode-toggle-chunk-narrowing
         ;;   :desc "Kill" "k" . 'polymode-kill-chunk
         ;;   :desc "Mark-Extend" "m" . 'polymode-mark-or-extend-chunk)
         ))
)

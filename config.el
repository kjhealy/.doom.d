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

(setq doom-font (font-spec :family "Berkeley Mono" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "Berkeley Mono" :size 16 :weight 'regular)
      ;; doom-variable-pitch-font (font-spec :family "TT Supermolot Condensed" :size 14)
      doom-variable-pitch-font (font-spec :family "Fabrikat Kompakt" :size 14)
      )

;; tweak
;; If integer, it means pixels, added below each line.
;; If float, a scaling factor relative to current window's default line height.
;; If nil, add no extra spacing.
(setq! line-spacing 0.15)

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; (setq doom-theme 'catppuccin)
;; (setq catppuccin-flavor 'mocha)

;; (setq doom-theme 'ef-frost)
;; (setq doom-theme 'ef-maris-dark)
;; (setq doom-theme 'doom-one)
(setq doom-theme 'kanagawa-wave)
(setq kanagawa-theme-keyword-italic 'nil)

;; (setq ef-themes-headings
;;       '((0 variable-pitch semibold 1.6)
;;         (1 variable-pitch semibold 1.5)
;;         (2 variable-pitch semibold 1.4)
;;         (3 variable-pitch regular 1.3)
;;         (4 variable-pitch regular 1.3)
;;         (5 variable-pitch 1.3) ; absence of weight means `bold'
;;         (6 variable-pitch 1.2)
;;         (7 variable-pitch 1.2)
;;         (t variable-pitch 1.1)))

;; They are nil by default...
;;(setq ef-themes-mixed-fonts t
 ;;     ef-themes-variable-pitch-ui t)


(setq corfu-auto-delay 0.5)
(use-package! corfu
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package! orderless
  :config
  (add-to-list 'orderless-matching-styles 'char-fold-to-regexp))

(custom-set-faces! '((corfu-popupinfo) :height 0.9))

;; Change the shortcut key for completion-at-point and restore
;; cmd-space as set mark
(map! :unless (modulep! +tng) :gi "C-S-SPC" #'completion-at-point)

(map! "C-SPC" #'set-mark-command)



;; Projectile
(setq projectile-project-search-path '(("~/Documents/source/" . 1)
                                       ("~/Documents/data" . 1)
                                       ("~/Documents/data/misc/" . 1)
                                       ("~/Documents/courses/" . 1)
                                       ("~/Documents/sites" . 1)
                                       ("~/Documents/talks/" . 1)
                                       ("~/Documents/ordinal-society" . 1)
                                       ("~/Documents/papers" . 1)
                                       "~/.doom.d"))

;;(setq doom-theme 'doom-dracula)
;;(setq doom-dracula-brighter-comments t)
;;(setq doom-dracula-brighter-modeline t)


(with-eval-after-load 'doom-themes
 (doom-themes-treemacs-config)
 (doom-themes-org-config))


;; specify nerd icons
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))


(use-package! treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist"))
  :config
  ;; Don't follow the cursor
  (treemacs-follow-mode -1)

  (set-popup-rule! "^ ?\\*Treemacs" :ignore t)
  (when +treemacs-git-mode
    ;; If they aren't supported, fall back to simpler methods
    (when (and (memq +treemacs-git-mode '(deferred extended))
               (not treemacs-python-executable)
               (not (executable-find "python3")))
      (setq +treemacs-git-mode 'simple))
    (treemacs-git-mode +treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq +treemacs-git-mode '(extended deferred))
              3
            0))))
  (treemacs-load-theme "nerd-icons")
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
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter)

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


(after! org
  ;; Set some faces
  (custom-set-faces!
    `((org-quote)
      :foreground ,(doom-color 'blue) :extend t)
    `((org-block-begin-line org-block-end-line)
      :background ,(doom-color 'bg)))
  ;; Change how LaTeX and image previews are shown
  (setq org-highlight-latex-and-related '(native entities script)
        org-image-actual-width (min (/ (display-pixel-width) 3) 800)))

;; (after! org-roam
;;   ;; Define advise
;;   (defun hp/org-roam-capf-add-kind-property (orig-fun &rest args)
;;     "Advice around `org-roam-complete-link-at-point' to add :company-kind property."
;;     (let ((result (apply orig-fun args)))
;;       (append result '(:company-kind (lambda (_) 'org-roam)))))
;;   ;; Wraps around the relevant functions
;;   (advice-add 'org-roam-complete-link-at-point :around #'hp/org-roam-capf-add-kind-property)
;;   (advice-add 'org-roam-complete-everywhere :around #'hp/org-roam-capf-add-kind-property))

(after! org
  ;; Prettification should ignore preceding letters
  (defun prettify-symbols-compose-in-text-mode-p (start end _match)
    "Similar to `prettify-symbols-default-compose-p' but ignore letters or words."
    ;; Check that the chars should really be composed into a symbol.
    (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?_))
                             '(?_) '(?. ?\\)))
           (syntaxes-end (if (memq (char-syntax (char-before end)) '(?_))
                             '(?_) '(?. ?\\))))
      (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
               (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
               ;; (nth 8 (syntax-ppss))
               (org-in-src-block-p)
               ))))
  ;; Replace two consecutive hyphens with the em-dash
  (defun kjh/org-mode-load-prettify-symbols ()
    (interactive)
    (pushnew! prettify-symbols-alist
              '(":PROPERTIES:" . "")
              '("--"  . "–") '("---" . "—")
              '("(?)" . "") '("(?)." . ".") '("(?)," . ","))
    (modify-syntax-entry ? " ")
    (prettify-symbols-mode 1)
    ;; Now, set the value of this
    (setq-local prettify-symbols-compose-predicate 'prettify-symbols-compose-in-text-mode-p)
    )
  (when (not IS-WINDOWS)
    (add-hook 'org-mode-hook 'kjh/org-mode-load-prettify-symbols)))

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


(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Appearance
   org-modern-radio-target    '("❰" t "❱")
   org-modern-internal-target '("↪ " t "")
   org-modern-todo nil
   org-modern-tag nil
   org-modern-timestamp t
   org-modern-statistics nil
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule "──────────"
   org-modern-hide-stars "·"
   org-modern-star ["⁖"]
   org-modern-keyword "‣"
   org-modern-list '((43 . "•")
                     (45 . "–")
                     (42 . "↪")))
  (custom-set-faces!
    `((org-modern-tag)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg) 0.1)
      :foreground ,(doom-color 'grey))
    `((org-modern-radio-target org-modern-internal-target)
      :inherit 'default :foreground ,(doom-color 'blue)))
  )

(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks 'just-brackets))

(use-package! oc-csl-activate
  :config
  (setq org-cite-activate-processor 'csl-activate)
  (setq org-cite-csl-activate-use-document-style t)
  (setq org-cite-csl-activate-use-document-locale t)
  (add-hook 'org-mode-hook
            (lambda ()
              (cursor-sensor-mode 1)
              (org-cite-csl-activate-render-all))))

(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))


(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

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


;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))


;; avy
(map! "M-g g" #'avy-goto-line)
(map! "M-g M-g" #'avy-goto-line)
;; (map! "C-:" #'avy-goto-char)

;; treemacs
(map! "C-c :" #'treemacs-select-window)


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
(use-package! citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (setq
   citar-file-variable "file"
   citar-symbol-separator "  "
   ;; The global bibliography source may be set to something,
   ;; but now let's set it on a per-file basis
   ;; org-cite-global-bibliography citar-bibliography
   ))

;; citar
(after! citar
    (setq! bibtex-completion-bibliography '("/Users/kjhealy/Documents/bibs/socbib.bib"))
    (setq! citar-bibliography '("/Users/kjhealy/Documents/bibs/socbib.bib"))
    (setq! citar-library-paths '("/Users/kjhealy/Documents/bibs/files"))
    (setq! citar-notes-paths '("Users/kjhealy/Documents/bibs/notes"))
    (setq org-cite-csl-styles-dir "~/.pandoc/csl")

  ;; Define advise
  (defun kjh/citar-capf-add-kind-property (orig-fun &rest args)
    "Advice around `org-roam-complete-link-at-point' to add :company-kind property."
    (let ((result (apply orig-fun args)))
      (append result '(:company-kind (lambda (_) 'reference)))))
  ;; Wraps around the relevant functions
  (advice-add 'citar-capf :around #'kjh/citar-capf-add-kind-property)
  )


;; Citar cite command is C-b
(map! :prefix "C-c"
      "b" #'citar-insert-citation)

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

  ;; base pipe shortcut like RStudio cmd-shift-m
  (defun kjh/then-R-operator ()
    "R - |> operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "|>")
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

  ;; (add-to-list 'auto-mode-alist '("\\.Rmarkdown" . poly-markdown+r-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))
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
         ))
         ;;   :desc "Kill" "k" . 'polymode-kill-chunk
         ;;   :desc "Mark-Extend" "m" . 'polymode-mark-or-extend-chunk)
)


;; Deal with https://github.com/doomemacs/doomemacs/issues/5951#issuecomment-1696629418
(after! tex
  (map! :leader
      (:when (modulep! :editor fold)
       ("C-f" nil))))


;;; zerodark-theme.el --- A dark, medium contrast theme for Emacs

;; Copyright (C) 2015  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: themes
;; URL: https://github.com/NicolasPetton/zerodark-theme
;; Version: 1.0

;; This file is NOT part of GNU Emacs

;;; License:
;;
;; Zerodark (https://github.com/zerodark-theme) is licensed under a
;; Creative Commons Attribution-ShareAlike 4.0 International License.


;;; Commentary:
;;
;; A dark theme inspired from One Dark and Niflheim
;;

;;; Code:

(deftheme zerodark
  "A dark medium contrast theme")

(defgroup zerodark
  nil
  "A dark theme inspired from One Dark and Niflheim.")

(defcustom zerodark-use-paddings-in-mode-line t
  "When non-nil, use top and bottom paddings in the mode-line."
  :type 'boolean
  :group 'zerodark)

(let ((class '((class color) (min-colors 89)))
      (default "#abb2bf")
      (background "#282c34")
      (background-darker "#21252b")
      (background-lighter "#3a3f4b")
      (background-red "#4c3840")
      (background-purple "#48384c")
      (background-blue "#38394c")
      (background-green "#3d4a41")
      (background-orange "#4a473d")
      (hl-line "#2c323b")
      (grey "#cccccc")
      (grey-dark "#666666")
      (highlight "#3e4451")
      (comment "#687080")
      (orange "#da8548")
      (orange-light "#ddbd78")
      (red "#ff6c6b")
      (purple "#c678dd")
      (purple-dark "#64446d")
      (blue "#61afef")
      (blue-dark "#1f5582")
      (green "#98be65")
      (green-light "#9eac8c")
      (peach "PeachPuff3")
      (diff-added-background "#284437")
      (diff-added-refined-background "#198754")
      (diff-removed-background "#553333")
      (diff-removed-refined-background "#981b1b")
      (diff-current-background "#3e4d58")
      (diff-current-refined-background "#456981"))
  (custom-theme-set-faces
   'zerodark
   `(default ((,class (:background ,background :foreground ,default))))
   `(cursor ((,class (:background ,default))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,background))))
   `(highlight ((,class (:background ,highlight :foreground ,default :underline nil))))
   `(region ((,class (:background ,highlight :foreground ,default))))
   `(secondary-selection ((,class (:background ,highlight :foreground ,default))))
   `(isearch ((,class (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((,class (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((,class (:background ,hl-line :underline unspecified :inherit nil))))

   `(match ((,class (:background ,background-green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,orange :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,green-light))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold :background ,background-red))))

   ;; Mode line faces
   `(mode-line ((,class (:background ,background-blue :height 0.9 :foreground ,blue
                                     :box ,(when zerodark-use-paddings-in-mode-line
                                              (list :line-width 4 :color background-blue))))))
   `(mode-line-inactive ((,class (:background ,background-darker :height 0.9 :foreground ,default
                                              :box ,(when zerodark-use-paddings-in-mode-line
                                                       (list :line-width 4 :color background-darker))))))
   `(header-line ((,class (:inherit mode-line-inactive))))

   ;; powerline
   `(powerline-active1 ((,class (:height 0.9 :foreground ,blue :background ,background-darker))))
   `(powerline-active2 ((,class (:height 0.9 :foreground ,blue :background ,background-lighter))))

   ;; mml
   `(message-mml-face ((,class (:foreground ,comment))))

   ;; Org-clock mode line
   `(org-mode-line-clock ((,class (:background unspecified (:inherit mode-line)))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))
   `(escape-glyph ((,class (:foreground ,blue :weight bold))))

   ;; linum
   `(linum ((,class (:foreground ,comment :background ,background))))
   ;; from hlinum
   `(linum-highlight-face ((,class (:foreground ,blue ,background ,background-blue))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,orange :background ,background :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,purple :background ,background :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,orange :background ,background :weight normal))))
   `(eshell-ls-executable ((,class (:foreground ,green :background ,background :weight bold))))

   ;; whitespace
   `(whitespace-space ((,class (:background unspecified :foreground ,highlight
                                            :inverse-video unspecified))))
   `(whitespace-hspace ((,class (:background unspecified :foreground ,highlight
                                             :inverse-video unspecified))))
   `(whitespace-tab ((,class (:background unspecified :foreground ,highlight
                                          :inverse-video unspecified))))
   `(whitespace-newline ((,class (:background unspecified :foreground ,highlight
                                              :inverse-video unspecified))))
   `(whitespace-trailing ((,class (:background ,red :foreground ,background :weight bold
                                               :inverse-video nil))))
   `(whitespace-line ((,class (:background unspecified :foreground ,red
                                           :inverse-video unspecified))))
   `(whitespace-space-before-tab ((,class (:inherit whitespace-space))))
   `(whitespace-space-after-tab ((,class (:inherit whitespace-space))))
   `(whitespace-indentation ((,class (:background unspecified :foreground ,highlight
                                                  :inverse-video unspecified))))
   `(whitespace-empty ((,class (:background ,orange :foreground ,highlight
                                            :inverse-video unspecified))))

   ;; link faces
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,blue :underline t))))

   ;; widget faces
   `(widget-field ((,class (:background ,highlight))))

   ;; compilation
   `(compilation-info ((,class (:foreground ,purple :weight bold))))
   `(compilation-warning ((,class (:foreground ,orange :weight bold))))
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-line-number ((,class (:foreground ,green :weight bold))))
   `(compilation-mode-line-exit ((,class (:foreground ,green :weight bold :inverse-video nil))))
   `(compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))
   `(compilation-mode-line-error ((,class (:foreground ,red :weight bold))))

   ;; dired
   `(dired-header ((,class (:foreground ,blue :weight bold))))
   `(dired-directory ((,class (:foreground ,purple :weight normal))))

   ;; magit
   `(magit-diff-context-highlight ((,class (:background ,background-darker))))
   `(magit-diff-file-heading ((,class (:weight bold :foreground ,blue))))
   `(magit-diff-file-heading-highlight ((,class (:weight bold :foreground ,blue :background ,background-blue))))
   `(magit-diff-removed-highlight ((,class (:background ,diff-removed-background))))
   `(magit-diff-removed ((,class (:background ,diff-removed-background))))
   `(magit-diff-added-highlight ((,class (:background ,diff-added-background))))
   `(magit-diff-added ((,class (:background ,diff-added-background))))
   `(magit-diff-lines-heading ((,class (:background ,blue-dark :foreground "white"))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,blue-dark))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))

   `(magit-section-highlight ((,class (:background ,background-darker))))
   `(magit-section-heading ((,class (:foreground ,grey :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :box 1))))
   `(magit-branch-local ((,class (:foreground ,purple :box 1))))
   `(magit-branch-remote ((,class (:foreground ,green :box 1))))

   `(magit-reflog-reset ((,class (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((,class (:background ,background-green :foreground ,green :weight bold))))

   `(magit-bisect-bad ((,class (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((,class (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-blame-heading ((,class (:foreground ,green :background ,background-green :box 1))))

   `(git-commit-summary ((,class (:weight bold))))

   `(magit-tag ((,class (:foreground ,purple :weight bold :box 1 :background "#202020"))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue :weight bold))))
   `(message-header-cc ((,class (:foreground ,purple))))
   `(message-header-other ((,class (:foreground ,purple))))
   `(message-header-subject ((,class (:foreground ,green))))
   `(message-header-to ((,class (:foreground ,purple))))
   `(message-cited-text ((,class (:foreground ,comment))))
   `(message-separator ((,class (:foreground ,red :weight bold))))

   ;; ido faces
   `(ido-first-match ((,class (:foreground ,purple :weight bold))))
   `(ido-only-match ((,class (:foreground ,purple :weight bold))))
   `(ido-subdir ((,class (:foreground ,blue))))

   ;; notmuch
   `(notmuch-message-summary-face ((,class (:background ,highlight :box (:line-width 2 :color ,background)))))
   `(notmuch-crypto-part-header ((,class (:foreground ,blue))))
   `(notmuch-crypto-decryption ((,class (:foreground ,purple))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,red))))
   `(notmuch-crypto-signature-good ((,class (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-good-key ((,class (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-bad ((,class (:background ,red :foreground ,background :weight bold))))

   ;; company
   `(company-preview ((,class (:background ,background-darker :foreground ,default))))
   `(company-preview-common ((,class (:background ,background-darker :foreground ,purple))))
   `(company-preview-search ((,class (:background ,blue :foreground ,default))))
   `(company-tooltip ((,class (:background ,background-darker :foreground ,default))))
   `(company-scrollbar-bg ((,class (:background ,background-darker))))
   `(company-scrollbar-fg ((,class (:background ,default))))
   `(company-tooltip-common ((,class (:foreground ,purple :weight bold :background ,background-darker))))
   `(company-tooltip-annotation ((,class (:foreground ,blue :weight bold :background ,background-blue))))
   `(company-tooltip-common-selection ((,class (:foreground ,purple :background ,background-lighter :weight bold))))
   `(company-tooltip-selection ((,class (:foreground ,default :background ,background-lighter))))
   `(company-tooltip-mouse ((,class (:foreground ,default :background ,background-lighter))))

   ;; web-mode
   `(web-mode-html-tag-face ((,class (:foreground ,purple :weight bold))))

   ;; js2-mode
   `(js2-function-param ((,class (:foreground ,blue))))

   ;; flycheck
   `(flycheck-fringe-error ((,class (:foreground ,red :background ,background-red :weight bold :inverse-video t))))
   `(flycheck-fringe-warning ((,class (:background ,background-orange :foreground ,orange :weight bold :inverse-video t))))
   `(flycheck-fringe-info ((,class (:background ,background-blue :foreground ,blue :weight bold :inverse-video t))))
   `(flycheck-warning ((,class (:underline (:color ,red :style wave)))))
   `(flycheck-error ((,class (:underline (:color ,red :style wave)))))

   ;; FIC
   `(font-lock-fic-face ((,class (:foreground ,background :background ,red :weight bold))))

   ;; org-mode todo
   `(org-hide ((,class (:foreground ,background))))
   `(org-todo ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(org-done ((,class (:foreground ,blue :background ,background-blue :weight bold))))
   `(org-date ((,class (:background ,background-lighter))))
   `(org-scheduled-previously ((,class (:foreground ,red))))
   `(org-scheduled ((,class (:foreground ,default))))
   `(org-headline-done ((,class (:foreground ,comment))))
   `(outline-1 ((,class (:foreground ,blue :weight bold))))
   `(outline-2 ((,class (:foreground ,purple :weight bold))))
   `(outline-3 ((,class (:weight bold))))
   `(outline-4 ((,class (:weight bold))))
   `(outline-5 ((,class (:weight bold))))
   `(outline-6 ((,class (:weight bold))))
   `(outline-7 ((,class (:weight bold))))
   `(outline-8 ((,class (:weight bold))))
   `(org-column-title ((,class (:foreground unspecified :background unspecified))))
   `(org-agenda-date ((,class (:foreground ,purple :weight bold))))
   `(org-agenda-date-today ((,class (:foreground ,blue :weight bold :background ,background-blue :box 1))))
   `(org-agenda-structure ((,class (:foreground ,blue :weight bold))))
   `(org-scheduled-today ((,class (:foreground ,default :weight bold))))
   `(org-agenda-done ((,class (:foreground ,comment))))
   `(org-time-grid ((,class (:foreground ,comment))))

   ;; org blocks
   `(org-block-begin-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))
   `(org-block-end-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))

   ;; Gnus faces -- from wombat, feel free to improve :)
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))

   ;; which-function
   `(which-func ((,class (:foreground ,purple))))

   ;; diff
   `(diff-removed ((,class (:foreground ,default :background ,diff-removed-background))))
   `(diff-added ((,class (:foreground ,default :background ,diff-added-background))))
   `(diff-hunk-header ((,class (:background ,highlight :weight bold :foreground ,default))))
   `(diff-file-header ((,class (:background ,highlight :weight bold :foreground ,default))))
   `(diff-refine-added ((,class (:foreground ,grey :background ,diff-added-refined-background))))
   `(diff-refine-removed ((,class (:background ,diff-removed-refined-background :foreground ,grey))))

   ;; ediff
   `(ediff-fine-diff-B ((,class (:foreground ,grey :background ,diff-added-refined-background))))
   `(ediff-current-diff-B ((,class (:background ,diff-added-background))))
   `(ediff-fine-diff-A ((,class (:background ,diff-removed-refined-background :foreground ,grey))))
   `(ediff-current-diff-C ((,class (:background ,diff-current-background))))
   `(ediff-fine-diff-C ((,class (:foreground ,grey :background ,diff-current-refined-background))))

   `(ediff-even-diff-A ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-B ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-C ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-A ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-B ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-C ((,class (:background ,highlight :foreground unspecified))))

   ;; ivy
   `(ivy-current-match ((,class (:background ,background-purple :weight bold :foreground ,purple))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,orange))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,green))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,orange))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,green))))
   `(ivy-match-required-face ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(ivy-remote ((,class (:foreground ,blue))))

   ;; helm
   `(helm-candidate-number ((,class (:weight bold))))
   `(helm-header-line-left-margin ((,class (:weight bold :foreground ,red))))
   `(helm-source-header ((,class (:height 1.2 :weight bold :foreground ,blue :background ,background-blue))))
   `(helm-selection ((,class (:background ,background-lighter))))
   `(helm-match ((,class (:foreground ,purple :background ,background-purple :weight bold))))
   `(helm-match-item ((,class (:inherit isearch))))
   `(helm-M-x-key ((,class (:foreground ,blue :weight bold :background ,background-blue))))
   `(helm-visible-mark ((,class (:weight bold :foreground ,orange :background ,background-darker))))
   `(helm-prefarg ((,class (:weight bold :foreground ,red :background ,background-red))))
   `(helm-separator ((,class (:weight bold :foreground , blue))))

   `(helm-grep-file ((,class ())))
   `(helm-grep-finish ((,class (:foreground ,green))))
   `(helm-grep-running ((,class (:foreground ,red))))
   `(helm-grep-lineno ((,class (:foreground ,blue))))
   `(helm-grep-match ((,class (:foreground ,purple :background ,background-purple :weight bold))))

   `(helm-moccur-buffer ((,class ())))

   `(helm-buffer-directory ((,class (:foreground ,purple))))
   `(helm-buffer-file ((,class ())))
   `(helm-buffer-process ((,class (:foreground ,purple))))
   `(helm-buffer-size ((,class (:foreground ,blue))))
   `(helm-buffer-saved-out ((,class (:foreground ,red :weight bold))))

   `(helm-ff-directory ((,class (:foreground ,purple))))
   `(helm-ff-dotted-directory ((,class (:foreground ,purple))))
   `(helm-ff-prefix ((,class (:weight bold :foreground ,red))))
   `(helm-ff-file ((,class ())))
   `(helm-ff-executable ((,class (:foreground ,green :weight bold :background ,background-green))))
   `(helm-ff-symlink ((,class (:foreground ,orange))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,red :weight bold :background ,background-red))))
   `(helm-history-deleted ((,class (:foreground ,red :weight bold :background ,background-red))))

   ;; visible mark
   `(visible-mark-face1 ((,class (:foreground ,orange-light :inverse-video t))))
   `(visible-mark-face2 ((,class (:foreground ,peach :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((,class (:background ,blue-dark))))

   ;; clojure
   `(clojure-keyword-face ((,class (:inherit font-lock-builtin-face))))

   ;; ledger
   `(ledger-font-report-clickable-face ((,class (:foreground ,blue))))
   `(ledger-font-posting-amount-face ((,class (:foreground ,purple))))
   `(ledger-font-posting-date-face ((,class (:foreground ,red :background))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,red :weight bold))))
   `(ledger-font-posting-account-face ((,class (:foreground ,blue))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,red))))
   `(ledger-font-xact-highlight-face ((,class (:background ,background-darker))))
   `(ledger-font-other-face ((,class (:inherit ,font-lock-comment-face))))
   `(ledger-font-periodic-xact-face ((,class (:foreground ,green))))

   `(diff-hl-change ((,class (:foreground ,purple :background ,background-purple))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,background-red))))
   `(diff-hl-insert ((,class (:foreground ,green :background ,background-green))))

   `(term-color-black ((,class (:foreground ,default :background ,background-darker))))
   `(term-color-red ((,class (:foreground ,red :background ,background-red))))
   `(term-color-green ((,class (:foreground ,green :background ,background-green))))
   `(term-color-yellow ((,class (:foreground ,orange :background ,background-orange))))
   `(term-color-blue ((,class (:foreground ,blue :background ,background-blue))))
   `(term-color-magenta ((,class (:foreground ,purple :background ,background-purple))))
   `(term-color-cyan ((,class (:foreground ,blue-dark))))
   `(term-color-white ((,class (:foreground ,grey))))
   `(term ((,class (:foreground ,default :background ,background))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   `(avy-lead-face ((,class :foreground ,red :background ,background-red)))
   `(avy-lead-face-0 ((,class :foreground ,purple :background ,background-purple)))
   `(avy-lead-face-1 ((,class :foreground ,blue :background ,background-blue)))
   `(avy-lead-face-2 ((,class :foreground ,green :background ,background-green)))
   )

  (custom-theme-set-variables
   'zerodark
   `(ansi-color-names-vector [,background
                              ,red
                              ,green
                              ,orange
                              ,blue
                              ,purple
                              ,blue-dark
                              ,default])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zerodark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zerodark-theme.el ends here

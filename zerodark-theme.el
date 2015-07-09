;;; zerodark-theme.el --- A dark, medium contrast theme for Emacs

;; Copyright (C) 2015  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: themes
;; URL: https://github.com/NicolasPetton/zerodark-theme
;; Version: 0.1

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

(let ((class '((class color) (min-colors 89)))
      (default "#abb2bf")
      (background "#282c34")
      (background-darker "#21252b")
      (background-lighter "#3a3f4b")
      (background-red "#4c3840")
      (background-blue "#38394c")
      (background-green "#3d4a41")
      (background-orange "#4a473d")
      (hl-line "#2c323b")
      (grey "#cccccc")
      (grey-dark "#666666")
      (highlight "#3e4451")
      (comment "#5c6370")
      (orange "#da8548")
      (red "#ff6c6b")
      (purple "#c678dd")
      (blue "#61afef")
      (green "#98be65")
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
   `(isearch ((,class (:background ,orange :foreground ,highlight))))
   `(lazy-highlight ((,class (:background ,peach :foreground ,highlight))))
   `(hl-line ((,class (:background ,hl-line :underline unspecified :inherit nil))))

   `(match ((,class (:background ,background-green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,orange :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold :background ,background-red))))

   ;; Mode line faces
   `(mode-line ((,class (:background ,background-blue :height 0.9 :foreground ,blue :box (:line-width 4 :color ,background-blue)))))
   `(mode-line-inactive ((,class (:background ,background-darker :height 0.9 :foreground ,default :box (:line-width 4 :color ,background-darker)))))
   `(header-line ((,class (:inherit mode-line-inactive))))

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
   `(whitespace-line ((,class (:background unspecified :foreground unspecified
                                           :inverse-video unspecified))))
   `(whitespace-space-before-tab ((,class (:inherit whitespace-space))))
   `(whitespace-space-after-tab ((,class (:inherit whitespace-space))))
   `(whitespace-indentation ((,class (:background unspecified :foreground ,highlight
                                                  :inverse-video unspecified))))
   `(whitespace-empty ((,class (:background unspecified :foreground ,highlight
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
   `(magit-diff-file-heading ((,class (:weight normal))))
   `(magit-diff-removed-highlight ((,class (:background ,diff-removed-background))))
   `(magit-diff-removed ((,class (:background ,diff-removed-background))))
   `(magit-diff-added-highlight ((,class (:background ,diff-added-background))))
   `(magit-diff-added ((,class (:background ,diff-added-background))))

   `(magit-section-highlight ((,class (:background ,background-darker))))
   `(magit-section-heading ((,class (:foreground ,grey :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :box 1))))
   `(magit-branch-local ((,class (:foreground ,purple :box 1))))
   `(magit-branch-remote ((,class (:foreground ,green :box 1))))

   `(magit-reflog-reset ((,class (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((,class (:background ,background-green :foreground ,green :weight bold))))

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

   ;; org-mode todo WORK IN PROGRESS
   `(org-hide ((,class (:foreground ,background))))
   `(org-todo ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(org-done ((,class (:foreground ,blue :background ,background-blue :weight bold))))
   `(org-date ((,class (:background ,background-lighter))))
   `(org-scheduled-previously ((,class (:foreground ,red))))
   `(org-scheduled ((,class (:foreground ,orange))))
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
   `(org-agenda-structure ((,class (:foreground ,orange :weight bold))))
   `(org-scheduled-today ((,class (:foreground ,default :weight bold))))
   `(org-agenda-done ((,class (:foreground ,comment))))

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
   ))




;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zerodark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zerodark-theme.el ends here

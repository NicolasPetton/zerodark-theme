;;; zerodark-theme.el --- A dark, medium contrast theme for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: themes
;; URL: https://github.com/NicolasPetton/zerodark-theme
;; Version: 3.0
;; Package: zerodark-theme
;; Package-Requires: ((s "1.9.0") (all-the-icons "2.0.0") (powerline "2.4") (magit "2.8.0"))

;; This file is NOT part of GNU Emacs

;;; License:
;;
;; Zerodark (https://github.com/zerodark-theme) is licensed under a
;; Creative Commons Attribution-ShareAlike 4.0 International License.


;;; Commentary:
;;
;; A dark theme inspired from One Dark and Niflheim.
;;
;; An optional mode-line format can be enabled with
;; `zerodark-setup-modeline-format'.
;;

;;; Code:

(require 's)
(require 'all-the-icons)
(require 'powerline)

(defmacro cached-for (secs &rest body)
  "Cache for SECS the result of the evaluation of BODY."
  (declare (debug t))
  (let ((cache (make-symbol "cache"))
        (last-run (make-symbol "last-run")))
    `(let (,cache ,last-run)
       (lambda ()
         (when (or (null ,last-run)
                   (> (- (time-to-seconds (current-time)) ,last-run)
                      ,secs))
           (setf ,cache (progn ,@body))
           (setf ,last-run (time-to-seconds (current-time))))
         ,cache))))

(deftheme zerodark
  "A dark medium contrast theme")

(defgroup zerodark
  nil
  "A dark theme inspired from One Dark and Niflheim.")

(defface zerodark-ro-alt-face
  '((t :background "#0088CC" :weight bold))
  "Face for read-only buffer in the mode-line.")

(defface zerodark-modified-alt-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Face for modified buffers in the mode-line.")

(defface zerodark-not-modified-alt-face
  '((t :foreground "#98be65" :height 0.9))
  "Face for not modified buffers in the mode-line.")

(defface zerodark-vc-alt-face
  '((t :foreground "#61afef"))
  "Face for vc status in the mode-line.")

(defface zerodark-ok-face
  '((t :foreground "#61afef"))
  "Face for ok status in the mode-line.")

(defface zerodark-warning-face
  '((t :foreground "#da8548"))
  "Face for warning status in the mode-line.")

(defface zerodark-error-face
  '((t :foreground "#ff6c6b"))
  "Face for error status in the mode-line.")

(defcustom zerodark-use-high-contrast-in-mode-line t
  "When non-nil, use more contrast for the active mode-line."
  :type 'boolean
  :group 'zerodark)

(defvar zerodark-modeline-position ":%l:%c %p "
  "Mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-position-alt (propertize ":%l:%c %p " 'face 'mode-line-inactive)
  "Alternate mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-buffer-identification '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-modified '(:eval (if (buffer-modified-p (current-buffer))
                                               (all-the-icons-faicon "floppy-o" :height 1 :v-adjust 0)
                                             (all-the-icons-faicon "check" :height 1 :v-adjust 0))))

(defvar zerodark-modeline-modified-alt '(:eval (if (buffer-modified-p (current-buffer))
                                                   (all-the-icons-faicon "floppy-o"
                                                                         :height 0.9
                                                                         :v-adjust 0
                                                                         :face (if (zerodark--active-window-p)
                                                                                   'zerodark-modified-alt-face
                                                                                 'mode-line-inactive))
                                                 (all-the-icons-faicon "check"
                                                                       :height 0.9
                                                                       :v-adjust 0
                                                                       :face (if (zerodark--active-window-p)
                                                                                 'zerodark-not-modified-alt-face
                                                                               'mode-line-inactive)))))

(defvar zerodark-modeline-ro '(:eval (if buffer-read-only (propertize "RO " 'face 'bold) "")))

(defvar zerodark-modeline-ro-alt '(:eval (if buffer-read-only
                                             (if (zerodark--active-window-p)
                                                 (progn
                                                   (propertize "RO " 'face 'zerodark-ro-alt-face))
                                               (propertize "RO " 'face 'bold))
                                           "")))

(defvar zerodark-buffer-coding '(:eval (unless (eq buffer-file-coding-system (default-value 'buffer-file-coding-system))
                                         mode-line-mule-info)))

(defvar zerodark-modeline-vc '(vc-mode ("   "
                                        (:eval (all-the-icons-faicon "code-fork" :height 0.9 :v-adjust 0))
                                        (:eval (s-truncate 25 vc-mode)))))

(defvar zerodark-modeline-vc-alt '(vc-mode ("   "
                                            (:eval (all-the-icons-faicon "code-fork"
                                                                         :height 0.9
                                                                         :v-adjust 0
                                                                         :face (when (zerodark--active-window-p)
                                                                                 (zerodark-git-face))))
                                            (:eval (propertize (s-truncate 25 vc-mode)
                                                               'face (when (zerodark--active-window-p)
                                                                       (zerodark-git-face)))))))

(defun zerodark-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when flycheck-mode
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face 'zerodark-error-face))
                                (propertize "✔ No Issues"
                                            'face 'zerodark-ok-face)))
                   (`running     (propertize "⟲ Running"
                                             'face 'zerodark-warning-face))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face 'zerodark-warning-face))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face 'zerodark-error-face))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face 'zerodark-error-face))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defvar zerodark--git-face-cached (cached-for 1 (zerodark--git-face-intern)))

(defun zerodark--git-face-intern ()
  "Return the face to use based on the current repository status."
  (if (magit-git-success "diff" "--quiet")
      ;; nothing to commit because nothing changed
      (if (zerop (length (magit-git-string
                          "rev-list" (concat "origin/"
                                             (magit-get-current-branch)
                                             ".."
                                             (magit-get-current-branch)))))
          ;; nothing to push as well
          'zerodark-ok-face
        ;; nothing to commit, but some commits must be pushed
        'zerodark-warning-face)
    'zerodark-error-face))

(defun zerodark-git-face ()
  "Return the face to use based on the current repository status.
The result is cached for one second to avoid hiccups."
  (funcall zerodark--git-face-cached))


(let ((class '((class color) (min-colors 89)))
      (default (if (true-color-p) "#abb2bf" "#afafaf"))
      (light (if (true-color-p) "#ccd4e3" "#d7d7d7"))
      (background (if (true-color-p) "#282c34" "#333333"))
      (background-darker (if (true-color-p) "#21252b" "#222222"))
      (mode-line-inactive (if "#1c2129" "#222222"))
      (mode-line-active (if (true-color-p) "#6f337e" "#875f87"))
      (background-lighter (if (true-color-p) "#3a3f4b" "#5f5f5f"))
      (background-red (if (true-color-p) "#4c3840" "#5f5f5f"))
      (background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
      (background-blue (if (true-color-p) "#38394c" "#444444"))
      (background-green (if (true-color-p) "#3d4a41" "#5f5f5f"))
      (background-orange (if (true-color-p) "#4a473d" "#5f5f5f"))
      (hl-line (if (true-color-p) "#2c323b" "#333333"))
      (grey (if (true-color-p) "#cccccc" "#cccccc"))
      (grey-dark (if (true-color-p) "#666666" "#666666"))
      (highlight (if (true-color-p) "#3e4451" "#5f5f5f"))
      (comment (if (true-color-p) "#687080" "#707070"))
      (orange (if (true-color-p) "#da8548" "#d7875f"))
      (orange-light (if (true-color-p) "#ddbd78" "#d7af87"))
      (red (if (true-color-p) "#ff6c6b" "#ff5f5f"))
      (purple (if (true-color-p) "#c678dd" "#d787d7"))
      (purple-dark (if (true-color-p) "#64446d" "#5f5f5f"))
      (blue (if (true-color-p) "#61afef" "#5fafff"))
      (blue-dark (if (true-color-p) "#1f5582" "#005f87"))
      (green (if (true-color-p) "#98be65" "#87af5f"))
      (green-light (if (true-color-p) "#9eac8c" "#afaf87"))
      (peach "PeachPuff3")
      (diff-added-background (if (true-color-p) "#284437" "#284437"))
      (diff-added-refined-background (if (true-color-p) "#198754" "#00875f"))
      (diff-removed-background (if (true-color-p) "#583333" "#580000"))
      (diff-removed-refined-background (if (true-color-p) "#981b1b" "#870000"))
      (diff-current-background (if (true-color-p) "#3e4d58" "#5f;5f5f"))
      (diff-current-refined-background (if (true-color-p) "#456981" "#5f5f87")))
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
   `(mode-line ((,class (:background ,(if zerodark-use-high-contrast-in-mode-line
                                          mode-line-active
                                        background-blue)
                                     :height 0.9
                                     :foreground ,(if zerodark-use-high-contrast-in-mode-line
                                                      light
                                                    blue)))))
   `(mode-line-inactive ((,class (:background ,(if zerodark-use-high-contrast-in-mode-line
                                                   mode-line-inactive
                                                 background-darker)
                                              :height 0.9
                                              :foreground ,(if zerodark-use-high-contrast-in-mode-line
                                                      comment
                                                    default)))))
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
   `(widget-field ((,class (:background ,highlight :box (:line-width 1 :color ,comment)))))
   `(widget-button ((,class (:inherit link))))

   ;; custom
   `(custom-button ((,class (:background ,background-lighter :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((,class (:background ,highlight :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,class (:background ,highlight :box (:line-width 2 :style pressed-button)))))
   `(custom-group-tag ((,class (:foreground ,purple :weight bold :height 1.4))))
   `(custom-variable-tag ((,class (:foreground ,purple :weight bold))))
   `(custom-state ((,class (:foreground ,green))))

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

   `(magit-process-ok ((,class (:foreground ,green :weight bold))))

   `(magit-section-highlight ((,class (:background ,background-darker))))
   `(magit-section-heading ((,class (:foreground ,grey :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :box 1))))
   `(magit-branch-local ((,class (:foreground ,purple :box 1))))
   `(magit-branch-remote ((,class (:foreground ,green :box 1))))

   `(magit-reflog-reset ((,class (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((,class (:background ,background-green :foreground ,green :weight bold))))
   `(magit-reflog-checkout ((,class (:background ,background-orange :foreground ,orange :weight bold))))
   `(magit-reflog-cherry-pick ((,class (:background ,background-purple :foreground ,purple :weight bold))))

   `(magit-bisect-bad ((,class (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((,class (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-blame-heading ((,class (:foreground ,green :background ,background-green :box 1))))

   `(git-commit-summary ((,class (:weight bold))))

   `(magit-tag ((,class (:foreground ,purple :weight bold :box 1 :background "#202020"))))
   `(magit-sequence-part ((,class (:foreground ,orange :weight bold))))
   `(magit-sequence-head ((,class (:foreground ,green :weight bold))))

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
   `(notmuch-search-count ((,class (:foreground ,red :weight bold))))
   `(notmuch-search-matching-authors ((,class (:foreground ,comment))))
   `(notmuch-search-subject ((,class (:foreground ,default))))
   `(notmuch-search-unread-face ((,class (:weight bold))))
   `(notmuch-search-date ((,class (:foreground ,purple))))
   `(notmuch-crypto-part-header ((,class (:foreground ,blue))))
   `(notmuch-crypto-decryption ((,class (:foreground ,purple))))
   `(notmuch-crypto-signature-unknown ((,class (:foreground ,red))))
   `(notmuch-crypto-signature-good ((,class (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-good-key ((,class (:background ,blue :foreground ,background :weight bold))))
   `(notmuch-crypto-signature-bad ((,class (:background ,red :foreground ,background :weight bold))))
   `(notmuch-tag-face ((,class (:foreground ,green :weight bold))))

   ;; company
   `(company-preview ((,class (:background ,background-darker :foreground ,default))))
   `(company-preview-common ((,class (:background ,background-darker :foreground ,purple))))
   `(company-preview-search ((,class (:background ,blue :foreground ,default))))
   `(company-tooltip ((,class (:background ,background-darker :foreground ,default))))
   `(company-scrollbar-bg ((,class (:background ,background-darker))))
   `(company-scrollbar-fg ((,class (:background ,background-blue))))
   `(company-tooltip-common ((,class (:foreground ,purple :weight bold :background ,background-darker))))
   `(company-tooltip-annotation ((,class (:foreground ,blue :weight bold :background ,background-blue))))
   `(company-tooltip-common-selection ((,class (:foreground ,purple :background ,background-lighter :weight bold))))
   `(company-tooltip-selection ((,class (:foreground ,default :background ,background-lighter))))
   `(company-tooltip-mouse ((,class (:foreground ,default :background ,background-lighter))))

   ;; web-mode
   `(web-mode-html-tag-face ((,class (:foreground ,purple :weight bold))))
   `(web-mode-symbol-face ((,class (:foreground ,red :weight bold))))

   ;; js2-mode
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-error ((,class (:foreground ,red))))

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
   `(diff-hunk-header ((,class (:background ,background-blue :weight bold :foreground ,blue))))
   `(diff-file-header ((,class (:weight bold))))
   `(diff-header ((,class (:background ,background :foreground ,blue))))
   `(diff-context ((,class (:foreground ,default))))
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
   `(ledger-font-posting-date-face ((,class (:foreground ,blue :background ,background-blue :box 1))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,default :weight bold))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,green :weight bold))))
   `(ledger-font-posting-account-face ((,class (:foreground ,default))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,red))))
   `(ledger-font-xact-highlight-face ((,class (:background ,background-darker))))
   `(ledger-font-other-face ((,class (:inherit ,font-lock-comment-face))))
   `(ledger-font-periodic-xact-face ((,class (:foreground ,orange))))

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

   `(erc-nick-default-face ((,class :foreground ,blue :background ,background-blue :weight bold)))
   `(erc-current-nick-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(erc-my-nick-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(erc-notice-face ((,class :foreground ,comment)))
   `(erc-input-face ((,class :foreground ,default :weight bold)))
   `(erc-prompt-face ((,class :foreground ,purple :background ,background-purple :weight bold :box 1)))
   `(erc-timestamp-face ((,class :foreground ,purple :weight bold)))

   `(hydra-face-red ((,class :foreground ,red :weight bold)))
   `(hydra-face-blue ((,class :foreground ,blue :weight bold)))

   ;; elfeed
   `(elfeed-search-date-face ((,class (:foreground ,blue))))
   `(elfeed-search-feed-face ((,class (:foreground ,blue))))
   `(elfeed-search-tag-face ((,class (:foreground ,green))))
   `(elfeed-search-title-face ((,class (:foreground ,purple))))

   ;; wgrep
   `(wgrep-face ((,class (:foreground ,orange))))
   `(wgrep-reject-face ((,class (:foreground ,red :weight bold :background ,background-red))))
   `(wgrep-done-face ((,class (:foreground ,blue :weight bold))))

   ;; AucTeX
   `(font-latex-math-face ((,class :foreground ,green-light)))
   `(font-latex-sectioning-5-face ((,class :foreground ,blue)))
   `(font-latex-string-face ((,class :inherit font-lock-string-face)))
   `(font-latex-warning-face ((,class :inherit warning)))

   ;; Anzu

   `(anzu-replace-highlight ((,class :background ,diff-removed-refined-background :strike-through t)))
   `(anzu-replace-to ((,class :background ,diff-added-refined-background)))
   `(anzu-mode-line ((,class :inherit mode-line :weight bold)))

   ;; jabber.el
   `(jabber-roster-user-online ((,class :foreground ,blue :weight bold)))
   `(jabber-roster-user-error ((,class :foreground ,red :background ,background-red :weight bold)))
   `(jabber-rare-time-face ((,class :foreground ,comment)))
   `(jabber-chat-prompt-local ((,class :foreground ,purple :background ,background-purple :weight bold)))
   `(jabber-chat-prompt-foreign ((,class :foreground ,green :background ,background-green :weight bold)))
   `(jabber-activity-personal-face ((,class :foreground ,red :background ,background-red :weight bold)))
   `(jabber-roster-user-away ((,class :foreground ,orange)))
   `(jabber-roster-user-xa ((,class :foreground ,orange)))
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

(defvar zerodark-modeline-bar '(:eval (propertize " "
                                                  'display
                                                  (pl/percent-xpm 30 100 0 0 0 10 nil nil))))

(defvar zerodark-modeline-bar-alt '(:eval (propertize " "
                                                      'display
                                                      (if (zerodark--active-window-p)
                                                          (if buffer-read-only
                                                              (pl/percent-xpm 30 100 0 0 0 6 "#0088CC" "#0088CC")
                                                            (pl/percent-xpm 30 100 0 0 0 6 "#c678dd" "#c678dd"))
                                                        (pl/percent-xpm 30 100 0 0 0 6 nil nil)))))

;;;###autoload
(defun zerodark-setup-modeline-format ()
  "Setup the mode-line format for zerodark."
  (interactive)
  (setq-default mode-line-format
                `("%e"
                  ,zerodark-modeline-bar
                  ,zerodark-modeline-ro
                  ,zerodark-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,zerodark-modeline-modified
                  " "
                  ,zerodark-modeline-buffer-identification
                  ,zerodark-modeline-position
                  ,zerodark-modeline-vc
                  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                  )))

;; So the mode-line can keep track of "the current window"
(defvar zerodark-selected-window nil
  "Selected window.")

(defun zerodark--set-selected-window (&rest _)
  "Set the selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq zerodark-selected-window window))))

(defun zerodark--active-window-p ()
  "Return non-nil if the current window is active."
  (eq (selected-window) zerodark-selected-window))

(add-hook 'window-configuration-change-hook #'zerodark--set-selected-window)
(add-hook 'focus-in-hook #'zerodark--set-selected-window)
(advice-add 'select-window :after #'zerodark--set-selected-window)
(advice-add 'select-frame  :after #'zerodark--set-selected-window)


;;;###autoload
(defun zerodark-setup-modeline-format-alt ()
  "Setup the alternate mode-line format for zerodark."
  (interactive)
  (let ((class '((class color) (min-colors 89)))
        (light (if (true-color-p) "#ccd4e3" "#d7d7d7"))
        (comment (if (true-color-p) "#687080" "#707070"))
        (purple "#c678dd")
        (mode-line (if "#1c2129" "#222222")))
    (custom-theme-set-faces
     'zerodark

     ;; Mode line faces
     `(mode-line ((,class (:background ,mode-line
                                       :height 0.9
                                       :foreground ,light))))
     `(mode-line-inactive ((,class (:background ,mode-line
                                                :height 0.9
                                                :foreground ,comment))))
     `(anzu-mode-line ((,class :inherit mode-line :foreground ,purple :weight bold :inverse-video t)))
     ))

  (setq-default mode-line-format
                `(,zerodark-modeline-bar-alt
                  "%e"
                  ,zerodark-modeline-ro-alt " "
                  ,zerodark-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,zerodark-modeline-modified-alt
                  " "
                  ,zerodark-modeline-buffer-identification
                  ,zerodark-modeline-position-alt
                  ,zerodark-modeline-vc-alt
                  "  "
                  (:eval (zerodark-modeline-flycheck-status))
                  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                  )))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'zerodark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zerodark-theme.el ends here

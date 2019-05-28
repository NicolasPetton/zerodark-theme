;;; zerodark-theme.el --- A dark, medium contrast theme for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: themes
;; URL: https://github.com/NicolasPetton/zerodark-theme
;; Version: 4.3
;; Package: zerodark-theme
;; Package-Requires: ((all-the-icons "2.0.0"))

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

;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; A dark theme inspired from One Dark and Niflheim.
;;
;; An optional mode-line format can be enabled with
;; `zerodark-setup-modeline-format'.
;;

;;; Code:

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
  "A dark theme inspired from One Dark and Niflheim."
  :group 'faces)

(defcustom zerodark-use-paddings-in-mode-line t
  "When non-nil, use top and bottom paddings in the mode-line."
  :type 'boolean)

(defcustom zerodark-theme-display-vc-status 'full
  "Control how version control information is displayed."
  :type '(choice (const :tag "Display fork symbol and branch name" 'full)
                 (const :tag "Display fork symbol only" t)
                 (const :tag "Do not display any version control information" nil)))

(defface zerodark-ro-face
  '((t :foreground "#0088CC" :weight bold))
  "Face for read-only buffer in the mode-line.")

(defface zerodark-modified-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Face for modified buffers in the mode-line.")

(defface zerodark-not-modified-face
  '((t :foreground "#98be65" :height 0.9))
  "Face for not modified buffers in the mode-line.")

(defface zerodark-buffer-position-face
  '((t :height 0.9))
  "Face for line/column numbers in the mode-line.")

(defface zerodark-vc-face
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

(defvar zerodark-modeline-position '(:eval (propertize ":%l:%c %p " 'face (if (zerodark--active-window-p)
                                                                              'zerodark-buffer-position-face
                                                                            'mode-line-inactive)))
  "Mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-buffer-identification '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-modified '(:eval (if (buffer-modified-p (current-buffer))
                                               (all-the-icons-faicon "floppy-o"
                                                                     :height 0.9
                                                                     :v-adjust 0
                                                                     :face (if (zerodark--active-window-p)
                                                                               'zerodark-modified-face
                                                                             'mode-line-inactive))
                                             (all-the-icons-faicon "check"
                                                                   :height 0.9
                                                                   :v-adjust 0
                                                                   :face (if (zerodark--active-window-p)
                                                                             'zerodark-not-modified-face
                                                                           'mode-line-inactive)))))

(defvar zerodark-modeline-ro '(:eval (if buffer-read-only
                                         (if (zerodark--active-window-p)
                                             (progn
                                               (propertize "RO " 'face 'zerodark-ro-face))
                                           (propertize "RO " 'face 'bold))
                                       "")))

(defvar zerodark-buffer-coding '(:eval (unless (eq buffer-file-coding-system (default-value 'buffer-file-coding-system))
                                         mode-line-mule-info)))

(defvar zerodark-modeline-vc '(vc-mode ("   "
                                        (:eval (all-the-icons-faicon "code-fork"
                                                                     :height 0.9
                                                                     :v-adjust 0
                                                                     :face (when (zerodark--active-window-p)
                                                                             (zerodark-git-face))))
                                        (:eval (when (eq zerodark-theme-display-vc-status 'full)
                                                 (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
                                                             'face (when (zerodark--active-window-p)
                                                                     (zerodark-git-face))))))))

(defun zerodark-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when flycheck-mode
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (zerodark-face-when-active 'zerodark-error-face)))
                                (propertize "✔ No Issues"
                                            'face (zerodark-face-when-active 'zerodark-ok-face))))
                   (`running     (propertize "⟲ Running"
                                             'face (zerodark-face-when-active 'zerodark-warning-face)))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face (zerodark-face-when-active 'zerodark-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (zerodark-face-when-active 'zerodark-error-face)))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face (zerodark-face-when-active 'zerodark-error-face)))
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
      (background-dark (if (true-color-p) "#24282f" "#222222"))
      (background-darker (if (true-color-p) "#22252c" "#222222"))
      (mode-line-inactive (if "#1c2129" "#222222"))
      (mode-line-active (if (true-color-p) "#6f337e" "#875f87"))
      (background-lighter (if (true-color-p) "#3a3f4b" "#5f5f5f"))
      (background-red (if (true-color-p) "#4c3840" "#5f5f5f"))
      (bright-background-red (if (true-color-p) "#744a5b" "#744a5b"))
      (background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
      (background-blue (if (true-color-p) "#38394c" "#444444"))
      (bright-background-blue (if (true-color-p) "#4e5079" "#4e5079"))
      (background-green (if (true-color-p) "#3d4a41" "#5f5f5f"))
      (bright-background-green (if (true-color-p) "#3f6d54" "#3f6d54"))
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
      (diff-added-refined-background (if (true-color-p) "#1e8967" "#1e8967"))
      (diff-removed-background (if (true-color-p) "#583333" "#580000"))
      (diff-removed-refined-background (if (true-color-p) "#b33c49" "#b33c49"))
      (diff-current-background (if (true-color-p) "#29457b" "#29457b"))
      (diff-current-refined-background (if (true-color-p) "#4174ae" "#4174ae")))
  (custom-theme-set-faces
   'zerodark
   `(default ((,class (:background ,background :foreground ,default))))
   `(cursor ((,class (:background ,default))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,background-dark :foreground ,comment))))
   `(border ((,class (:foreground ,background-lighter))))
   `(vertical-border ((,class (:foreground ,background-lighter))))
   `(highlight ((,class (:background ,highlight :foreground ,default :underline nil))))
   `(region ((,class (:background ,highlight))))
   `(secondary-selection ((,class (:background ,highlight :foreground ,default))))
   `(isearch ((,class (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((,class (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((,class (:background ,hl-line :underline unspecified :inherit nil))))
   `(shadow ((,class (:foreground ,comment))))

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
                                             (list :line-width 6 :color background-blue))))))
   `(mode-line-inactive ((,class (:background ,background-darker :height 0.9 :foreground ,default
                                              :box ,(when zerodark-use-paddings-in-mode-line
                                                      (list :line-width 6 :color background-darker))))))
   `(header-line ((,class (:inherit mode-line-inactive))))

   ;; error & success
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

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

   ;; native line numbers (emacs 26)
   `(line-number ((,class (:foreground ,comment :background ,background-darker))))
   `(line-number-current-line ((,class (:foreground ,blue :background ,background-darker))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,blue :background ,background :weight bold))))
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
   `(compilation-mode-line-fail ((,class (:foreground ,red :weight bold))))

   ;; dired
   `(dired-header ((,class (:foreground ,blue :background ,background-blue :weight bold))))
   `(dired-directory ((,class (:foreground ,purple :weight bold))))

   ;; diff
   `(diff-removed ((,class (:background ,background-red :foreground ,red))))
   `(diff-added ((,class (:background ,background-green :foreground ,green))))
   `(diff-hunk-header ((,class (:background ,background-blue :weight bold :foreground ,blue))))
   `(diff-file-header ((,class (:weight bold))))
   `(diff-header ((,class (:background ,background :foreground ,blue))))
   `(diff-context ((,class (:foreground ,default))))
   `(diff-refine-added ((,class (:foreground ,green :background ,bright-background-green))))
   `(diff-refine-removed ((,class (:background ,bright-background-red :foreground ,red))))

   ;; ediff
   `(ediff-fine-diff-B ((,class (:inherit diff-refine-added))))
   `(ediff-current-diff-B ((,class (:inherit diff-added))))
   `(ediff-fine-diff-A ((,class (:inherit diff-refine-removed))))
   `(ediff-current-diff-A ((,class (:inherit diff-removed))))
   `(ediff-fine-diff-C ((,class (:foreground ,blue :background ,bright-background-blue))))
   `(ediff-current-diff-C ((,class (:background ,background-blue :foreground ,blue))))

   ;; magit
   `(magit-diff-context-highlight ((,class (:background ,background-darker))))
   `(magit-diff-file-heading ((,class (:weight bold :foreground ,blue))))
   `(magit-diff-file-heading-highlight ((,class (:weight bold :foreground ,blue :background ,background-blue))))
   `(magit-diff-removed-highlight ((,class (:inherit diff-removed))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-added-highlight ((,class (:inherit diff-added))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-lines-heading ((,class (:background ,blue-dark :foreground "white"))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,blue-dark))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))

   `(magit-process-ok ((,class (:foreground ,green :weight bold))))

   `(magit-section-highlight ((,class (:background ,background-darker))))
   `(magit-section-heading ((,class (:foreground ,grey :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :background ,background-darker :box 1))))
   `(magit-branch-local ((,class (:foreground ,purple :background ,background-darker :box 1))))
   `(magit-branch-remote ((,class (:foreground ,green :background ,background-darker :box 1))))

   `(magit-reflog-reset ((,class (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((,class (:background ,background-green :foreground ,green :weight bold))))
   `(magit-reflog-checkout ((,class (:background ,background-orange :foreground ,orange :weight bold))))
   `(magit-reflog-cherry-pick ((,class (:background ,background-purple :foreground ,purple :weight bold))))

   `(magit-refname-pullreq ((,class (:background , background-orange :foreground ,orange :weight bold))))

   `(magit-bisect-bad ((,class (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((,class (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-signature-bad ((,class (:foreground ,red))))
   `(magit-signature-good ((,class (:foreground ,blue))))

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
   `(notmuch-tree-match-author-face ((,class (:foreground ,purple))))
   `(notmuch-tree-match-tag-face ((,class (:foreground ,green :weight bold))))

   ;; mu4e
   `(mu4e-highlight-face ((,class (:foreground ,purple :background ,background :weight bold))))
   `(mu4e-header-value-face ((,class (:foreground ,purple))))
   `(mu4e-contact-face ((,class (:foreground ,purple))))
   `(mu4e-special-header-value-face ((,class (:foreground ,green :weight bold))))

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
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,peach))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,peach))))

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
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-headline-done ((,class (:foreground ,comment))))
   `(outline-1 ((,class (:foreground ,blue :weight bold))))
   `(outline-2 ((,class (:foreground ,purple :weight bold))))
   `(outline-3 ((,class (:foreground ,peach :weight bold))))
   `(outline-4 ((,class (:foreground ,green-light :weight bold))))
   `(outline-5 ((,class (:foreground ,blue :weight bold))))
   `(outline-6 ((,class (:foreground ,purple :weight bold))))
   `(outline-7 ((,class (:foreground ,peach :weight bold))))
   `(outline-8 ((,class (:foreground ,green-light :weight bold))))
   `(org-column-title ((,class (:foreground unspecified :background unspecified))))
   `(org-agenda-date ((,class (:foreground ,purple :weight bold))))
   `(org-agenda-date-today ((,class (:foreground ,blue :weight bold :background ,background-blue :box 1))))
   `(org-agenda-structure ((,class (:foreground ,blue :weight bold))))
   `(org-scheduled-today ((,class (:foreground ,default :weight bold))))
   `(org-agenda-done ((,class (:foreground ,comment))))
   `(org-time-grid ((,class (:foreground ,comment))))

   ;; org columns
   `(org-column ((,class (:background ,background-darker))))
   `(org-column-title ((,class (:background ,background-blue :foreground ,blue :weight bold))))

   ;; org blocks
   `(org-block-begin-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))
   `(org-block-end-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))

   ;; org-drill
   `(org-drill-hidden-cloze-face ((,class (:background ,red :foreground ,background))))
   `(org-drill-visible-cloze-face ((,class (:background ,blue :foreground ,background-blue))))
   `(org-drill-visible-cloze-hint-face ((,class (:background ,green :foreground ,background-green))))

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
   `(gnus-header-content ((,class (:foreground ,purple))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground ,green))))
   `(gnus-header-name ((,class (:foreground ,blue))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))

   ;; which-function
   `(which-func ((,class (:foreground ,purple))))

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
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,green))))
   `(ivy-match-required-face ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(ivy-modified-buffer ((,class (:foreground ,red))))
   `(ivy-remote ((,class (:foreground ,blue))))
   `(ivy-highlight-face ((,class (:foreground ,blue :weight bold))))

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

   `(helm-swoop-target-line-face ((,class (:foreground ,comment :background ,background-lighter))))
   `(helm-swoop-target-line-block-face ((,class (:foreground ,comment :background ,background-lighter :weight bold))))
   `(helm-swoop-target-word-face ((,class (:foreground ,purple :background ,background-purple :weight bold))))
   `(helm-swoop-line-number-face ((,class (:foreground ,comment))))

   ;; visible mark
   `(visible-mark-face1 ((,class (:foreground ,orange-light :inverse-video t))))
   `(visible-mark-face2 ((,class (:foreground ,peach :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((,class (:foreground ,blue :weight bold))))

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

   `(diff-hl-change ((,class (:foreground ,bright-background-blue :background ,bright-background-blue))))
   `(diff-hl-delete ((,class (:foreground ,bright-background-red :background ,bright-background-red))))
   `(diff-hl-insert ((,class (:foreground ,bright-background-green :background ,bright-background-green))))

   `(git-gutter:added ((,class (:foreground ,green :background ,background-green))))
   `(git-gutter:deleted ((,class (:foreground ,red :background ,background-red))))
   `(git-gutter:modified ((,class (:foreground ,purple :background ,background-purple))))
   `(git-gutter:separator ((,class (:background ,background-orange))))
   `(git-gutter:unchanged ((,class (:background ,background-orange))))

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

   `(sh-heredoc ((,class (:foreground ,orange :weight bold))))

   `(avy-lead-face ((,class :foreground ,red :background ,background-red)))
   `(avy-lead-face-0 ((,class :foreground ,purple :background ,background-purple)))
   `(avy-lead-face-1 ((,class :foreground ,blue :background ,background-blue)))
   `(avy-lead-face-2 ((,class :foreground ,green :background ,background-green)))

   `(erc-nick-default-face ((,class :foreground ,blue :background ,background)))
   `(erc-current-nick-face ((,class :foreground ,purple :weight bold)))
   `(erc-my-nick-face ((,class :foreground ,purple :weight bold)))
   `(erc-notice-face ((,class :foreground ,comment)))
   `(erc-input-face ((,class :foreground ,purple :slant italic)))
   `(erc-prompt-face ((,class :foreground ,purple :background ,background :weight bold)))
   `(erc-timestamp-face ((,class :foreground ,purple)))

   ;; slack
   `(slack-message-output-header ((,class :foreground ,blue :background ,background-blue :weight bold)))

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
   `(font-latex-bold-face ((,class :foreground ,green :weight bold)))
   `(font-latex-italic-face ((,class :foreground ,green :slant italic)))
   `(font-latex-warning-face ((,class :inherit warning)))

   ;; Anzu

   `(anzu-replace-highlight ((,class :foreground ,red :background ,background-red :strike-through t)))
   `(anzu-replace-to ((,class :foreground ,green :background ,background-green)))
   `(anzu-match-1 ((,class :foreground ,red :background ,background-red :box t)))
   `(anzu-match-2 ((,class :foreground ,red :background ,background-red :box t)))
   `(anzu-match-3 ((,class :foreground ,red :background ,background-red :box t)))
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

   ;; ace-window
   `(aw-leading-char-face ((,class :foreground ,red :weight bold)))
   `(aw-background-face ((,class :foreground ,comment)))

   ;; paren-face.el
   `(parenthesis ((,class (:foreground ,comment))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-mismatched-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,red :weight bold :background ,background-red)))

   ;; makefile
   `(makefile-space ((,class (:background ,background-blue))))

   ;; epa
   `(epa-validity-high ((,class (:foreground ,green))))
   `(epa-validity-low ((,class (:foreground ,default))))
   `(epa-validity-disabled ((,class (:foreground ,red :weight bold :background ,background-red))))
   `(epa-field-name ((,class (:foreground ,purple :weight bold))))
   `(epa-field-body ((,class (:foreground ,orange))))

   ;; tabbar
   `(tabbar-default ((,class (:inherit variable-pitch :background ,background-darker :foreground ,green-light :height 0.9))))
   `(tabbar-button ((,class (:inherit tabbar-default ))))
   `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
   `(tabbar-highlight ((,class (:underline t))))
   `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,orange :background ,background :weight bold))))
   `(tabbar-separator ((,class (:inherit tabbar-default :background ,background-darker))))
   `(tabbar-unselected ((,class (:inherit tabbar-default :slant italic :weight semi-bold))))

   ;; markup-face
   `(markup-title-0-face ((,class (:foreground ,blue :weight bold :underline t))))
   `(markup-title-1-face ((,class (:foreground ,purple :weight bold :underline t))))
   `(markup-title-2-face ((,class (:foreground ,peach :weight bold :underline t))))
   `(markup-title-3-face ((,class (:foreground ,green-light :weight bold :underline t))))
   `(markup-title-4-face ((,class (:foreground ,blue :weight bold :underline t))))
   `(markup-title-5-face ((,class (:foreground ,purple :weight bold :underline t))))
   `(markup-error-face ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(markup-gen-face ((,class (:foreground ,blue))))
   `(markup-typewriter-face ((,class (:inherit shadow))))
   `(markup-meta-face ((,class (:foreground ,comment))))
   `(markup-meta-hide-face ((,class (:foreground ,comment))))
   `(markup-verbatim-face ((,class (:inherit shadow :background ,background-lighter))))
   `(markup-reference-face ((,class (:inherit link))))
   `(markup-complex-replacement-face ((,class (:background ,background-green))))
   `(markup-secondary-text-face ((,class (:foreground ,comment))))

   ;; Elbank
   `(elbank-progressbar-fill-face ((,class (:background ,diff-added-refined-background :weight bold))))
   `(elbank-progressbar-overflow-face ((,class (:background ,diff-removed-refined-background :weight bold))))
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

(defun zerodark-face-when-active (face)
  "Return FACE if the window is active."
  (when (zerodark--active-window-p)
    face))

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
(defun zerodark-setup-modeline-format ()
  "Setup the mode-line format for zerodark."
  (interactive)
  (require 'flycheck)
  (require 'magit)
  (require 'all-the-icons)
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
                                       :foreground ,light
                                       :box ,(when zerodark-use-paddings-in-mode-line
                                               (list :line-width 6 :color mode-line))))))
     `(mode-line-inactive ((,class (:background ,mode-line
                                                :height 0.9
                                                :foreground ,comment
                                                :box ,(when zerodark-use-paddings-in-mode-line
                                                        (list :line-width 6 :color mode-line))))))
     `(anzu-mode-line ((,class :inherit mode-line :foreground ,purple :weight bold)))
     ))

  (setq-default mode-line-format
                `("%e"
                  " "
                  ,zerodark-modeline-ro " "
                  ,zerodark-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,zerodark-modeline-modified
                  " "
                  ,zerodark-modeline-buffer-identification
                  ,zerodark-modeline-position
                  ,(if zerodark-theme-display-vc-status
                       zerodark-modeline-vc
                     "")
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

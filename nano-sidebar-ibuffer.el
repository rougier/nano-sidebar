;; -*- lexical-binding:t -*-
;; Nicolas .P Rougier emacs configuration - ibuffer configuration
;; ---------------------------------------------------------------------
(require 'nano-sidebar)

(defun ibuffer-advice (format)
  (with-current-buffer "*Ibuffer*"
    (save-excursion
    (let ((inhibit-read-only t))

      ;; Remove header and insert ours
      (goto-char (point-min))
      (search-forward "-\n" nil t)
      (delete-region 1 (point))
      (goto-char (point-min))
      (insert (concat
               (propertize "\n" 'face '(:height 1.2))
               (propertize " "  'display `(raise +0.25))
               (propertize "  Buffers list (ibuffer)"
                           'face 'nano-faded)
               (propertize " "  'display `(raise -0.35))
               "\n"))
      (insert "")

      ;; Transform titles
      (goto-char (point-min))
      (while (re-search-forward "\\[ \\(.*\\) \\]" nil t)
        (let* ((title (match-string 0))
               (property (get-text-property 0 'ibuffer-filter-group-name title)))
          (replace-match "\n")
          (insert (concat
                   (propertize
                    (format "   %s " (substring title 2 -2))
                    'ibuffer-filter-group-name property)
                   (propertize
                    (make-string (- 30 (length title)) ?—)
                    'face 'nano-faded)
                   "\n"))))))))


(setq ibuffer-saved-filter-groups
       '(("home"
 	      ("Configuration" (or (filename . ".emacs.d")
 			                   (filename . "emacs-config")))
 	      ("Org" (or (mode . org-mode)
 		             (filename . "OrgMode")))
          ("Code" (or  (derived-mode . prog-mode)
                       (mode . ess-mode)
                       (mode . compilation-mode)))
          ("Text" (and (derived-mode . text-mode)
                       (not  (starred-name))))
          ("TeX"  (or (derived-mode . tex-mode)
                      (mode . latex-mode)
                      (mode . context-mode)
                      (mode . ams-tex-mode)
                      (mode . bibtex-mode)))
 	      ("Help" (or (name . "\*Help\*")
 		              (name . "\*Apropos\*")
 		              (name . "\*info\*"))))))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-header-line nil)
(setq ibuffer-eliding-string (propertize "…" 'face 'nano-salient))
(setq ibuffer-fontification-alist '((0 t nano-salient)))
(setq ibuffer-formats
      '(("  "  mark " "(name 24 24 :left :elide) "  " modified)
        (mark " " (name 16 -1) " " filename)))

(defun ibuffer-setup ()
  (ibuffer-switch-to-saved-filter-groups "home")
  (ibuffer-auto-mode 1))

(defun nano-sidebar-init-ibuffer (frame sidebar)
  "Default sidebar initialization"

  (select-frame frame)
  (let ((buffer (current-buffer)))
    (ibuffer)
    (switch-to-buffer buffer))
  (select-frame sidebar)
  (switch-to-buffer "*Ibuffer*")
  (set-window-dedicated-p (get-buffer-window "*Ibuffer*") t)
  (hl-line-mode)
  (setq header-line-format nil)
  (setq mode-line-format nil))


(setq nano-sidebar-default-init 'nano-sidebar-init-ibuffer)
(advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-advice)
(add-hook 'ibuffer-mode-hook #'ibuffer-setup)


;;; nano-sidebar.el --- NΛNO sidebar -*- lexical-binding: t -*-

;; GNU Emacs / NΛNO sidebar
;; Copyright (C) 2021 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/nano-sidebar
;; Keywords: convenience
;; Version: 0.1

;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows to have configurable sidebars on a per frame basis.
;;
;; It is possible to use the sidebar without any configuration
;;
;; (require 'nano-sidebar)
;; (nano-sidebar-toggle)
;;
;; In such case, a sidebar with default values is created and the
;; default init function is called.
;;
;; You can also configure sidbars individually by first naming a
;; frame and then provide a specific configuration for that name:
;;
;; (defun nano-sidebar-custom-init (frame sidebar)
;;     (select-frame sidebar)
;;     (do-some-stuff...))
;;
;; (add-to-list 'nano-sidebar-properties
;;   `("custom" 36 dark "black" nano-sidebar-custom-init))
;;
;; (set-frame-parameter nil 'name "custom")
;; (nano-sidebar-toggle)


;;; Code:
(defgroup nano-sidebar nil
  "Sidebar"
  :group 'nano)

(defcustom nano-sidebar-default-width 36
  "Default sidebar width in characters"
  
  :type '(integer)
  :group 'nano-sidebar)

(defcustom nano-sidebar-default-mode 'dark
  "Default sidebar mode (light or dark)"
  
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark"  dark))
  :group 'nano-sidebar)

(defcustom nano-sidebar-default-background "#2E3440"
  "Default sidebar background color"
  
  :type '(string)
  :group 'nano-sidebar)

(defcustom nano-sidebar-default-init 'nano-sidebar-init
  "Default sidebar initialization function."
  
  :type '(sexp)
  :group 'nano-sidebar)

;;(defun nano-sidebar-default-init (parent sidebar))

(defcustom nano-sidebar-properties
  `(("default" 36 dark "#2E3440" nano-sidebar-init))
  
   "Individual properties of sidebars that are associated with
a parent frame based on its name."
   :type '(repeat (list (string   :tag "Parent name")
                        (integer  :tag "Sidebar width")
                        (symbol   :tag "Sidebar mode")
                        (string   :tag "Sidebar background color")
                        (sexp     :tag "Sidebar initialization")))
   :group 'nano-sidebar)


(defun nano-sidebar-get-width (&optional frame)
  "Retrieve the width of the sidebar associated with frame.

The association is done using the name of the frame, if any, and
the 'nano-sidebar-properties variable."
  
  (let* ((name (frame-parameter frame 'name))
         (width nano-sidebar-default-width))
    (dolist (element nano-sidebar-properties)
      (if (string= name (nth 0 element))
          (setq width (nth 1 element))))
    width))


(defun nano-sidebar-get-mode (&optional frame)
  "Retrieve the mode of the sidebar associated with frame.

The association is done using the name of the frame, if any, and
the 'nano-sidebar-properties variable."

  (let* ((name (frame-parameter frame 'name))
         (mode nano-sidebar-default-mode))
    (dolist (element nano-sidebar-properties)
      (if (string= name (nth 0 element))
          (setq mode (nth 2 element))))
    mode))

(defun nano-sidebar-get-background (&optional frame)
  "Retrieve the background color of the sidebar associated with frame.

The association is done using the name of the frame, if any, and
the 'nano-sidebar-properties variable."

  (let* ((name (frame-parameter frame 'name))
         (background nano-sidebar-default-background))
    (dolist (element nano-sidebar-properties)
      (if (string= name (nth 0 element))
          (setq background (nth 3 element))))
    background))

(defun nano-sidebar-get-init (&optional frame)
  "Retrieve the initialization function of the sidebar associated with frame.

The association is done using the name of the frame, if any, and
the 'nano-sidebar-properties variable."

  (let* ((name (frame-parameter frame 'name))
         (init nano-sidebar-default-init))
    (dolist (element nano-sidebar-properties)
      (if (string= name (nth 0 element))
          (setq init (nth 4 element))))
    init))

         
(defun nano-sidebar-init (frame sidebar)
  "Default sidebar initialization"

  (select-frame sidebar)
  (when (not (get-buffer "NΛNO sidebar"))
    (switch-to-buffer "NΛNO sidebar")
    (insert (concat 
             (propertize " "  'display `(raise +0.20))
             "         N Λ N O sidebar"
             (propertize " "  'display `(raise -0.15))
             "\n")))

  (switch-to-buffer "NΛNO sidebar")
  (hl-line-mode)
  (setq header-line-format "")
  (face-remap-add-relative 'header-line
                           `(:box (:line-width 3
                                   :color ,(face-background 'default)
                                   :style nil)))
  (setq mode-line-format nil)
  ;; (set-face-background 'fringe "#f0f0f0")
  )
  
(defun nano-sidebar-create ()
  "Create a new sidebar frame"

  ;; This seems to be needed for some unknown reason
  (let* ((parent (window-frame))
         (sidebar-mode (nano-sidebar-get-mode parent)))
    (setq frame-background-mode sidebar-mode))

  ;; This ensures the sidebar frame surrogate minibuffer will be the
  ;; parent's frame minibuffer. Not sure if we need to save previous
  ;; value.
  (setq default-minibuffer-frame (window-frame))
  
  (let* ((parent (window-frame))
         (width (frame-pixel-width))
         (height (frame-pixel-height))

         (sidebar-mode (nano-sidebar-get-mode parent))
         (sidebar-width (nano-sidebar-get-width parent))
         (sidebar-background (nano-sidebar-get-background parent))
         (sidebar-init (nano-sidebar-get-init parent))
         
         (sidebar (make-frame `((parent-frame . ,parent)
                                (sidebar-p    . t)
                                (no-accept-focus . nil)
                                (min-width  . t)
                                (min-height . t)
                                (border-width . 0)
                                (internal-border-width . 0)
                                (vertical-scroll-bars . nil)
                                (horizontal-scroll-bars . nil)
                                (left-fringe . 0)
                                (right-fringe . 1)
                                (user-position . nil)
                                (user-size . nil)
                                (keep-ratio . (height-only . nil))
                                (menu-bar-lines . 0)
                                (tool-bar-lines . 0)
                                (line-spacing . 0)
                                (desktop-dont-save . t)
                                (unsplittable . t)
                                (background-mode . ,sidebar-mode)
                                (background-color . ,sidebar-background)
                                (no-other-frame . t)
                                (undecorated . nil)
                                (pixelwise . t)
                                (modeline . nil)
                                (visibility . nil)
                                (cursor-type . nil)
                                (minibuffer . nil)))))

    (set-frame-parameter sidebar 'parent parent)
    (set-frame-parameter parent 'sidebar sidebar)

    (if sidebar-init
        (funcall sidebar-init parent sidebar))
    
    (select-frame parent)
    
    sidebar))

(defun nano-sidebar-toggle ()
  (interactive)

  (let* ((selected-frame (window-frame))
         (selected-window (selected-window))
         (sidebar (frame-parameter nil 'sidebar))
         (sidebar-p (frame-parameter nil 'sidebar-p))
         (sidebar (if (frame-live-p sidebar) sidebar))
         (sidebar (if sidebar-p
                      (window-frame)
                    (or sidebar (nano-sidebar-create))))
         (parent (frame-parameter sidebar 'parent-frame))

         (sidebar-mode (nano-sidebar-get-mode parent))
         (sidebar-width (nano-sidebar-get-width parent))
         (sidebar-background (nano-sidebar-get-background parent))
         (sidebar-init (nano-sidebar-get-init parent))

         (visible (frame-parameter sidebar 'visibility))
         (width (frame-pixel-width parent))
         (height (frame-pixel-height parent)))

    (when visible
      (make-frame-invisible sidebar))

    (when (not visible)
      (make-frame-visible sidebar)
      (modify-frame-parameters sidebar `((top    . 0)
                                         (height . 1.0)
                                         (width  . ,sidebar-width)
                                         (left   . ,(* -1 width))))
      (select-frame-set-input-focus selected-frame))

    (setq frame-background-mode
          (frame-parameter parent 'background-mode))))

(provide 'nano-sidebar)
;;; nano-sidebar.el ends here

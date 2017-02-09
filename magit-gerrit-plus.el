;; magit-gerrit-plus.el --- Gerrit in Magit -*- lexical-binding: t -*-

;; Copyright © 2017 Honza Pokorny <me@honza.ca>

;; Author: Honza Pokorny <me@honza.ca>
;; URL: https://github.com/honza/magit-gerrit-plus

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'dash)
(require 'magit)

;;; ---------------------------------------------------------------------------
;;; GERRIT CODE ---------------------------------------------------------------
;;; ---------------------------------------------------------------------------

(setq-default magit-gerrit-ssh-creds nil)

(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun ini-decode (ini_text)
  ;; text -> alist
  (interactive)
  (if (not (stringp ini_text))
      (error "Must be a string"))
  (let ((lines (split-string ini_text "\n"))
        (section)
        (section-list)
        (alist))
    (dolist (l lines)
      ;; skip comments
      (unless (or (string-match "^;" l)
                  (string-match "^[ \t]$" l))
        ;; catch sections
        (if (string-match "^\\[\\(.*\\)\\]$" l)
            (progn
              (if section
                  ;; add as sub-list
                  (setq alist (cons `(,section . ,section-list) alist))
                (setq alist section-list))
              (setq section (match-string 1 l))
              (setq section-list nil)))
	      ;; catch properties
	      (if (string-match "^\\(.+\\)=\\(.+\\)$" l)
            (let ((property (match-string 1 l))
                  (value (match-string 2 l)))
              (progn
                (setq section-list (cons `(,property . ,value) section-list)))))))
    (if section
        ;; add as sub-list
        (setq alist (cons `(,section . ,section-list) alist))
      (setq alist section-list))
    alist))

(defun gerrit-config-file ()
  (let ((gitreview-file (concat (projectile-project-root)
                                ".gitreview")))
    (when (file-exists-p gitreview-file)
      (with-temp-buffer
        (insert-file-contents gitreview-file)
        (buffer-string)))))


(defun gerrit-host ()
  (let ((config (ini-decode (gerrit-config-file))))
    (assoc-recursive config "gerrit" "host")))

(defun gitreview-ssh-credentials ()
  (concat (user-login-name) "@" (gerrit-host)))

(defun gerrit-command (cmd)
  (concat "-x -p 29418 "
          (or magit-gerrit-ssh-creds
              (gitreview-ssh-credentials))
          " gerrit "
          cmd))

(defun gerrit-query (prj &optional status)
  (concat "query"
    " --format=JSON"
    ;; " --all-approvals"
    ;; " --comments"
    ;; " --current-patch-set"
    (concat " project:" prj)
    (concat " status:" (or status "open"))))

(defun gerrit-ssh-cmd (cmd)
  (shell-command-to-string
    (concat "ssh " (gerrit-command cmd))))

(defun format-patch-line (patch)
  (concat
    (alist-get 'subject patch)
    " ("
    (assoc-recursive patch 'owner 'name)
    ")"))

(defun get-reviews ()
  (let* ((raw (gerrit-ssh-cmd (gerrit-query "openstack/tripleo-ui")))
         (lines (split-string raw "\n"))
         (line-count (length lines))
         (review-count (- line-count 1)))

    (mapcar (lambda(patch)
               (format-patch-line (json-read-from-string patch)))
             (butlast lines))))

;;; ---------------------------------------------------------------------------
;;; MAGIT CODE ----------------------------------------------------------------
;;; ---------------------------------------------------------------------------

(defgroup magit-gerrit-plus nil
  "Gerrit-Plus support for Magit."
  :group 'magit-extensions)

(defgroup magit-gerrit-plus-commands nil
  "Options controlling behavior of certain commands."
  :group 'magit-gerrit-plus)


(defcustom magit-gerrit-plus-executable "stg"
  "The name of the Gerrit-Plus executable."
  :group 'magit-gerrit-plus
  :type 'string)

(defcustom magit-gerrit-plus-mode-lighter " Stg"
  "Mode-line lighter for Magit-Gerrit-Plus mode."
  :group 'magit-gerrit-plus
  :type 'string)

(defgroup magit-gerrit-plus-faces nil
  "Faces used by Magit-Gerrit-Plus."
  :group 'magit-gerrit-plus
  :group 'magit-faces)

(magit-define-popup magit-gerrit-plus-popup
  "Popup console for Gerrit-Plus commands."
  'magit-gerrit-plus-commands
  :actions '((?i "Init" magit-gerrit-plus-init)
             (?n "New"  magit-gerrit-plus-new-popup)))

;;;###autoload
(defun magit-gerrit-plus-init ()
  "Initialize Gerrit-Plus support for the current branch."
  (interactive)
  (message "This is init")
  (magit-refresh))

(magit-define-popup magit-gerrit-plus-new-popup
  "Popup console for Gerrit-Plus new."
  'magit-gerrit-plus-commands
  :switches '((?a "Add \"Acked-by:\" line" "--ack"))
  :options  '((?n "Set patch name" ""
                  (lambda (prompt default) (read-from-minibuffer "Patch name: " default))))
  :actions  '((?N  "New"  magit-gerrit-plus-new))
  :default-action #'magit-gerrit-plus-new)

;;;###autoload
(defun magit-gerrit-plus-new (&rest args)
  (interactive)
  (message "hello from new"))

;;; Mode

(defvar magit-gerrit-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "/" 'magit-gerrit-plus-popup)
    map))

(magit-define-popup-action 'magit-dispatch-popup ?/ "Gerrit" 'magit-gerrit-plus-popup)

(defun magit-review-section (section title files)
  (magit-insert-section (section title)
    (magit-insert-heading title)
      (while files
        (let ((file (pop files)))
          (insert file)
          (insert "\n")))
    (insert "\n")))

(defun magit-insert-reviews ()
  (magit-review-section 'gerrit-reviews
                        "Reviews"
                        ;; '("one" "two")
                        (get-reviews)))

;;;###autoload
(define-minor-mode magit-gerrit-plus-mode
  "Gerrit-Plus support for Magit."
  :lighter magit-gerrit-plus-mode-lighter
  :keymap  magit-gerrit-plus-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (if magit-gerrit-plus-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                'magit-insert-reviews t t))
    (remove-hook 'magit-status-sections-hook 'magit-insert-reviews t))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-gerrit-plus-mode)
(define-obsolete-function-alias 'turn-on-magit-gerrit-plus 'magit-gerrit-plus-mode)
(add-hook 'magit-status-mode-hook (lambda ()
                                    (magit-gerrit-plus-mode t)) t)

(provide 'magit-gerrit-plus)
;;; magit-gerrit-plus.el ends here

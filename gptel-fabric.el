;;; gptel-fabric.el --- Use Fabric prompts with gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Alex Olkhovskiy <gtptel-fabric@monoinbox.co.uk>
;; Maintainer: Alex Olkhovskiy <gtptel-fabric@monoinbox.co.uk>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.8.0"))
;; Keywords: convenience, tools, comm, llm, ai
;; URL: https://github.com/lxol/gptel-fabric
;; Homepage: https://github.com/lxol/gptel-fabric

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Fabric prompts (patterns) with gptel for Emacs.
;; It allows you to use the Fabric prompt patterns without the Fabric binary,
;; leveraging gptel for LLM queries.
;;
;; Main features:
;; - Load Fabric patterns from both standard and custom directories
;; - Interactive pattern selection with completion
;; - Easy creation of shortcuts for specific patterns
;; - Support for both system and user prompts from Fabric

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)

(defgroup gptel-fabric nil
  "Integration of Fabric prompts with gptel."
  :group 'gptel
  :prefix "gptel-fabric-")

(defcustom gptel-fabric-config-dir (expand-file-name "~/.config/fabric/")
  "Directory where Fabric configuration is stored."
  :type 'directory
  :group 'gptel-fabric)

(defcustom gptel-fabric-patterns-dir nil
  "Directory for standard Fabric patterns.
If nil, defaults to `gptel-fabric-config-dir'/patterns/"
  :type '(choice (const :tag "Default" nil)
                 directory)
  :group 'gptel-fabric)

(defcustom gptel-fabric-custom-patterns-dir nil
  "Directory for custom Fabric patterns.
If nil, defaults to `gptel-fabric-config-dir'/patterns-custom/"
  :type '(choice (const :tag "Default" nil)
                 directory)
  :group 'gptel-fabric)

(defcustom gptel-fabric-default-pattern nil
  "Default pattern to use when none is specified."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'gptel-fabric)

(defcustom gptel-fabric-history-length 20
  "Number of recently used patterns to remember."
  :type 'integer
  :group 'gptel-fabric)

(defvar gptel-fabric-patterns-cache nil
  "Cache of available Fabric patterns.")

(defvar gptel-fabric-history nil
  "History of recently used patterns.")

(defvar gptel-fabric-current-pattern nil
  "Currently selected Fabric pattern.")

(defun gptel-fabric--get-patterns-dir ()
  "Get the standard patterns directory."
  (or gptel-fabric-patterns-dir
      (expand-file-name "patterns" gptel-fabric-config-dir)))

(defun gptel-fabric--get-custom-patterns-dir ()
  "Get the custom patterns directory."
  (or gptel-fabric-custom-patterns-dir
      (expand-file-name "patterns-custom" gptel-fabric-config-dir)))

(defun gptel-fabric--pattern-exists-p (pattern-name &optional custom)
  "Check if PATTERN-NAME exists.
If CUSTOM is non-nil, check in custom patterns directory."
  (let* ((base-dir (if custom
                       (gptel-fabric--get-custom-patterns-dir)
                     (gptel-fabric--get-patterns-dir)))
         (pattern-dir (expand-file-name pattern-name base-dir)))
    (file-directory-p pattern-dir)))

(defun gptel-fabric--read-pattern-file (pattern-name file &optional custom)
  "Read FILE from PATTERN-NAME directory.
FILE should be either \\='system.md\\=' or \\='user.md\\='.
If CUSTOM is non-nil, read from custom patterns directory."
  (condition-case err
      (let* ((base-dir (if custom
                          (gptel-fabric--get-custom-patterns-dir)
                        (gptel-fabric--get-patterns-dir)))
             (file-path (expand-file-name file
                                         (expand-file-name pattern-name base-dir))))
        (when (file-exists-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (string-trim (buffer-string)))))
    (error
     (message "Error reading pattern file %s: %s" pattern-name (error-message-string err))
     nil)))

(defun gptel-fabric--get-pattern-prompt (pattern-name)
  "Get the complete prompt for PATTERN-NAME.
Returns a plist with :system and :user prompts."
  (let* ((custom (gptel-fabric--pattern-exists-p pattern-name t))
         (system-prompt (gptel-fabric--read-pattern-file pattern-name "system.md" custom))
         (user-prompt (gptel-fabric--read-pattern-file pattern-name "user.md" custom)))
    (when (or system-prompt user-prompt)
      (list :name pattern-name
            :system system-prompt
            :user user-prompt
            :custom custom))))

(defun gptel-fabric--list-patterns ()
  "List all available Fabric patterns."
  (let ((patterns '()))
    ;; Get standard patterns
    (let ((standard-dir (gptel-fabric--get-patterns-dir)))
      (when (file-directory-p standard-dir)
        (dolist (dir (directory-files standard-dir nil "^[^.]"))
          (when (file-directory-p (expand-file-name dir standard-dir))
            (push dir patterns)))))
    ;; Get custom patterns
    (let ((custom-dir (gptel-fabric--get-custom-patterns-dir)))
      (when (file-directory-p custom-dir)
        (dolist (dir (directory-files custom-dir nil "^[^.]"))
          (when (file-directory-p (expand-file-name dir custom-dir))
            (push (concat dir " (custom)") patterns)))))
    (sort patterns #'string<)))

(defun gptel-fabric-refresh-cache ()
  "Refresh the patterns cache."
  (interactive)
  (setq gptel-fabric-patterns-cache (gptel-fabric--list-patterns))
  (message "Fabric patterns cache refreshed: %d patterns found"
           (length gptel-fabric-patterns-cache)))

(defun gptel-fabric--ensure-cache ()
  "Ensure patterns cache is populated."
  (unless gptel-fabric-patterns-cache
    (gptel-fabric-refresh-cache)))

(defun gptel-fabric-select-pattern ()
  "Select a Fabric pattern interactively."
  (interactive)
  (gptel-fabric--ensure-cache)
  (let* ((patterns gptel-fabric-patterns-cache)
         (selection (completing-read
                    "Select Fabric pattern: "
                    patterns
                    nil t nil
                    'gptel-fabric-history
                    (or gptel-fabric-default-pattern
                        (car gptel-fabric-history)))))
    (setq gptel-fabric-current-pattern
          (if (string-suffix-p " (custom)" selection)
              (substring selection 0 -9)
            selection))
    (message "Selected pattern: %s" gptel-fabric-current-pattern)
    gptel-fabric-current-pattern))

(defun gptel-fabric--apply-pattern (pattern-name &optional input-text)
  "Apply PATTERN-NAME to create a gptel query.
If INPUT-TEXT is provided, it will be used as the input for the pattern."
  (let ((pattern-data (gptel-fabric--get-pattern-prompt pattern-name)))
    (unless pattern-data
      (error "Pattern '%s' not found" pattern-name))
    (let* ((system-prompt (plist-get pattern-data :system))
           (final-prompt (cond
                          ((and system-prompt input-text)
                           (concat system-prompt "\n\n" input-text))
                          (system-prompt system-prompt)
                          (input-text input-text)
                          (t ""))))
      final-prompt)))

(defun gptel-fabric-send-with-pattern (pattern-name &optional arg)
  "Send current region or buffer to gptel with PATTERN-NAME.
With prefix ARG, insert response at point instead of in a new buffer."
  (interactive
   (list (or gptel-fabric-current-pattern
             (gptel-fabric-select-pattern))
         current-prefix-arg))
  (let* ((input-text (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-substring-no-properties (point-min) (point-max))))
         (pattern-data (gptel-fabric--get-pattern-prompt pattern-name)))
    (unless pattern-data
      (error "Pattern '%s' not found" pattern-name))
    (let* ((system-prompt (plist-get pattern-data :system))
           (gptel--system-message (or system-prompt gptel--system-message)))
      (if arg
          ;; Insert at point
          (gptel-send input-text)
        ;; Send to new buffer
        (let ((buf (generate-new-buffer (format "*gptel-fabric: %s*" pattern-name))))
          (with-current-buffer buf
            (insert input-text)
            (gptel-mode 1)
            (goto-char (point-max))
            (insert "\n\n")
            (gptel-send))
          (switch-to-buffer buf))))))

(defun gptel-fabric-replace-with-pattern (pattern-name)
  "Replace region or buffer with LLM output using PATTERN-NAME.
If a region is active, replaces the region.
Otherwise, replaces the entire buffer."
  (interactive
   (list (or gptel-fabric-current-pattern
             (gptel-fabric-select-pattern))))
  (unless (stringp pattern-name)
    (error "Invalid pattern name"))
  (when buffer-read-only
    (error "Buffer is read-only"))
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (point-min)))
         (end (if use-region (region-end) (point-max)))
         (input-text (buffer-substring-no-properties start end))
         (pattern-data (gptel-fabric--get-pattern-prompt pattern-name)))
    (unless pattern-data
      (error "Pattern '%s' not found" pattern-name))
    (when (string-empty-p input-text)
      (error "No text to process"))
    (let* ((system-prompt (plist-get pattern-data :system))
           (gptel--system-message (or system-prompt gptel--system-message))
           (original-buffer (current-buffer)))
      ;; Store the boundaries for replacement
      (message "Processing with %s pattern..." pattern-name)
      (gptel-request
       input-text
       :system system-prompt
       :callback
       (lambda (response info)
         (if (and (stringp response) (not (string-empty-p response)))
             (when (buffer-live-p original-buffer)
               (with-current-buffer original-buffer
                 (save-excursion
                   ;; Delete the original content
                   (delete-region start end)
                   ;; Insert the response
                   (goto-char start)
                   (insert response))
                 (message "Replaced %s with %s pattern output"
                          (if use-region "region" "buffer")
                          pattern-name)))
           (message "Error: Failed to get response from LLM: %s"
                    (or (plist-get info :error) "Unknown error"))))))))

(defun gptel-fabric-query-with-pattern (pattern-name query)
  "Send QUERY to gptel using PATTERN-NAME."
  (interactive
   (list (or gptel-fabric-current-pattern
             (gptel-fabric-select-pattern))
         (read-string "Query: ")))
  (let* ((pattern-data (gptel-fabric--get-pattern-prompt pattern-name)))
    (unless pattern-data
      (error "Pattern '%s' not found" pattern-name))
    (let* ((system-prompt (plist-get pattern-data :system))
           (gptel--system-message (or system-prompt gptel--system-message))
           (buf (generate-new-buffer (format "*gptel-fabric: %s*" pattern-name))))
      (with-current-buffer buf
        (insert query)
        (gptel-mode 1)
        (goto-char (point-max))
        (insert "\n\n")
        (gptel-send))
      (switch-to-buffer buf))))

;;;###autoload
(defun gptel-fabric-create-pattern-command (pattern-name)
  "Create an interactive command for PATTERN-NAME.
This can be used to create shortcuts for frequently used patterns."
  (eval
   `(defun ,(intern (format "gptel-fabric-run-%s" pattern-name)) (&optional arg)
      ,(format "Run Fabric pattern '%s' on region or buffer.\nWith prefix ARG, insert at point." pattern-name)
      (interactive "P")
      (gptel-fabric-send-with-pattern ,pattern-name arg))))

;;;###autoload
(defun gptel-fabric-create-replace-command (pattern-name)
  "Create an interactive replace command for PATTERN-NAME.
This creates a command that replaces the region/buffer with the LLM output."
  (eval
   `(defun ,(intern (format "gptel-fabric-replace-%s" pattern-name)) ()
      ,(format "Replace region or buffer using Fabric pattern '%s'." pattern-name)
      (interactive)
      (gptel-fabric-replace-with-pattern ,pattern-name))))

(defun gptel-fabric-proofread ()
  "Proofread and replace the region or buffer.
Uses the proofreader pattern if it exists in custom patterns,
otherwise prompts to select another pattern."
  (interactive)
  (let ((pattern-name (if (gptel-fabric--pattern-exists-p "proofreader" t)
                          "proofreader"
                        (progn
                          (message "Proofreader pattern not found. Select a pattern for proofreading:")
                          (gptel-fabric-select-pattern)))))
    (gptel-fabric-replace-with-pattern pattern-name)))

(defun gptel-fabric-list-patterns ()
  "Display a list of all available Fabric patterns."
  (interactive)
  (gptel-fabric--ensure-cache)
  (let ((buf (get-buffer-create "*Fabric Patterns*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Available Fabric Patterns\n")
        (insert "=========================\n\n")
        (dolist (pattern gptel-fabric-patterns-cache)
          (let* ((clean-name (if (string-suffix-p " (custom)" pattern)
                                 (substring pattern 0 -9)
                               pattern))
                 (is-custom (string-suffix-p " (custom)" pattern))
                 (pattern-data (gptel-fabric--get-pattern-prompt clean-name)))
            (insert (format "• %s%s\n"
                           clean-name
                           (if is-custom " [CUSTOM]" "")))
            (when pattern-data
              (when-let ((system (plist-get pattern-data :system)))
                (insert (format "  System: %s\n"
                               (substring system 0 (min 60 (length system))))))
              (insert "\n")))))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer buf)))

(defun gptel-fabric-describe-pattern (pattern-name)
  "Describe PATTERN-NAME showing its prompts."
  (interactive
   (list (completing-read "Describe pattern: "
                         (or gptel-fabric-patterns-cache
                             (gptel-fabric--list-patterns))
                         nil t)))
  (let* ((clean-name (if (string-suffix-p " (custom)" pattern-name)
                         (substring pattern-name 0 -9)
                       pattern-name))
         (pattern-data (gptel-fabric--get-pattern-prompt clean-name)))
    (if pattern-data
        (let ((buf (get-buffer-create (format "*Fabric Pattern: %s*" clean-name))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Fabric Pattern: %s\n" clean-name))
              (insert (make-string 50 ?=) "\n\n")
              (when (plist-get pattern-data :custom)
                (insert "Type: CUSTOM\n\n"))
              (when-let ((system (plist-get pattern-data :system)))
                (insert "System Prompt:\n")
                (insert (make-string 30 ?-) "\n")
                (insert system "\n\n"))
              (when-let ((user (plist-get pattern-data :user)))
                (insert "User Prompt:\n")
                (insert (make-string 30 ?-) "\n")
                (insert user "\n")))
            (goto-char (point-min))
            (special-mode))
          (switch-to-buffer buf))
      (message "Pattern '%s' not found" clean-name))))

;;;###autoload
(defun gptel-fabric-setup ()
  "Setup gptel-fabric with default keybindings."
  (interactive)
  (gptel-fabric-refresh-cache)
  (message "gptel-fabric setup complete. Found %d patterns."
           (length gptel-fabric-patterns-cache)))

(provide 'gptel-fabric)
;;; gptel-fabric.el ends here

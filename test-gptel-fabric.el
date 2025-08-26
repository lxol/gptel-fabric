;;; test-gptel-fabric.el --- Tests for gptel-fabric -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Alex Olkhovskiy

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

;; Test suite for gptel-fabric.el

;;; Code:

(require 'ert)
(require 'gptel-fabric)

(defvar test-gptel-fabric-temp-dir nil
  "Temporary directory for test patterns.")

(defun test-gptel-fabric-setup ()
  "Set up test environment."
  (setq test-gptel-fabric-temp-dir (make-temp-file "gptel-fabric-test" t))
  ;; Create test pattern structure
  (let ((patterns-dir (expand-file-name "patterns" test-gptel-fabric-temp-dir))
        (custom-dir (expand-file-name "patterns-custom" test-gptel-fabric-temp-dir)))
    (make-directory patterns-dir t)
    (make-directory custom-dir t)
    ;; Create a test pattern
    (let ((test-pattern-dir (expand-file-name "test-pattern" patterns-dir)))
      (make-directory test-pattern-dir t)
      (with-temp-file (expand-file-name "system.md" test-pattern-dir)
        (insert "Test system prompt"))
      (with-temp-file (expand-file-name "user.md" test-pattern-dir)
        (insert "Test user prompt")))
    ;; Create a custom test pattern
    (let ((custom-pattern-dir (expand-file-name "custom-test" custom-dir)))
      (make-directory custom-pattern-dir t)
      (with-temp-file (expand-file-name "system.md" custom-pattern-dir)
        (insert "Custom test system prompt"))))
  (setq gptel-fabric-config-dir test-gptel-fabric-temp-dir)
  (setq gptel-fabric-patterns-cache nil))

(defun test-gptel-fabric-teardown ()
  "Clean up test environment."
  (when (and test-gptel-fabric-temp-dir
             (file-exists-p test-gptel-fabric-temp-dir))
    (delete-directory test-gptel-fabric-temp-dir t))
  (setq test-gptel-fabric-temp-dir nil)
  (setq gptel-fabric-patterns-cache nil))

(ert-deftest test-gptel-fabric-directories ()
  "Test directory configuration."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should (equal (gptel-fabric--get-patterns-dir)
                      (expand-file-name "patterns" test-gptel-fabric-temp-dir)))
        (should (equal (gptel-fabric--get-custom-patterns-dir)
                      (expand-file-name "patterns-custom" test-gptel-fabric-temp-dir))))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-pattern-exists ()
  "Test pattern existence checking."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should (gptel-fabric--pattern-exists-p "test-pattern"))
        (should-not (gptel-fabric--pattern-exists-p "non-existent"))
        (should (gptel-fabric--pattern-exists-p "custom-test" t))
        (should-not (gptel-fabric--pattern-exists-p "custom-test" nil)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-read-pattern-file ()
  "Test reading pattern files."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should (equal (gptel-fabric--read-pattern-file "test-pattern" "system.md")
                      "Test system prompt"))
        (should (equal (gptel-fabric--read-pattern-file "test-pattern" "user.md")
                      "Test user prompt"))
        (should-not (gptel-fabric--read-pattern-file "test-pattern" "nonexistent.md"))
        (should (equal (gptel-fabric--read-pattern-file "custom-test" "system.md" t)
                      "Custom test system prompt")))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-get-pattern-prompt ()
  "Test getting complete pattern prompts."
  (test-gptel-fabric-setup)
  (unwind-protect
      (let ((pattern-data (gptel-fabric--get-pattern-prompt "test-pattern")))
        (should pattern-data)
        (should (equal (plist-get pattern-data :name) "test-pattern"))
        (should (equal (plist-get pattern-data :system) "Test system prompt"))
        (should (equal (plist-get pattern-data :user) "Test user prompt"))
        (should-not (plist-get pattern-data :custom)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-list-patterns ()
  "Test listing patterns."
  (test-gptel-fabric-setup)
  (unwind-protect
      (let ((patterns (gptel-fabric--list-patterns)))
        (should (member "test-pattern" patterns))
        (should (member "custom-test (custom)" patterns))
        (should (= (length patterns) 2)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-refresh-cache ()
  "Test cache refresh."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should-not gptel-fabric-patterns-cache)
        (gptel-fabric-refresh-cache)
        (should gptel-fabric-patterns-cache)
        (should (= (length gptel-fabric-patterns-cache) 2)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-ensure-cache ()
  "Test cache initialization."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should-not gptel-fabric-patterns-cache)
        (gptel-fabric--ensure-cache)
        (should gptel-fabric-patterns-cache))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-apply-pattern ()
  "Test applying pattern to create prompts."
  (test-gptel-fabric-setup)
  (unwind-protect
      (let ((prompt (gptel-fabric--apply-pattern "test-pattern" "Input text")))
        (should (stringp prompt))
        (should (string-match-p "Test system prompt" prompt))
        (should (string-match-p "Input text" prompt)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-error-handling ()
  "Test error handling for invalid patterns."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should-error (gptel-fabric--apply-pattern "non-existent" "text"))
        (should-not (gptel-fabric--get-pattern-prompt "non-existent")))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-create-commands ()
  "Test dynamic command creation."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (gptel-fabric-create-pattern-command "test-pattern")
        (should (fboundp 'gptel-fabric-run-test-pattern))
        (gptel-fabric-create-replace-command "test-pattern")
        (should (fboundp 'gptel-fabric-replace-test-pattern)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-custom-variables ()
  "Test custom variable defaults."
  (should (stringp gptel-fabric-config-dir))
  (should (numberp gptel-fabric-history-length))
  (should (>= gptel-fabric-history-length 0)))

(provide 'test-gptel-fabric)
;;; test-gptel-fabric.el ends here

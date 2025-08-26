;;; test-gptel-fabric-standalone.el --- Standalone tests for gptel-fabric -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Standalone tests that can run without gptel dependency.
;; These test the core pattern loading and management functionality.

;;; Code:

(require 'ert)

;; Mock gptel to allow loading gptel-fabric
(defvar gptel--system-message nil)
(defun gptel-send (&rest _args) (message "Mock gptel-send"))
(defun gptel-request (&rest _args) (message "Mock gptel-request"))
(defun gptel-mode (&rest _args) (message "Mock gptel-mode"))
(provide 'gptel)

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

(ert-deftest test-gptel-fabric-list-patterns ()
  "Test listing patterns."
  (test-gptel-fabric-setup)
  (unwind-protect
      (let ((patterns (gptel-fabric--list-patterns)))
        (should (member "test-pattern" patterns))
        (should (member "custom-test (custom)" patterns))
        (should (= (length patterns) 2)))
    (test-gptel-fabric-teardown)))

(ert-deftest test-gptel-fabric-cache ()
  "Test pattern caching."
  (test-gptel-fabric-setup)
  (unwind-protect
      (progn
        (should-not gptel-fabric-patterns-cache)
        (gptel-fabric-refresh-cache)
        (should gptel-fabric-patterns-cache)
        (should (listp gptel-fabric-patterns-cache)))
    (test-gptel-fabric-teardown)))

(provide 'test-gptel-fabric-standalone)
;;; test-gptel-fabric-standalone.el ends here
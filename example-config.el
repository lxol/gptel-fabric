;;; example-config.el --- Example configuration for gptel-fabric -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides example configurations for gptel-fabric.
;; Copy relevant parts to your Emacs configuration.

;;; Code:

;;;; Basic Setup
(use-package gptel-fabric
  :load-path "/path/to/gptel-fabric"
  :after gptel
  :config
  (gptel-fabric-setup)

  ;; Set default pattern
  (setq gptel-fabric-default-pattern "summarize")

  ;; Custom directories (optional)
  ;; (setq gptel-fabric-config-dir "~/my-fabric/")
  ;; (setq gptel-fabric-custom-patterns-dir "~/my-patterns/")

  ;; History length
  (setq gptel-fabric-history-length 30))

;;;; Keybindings
(with-eval-after-load 'gptel-fabric
  ;; Global keybindings
  (global-set-key (kbd "C-c g f s") 'gptel-fabric-select-pattern)
  (global-set-key (kbd "C-c g f r") 'gptel-fabric-send-with-pattern)
  (global-set-key (kbd "C-c g f e") 'gptel-fabric-replace-with-pattern)
  (global-set-key (kbd "C-c g f p") 'gptel-fabric-proofread)
  (global-set-key (kbd "C-c g f q") 'gptel-fabric-query-with-pattern)
  (global-set-key (kbd "C-c g f l") 'gptel-fabric-list-patterns)
  (global-set-key (kbd "C-c g f d") 'gptel-fabric-describe-pattern)
  (global-set-key (kbd "C-c g f R") 'gptel-fabric-refresh-cache))

;;;; Create Pattern Shortcuts
(with-eval-after-load 'gptel-fabric
  ;; Create commands for frequently used patterns
  (gptel-fabric-create-pattern-command "summarize")
  (gptel-fabric-create-pattern-command "explain_code")
  (gptel-fabric-create-pattern-command "improve_code")
  (gptel-fabric-create-pattern-command "find_vulnerabilities")
  (gptel-fabric-create-pattern-command "create_readme")

  ;; Create replace commands for patterns that modify text
  (gptel-fabric-create-replace-command "improve_writing")
  (gptel-fabric-create-replace-command "fix_grammar")

  ;; If you have custom patterns
  ;; (gptel-fabric-create-replace-command "proofreader")
  ;; (gptel-fabric-create-pattern-command "lxol_youtube_details")

  ;; Bind pattern commands to keys
  (global-set-key (kbd "C-c g p s") 'gptel-fabric-run-summarize)
  (global-set-key (kbd "C-c g p e") 'gptel-fabric-run-explain_code)
  (global-set-key (kbd "C-c g p i") 'gptel-fabric-run-improve_code)
  (global-set-key (kbd "C-c g p v") 'gptel-fabric-run-find_vulnerabilities)
  (global-set-key (kbd "C-c g p r") 'gptel-fabric-run-create_readme)

  ;; Bind replace commands
  (global-set-key (kbd "C-c g r w") 'gptel-fabric-replace-improve_writing)
  (global-set-key (kbd "C-c g r g") 'gptel-fabric-replace-fix_grammar))

;;;; Hydra Configuration (optional, requires hydra package)
(with-eval-after-load 'hydra
  (defhydra hydra-gptel-fabric (:color blue :hint nil)
    "
  gptel-fabric: %(gptel-fabric-current-pattern)

  ^Select^              ^Run^                   ^Info^
  ^^^^^^^^---------------------------------------------------------
  _s_: Select pattern   _r_: Run on region     _l_: List patterns
  _h_: From history     _b_: Run on buffer     _d_: Describe pattern
  ^ ^                   _q_: Query with pattern _R_: Refresh cache
  ^ ^                   _i_: Insert at point

  ^Quick Patterns^
  ^^^^^^^^---------------------------------------------------------
  _1_: summarize        _2_: explain_code      _3_: improve_code
  _4_: find_bugs        _5_: create_readme     _6_: analyze_code
  "
    ("s" gptel-fabric-select-pattern)
    ("h" (gptel-fabric-select-pattern t))
    ("r" gptel-fabric-send-with-pattern)
    ("b" (gptel-fabric-send-with-pattern nil nil))
    ("q" gptel-fabric-query-with-pattern)
    ("i" (gptel-fabric-send-with-pattern nil t))
    ("l" gptel-fabric-list-patterns)
    ("d" gptel-fabric-describe-pattern)
    ("R" gptel-fabric-refresh-cache)
    ;; Quick patterns
    ("1" (gptel-fabric-send-with-pattern "summarize"))
    ("2" (gptel-fabric-send-with-pattern "explain_code"))
    ("3" (gptel-fabric-send-with-pattern "improve_code"))
    ("4" (gptel-fabric-send-with-pattern "find_bugs"))
    ("5" (gptel-fabric-send-with-pattern "create_readme"))
    ("6" (gptel-fabric-send-with-pattern "analyze_code"))
    ("q" nil "quit"))

  (global-set-key (kbd "C-c g F") 'hydra-gptel-fabric/body))

;;;; Mode-specific Configuration
(with-eval-after-load 'gptel-fabric
  ;; Python mode
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-g e")
                            (lambda () (interactive)
                              (gptel-fabric-send-with-pattern "explain_code")))
              (local-set-key (kbd "C-c C-g i")
                            (lambda () (interactive)
                              (gptel-fabric-send-with-pattern "improve_code")))
              (local-set-key (kbd "C-c C-g v")
                            (lambda () (interactive)
                              (gptel-fabric-send-with-pattern "find_vulnerabilities")))))

  ;; JavaScript/TypeScript modes
  (dolist (mode '(js-mode js2-mode typescript-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (local-set-key (kbd "C-c C-g e")
                              (lambda () (interactive)
                                (gptel-fabric-send-with-pattern "explain_code")))
                (local-set-key (kbd "C-c C-g t")
                              (lambda () (interactive)
                                (gptel-fabric-send-with-pattern "write_tests")))))))

;;;; Custom Helper Functions
(defun my/gptel-fabric-summarize-and-insert ()
  "Summarize region or buffer and insert at point."
  (interactive)
  (gptel-fabric-send-with-pattern "summarize" t))

(defun my/gptel-fabric-proofread-replace ()
  "Proofread and replace selected text using custom proofreader pattern."
  (interactive)
  (if (gptel-fabric--pattern-exists-p "proofreader" t)
      (gptel-fabric-replace-with-pattern "proofreader")
    (message "Proofreader pattern not found. Please create it in patterns-custom/")))

(defun my/gptel-fabric-pattern-for-mode ()
  "Select and run a pattern appropriate for current major mode."
  (interactive)
  (let ((pattern (cond
                  ((derived-mode-p 'prog-mode) "explain_code")
                  ((derived-mode-p 'text-mode) "improve_writing")
                  ((derived-mode-p 'org-mode) "create_summary")
                  (t (gptel-fabric-select-pattern)))))
    (gptel-fabric-send-with-pattern pattern)))

;; Bind helper functions
(global-set-key (kbd "C-c g f S") 'my/gptel-fabric-summarize-and-insert)
(global-set-key (kbd "C-c g f P") 'my/gptel-fabric-proofread-replace)
(global-set-key (kbd "C-c g f m") 'my/gptel-fabric-pattern-for-mode)

(provide 'example-config)
;;; example-config.el ends here

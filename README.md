# gptel-fabric

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/gptel-fabric-badge.svg)](https://melpa.org/#/gptel-fabric)
[![MELPA Stable](https://stable.melpa.org/packages/gptel-fabric-badge.svg)](https://stable.melpa.org/#/gptel-fabric)

An Emacs package that integrates [Fabric](https://github.com/danielmiessler/fabric) prompts with [gptel](https://github.com/karthink/gptel) for powerful LLM interactions.

## Features

- Use Fabric patterns (prompts) directly within Emacs
- No Fabric binary required - works entirely through gptel
- Support for both standard and custom Fabric patterns
- Interactive pattern selection with completion
- Create custom shortcuts for frequently used patterns
- Pattern caching for fast access
- History of recently used patterns

## Requirements

- Emacs 27.1 or later
- [gptel](https://github.com/karthink/gptel) 0.8.0 or later
- Fabric patterns installed (typically in `~/.config/fabric/`)

## Installation

### From MELPA

```elisp
(use-package gptel-fabric
  :ensure t
  :after gptel
  :config
  (gptel-fabric-setup))
```

### Prerequisites

1. Install and configure [gptel](https://github.com/karthink/gptel)
2. Have Fabric patterns installed in `~/.config/fabric/` (or configure custom location)

### Manual Installation

```elisp
;; Clone this repository, then add to your init file:
(add-to-list 'load-path "/path/to/gptel-fabric")
(require 'gptel-fabric)

;; Optional: Setup with default configuration
(gptel-fabric-setup)
```

### Using use-package

```elisp
(use-package gptel-fabric
  :load-path "/path/to/gptel-fabric"
  :after gptel
  :config
  (gptel-fabric-setup)
  ;; Optional: Set default pattern
  (setq gptel-fabric-default-pattern "summarize"))
```

## Configuration

### Basic Configuration

```elisp
;; Set Fabric config directory (default: ~/.config/fabric/)
(setq gptel-fabric-config-dir "~/my-fabric-config/")

;; Set custom patterns directory explicitly
(setq gptel-fabric-custom-patterns-dir "~/my-patterns/")

;; Set default pattern
(setq gptel-fabric-default-pattern "summarize")

;; Set history length
(setq gptel-fabric-history-length 30)
```

## Usage

### Interactive Commands

- `gptel-fabric-select-pattern` - Select a pattern interactively
- `gptel-fabric-send-with-pattern` - Send region/buffer with selected pattern
  - Without prefix: Opens result in new buffer
  - With prefix (C-u): Inserts result at point
- `gptel-fabric-replace-with-pattern` - Replace region/buffer with LLM output
  - Replaces region if active, otherwise entire buffer
- `gptel-fabric-proofread` - Quick proofreading with replacement
- `gptel-fabric-query-with-pattern` - Send a query with selected pattern
- `gptel-fabric-list-patterns` - List all available patterns
- `gptel-fabric-describe-pattern` - Show details of a specific pattern
- `gptel-fabric-refresh-cache` - Refresh the patterns cache

### Creating Custom Shortcuts

You can create dedicated commands for frequently used patterns:

```elisp
;; Create a command for the 'summarize' pattern
(gptel-fabric-create-pattern-command "summarize")
;; This creates: gptel-fabric-run-summarize

;; Create a replace command for proofreading
(gptel-fabric-create-replace-command "proofreader")
;; This creates: gptel-fabric-replace-proofreader

;; Bind to keys
(global-set-key (kbd "C-c g s") 'gptel-fabric-run-summarize)
(global-set-key (kbd "C-c g p") 'gptel-fabric-replace-proofreader)

;; Or use the built-in proofreading function
(global-set-key (kbd "C-c g P") 'gptel-fabric-proofread)
```

### Example Workflow

1. **Summarize text:**
   ```elisp
   ;; Select text in buffer
   M-x gptel-fabric-send-with-pattern RET summarize RET
   ```

2. **Proofread and replace text:**
   ```elisp
   ;; Select text or use whole buffer
   M-x gptel-fabric-proofread RET
   ;; Or with any pattern:
   M-x gptel-fabric-replace-with-pattern RET improve_writing RET
   ```

3. **Quick query with pattern:**
   ```elisp
   M-x gptel-fabric-query-with-pattern RET explain_code RET
   ;; Type your code or question
   ```

4. **Using with keybindings:**
   ```elisp
   ;; Add to your config
   (define-key gptel-mode-map (kbd "C-c f") 'gptel-fabric-send-with-pattern)
   (define-key gptel-mode-map (kbd "C-c F") 'gptel-fabric-select-pattern)
   (define-key gptel-mode-map (kbd "C-c r") 'gptel-fabric-replace-with-pattern)
   ```

## Pattern Structure

Fabric patterns are expected to have the following structure:
```
~/.config/fabric/
├── patterns/
│   ├── summarize/
│   │   ├── system.md
│   │   └── user.md
│   └── explain_code/
│       ├── system.md
│       └── user.md
└── patterns-custom/
    └── my-pattern/
        ├── system.md
        └── user.md
```

- `system.md` - Contains the system prompt for the LLM
- `user.md` - Contains user prompt template (optional)

## Tips

1. **Pattern Discovery:** Use `gptel-fabric-list-patterns` to see all available patterns
2. **Pattern Details:** Use `gptel-fabric-describe-pattern` to view pattern prompts
3. **Custom Patterns:** Custom patterns are marked with `[CUSTOM]` in listings
4. **Performance:** Patterns are cached on first use; refresh with `gptel-fabric-refresh-cache`

## Development

### Running Tests

```bash
# With gptel installed (full test suite)
make test

# Or specify gptel location
GPTEL_DIR=/path/to/gptel make test

# Standalone tests (without gptel)
make test-standalone
```

### Byte Compilation

```bash
make compile
```

### Linting

```bash
make lint
make package-lint  # Requires package-lint
```

### Full Check

```bash
make check  # Runs lint and tests
```

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

Before submitting, please ensure:
- All tests pass (`make test`)
- Code is properly formatted
- Documentation is updated if needed

## Acknowledgments

- [gptel](https://github.com/karthink/gptel) for the excellent LLM integration
- [Fabric](https://github.com/danielmiessler/fabric) for the comprehensive prompt patterns

## Author

Alex Olkhovskiy <gtptel-fabric@monoinbox.co.uk>

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2025-08-26

### Added
- Initial release of gptel-fabric
- Core functionality to use Fabric patterns with gptel
- Support for both standard and custom Fabric patterns
- Interactive pattern selection with completion and history
- Replace functionality for in-place text modification
- Built-in proofreading function
- Dynamic command creation for frequently used patterns
- Pattern caching for improved performance
- Comprehensive test suite
- Full documentation and examples

### Features
- `gptel-fabric-send-with-pattern` - Send region/buffer with pattern
- `gptel-fabric-replace-with-pattern` - Replace region/buffer with LLM output
- `gptel-fabric-query-with-pattern` - Send custom query with pattern
- `gptel-fabric-proofread` - Quick proofreading with replacement
- `gptel-fabric-select-pattern` - Interactive pattern selection
- `gptel-fabric-list-patterns` - List all available patterns
- `gptel-fabric-describe-pattern` - Show pattern details
- `gptel-fabric-create-pattern-command` - Create shortcuts for patterns
- `gptel-fabric-create-replace-command` - Create replace shortcuts
- `gptel-fabric-refresh-cache` - Refresh pattern cache

[0.1.0]: https://github.com/lxol/gptel-fabric/releases/tag/v0.1.0

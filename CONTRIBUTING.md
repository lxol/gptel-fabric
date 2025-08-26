# Contributing to gptel-fabric

Thank you for your interest in contributing to gptel-fabric! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please be respectful and constructive in all interactions.

## How to Contribute

### Reporting Issues

- Check if the issue already exists
- Include Emacs version, gptel version, and gptel-fabric version
- Provide minimal reproducible example
- Include relevant error messages and stack traces

### Suggesting Features

- Check if the feature has been requested
- Explain the use case clearly
- Provide examples of how it would work

### Pull Requests

1. Fork the repository
2. Create a feature branch from `main`
3. Make your changes
4. Add tests if applicable
5. Update documentation
6. Run tests and linting
7. Submit PR with clear description

## Development Setup

### Prerequisites

- Emacs 27.1+
- gptel installed
- Fabric patterns available

### Setup

```bash
git clone https://github.com/lxol/gptel-fabric
cd gptel-fabric
make compile
```

### Testing

```bash
# Run all tests
make test

# Run specific test
emacs -batch -L . -l ert -l test-gptel-fabric.el \
  -f ert-run-tests-batch-and-exit
```

### Code Style

- Use `lexical-binding: t`
- Follow Emacs Lisp conventions
- Prefix all functions with `gptel-fabric-`
- Document all public functions
- Keep lines under 80 characters when possible

### Documentation

- Update docstrings for all functions
- Update README.md for user-facing changes
- Update CHANGELOG.md following Keep a Changelog format
- Include examples where helpful

## Testing Guidelines

- Write tests for new functionality
- Ensure existing tests pass
- Test with different Fabric pattern structures
- Test error conditions

## Commit Messages

Follow conventional commits format:

```
type: description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Test changes
- `refactor`: Code refactoring
- `style`: Formatting changes
- `chore`: Maintenance tasks

## Release Process

1. Update version in `gptel-fabric.el`
2. Update version in `gptel-fabric-pkg.el`
3. Update CHANGELOG.md
4. Create git tag: `git tag v0.1.0`
5. Push tag: `git push origin v0.1.0`

## MELPA Submission

For MELPA submission:
1. Ensure package follows MELPA conventions
2. Run `make package-lint`
3. Check byte compilation: `make compile`
4. Submit PR to MELPA repository

## Questions?

Feel free to open an issue for any questions about contributing.

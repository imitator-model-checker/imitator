# Contributing to IMITATOR

Thank you for considering contributing to IMITATOR! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Git Workflow](#git-workflow)
- [Commit Messages](#commit-messages)
- [Code Formatting](#code-formatting)
- [Pull Requests](#pull-requests)
- [Development Setup](#development-setup)

## Code of Conduct

Please be respectful and constructive in all interactions with other contributors. We are committed to providing a welcoming and inclusive environment for everyone.

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/your-username/imitator.git
   cd imitator
   ```
3. **Add upstream remote** to stay in sync:
   ```bash
   git remote add upstream https://github.com/imitator-model-checker/imitator.git
   ```
4. **Create a feature branch** (see [Git Workflow](#git-workflow) below)

## Git Workflow

IMITATOR follows a **modified Gitflow** branching model:

### Main Branches

- **`master`** — Production-ready code. Only receives pull requests from `develop` or hotfixes. Always stable and deployable.
- **`develop`** — Integration branch for features. Base branch for feature development. Should be relatively stable.

### Supporting Branches

#### Feature Branches
Create feature branches from `develop` for new features or enhancements:

```bash
git checkout develop
git pull upstream develop
git checkout -b feature/description-of-feature
```

Branch naming convention: `feature/what-it-does` or `feature/issue-123-short-desc`

#### Bugfix Branches
For bug fixes targeting the next release:

```bash
git checkout develop
git pull upstream develop
git checkout -b bugfix/description-of-fix
```

Branch naming convention: `bugfix/what-it-fixes` or `bugfix/issue-456-short-desc`

#### Hotfix Branches
For critical production fixes (rare):

```bash
git checkout master
git pull upstream master
git checkout -b hotfix/critical-issue
```

After merging to `master`, the hotfix should also be merged back into `develop`.

### Workflow Steps

1. **Keep your branch updated** before submitting a PR:
   ```bash
   git fetch upstream
   git rebase upstream/develop
   ```

2. **Push to your fork**:
   ```bash
   git push origin feature/your-feature
   ```

3. **Submit a Pull Request** to the `develop` branch (see [Pull Requests](#pull-requests))

## Commit Messages

IMITATOR uses **Conventional Commits** for clear, semantic commit history. This enables automatic changelog generation and better commit navigation.

### Format

```
<type>(<scope>): <subject>

<body>

<footer>
```

### Type
Must be one of:
- **feat**: A new feature
- **fix**: A bug fix
- **docs**: Documentation changes
- **style**: Code style changes (formatting, missing semicolons, etc.)
- **refactor**: Code refactoring without feature changes
- **perf**: Performance improvements
- **test**: Adding or updating tests
- **ci**: CI/CD configuration changes
- **chore**: Dependency updates, build process changes

### Scope
Optional but recommended. The area of the codebase affected:
- Examples: `synthesis`, `templates`

### Subject
- Use imperative mood ("add" not "added" or "adds")
- Don't capitalize the first letter
- Don't end with a period
- Maximum 50 characters

### Body (Optional)
- Explain *what* and *why*, not *how*
- Wrap at 72 characters
- Separate from subject with a blank line

### Footer (Optional)
- Reference issues: `Fixes #123`, `Closes #456`, `Resolves #789`
- Use `BREAKING CHANGE:` prefix for breaking changes

### Examples

```
feat(convex-hull): improve hull computation efficiency

Implement incremental hull updates to reduce redundant
calculations. This change reduces synthesis time by 15% for
certain benchmarks.

Fixes #42
```

```
docs: update installation instructions for macOS
```

## Code Formatting

IMITATOR uses `ocamlformat` for OCaml source files. The repository provides a helper script that reads the project configuration from `.ocamlformat` and automatically honours `.ocamlformat-ignore`.

Before committing OCaml changes, run:

```bash
scripts/format.sh
```

By default, this formats only modified or newly added `.ml` and `.mli` files. Useful options include:

```bash
scripts/format.sh --staged        # format files staged for commit
scripts/format.sh --base develop  # format files changed relative to develop
scripts/format.sh --all           # format every tracked .ml/.mli file
scripts/format.sh --check         # check formatting without modifying files
```

If `ocamlformat` is missing, install it with:

```bash
opam install ocamlformat
```

## Pull Requests

### Before Submitting

1. **Ensure your code passes all tests**:
   ```bash
   dune build
   ```

2. **Format OCaml files** with the repository helper:
   ```bash
   scripts/format.sh
   ```

3. **Verify commit messages** follow Conventional Commits format

4. **Keep commits logically organized** — each commit should represent a single, coherent change

5. **Rebase on `develop`** to maintain a clean history:
   ```bash
   git fetch upstream
   git rebase upstream/develop
   ```

6. **Force push if needed** (only to your own fork):
   ```bash
   git push origin feature/your-feature --force-with-lease
   ```

### Creating a Pull Request

1. **Use a clear, descriptive title** following the Conventional Commits format
2. **Reference related issues** in the PR description
3. **Describe the changes** and motivation
4. **Link to any relevant benchmarks or documentation**

### PR Checklist

- [ ] Commits follow Conventional Commits format
- [ ] OCaml files formatted with `scripts/format.sh`
- [ ] Code builds without errors
- [ ] All tests pass
- [ ] Documentation updated (if applicable)
- [ ] No unnecessary changes included
- [ ] Rebased on latest `develop`

### Code Review

All pull requests require at least one approval from a maintainer before merging. Please:

- Be open to feedback
- Respond to review comments promptly
- Make requested changes in new commits (don't force-push during review unless asked)
- Re-request review after addressing comments

### Merging

- PRs are squashed and merged into `develop` by maintainers
- Ensure your branch is up-to-date before merge
- After merge, delete your feature branch

## Development Setup (Docker)

You can develop and test IMITATOR using Docker, which provides a consistent environment and eliminates dependency issues.

#### Building the Docker Image

1. From the repository root:
   ```bash
   docker build -t imitator:latest .
   ```

2. Verify the build:
   ```bash
   docker run --rm imitator:latest --help
   ```

#### Running IMITATOR in Docker

**Basic usage** (run IMITATOR with input files):
```bash
docker run --rm -v $(pwd):/workspace imitator:latest /workspace/path/to/model.imi
```

**With options**:
```bash
docker run --rm -v $(pwd):/workspace imitator:latest /workspace/model.imi -mode statespace
```

**Interactive shell** (to explore the container):
```bash
docker run --rm -it --entrypoint bash -v $(pwd):/workspace imitator:latest
```

#### Docker Notes

- Use `-v $(pwd):/workspace` to mount your current directory into the container
- All model files and benchmarks should be accessible from the mounted volume
- Output files will be created in the mounted directory and accessible from your host machine
- The container runs the `imitator` binary by default, but you can override it with `bash` for interactive use

## Questions?

- Check the [IMITATOR website](https://www.imitator.fr)
- Open an issue for bug reports or feature requests
- Reach out to the maintainers for guidance

Happy contributing!

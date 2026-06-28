# Simple Modern Emacs

A small Emacs config for modern software work.

This is not a framework. It is a readable `init.el` with good defaults, LSP,
completion, Git, projects, formatting, and language modes.

## Goals

- Start fast.
- Stay readable.
- Prefer built-in Emacs features.
- Add packages only when they remove real friction.
- Work well for Go, Rust, TypeScript, Python, shell, Markdown, YAML, Docker,
  Terraform, and everyday project work.

Opinion [high]: This should stay boring.
Flip fact: If it turns into a framework, it becomes less useful.

## Requirements

- Emacs 29 or newer.
- `git`.
- Optional but useful: `ripgrep`.
- Language servers for the languages you use.

Common language server installs:

```sh
go install golang.org/x/tools/gopls@latest
rustup component add rust-analyzer
npm install -g typescript typescript-language-server vscode-langservers-extracted
python -m pip install python-lsp-server
```

## Install

```sh
git clone https://github.com/owainlewis/emacs.git ~/.emacs.d
```

Or from an existing checkout:

```sh
./install.sh
```

First launch installs packages from GNU ELPA, NonGNU ELPA, and MELPA.

## What You Get

Navigation:

- `vertico`
- `orderless`
- `marginalia`
- `consult`
- built-in `project`

Coding:

- built-in `eglot` for LSP
- built-in `flymake` diagnostics
- `corfu` completion
- `cape` completion extras
- `editorconfig`
- `apheleia` for explicit formatting
- tree-sitter mode remaps when available

Tools:

- `magit`
- `which-key`
- `recentf`
- `savehist`
- `exec-path-from-shell` on macOS and X sessions

Languages:

- Go
- Rust
- TypeScript
- JavaScript
- Python
- Markdown
- YAML
- Dockerfile
- Terraform
- Org

## Key Bindings

Project:

- `C-c p p`: switch project
- `C-c p f`: find file in project
- `C-c p b`: switch project buffer
- `C-c p s`: project shell
- `C-c p r`: project replace

Search:

- `C-s`: search current buffer
- `C-c s r`: ripgrep
- `C-c s g`: grep
- `C-c s f`: find file
- `C-c s i`: imenu

Code:

- `C-c a`: code action
- `C-c r`: rename symbol
- `C-c o`: organize imports
- `C-c f`: format buffer
- `M-.`: go to definition
- `M-,`: jump back
- `M-n`: next diagnostic
- `M-p`: previous diagnostic

Git:

- `C-c g`: Magit status

## Design Notes

The config uses package.el and `use-package`. There is no custom package
manager, no generated files, and no module system.

Formatting is explicit. `C-c f` formats the current buffer through `apheleia`.
It does not format every file on save by default.

LSP is built-in through Eglot. Install the language server for your language and
open a project file. Eglot starts automatically for common programming modes.

Tree-sitter modes are used when Emacs has the relevant mode available. Grammar
installation is left to the user because it varies by system.

## Development

Smoke test the config:

```sh
emacs --batch -Q --load tests/check-init.el
```

Test first-run package install:

```sh
OWAIN_EMACS_INSTALL_PACKAGES=1 emacs --batch -Q --load init.el
```

Byte compile:

```sh
emacs --batch -Q --load init.el --eval '(byte-compile-file "init.el")'
```

## License

MIT.

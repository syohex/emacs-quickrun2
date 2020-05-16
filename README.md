# quickrun2.el ![](https://github.com/syohex/emacs-quickrun2/workflows/CI/badge.svg)

## Introduction

**quickrun2.el** is Emacs port of [quickrun.vim](https://github.com/thinca/vim-quickrun).


`quickrun2.el` is a extension to execute editing buffer.
`quickrun2.el` is similar to emacs builtin `executable-interpret`. However `quickrun2.el`
runs any programs compiling languages, script languages, markup languages etc.


## Default supported languages

- C
- C++
- Perl
- Ruby
- Python
- node.js
- TypeScript
- Go Language
- Rust
- Julia


## Basic Usage

#### `quickrun2`

Execute current file. If `quickrun2.el` does not find language source,
then `quickrun2.el` asks you command-key(You always input command
if you use `C-u` prefix key)

## Customize

### `quickrun2-focus-p`(Default: `t`)

If this value is `nil`, quickrun2.el does not move focus to output buffer.


## language source

### Example

```lisp
(quickrun2-define-source typescript
  :inherit 'interpreter-base
  :major-mode '(typescript-mode)
  :pattern "\\.ts\\'"
  :command "ts-node")
```

### Properties

```lisp
(quickrun2-define-source example
  :major-mode 'example-mode
  :pattern "\\.exam\\'"
  :output #'quickrun2--exe-output
  :exec '((compiler "-x" "c" "-std=gnu99" "-o" output source link-option) (output))
  :compiler c-compiler
  :link-option #'quickrun2--c-link-option
  :remove '(output))
```

#### `:major-mode` (symbol or symbol list, required)

This source is used if current major-mode equals to this value

#### `:pattern` (string, required)

This source is used if current file name is matched against this pattern.

#### `:exec` (list of list required)

Commands executed

#### `:remove` (list optional)

remove files. You should set if intermediate files are generated

#### `:inherit` (symbol optional)

Inherit base class. You can define base class by `quickrun2-define-base-source`

#### other parameters(symbol or string or function)

You can add any parameters

## Key bindings in quickrun2 buffer

| Key       | Command                |
|:---------:|:-----------------------|
| `q`       | Close quickrun2 window  |
| `C-c C-c` | Kill quickrun2 process  |

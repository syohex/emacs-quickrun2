# quickrun2.el ![](https://github.com/syohex/emacs-quickrun2/workflows/CI/badge.svg)

## Introduction

**quickrun2.el** is Emacs port of [quickrun.vim](https://github.com/thinca/vim-quickrun).


`quickrun2.el` is a extension to execute editing buffer.
`quickrun2.el` is similar to executable-interpret, but `quickrun2.el` provides more convenient
commands. `quickrun2.el` execute not only script languages(Perl, Ruby, Python etc), but also
compiling languages(C, C++, Go, Java etc) and markup language.


## Default supported languages

* C
* C++
* Perl
* Ruby
* Python
* node.js
* Go Language
* Rust


## Basic Usage

#### `quickrun2`

Execute current file. If `quickrun2.el` does not find langauge source,
then `quickrun2.el` asks you command-key(You always input command
if you use `C-u` prefix key)

## Customize

### `quickrun2-focus-p`(Default: `t`)

If this value is `nil`, quickrun2.el does not move focus to output buffer.


## Add language source

```lisp
(quickrun2-define-source typescript
  :inherit 'interpreter-base
  :major-mode '(typescript-mode)
  :pattern "\\.ts\\'"
  :command "ts-node")
```


## Key bindings in quickrun2 buffer

| Key       | Command                |
|:---------:|:-----------------------|
| `q`       | Close quickrun2 window  |
| `C-c C-c` | Kill quickrun2 process  |

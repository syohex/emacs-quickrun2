# quickrun2.el ![](https://github.com/syohex/emacs-quickrun2/workflows/CI/badge.svg)

## Introduction

**quickrun2.el** is Emacs port of [quickrun.vim](https://github.com/thinca/vim-quickrun).


`quickrun2.el` is a extension to execute editing buffer.
`quickrun2.el` is similar to executable-interpret, but `quickrun2.el` provides more convenient
commands. `quickrun2.el` execute not only script languages(Perl, Ruby, Python etc), but also
compiling languages(C, C++, Go, Java etc) and markup language.


## Support Programming Languages

`quickrun2.el` supports following programming languages and markup languages
as default. But you can register your own command and apply other languages.

**Programming Language(commands used)**

* C(gcc or clang or Visual C++)
* C++(g++ or clang++ or Visual C++)
* Perl(perl)
* Ruby(ruby or mruby)
* Python(python)
* node.js
* Go Language(go or gccgo)

See also `quickrun2--support-languages` global variable.


## Basic Usage

#### `quickrun2`

Execute current file. If `quickrun2.el` does not find command-key,
then `quickrun2.el` asks you command-key(You always input command
if you use `C-u` prefix key)

## Customize

### `quickrun2-focus-p`(Default: `t`)

If this value is `nil`, quickrun2.el does not move focus to output buffer.


## Add language source

```lisp
(quickrun2-define-source typescript
  :inherit 'interpreter-base
  :command "ts-node")
```


## Key bindings in quickrun2 buffer

| Key       | Command                |
|:---------:|:-----------------------|
| `q`       | Close quickrun2 window  |
| `C-c C-c` | Kill quickrun2 process  |

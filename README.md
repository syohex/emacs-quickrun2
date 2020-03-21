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

Execute current buffer. If `quickrun2.el` does not find command-key,
then `quickrun2.el` asks you command-key(You always input command
if you use `C-u` prefix key)

buffer file, `quickrun2.el` sends its content to stdin of executed program. Please set
`quickrun2-input-file-extension` to `nil` If you want to disable this feature.

## Customize

### `quickrun2-focus-p`(Default: `t`)

If this value is `nil`, quickrun2.el does not move focus to output buffer.


## User Defined Command

You can add your own command or override existsing command  by `quickrun2-add-command` as below.

```lisp
;; Use this parameter as C++ default
(quickrun2-add-command "c++/c1z"
  '((:command . "g++")
    (:exec    . ("%c -std=c++1z %o -o %e %s"
		 "%e %a"))
    (:remove  . ("%e")))
  :default "c++")

;; Use this parameter in pod-mode
(quickrun2-add-command "pod"
  '((:command . "perldoc")
    (:exec    . "%c -T -F %s"))
  :mode 'pod-mode)

;; You can override existing command
(quickrun2-add-command "c/gcc"
  '((:exec . ("%c -std=c++1z %o -o %e %s"
	      "%e %a")))
  :override t)
```

First argument of `quickrun2-add-command` is command key. Second argument of it is
command parameter, which is described laster. `quickrun2-add-command` also takes
key parameters, `:default`, `:mode`, `:override`.

| Argument         | Description                                                 |
|:-----------------|:------------------------------------------------------------|
| `:default` lang  | Use this command parameter as default in specified language |
| `:mode` mode     | this command parameter in specified mode                    |
| `:override` bool | Override existing parameter with specified parameter        |


### Command Parameter

Command alist has following parameters,

#### `:command`(mandatory parameter)

Command name. `%c` is expanded into this value.

#### `:cmdopt`(optional)

Command(`:command`) option. `%o` is expanded into this value.

#### `:execute`

Executed commands. You can also set command list parameter.
If you set list parameter, `quickrun2.el` executes command
list in order.

If this parameter is omitted, `quickrun2.el` use default execute
command template "%c %o %s %a".

#### `:remove`

Remove files after executing.
If command create some intermediate files, you should set this
parameter. :remove value is atom or list.


#### `:tempfile`

Use temporary file or not. `quickrun2.el` uses temporary file
if you omit this parameter.

NOTE: If you set this parameter, you cannot use `quickrun2-region`.

#### `:description`

Description of this command. This parameter is used in
`helm-quickrun2` or `anything-quickrun2`


### Placeholders

You can use following placeholders in command parameter

| Placeholder | Expanded                                      |
|:-----------:|:----------------------------------------------|
|  `%c`       |  Command                                      |
|  `%o`       |  Command line option                          |
|  `%s`       |  Source(absolute path)                        |
|  `%a`       |  Script's arguments                           |
|  `%n`       |  Source without extension(absolute path)      |
|  `%N`       |  Source without extension(nondirectory)       |
|  `%d`       |  Directory name of Source(absolute path)      |
|  `%e`       |  Source with executable suffix(absolute path) |
|  `%E`       |  Source with executable suffix(nondirectory)  |

Source file name(`%s`, `%n` etc) is not original file name except
Java language. Because `quickrun2.el` copys source file to temporary
file firstly.


## Change Default Command

`quickrun2-set-default` changes default command in language that is registerd
multiple command parameters(like c, c++,Javascript).

```lisp
(quickrun2-set-default "c" "c/clang")
```

This means that quickrun2 uses "c/clang" for C files.


## Timeout Seconds

`quickrun2.el` kills process if program run over 10 seconds as default.
This avoids infinite loop program or endless program by some mistakes.
You control timeout second to set `quickrun2-timeout-seconds`.
This feature is disabled if `quickrun2-timeout-seconds` is `nil`.
(You can also kill process by `C-c C-c` in quickrun2 buffer)


## Key bindings in quickrun2 buffer

| Key       | Command                |
|:---------:|:-----------------------|
| `q`       | Close quickrun2 window  |
| `C-c C-c` | Kill quickrun2 process  |

# quickrun2.el

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

#### `quickrun2-region`

Execute region. (Java is not supported)

#### `quickrun2-with-arg`

Execute current buffer with arguments.
`quickrun2.el` asks you command line argument

#### `quickrun2-shell`

Execute current buffer in eshell for interactive command such as program
which reads input from STDIN.

#### `quickrun2-compile-only`

Compile current buffer with compile.el framework, not execute.
quickrun2 with `C-u C-u` prefix behaves same as quickrun2-compile-only.

#### `quickrun2-replace-region`

Replace region of code with its output.

## Note

If quickrun2 returns `command not found`, please check `(executable-find "THE_COMMAND_NAME")` [for example `(executable-find "gnuplot")`] .
If this returns `nil`, I strongly recommend you use https://github.com/purcell/exec-path-from-shell

## Send File to STDIN

If `executed_file.qrinput`(like `foo.c.qrinput`) is existed in directory same as executed
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


#### `:compile-only`

Command exected by `quickrun2-compile-only`. This option is used for
syntax check or converting another language(e.g. CoffeeScript => JavaScript).

### `:compile-conf`

Configuration of `quickrun2-compile-only`. This parameter must be alist.

#### `:remove`

Remove files after executing.
If command create some intermediate files, you should set this
parameter. :remove value is atom or list.

#### `:outputter`

Please see Outputter section.

#### `:default-directory`

Directory where commands are executed.

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


## Buffer Local Variables

Buffer local variables is priority to default parameters.

#### `quickrun2-option-cmd-alist`

Command alist.

#### `quickrun2-option-command`

Command key(Expanded to %c)

#### `quickrun2-option-cmdkey`

Command key of command parameter.

#### `quickrun2-option-cmdopt`

Command option(Expanded to %o)

#### `quickrun2-option-args`

Program argument(Expanded to %a.)

#### `quickrun2-option-shebang`

If this value is `non-nil` and first line of source file is started "#!",
the following string is treated as ":command".

#### `quickrun2-option-outputter`

Outputter function. See *Outputter* section


### Example of buffer local variable

Setting C++11.

```c++
#include <iostream>
#include <vector>
#include <string>

int main (int argc, char *argv[])
{
    std::vector <std::string> lst = { "a", "b", "c", "d" };

    for (auto x : lst) {
        std::cout << "[" << x << "]" << std::endl;
    }

    for (auto i = 1; i < argc; i++) {
        std::cout << "[" << argv[i] << "]" << std::endl;
    }

    return 0;
}

/*
  Local Variables:
  quickrun2-option-cmd-alist: ((:command . "g++")
                               (:exec    . ("%c -std=c++14 -o %n %s"
                                           "%n apple orange melon"))
                               (:remove  . ("%n")))
  End:
*/
```


## Hooks

#### `quickrun2-after-run-hook`

Run hooks after execute all commands.


## Outputter

Outputter is a function for processing command output. Default outputter is
to output to \*quickrun2\* buffer and processing ANSI Color sequence.

`quickrun2.el` defines following functions as default.

#### `buffer:buffername`

Output to buffer. [outputter *buffer* sample](sample/sample_outputter_buffer.pl)

#### `file:filename`

Output to file. [outputter *file* sample](sample/sample_outputter_file.pl)

#### `variable:varname`

Output to variable. [outputter *variable* sample](sample/sample_outputter_variable.pl)

#### `browser`

Output to Web browser(using function *browse-url*) [outputter *browser* sample](sample/sample_outputter_browser.pl)

#### `message`

Output to \*Message\* buffer(using function *message*) [outputter *message* sample](sample/sample_outputter_message.pl)

#### `multi`

Use multiple outputters. [outputter *multi* sample](sample/sample_outputter_multi.pl)

#### `null`

No output. [outputter *null* sample](sample/sample_outputter_null.pl)


## Using quickrun2 as function from other functions

`quickrun2` can be used as function from other functions.
You can pass configuration by `:source` argument.
Sample is following:

```lisp
(defun test-perl ()
  (interactive)
  (let* ((cmd "git rev-parse --show-toplevel")
         (topdir (with-temp-buffer
                   (call-process-shell-command cmd nil t nil)
                   (goto-char (point-min))
                   (if (re-search-forward "^\\(.+\\)$" nil t)
                       (match-string 1)))))
    (quickrun2 :source `((:command . "prove")
                        (:default-directory . ,topdir)
                        (:exec . ("%c -bv --color %s"))))))
```

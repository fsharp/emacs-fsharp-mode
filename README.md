# fsharp-mode

Provides support for the F# language in Emacs. Includes the following features:

- Support for F# Interactive
- Displays type signatures and tooltips
- Provides syntax highlighting and indentation.
- Intellisense support.

The following features are under development:

- Intelligent indentation

Requires Emacs 25.1+, Mono 3.10.X and F# 3.0 or higher. Without F# 3.0 (or higher)
the background compiler process will not function correctly.

## Build Status

### Travis [![Travis build status](https://travis-ci.org/fsharp/emacs-fsharp-mode.png)](https://travis-ci.org/fsharp/emacs-fsharp-mode)

See [.travis.yml](.travis.yml) for details.

## Installation

### Package

`fsharp-mode` is available on [MELPA](https://melpa.org) and can
be installed using the built-in package manager.

If you're not already using MELPA, add the following to your init.el:

```lisp
;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless package-archive-contents (package-refresh-contents))
(package-initialize)

;;; Install fsharp-mode
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))

(require 'fsharp-mode)
```



### Manual installation

1. Clone this repo and run `make install`:
    ```
    git clone git://github.com/fsharp/emacs-fsharp-mode.git
    cd emacs-fsharp-mode/emacs
    make test-all # optional
    make install
    ```

2. Add the following to your init.el:
    ```lisp
    (add-to-list 'load-path "~/.emacs.d/fsharp-mode/")
    (autoload 'fsharp-mode "fsharp-mode"     "Major mode for editing F# code." t)
    (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
    ```

Note that if you do not use `make install`, which attempts to download
the dependencies from MELPA for you, then you must ensure that you have
installed them yourself. A list can be found in `fsharp-mode-pkg.el`.

If you run into any problems with installation, please check that you
have Emacs 24 on your PATH using `emacs --version`.
Note that OSX comes with Emacs 22 by default and installing a .app of
Emacs 24 will not add it to your PATH. One option is:

`alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'`


## Usage

fsharp-mode should launch automatically whenever you open an F#
buffer. When the intellisense process is running, the following features will be available:

1. Type information for symbol at point will be displayed in the minibuffer.
2. Errors and warnings will be automatically highlighted, with mouseover
   text. Jump to the next and previous error using <kbd>M-n</kbd> and <kbd>M-p</kbd>.
3. To display a tooltip, move the cursor to a symbol and press
   <kbd>C-c C-t</kbd> (default).
4. To jump to the definition of a symbol at point, use <kbd>C-c C-d</kbd>.
5. Completion will be invoked automatically on dot, as in Visual Studio.
   It may be invoked manually using `fsharp-ac/complete-at-point`,
   bound by default to <kbd>C-c C-.</kbd>.
6. To stop the intellisense process for any reason, use <kbd>C-c C-q</kbd>.

### Projects

fsharp-mode offers intellisense for projects using the MSBuild/`.fsproj`
format. This allows project files to be shared with other developers using
Visual Studio and Xamarin Studio/Monodevelop. To create a new project file, it
is recommended that you take an existing project file and modify the list of
source files. One such project file can be found in the fsharp-mode repository
[here](https://github.com/fsharp/emacs-fsharp-mode/blob/master/test/Test1/Test1.fsproj). Alternatively
projects generated via [forge](https://github.com/ionide/Forge) are setup
with the needed files.

If, on loading a new `.fs` file, a `.fsproj` file is found in the
current or an enclosing directory, the intellisense process will be
launched if necessary, and the project loaded. When a project file is
loaded for the first time, this will be reported in the
minibuffer. Projects can also be manually loaded using <kbd>C-c
C-p</kbd>. This may be needed if a project file is in an unusual
location relative to a source file, or if the project file has been
modified (for example to add or remove a source file).

### Scripts

F# scripts (`.fsx` files) are standalone, and require no project file. If you wish open a script file and the intellisense process is not yet running, it will be launched automatically.

## Configuration

### Compiler and REPL paths

The F# compiler and interpreter should be set to good defaults for
your OS as long as the relevant executables can be found on your PATH
or in other standard locations. If you have a non-standard setup you
may need to configure these paths manually.

On Windows:

```lisp
(setq inferior-fsharp-program "\"c:\\Path\To\Fsi.exe\"")
(setq fsharp-compiler "\"c:\\Path\To\Fsc.exe\"")
```

On Unix-like systems, you must use the *--readline-* flag to ensure F#
Interactive will work correctly with Emacs. Typically `fsi` and `fsc` are
invoked through the shell scripts `fsharpi` and `fsharpc`.

```lisp
(setq inferior-fsharp-program "path/to/fsharpi --readline-")
(setq fsharp-compiler "path/to/fsharpc")
```

### Behavior

There are a few variables you can adjust to change how fsharp-mode behaves:

- `fsharp-ac-use-popup`: Show tooltips using a popup at the cursor
  position. If set to nil, display the tooltip in a split window.

- `fsharp-doc-idle-delay`: Set the time (in seconds) to wait before
  showing type information in the minibuffer.
  
- `fsharp-ac-intellisense-enabled`: This mode overrides some aspects of
  auto-complete configuration and runs the background process automatically.
  Set to nil to prevent this. Note that this will only prevent the background
  process from being launched in the *future*. If it is already running you
  will also need to quit it using <kbd>C-c C-q</kbd>.

You might also add `company-sort-prefer-same-case-prefix` to the list
of company transformers to to prefer exact case sort in completion
lists:

```lisp
(add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
```



### Key Bindings

If you are new to Emacs, you might want to use the menu (call
menu-bar-mode if you don't see it). However, it's usually faster to learn
a few useful bindings:

 Key binding                         | Description
-------------------------------------|----------------------------------------
<kbd>C-c C-r</kbd>                   | Evaluate region
<kbd>C-c C-f</kbd>                   | Load current buffer into toplevel
<kbd>C-c C-e</kbd>                   | Evaluate current toplevel phrase
<kbd>C-M-x</kbd>                     | Evaluate current toplevel phrase
<kbd>C-M-h</kbd>                     | Mark current toplevel phrase
<kbd>C-c C-s</kbd>                   | Show interactive buffer
<kbd>C-c C-c</kbd>                   | Compile with fsc
<kbd>C-c x</kbd>                     | Run the executable
<kbd>C-c C-a</kbd>                   | Open alternate file (.fsi or .fs)
<kbd>C-c l</kbd>                     | Shift region to left
<kbd>C-c r</kbd>                     | Shift region to right
<kbd>C-c &lt;up&gt;</kbd>            | Move cursor to the beginning of the block
<kbd>C-c C-p</kbd>                   | Load a project for autocompletion and tooltips
<kbd>C-c C-d</kbd>, <kbd>M-.</kbd>   | Jump to definition of symbol at point
<kbd>C-c C-b</kbd>, <kbd>M-,</kbd>   | Return to where point was before jump.
<kbd>C-c C-t</kbd>                   | Request a tooltip for symbol at point
<kbd>C-c C-q</kbd>                   | Quit current background compiler process

To interrupt the interactive mode, use <kbd>C-c C-c</kbd>. This is useful if your
code does an infinite loop or a very long computation.

If you want to shift the region by 2 spaces, use: <kbd>M-2 C-c r</kbd>

In the interactive buffer, use <kbd>M-RET</kbd> to send the code without
explicitly adding the `;;` thing.

For key bindings that will be more familiar to users of Visual Studio, adding
the following to your `init.el` may be a good start:

```lisp
(add-hook 'fsharp-mode-hook
 (lambda ()
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
   (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))
```


## Editor

In order to change tab size it is possile to put this in emacs profile:

```lisp
(setq-default fsharp-indent-offset 2)
```

Because the F# language is sensitive to indentation, you might wan't to highlight indentation:

```lisp
(add-hook 'fsharp-mode-hook 'highlight-indentation-mode)
```

## Troubleshooting

`fsharp-mode` is still under development, so you may encounter some issues. Please report them so we can improve things! Either open an issue on [Github](https://github.com/fsharp/emacs-fsharp-mode/) with the label `Emacs`, or email the [mailing list](http://groups.google.com/group/fsharp-opensource).

### `fsharp-ac-debug`

If you set the variable `fsharp-ac-debug` to a non-`nil` value, e.g. `(setq fsharp-ac-debug 0)`, then some debug output will be seen in the buffer `*fsharp-debug*`. Setting `fsharp-ac-debug` to an 1 or 2 will cause a truncated or complete copy of communication between Emacs and the background intellisense process to be logged in `*fsharp-debug*`. This can make things rather slow, but would be useful for bug reports.

### Project file issues

If your project file does not seem to be being parsed correctly, so that you have missing references or other incorrect intellisense results, it is possible to obtain a detailed log of the project file loading process as follows:

* Open the F# file where the problems are visible.
* Set `fsharp-ac-debug` to 2, for example by `M-: (setq fsharp-ac-debug 2)`.
* Reload the project file using `C-c C-p`.
* Attach the detailed log output from your `*Messages*` buffer to a [new issue](https://github.com/fsharp/emacs-fsharp-mode/issues/new).

As an example, the log output will start similarly to:

```
<full path to>/MyProject.fsproj:
Loading default tasks for ToolsVersion: 4.0 from /usr/lib/mono/4.5/Microsoft.Common.tasks
<full path to>/MyProject.fsproj: Importing project
...
```

As an alternative, the command line tool responsible can be invoked directly to obtain the same log. Assuming you have installed from MELPA and are running on Linux or OS X, the following should work:

    mono ~/.emacs.d/elpa/fsharp-mode-<date>/bin/FSharp.Compiler.Service.ProjectCrackerTool.exe --text <path to>/MyProject.fsproj true

On Windows the `mono` prefix is not required, and if you have installed using a different method then the files will be in a different location.

### `Error: F# completion process produced malformed JSON.`

This is probably the result of the background intellisense process crashing and printing a stacktrace in plain text. Please report the crash, preferably with how to reproduce, and the contents of the `*fsharp-complete*` buffer.

### `Error: background intellisense process not running.`

You have requested some intellisense information (such as completions or a tooltip), but the background process is not running. The most common cause of this is that a standard `.fs` file is being visited in the current buffer, but a `.fsproj` project file was not found in the same directory. Try loading one with <kbd>C-c C-p</kbd>.

### `Error: this file is not part of the loaded project.`

In this case you have requested intellisense for the visited file, which is a standard `.fs` file *not* included in the current loaded project. Try loading the appropriate project with <kbd>C-c C-p</kbd>.

### `(void-function -any)` or similar

MELPA's use of dates instead of proper version numbers means that the libraries that `fsharp-mode` depends on -- `dash.el` and `s.el` -- may be out of date if you have previously installed them. Try updating all your packages to the latest versions using `M-x package-list-packages U x`.

### Windows completion menu performance

There are some issues with the `pos-tip` library used to display the documentation tooltips for completions. This can cause sluggish performance when scrolling through the list if you try to move up or down just before the tooltip is displayed. We are looking into proper solutions for this with the `pos-tip` maintainer. For now you can work around the issue with `(setq ac-quick-help-prefer-pos-tip nil)`. This will use an alternative method for displaying these tooltips that is faster but uglier.

### Installing from Git

If you installed by cloning the git repository and you are having problems, please sanity check by running `make test-all` in the `emacs` folder.

### Using a different version of FsAutoComplete

Installing with `make install ac_from_src=yes` will build FsAutoComplete from their lastest master commit instead of using their current binary distribution. Build from a specific commit by passing `ac_commit=$COMMIT_HASH`.

## Contributing

This project is maintained by the
[F# Software Foundation](http://fsharp.org/), with the repository hosted
on [GitHub](https://github.com/fsharp/emacs-fsharp-mode).

Pull requests are welcome. Please run the test-suite with `make
test-all` before submitting a pull request.

Maintainers
-----------

The maintainers of this repository appointed by the F# Core Engineering Group are:

 - [Jürgen Hötzel](https://github.com/juergenhoetzel), [Steffen Forkmann](http://github.com/forki), [Karl Nilsson](http://github.com/kjnilsson) and [Guillermo López-Anglada](http://github.com/guillermooo)
 - The primary maintainer for this repository is [Jürgen Hötzel](https://github.com/juergenhoetzel)

Previous maintainers:
 - [Robin Neatherway](https://github.com/rneatherway)

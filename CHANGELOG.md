## 1.10 (2019.12-01)

Features:
  - #210: Remove old FsAutoComplete support (use LSP)
	- provide eglot (Emacs LSP client) integration and add eglot
      integration tests (using Emacs buttercup)
    - Use Cask instead to automate the package development cycle;
      development, dependencies, testing, building, packaging
    - Make project.el aware of F# projects
  - Use Emacs org-mode for README
Bugfixes:
    - #68: Indentation Cleanup / SMIE mode not being applied properly
      (Gastove)

## 1.9.14 (2019-06-09)

Features:
  - #207: Update to FsAutoComplete 0.38.1
  - #206: Set default build command to msbuild if found
Bugfixes:
  - #198: Use buffer-local version of company-quickhelp-mode

## 1.9.13 (2018)

Features:
  - #193: Update to FSAC 0.36
	 - Fixes #183: Load .Net Core projects that reference other projects
	 - Fixes #182: .fs files not parsed
Bugfixes:
  - #190: Fix attribute locking, improve imenu support
  - #189: Fix bug in font locking for active patterns
  - #187: Fix Infinite loop when file begins with a comment preceded
    by whitespace
  - #180: Use scoop instead of  Chocolatey package for Appveyor testing
  - #179: Use portable (Windows support) Makefile
  - #176: Add F# Tools 10.1 SDK directory to search dirs
  - #175: Paths with characters outside ASCII gives error FS2302 (Windows)
  - #171: Fix for phrase detection for if/then/else constructs

## 1.9.12 (2018-05-18)

Features:
  - #170: Flycheck verify (Improved fsautocomplete diagnostics)

Bugfixes:
  - #167: Fix error when visiting a new F# script file and
    fsautocomplete is not started
  - #168: Add Flycheck predicate function to prevent error when
    fsautocomplete is not running
  - #162: Stop matching [ as part of normal residue
  - #157: Don't change global value of `comment-indent-function'
  - #153: Add access control keywords to declaration regexes

## 1.9.11 (2017-10-21)

Features:
  - #151: Correctly find MSBuild from VS2017

Bugfixes:
  - #152: Handle failure to find build commands gracefully

## 1.9.10 (2017-09-18)

Bugfixes:
  - #146: Understand FSAC 0.34 error msgs

## 1.9.9 (2017-09-15)

Features
  - #143: Update to FsAutoComplete 0.34.0

Bugfixes:
  - #139: Disable flycheck and fsharp-doc-mode when fsharp-ac-intellisense-enabled is nil

## 1.9.8 (2017-06-17)

Features:
  - #134: Improved logging
  - #137: fsharp-shift-region-[left,right]: change bindings to 'C-c <' and 'C-c >'

Bugfixes:
  - #136: Use correct F# interactive prompt regex

## 1.9.7 (2017-06-06)

Bugfixes:
  - #131: Don't panic on malformed JSON (debug messages)
  - #133: Update faceup to capture font-locking <|

## 1.9.6 (2017-04-16)

Features:
  - #127: Update to FsAutoComplete 0.32.0 (.NET Core project support)

Bugfixes:
  - #125: Small fixes to try to prevent fsharp-mode to freeze all emacs 
  - #122: Make fsharp-doc-mode hook buffer-local

## 1.9.5 (2017-01-21)

Bugfixes:
  - #117: Fix `type` locking
  - #118: Don't change company-idle-delay
  - #120: Fix FSAC hanging issue

## 1.9.4 (2016-11-30)

Features
  - #116: Improve Active Pattern font locking, eval-when-compile the main font-lock-keywords form
  - #114: Clean up font-locking code

## 1.9.3 (2016-10-31)
Features
  - #111: Update to FsAutoComplete 0.32.0
  - #109: Define inferior-fsharp-mode as variant of comint mode

Bugfixes:
  - #110: Dont change default indent region function
  - #105: Don't send trailing newline to fsautocomplete
  - #104: Dont change `company-minimum-prefix-length'

## 1.9.2 (2016-09-30)
Features
  - #98: Enable imenu support

## 1.9.1 (2016-07-19)

Features:
  - Update to FsAutoComplete 0.29.0.

## 1.9.0 (2016-07-09)

Features:
  - #71: fontify the doc string (@nosami).
  - #77: Use new typesig command for fsharp-doc mode (@rneatherway).
  - #88: Use flycheck for error reporting (@juergenhoetzel).

Bugfixes:
  - #75: Do not change current buffer when starting FSI (@rneatherway).
  - #76: Record type highlighting (@rneatherway).
  - #79: Overlays should not grow when typing (@rneatherway).
  - #82: Inferior fsi: #silentcd to local directory in Tramp (@juergenhoetzel).
  - #83: Fix completion of type annotated symbols (@juergenhoetzel).
  - #85: Don't modify company-transformers (@nosami).
  - #86: Don't clobber company-backends (@nosami).

## 1.8.1 (2016-04-14)

Features:
  - #66: Tramp support (@juergenhoetzel).
  - #69: Prefer exact case sort in completion list (@nosami).

## 1.8.0 (2016-04-05)

Features:
  - Update to FsAutoComplete 0.28.0 to support #65.
  - #65: Faster completions (thanks to @nosami).
  - #56: Use FsAutoComplete "startswith" filter (thanks to @juergenhoetzel).

Bugfixes:
  - #67: Fix use of popup (thanks to @drvink)
  - #60: Unbreak company support on non-graphic displays (thanks to @drvink)
  - #58: Handle buffers not visiting a file (thanks to @juergenhoetzel).

## 1.7.4 (2016-02-05)

Features:
  - #49: Use company for completions (thanks to @nosami).

Bugfixes:
  - Update to FsAutoComplete 0.27.2, fixes project cracking for files
    with spaces in the path.

## 1.7.3 (2016-01-26)

Bugfixes:
  - Update to FsAutoComplete 0.27.1, fixes Windows VS2015-only support.

## 1.7.2 (2016-01-08)

Bugfixes:
  - #50: Inhibit electric-indent for fsharp-mode buffers (thanks to @joranvar).

## 1.7.1 (2015-11-24)

Features:
  - #45: Update FSAC to 0.27, enable project cracking logs.

## 1.7.0 (2015-11-24)

Features:
  - #34: Switch to SMIE-based indentation engine (thanks to m00nlight).
  - #31: Add highlighting of other usages of symbol at point.

## 1.6.3 (2015-10-24)

Bugfixes:
  - Update to FsAutoComplete 0.26.1, which fixes Windows support.

## 1.6.2 (2015-10-20)

Bugfixes:
  - Update to FsAutoComplete 0.26.0.
  - #30: Allow use of symbols containing '%'
  - #28: Fix FSI usage in buffers whose name differs from filename
  - #27: Fix test of fsharp-ac-debug

## 1.6.1 (2015-09-02)

Bugfixes:
  - Update to FsAutoComplete 0.23.1. Fixed MSBuild v14 on non-English
    systems.

## 1.6.0 (2015-09-01)

Features:
  - Update to FSharp.AutoComplete 0.23.0. Contains many improvements,
    which can be found in the changelog at
    https://github.com/fsharp/FsAutoComplete/releases
  - #20: Add C-x C-e as default keybinding for eval.
  - #22: Allow .fsx files to be compiled as well.

Bugfixes:
  - #16: Remove BOM from process output.

## 1.5.4 (2015-06-04)

Features:
  - #4: Update to FSharp.AutoComplete 0.18.0. All unsaved buffer
    contents (not just the current buffer) will now be used for type
    checking.

Bugfixes:
  - #9: Correct quoting of path to fsi.exe on Windows.

## 1.5.3 (2015-05-26)

Note that in since 1.5.2 fsharp-mode has been migrated from
https://github.com/fsharp/fsharpbinding to a
[separate repository](https://github.com/fsharp/emacs-fsharp-mode).
The issue number `#2` below, and all future issue numbers, refer to the
new repository.

Features:
  - #993: Push the mark before going to definition (using etags)

Bugfixes:
  - #1005: Fix issue with compile-command quoting
  - #2: Add `do!` as a keyword.

## 1.5.2 (2015-03-20)

Bugfixes:
  - #973: Force comint-process-echoes to nil to avoid hangs

## 1.5.1 (2015-01-14)

Bugfixes:
  - #923: Autocompletion not working on Emacs 24.4+ on Windows

## 1.5.0 (2014-11-25)

Incorporate FSharp.AutoComplete version 0.13.3, which has corrected help text for the parse command and uses FCS 0.0.81.

Features:
  - #235: Support multiple projects simultaneously

Bugfixes:
  - #824: Emacs should give a better error message if fsautocomplete not found
  - #808: C-c C-p gives an error if no project file above current file's directory
  - #790: Can't make fsac requests in indirect buffers
  - #754: Compiler warnings when installing fsharp-mode from MELPA

## 1.4.2 (2014-10-30)

Incorporate FSharp.AutoComplete version 0.13.2, which returns more information if the project parsing fails.

Features:
  - #811: Return exception message on project parsing fail

## 1.4.1 (2014-10-30)

Incorporate FSharp.AutoComplete version 0.13.1, which contains a fix for goto definition.

Bugfixes:
  - #787: Correct off-by-one error in fsac goto definition

## 1.4.0 (2014-10-26)

The main feature of this release is that the project parsing logic has
been moved to FSharp.Compiler.Service as part of fixing #728.

Features:
  - #319: Better error feedback when no completion data available
  - #720: Rationalise emacs testing, also fixed #453

Bugfixes:
  - #765: Do not offer completions in irrelevant locations (strings/comments)
  - #721: Tests for Emacs syntax highlighting, and resultant fixes
  - #248: Run executable file now uses output from FSharp.AutoComplete
  - #728: Fix project support on Windows

## 1.3.0 (2014-08-28)

Changes by @rneatherway unless otherwise noted.

Major changes in this release are performance improvements thanks to @juergenhoetzel (avoiding parsing the current buffer unless necessary), and
fixes for syntax highlighting.


Features:
  - #481: Only parse the current buffer if it is was modified (@juergenhoetzel)

Bugfixes:
  - #619: Disable FSI syntax highlighting
  - #670: Prevent double dots appearing during completion
  - #485: Fetch SSL certs before building exe in emacs dir
  - #496: Corrections to emacs syntax highlighting
  - #597: Highlight preprocessor and async
  - #605: Add FSI directives to syntax highlighting of emacs
  - #571: Correct range-check for emacs support
  - #572: Ensure fsi prompt is readonly
  - #452: Fetch SSL certs before building exe in emacs dir

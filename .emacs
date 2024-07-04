;;; .emacs -- a configuration file for Emacs
;;;
;;; Copyright (C) 2015-2024 Roman Tsykaliak
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Commentary:
;;;
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compile-command "make -k --jobs=5 --load-average=4 ")
 '(context-menu-mode t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alt")
 '(display-battery-mode t)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(eval-expression-print-length nil)
 '(fill-column 48)
 '(flycheck-gcc-args
   '("-Werror" "-Wall" "-Wextra" "-Wpointer-arith" "-Wstrict-overflow=3" "-pedantic" "-O2"))
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(languagetool-console-arguments '("--languagemodel" "/home/danika/ngram-data"))
 '(languagetool-console-command "~/languagetool/languagetool-commandline.jar")
 '(languagetool-correction-language "en-GB")
 '(languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
 '(languagetool-mother-tongue "de-DE")
 '(languagetool-server-command "~/languagetool/languagetool-server.jar")
 '(menu-bar-mode nil)
 '(mode-line-format
   '("%e" mode-line-front-space
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
      display
      (min-width
       (5.0)))
     " " mode-line-position
     (vc-mode vc-mode)
     " " mode-line-modes mode-line-misc-info mode-line-frame-identification mode-line-buffer-identification mode-line-end-spaces))
 '(package-selected-packages
   '(languagetool markdown-mode transpose-frame cmake-mode pdf-tools password-generator flycheck osx-dictionary nov picpocket buffer-move stickyfunc-enhance move-text win-switch))
 '(scroll-bar-mode nil)
 '(scroll-up-aggressively 0.25)
 '(tool-bar-mode nil)
 '(whitespace-line-column 48)
 '(whitespace-style '(face trailing tabs lines-tail empty)))
;;  '(custom-enabled-themes '(modus-vivendi))
;;  '(ede-project-directories '("/home/danika"))
;;  '(semantic-default-submodes
;;    '(global-semantic-highlight-func-mode global-semantic-decoration-mode global-semantic-stickyfunc-mode global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode global-cedet-m3-minor-mode global-semantic-idle-local-symbol-highlight-mode))
;; '(semantic-symref-tool 'global)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(let ((default-directory  "~/.emacs.d/lisp/"))
  ;; To have libraries in particular paths take precedence over other
  ;; libraries with the same name, elsewhere, put the directories or
  ;; those prioritized libraries at the beginning of ‘load-path’
  ;; (Shadow).  Since various packages store information in
  ;; ~/.emacs.d/, it is unwise to add all of its descendant
  ;; directories to ‘load-path’.  We only added directory ‘lisp’,
  ;; to avoid loading files that are not libraries.
  (setq load-path
        (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           ;; Assuming that you install packages in ~/.emacs.d/lisp/
           ;; and that some of the installed packages have only a
           ;; single file while others have multiple files inside
           ;; a package-specific directory, you need to combine the
           ;; steps from above.
           (append
            (copy-sequence
             (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))
(require 'package
 ;; enable installation of packages from MELPA by adding an entry to
 ;; package-archives after (require 'package) and before the call to
 ;; package-initialize in your init.el or .emacs file. Then just use
 ;; M-x package-list-packages to browse and install packages from
 ;; MELPA and elsewhere.
)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             ;; MELPA ("Milkypostman's Emacs Lisp Package Archive")
             ;; * Up-to-date packages built on our servers from
             ;;   upstream source
             ;; * Installable in amy Emacs with 'package.el' - no
             ;;   local version-control tools needed
             ;; * Curated - no obsolete, renamed, forked or randomly
             ;;   hacked packages
             ;; * Comprehensive - more packages than any other archive
             ;; * Automatic updates - new commits result in new packages
             ;; * Extensible - contribute recipes via github, and we'll
             ;;   build the packages
             )
 ;; (when (< emacs-major-version 24)
 ;;  ;; for important compatibility libraries like cl-lib
 ;;  (add-to-list 'package-archives
 ;;               '("gnu" . "https://elpa.gnu.org/packages/")
 ;;               ;; GNU ELPA ("Emacs Lisp Package Archive")
 ;;               ))
 ;; (add-to-list 'package-archives
 ;;              '("marmalade" . "http://marmalade-repo.org/packages/")
 ;;              ;; marmalade-repo is an Emacs package repository.
 ;;              ;; marmalade is free software, you can help improve it.
 ;;              ;; we hold packages of Emacs code that Emacs users have
 ;;              ;; uploaded for your enjoyment and use.
 ;;              )
(package-initialize)
(defalias
 ;; always use ibuffer
 'list-buffers 'ibuffer)
;; (require 'undo-tree
;;  ;; undo-tree allows you to visual the whole history of your editing
;;  ;; in a tree. It also provides regular undo/redo behaviours in other
;;  ;; editors. undo-tree can even provide a diff between two different
;;  ;; states. Highly recommended.
;; )
;; (global-undo-tree-mode t
;;  ;; If you want to replace the standard Emacs' undo system with
;;  ;; the 'undo-tree-mode' system in all buffers, you can enable it
;;  ;; globally.
;; )
;; ;; (undo-tree-auto-save-history t
;;  ;; When non-nil, ‘undo-tree-mode’ will save undo history to file
;;  ;; when a buffer is saved to file.
;;  ;; It will automatically load undo history when a buffer is loaded
;;  ;; from file, if an undo save file exists.
;;  ;; By default, undo-tree history is saved to a file called
;;  ;; ".<buffer-file-name>.~undo-tree~" in the same directory as the
;;  ;; file itself. To save under a different directory, customize
;;  ;; ‘undo-tree-history-directory-alist’ (see the documentation for
;;  ;; that variable for details).
;; ;; )
;; ;; (undo-tree-visualizer-diff t
;;  ;; When non-nil, display diff by default in undo-tree visualizer.
;;  ;; You can always toggle the diff display using d, regardless of the
;;  ;; setting of this variable.
;; ;; )
(require 'flycheck
 ;; supports over 30 programming and markup languages with more than
 ;;   60 different syntax checking tools
 ;; fully automatic, fail-safe, on-the-fly syntax checking in
 ;;   background
 ;; nice error indication and highlighting
 ;; optional error list pop-up
 ;; many customization options
 ;; a comprehensive manual
 ;; a simple interface to define new syntax checkers
 ;; a "doesn't get in your way" guarantee
 ;; many 3rd party extensions
)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)
 ;; Provide an error display function to show errors in
 ;; a tooltip.
;; )
;; Proselint via Emacs flycheck.  Based on Linting Prose in Emacs…
;; Needs proselint installed: sudo port install proselint
;; Also needs a flycheck checker defined:
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (gfm-mode
          markdown-mode
          org-mode
          text-mode))
(add-to-list 'flycheck-checkers 'proselint)
;;(require 'info+
 ;; info+ helps you read Info documentation mere enjoyable with extra
 ;; highlighting it provides
;;)
;;(require 'help+
 ;; extensions to 'help.el' for Emacs
;;)
;;(require 'help-fns+
 ;; extensions to 'help-fns.el'
;;)
;;(require 'help-mode+
 ;; extensions to 'help-mode.el'
 ;; Links to libraries are provided whenever library names appear in
 ;;   buffer '*Help*'. After loading help-mode+.el, library names in
 ;;   buffer Help have mouse-over links to the corresponding library
 ;;   code.
 ;; For example, 'C-h v features' describes the variable 'features';
 ;;   this description list all of the libraries currently loaded in
 ;;   Emacs.
 ;;   In vanilla Emacs (without help-mode+.el loaded), the library
 ;;     names are not linked, unless a library (such as 'grep')
 ;;     happens to have the same name as an Emacs function or
 ;;     variable, in which case clicking the name displays
 ;;     the function or variable description in buffer Help.
 ;;   With help-mode+.el loaded, each library name in
 ;;     the 'C-h v features' list is linked to the library (code)
 ;;     itself. Click a name to edit/view the library file.
;;)
 ;; (require 'sr-speedbar
 ;;   ;; now you type windows key with 's' ('s-s' in Emacs) will show
 ;;   ;; the speedbar in an extra window, same frame. You can customize
 ;;   ;; the initial width of the speedbar window.
 ;; )
 ;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(require 'cc-mode
 ;; enable code completion using Semantic
)
;; (require 'cedet)
(require 'semantic)
(require 'semantic/ia
 ;; to enable more advanced functionality for name completion, etc.
)
(require 'semantic/bovine/gcc) ;; give access to system include files
;; (semantic-add-system-include "/usr/include/c++/9" 'c++-mode)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive)
 ;; Limit search by customization of the semanticdb-find-default-throttle
 ;; variable for concrete modes — for example, don't use information from
 ;; system include files, by removing system symbol from list of objects to
 ;; search for c-mode.
)
;; (when (cedet-gnu-global-version-check t)
;;   ;; if you want to enable support for gnu global
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)
;; )
;; (when (cedet-ectag-version-check t)
;;  ;; enable ctags for some languages:
;;  ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;;   (semantic-load-enable-primary-exuberent-ctags-support))
;; (defun my-cedet-hook ()
;;   "Key bindings for standalone CEDET."
;;   ;; Some of these features are implemented by semantic/ia package, while
;;   ;; other are implemented by Senator, and Semantic's kernel.
;;   (local-set-key [(control-return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
;; ;; (add-hook 'c-mode-common-hook 'my-cedet-hook)
;; (add-hook 'prog-mode-hook 'my-cedet-hook)
;; (defun my-c-mode-cedet-hook ()
;;   "Display a list of possible completions."
;;   ;; Evaluation of this code will lead to execution of the
;;   ;; semantic-complete-self-insert command when user will press . or > after
;;   ;; variables, that are instances of some data structure, and displaying
;;   ;; a list of possible completions for given class or structure.
;;  (local-set-key "." 'semantic-complete-self-insert)
;;  (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
(require 'stickyfunc-enhance
 ;; One of the problem with current semantic-stickyfunc-mode is that it does not
 ;; display all parameters that are scattered on multiple lines.  This package
 ;; handles that problem: semantic-stickyfunc-enhance.  Extra: stock
 ;; semantic-stickyfunc-mode does not include assigned values to function
 ;; parameters of Python. This package also fixed that problem.
)
(semantic-mode t
 ;; Emacs parses the buffers you visit for their semantic content.
 ;; ‘Semantic’ is a package that provides language-aware editing commands
 ;; based on source code parsers. Parsing is a process of analyzing source
 ;; code based on programming language syntax. Emacs understands your source
 ;; code through this process to provides features such as contextual code
 ;; completion, code navigation.
 ;;
 ;; ‘global-semanticdb-minor-mode’        - Maintain tag database.
 ;;  Semantic caches parsing result for future use.  To do that,
 ;;  semanticdb-minor-mode must be activated.  In Semantic DB mode, Semantic
 ;;  parsers store results in a database, which can be saved for future Emacs
 ;;  sessions.  The cache is saved in directory specified by
 ;;  semanticdb-default-save-directory variable.  The default directory is
 ;;  ~/.emacs.d/semanticdb.
 ;; ‘global-semantic-idle-scheduler-mode’ - Reparse buffer when idle.
 ;;  When semantic-idle-scheduler-mode is enabled, Emacs periodically checks to
 ;;  see if the buffer is out of date, and reparses while the user is idle (not
 ;;  typing).  When this mode is off, a buffer is only reparsed when user
 ;;  explicitly issue some command.  With semantic-idle-scheduler-mode, Emacs
 ;;  keeps track live changes of your source code.
 ;; ‘global-semantic-idle-summary-mode’   - Show summary of tag at point.
 ;;  When enabled, displays functioninterface in the minibuffer.  It works well
 ;;  for C but not C++, since C++ can overload function and
 ;;  semantic-idle-summary-mode can only display one interface at a time.  Since
 ;;  this mode is part of Semantic, it also relies on SemanticDB that is created
 ;;  from parsing source files.
 ;; ‘global-semantic-idle-completions-mode’ - Show completions when idle.
 ;; ‘global-semantic-decoration-mode’     - Additional tag decorations.
 ;; ‘global-semantic-highlight-func-mode’ - Highlight the current tag.
 ;; ‘global-semantic-stickyfunc-mode’     - Show current fun in header line.
 ;;  When enable, this mode shows the function point is currently in at the
 ;;  first line of the current buffer.  This is useful when you have a very
 ;;  long function that spreads more than a screen, and you don't have to scroll
 ;;  up to read the function name and then scroll down to original position.
 ;; ‘global-semantic-mru-bookmark-mode’   - Provide ‘switch-to-buffer’-like
 ;;                                         keybinding for tag names.
 ;; ‘global-cedet-m3-minor-mode’          - A mouse 3 context menu.
 ;; ‘global-semantic-idle-local-symbol-highlight-mode’ - Highlight references
 ;;                                         of the symbol under point.
)
;; (global-semanticdb-minor-mode t
 ;; in Semantic DB mode, Semantic parsers store results in a database,
 ;; which can be saved for future Emacs sessions. The cache is saved
 ;; in derectory specified by semanticdb-default-save-directory
 ;; variable. The default directory is ~/.emacs.d/semanticdb
;; )
;; (global-semantic-idle-scheduler-mode t
 ;; Emacs periodically checks to see if the buffer is out of date,
 ;; and reparses while the user is idle (not typing).
;; )
(add-hook 'c-mode-common-hook #'hs-minor-mode
 ;; Fold and hide blocks of text.  Blocks are defined by regular
 ;; expressions which match the start and end of a text region.
 ;; For example, anything in between { and } is a block.  The regular
 ;; expressions are defined in hs-special-modes-alist.
)
;; (require 'auto-complete)
;; ;; (add-hook 'prog-mode-hook 'auto-complete-mode
;;  ;; an interactive autoloaded compiled Lisp function
;; ;; )
;; (require 'auto-complete-config)
;; (require 'yasnippet
;;  ;; YASnippet is a template system for Emacs.  It allows you to type an
;;  ;; abbreviation and automatically expand it into function templates.  Bundled
;;  ;; language templates include: C, C++, C#, Perl, Python, Ruby, SQL, LaTeX,
;;  ;; HTML, CSS and more.  The snippet syntax is inspired from TextMate's syntax,
;;  ;; you can even import most TextMate templates to YASnippet.
;; )
;; (yas-global-mode 1)
;; (ac-config-default) ;; has `ac-source-gtags` and `ac-source-yasnippet` in a mode
;; (add-hook 'prog-mode-hook
;;  ;; Instead of bounding keys for semantic-ia-complete-symbol-menu,
;;  ;; semantic-ia-complete-symbol & other functions, you can simply add new
;;  ;; names completion sources, and after that use auto-complete bindings to
;;  ;; get names completion:
;;           #'(lambda ()
;;              (add-to-list 'ac-sources 'ac-source-gtags)
;;              (add-to-list 'ac-sources 'ac-source-semantic)))
;; (require 'auto-complete-c-headers)
;; (require 'ac-c-headers)
;; (add-hook 'c-mode-common-hook
;;  ;; Require this script (and auto-complete) then add to ac-sources.
;;           #'(lambda ()
;;               (add-to-list 'ac-sources 'ac-source-c-headers)
;;               (add-to-list 'ac-sources 'ac-source-c-header-symbols t))
;;  ;; Then header filenames and symbols in imported headers are completed.
;; )
(defun c-indent-to-the-end-of-line ()
  "Indent text so that it ends at the end of line."
  (let (new-indent)
    (save-excursion
      (end-of-line)
      (setq new-indent (- c-max-one-liner-length (current-column)))
      (if (> new-indent 0)
          (progn (beginning-of-line)
                 (indent-to new-indent))
        (progn (back-to-indentation)
               (setq new-indent (+ (current-column) new-indent))
               (indent-to-left-margin)
               (indent-to new-indent))))))
;; (defun c-add-indentation-after-curly-bracket (_langelem)
;;   "Add a space after the first curly bracket on a line.
;; } calls twice, delete spaces
;; ; calls once, insert spaces
;; : calls once, delete spaces if public, private, or protected"
;;   (let ((spaces (1- c-basic-offset)) (char (char-before)))
;;     (save-excursion
;;       (cond ((eq ?} char) ;; delete space
;;              (back-to-indentation)
;;              (when (eq ?{ (char-after))
;;                (forward-char)
;;                (if (eq ?  (char-after)) ;; space?
;;                    (delete-char spaces))))
;;             ((eq ?\; char) ;; insert space
;;              (back-to-indentation)
;;              (when (eq ?{ (char-after))
;;                (forward-char)
;;                (unless (eq ?  (char-after))     ;; not a space
;;                    (insert-char ?  spaces t)))) ;; add spaces
;;             ((eq ?: char)
;;              (when (eq (c-langelem-sym _langelem) 'class-open)
;;                (back-to-indentation)
;;                (when (looking-at "{ ")
;;                  (forward-char)
;;                  (skip-chars-forward " \t")
;;                  (when (or (looking-at "public")
;;                            (looking-at "private")
;;                            (looking-at "protected"))
;;                    (back-to-indentation)
;;                    (forward-char)
;;                    (delete-char spaces)))))))))
(defconst danika-cpp-style ;; Check with "\C-c\C-s" from ‘c-offsets-alist’
  `((c-basic-offset . 2) ;; Amount of basic offset used by + and - symbols in c-offsets-alist.
    (c-recognize-knr-p . nil) ;; If this variable is non-nil, C mode and Objective C mode recognize
            ;; K&R constructs.  This variable is needed because of ambiguities in C syntax that make
            ;; recognition of K&R constructs problematic and slow.  If you always use ANSI C prototype
            ;; syntax, set this variable to nil to speed up C indentation.  This variable is nil by
            ;; default in C++ mode, and t by default in C mode and Objective C mode.
    ;; (c-special-indent-hook . c-gnu-impose-minimum) ;; Hook for user-defined special indentation
                           ;; adjustments.  This hook gets called after a line is indented by the mode.
    (c-enable-xemacs-performance-kludge-p . t) ;; speed up indentation in XEmacs
    (indent-tabs-mode . nil)
    (c-tab-always-indent . nil) ;; If t, hitting TAB always just indents the current line.  If nil,
                                ;; hitting TAB indents the current line if point is at the left margin
                                ;; or in the line’s indentation, otherwise it inserts a ‘real’ tab
                                ;; character.
    (c-syntactic-indentation . t) ;; If nil, the line is just indented one step according to
                                  ;; `c-basic-offset'.  If t, indentation is done according to
                                  ;; the syntactic context in `c-offsets-alist'.
    (c-insert-tab-function . c-indent-to-the-end-of-line) ;; tab-to-tab-stop)
    (c-comment-prefix-regexp . "//+\\|\\**") ;; When a comment line gets divided by M-j or the like,
        ;; CC Mode inserts the comment line prefix from a neighboring line at the start of the new line.
    (c-comment-only-line-offset . 0) ;; Extra offset for line which contains only the start of a
                                     ;; comment.  It can be either an integer or a cons cell of the
                                     ;; form (non-anchored-offset . anchored-offset), where
                                     ;; non-anchored-offset is the amount of offset given to
                                     ;; non-column-zero anchored comment-only lines, and
                                     ;; anchored-offset is the amount of offset to give column-zero
                                     ;; anchored comment-only lines.  Just an integer as value is
                                     ;; equivalent to (val . 0).
    (c-block-comment-prefix . "* ") ;;    It’s only used when a one-line block comment is broken into
         ;; two or more lines for the first time; otherwise the appropriate prefix is adapted from the
         ;; comment.  This variable is not used for C++ line style comments.
    (c-hanging-braces-alist . ( ;; from `c-offsets-alist` variable
                               (defun-open after) ;; On a brace that opens a function definition.
                                                  ;; Makes `int* test{` go on a new line.
                               (defun-close . c-snug-1line-defun-close) ;; On a brace that closes
                                                                        ;; a function definition.
                               (defun-block-intro)
                               (class-open after) ;; Brace that opens a class definition.
                               (class-close before after)
                               (inexpr-class-open after) ;; The class is inside an expression.  Used
                                                         ;; e.g. for Java anonymous classes.
                               (inexpr-class-close before) ;; On a brace that closes a class definition.
                               (inline-open after) ;; Brace that opens an in-class inline method.
                               (inline-close before after) ;;Brace that closes an in-class inline method.
                               (block-open after) ;; Statement block open brace.
                               (block-close . c-snug-do-while) ;; Statement block close brace.
                               (statement) ;; A C (or like) statement.
                               (statement-cont) ;; A continuation of a C (or like) statement.
                               (statement-case-open after) ;; On the first line in a case block
                                                                  ;; starting with brace.
                               (statement-block-intro) ;; The first line in a new statement block.
                               (substatement) ;; The first line after
                                                           ;; an if/while/for/do/else.
                               (substatement-open after) ;; The brace that opens a substatement block.
                               (brace-list-open) ;; Open brace of an enum or static array list.
                               (brace-list-close) ;; Close brace of an enum or static array list.
                               (brace-list-intro) ;; First line in an enum or static array list.
                               (brace-list-entry) ;; Subsequent lines in an enum or static array list.
                               (brace-entry-open) ;; Subsequent lines in an enum or static array
                                                  ;; list that start with an open brace.
                               (extern-lang-open after) ;; Brace that opens an "extern" block.
                               (extern-lang-close after)
                               (namespace-open after) ;; Similar to the three ‘extern-lang’
                                                             ;; symbols, but for C++ "namespace" blocks.
                               (namespace-close before)
                               (module-open) ;; Similar to the three ‘extern-lang’ symbols,
                                             ;; but for CORBA IDL "module" blocks.
                               (module-close)
                               (composition-open) ;; Similar to the three ‘extern-lang’ symbols,
                                                        ;; but for CORBA CIDL "composition" blocks.
                               (composition-close)
                               (arglist-cont-nonempty) ;; On one of the subsequent argument list lines
                                                       ;; when at least one argument follows on the
                                                       ;; same line as the arglist opening parenthesis.
                               (else-clause) ;; The `else' of an if-else construct.
                               (topmost-intro) ;; The first line in a topmost construct
                                               ;; definition.
                               (topmost-intro-cont)
                               (member-init-intro)  ;; First line in a member initialization list.
                               (inher-intro) ;; First line of a multiple inheritance list.
                               (case-label) ;; A case or default label.
                               (access-label) ;; C++ private/protected/public access label.
                               (do-while-closure) ;; The `while' that ends a do-while construct.
                               (inlambda) ;; In the header or body of a lambda function.
                               (inexpr-statement) ;; A statement block inside an expression.
                               ))
    (c-hanging-colons-alist . ((case-label) ;; A case or default label.
                               (label after)
                               (access-label after)
                               (member-init-intro before) ;; MyClass(int arg) :
                               (inher-intro) ;; First line of a multiple inheritance list.
                               ))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist
                                      c-semi&comma-no-newlines-before-nonblanks)
                                   )
    (c-indent-comments-syntactically-p . t)
    (comment-column . 20)
    (comment-fill-column . nil)
    (fill-column . 48)
    (c-max-one-liner-length . 48)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (  ;; List of various C/C++/ObjC constructs to "clean up".  The following
                         ;; clean ups only take place when the auto-newline feature is turned
                         ;; on, as evidenced by the ‘/la’ appearing next to the mode name:
                       brace-else-brace ;; Clean up `} else {' constructs by placing entire
                                        ;; construct on a single line.  The clean-up occurs when
                                        ;; you type the `{' after the else, but only if there is
                                        ;; nothing but white space between the braces and the else.
                       brace-elseif-brace ;; Similar to brace-else-brace, but clean up
                                          ;; "} else if (...) {" constructs.  Clean up
                                          ;; occurs after the open parenthesis and the open brace.
                       brace-catch-brace ;; Similar to brace-elseif-brace, but clean up
                                         ;; "} catch (...) {" constructs.
                       empty-defun-braces ;; Clean up empty defun braces by placing the
                                          ;; braces on the same line.  Clean up occurs when
                                          ;; the defun closing brace is typed.
                       one-liner-defun ;; If the code inside a function body can fit in
                                       ;; a single line, then remove any newlines
                                       ;; between that line and the defun braces so that
                                       ;; the whole body becomes a single line.
                                       ;; ‘c-max-one-liner-length’ gives the maximum
                                       ;; length allowed for the resulting line.  Clean
                                       ;; up occurs when the closing brace is typed.
                       defun-close-semi ;; Clean up the semicolon after a struct or similar type
                                        ;; declaration, by placing the semicolon on the same line as
                                        ;; the closing brace.  Clean-up occurs when you type the
                                        ;; semicolon.
                       list-close-comma ;; Clean up commas following braces in array
                                        ;; and aggregate initializers.  Clean up occurs
                                        ;; when the comma is typed.  The space before the
                                        ;; comma is zapped just like the space before the
                                        ;; semicolon in defun-close-semi.
                       scope-operator ;; Clean up double colons which may designate
                                      ;; a C++ scope operator split across multiple
                                      ;; lines.  Note that certain C++ constructs can
                                      ;; generate ambiguous situations.  This clean up
                                      ;; only takes place when there is nothing but
                                      ;; whitespace between colons.  Clean up occurs
                                      ;; when the second colon is typed.
                         ;; The following clean ups always take place when they are on this list,
                         ;; regardless of the auto-newline feature, since they typically don’t
                         ;; involve auto-newline inserted newlines:
                       ;; space-before-funcall ;; Insert exactly one space before the opening
                                               ;; parenthesis of a function call.  Clean up
                                               ;; occurs when the opening parenthesis is typed.
                       ;; compact-empty-funcall ;; Clean up any space before the function call
                                                ;; opening parenthesis if and only if the
                                                ;; argument list is empty.  This is typically
                                                ;; useful together with ‘space-before-funcall’ to
                                                ;; get the style "foo (bar)" and "foo()".
                                                ;; Clean up occurs when the closing parenthesis
                                                ;; is typed.
                       comment-close-slash ;; When a slash is typed after the comment prefix
                                           ;; on a bare line in a c-style comment, the comment
                                           ;; is closed by cleaning up preceding space and
                                           ;; inserting a star if needed.
                       ))
    (c-offsets-alist . ( ;; Association list of syntactic element symbols and their indentation offsets.  Check with "\C-c\C-o"
                        (arglist-intro . ++) ;; c-lineup-arglist-intro-after-paren ++)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . c-lineup-comment)
                        (arglist-cont c-lineup-argcont c-lineup-gcc-asm-reg)
                        (arglist-cont-nonempty c-lineup-argcont c-lineup-arglist)
                        (arglist-close . 0) ;; c-lineup-arglist) ;; c-lineup-close-paren)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0) ;; c-add-indentation-after-curly-bracket)
                        (substatement-open . 0) ;; c-add-indentation-after-curly-bracket)
                        (statement-cont c-lineup-assignments
                                        c-lineup-cascaded-calls
                                        c-lineup-ternary-bodies
                                        c-lineup-string-cont
                                        +)
                              ;; .
                              ;; (,(when (fboundp 'c-no-indent-after-java-annotations)
                              ;;    'c-no-indent-after-java-annotations)
                              ;;  ,(when (fboundp 'c-lineup-assignments)
                              ;;    'c-lineup-assignments)
                              ;; ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ;; case w/o {
                        (access-label . /)
                        (innamespace . 0)
                        (knr-argdecl-intro . +)
                        (substatement-label . 2)
                        (brace-list-open . 0)
                        (brace-list-intro c-lineup-class-decl-init-after-brace +)
                        (class-open . 0) ;; c-add-indentation-after-curly-bracket)
                        (knr-argdecl . 0)
                        (annotation-top-cont . 0)
                        (annotation-var-cont . +)
                        (brace-list-close . 0) ;; c-lineup-whitesmith-in-block)
                        (brace-entry-open . 0)
                        (do-while-closure . 0)
                        (cpp-macro-cont . +)
                        (cpp-define-intro c-lineup-cpp-define +)
                        (friend . 0)
                        (objc-method-intro . [0])
                        (objc-method-args-cont . c-lineup-ObjC-method-args)
                        (objc-method-call-cont c-lineup-ObjC-method-call-colons
                                               c-lineup-ObjC-method-call
                                               +)
                        (extern-lang-open . 0)
                        (namespace-open . 0)
                        (module-open . 0)
                        (composition-open . 0)
                        (extern-lang-close . 0)
                        (module-close . 0)
                        (composition-close . 0)
                        (inextern-lang . +)
                        (inmodule . +)
                        (incomposition . +)
                        (template-args-cont c-lineup-template-args +)
                        (inlambda . c-lineup-inexpr-block)
                        (lambda-intro-cont . +)
                        (inexpr-statement c-lineup-inexpr-block +)
                        (inexpr-class c-lineup-inexpr-block +)
                        (inclass . +)
                        (defun-block-intro . +) ;; c-lineup-arglist-intro-after-paren +) ;; c-lineup-whitesmith-in-block +)
                        (inline-close . 0) ;; c-lineup-whitesmith-in-block)
                        (class-close . 0)
                        (statement . 0) ;; c-add-indentation-after-curly-bracket)
                        (defun-close . 0) ;; c-lineup-whitesmith-in-block)
                        (topmost-intro-cont . c-lineup-topmost-intro-cont)
                        (statement-block-intro . +) ;; c-lineup-arglist-intro-after-paren +) ;; c-lineup-whitesmith-in-block +)
                        (block-close . 0) ;; c-lineup-whitesmith-in-block)
                        (namespace-close . 0)
                        (else-clause . 0)
                        (catch-clause . 0)
                        (substatement . +)
                        (defun-open . 0) ;; c-add-indentation-after-curly-bracket)
                        (stream-op . c-lineup-streamop)
                        (brace-list-entry . c-lineup-class-decl-init-after-brace)
                        (member-init-cont . c-lineup-multi-inher)
                        (c . 0) ;; c-lineup-C-comments)
                        (inher-cont . c-lineup-multi-inher)
                        (string . -1000)
                        (cpp-macro . -1000)
                        ))
    ) "Danika's C++ Programming Style.")
;;(require 'filladapt
;; These functions enhance the default behavior of Emacs' Auto Fill
;; mode and the commands fill-paragraph, lisp-fill-paragraph,
;; fill-region-as-paragraph and fill-region.
;;
;; The chief improvement is that the beginning of a line to be
;; filled is examined and, based on information gathered, an
;; appropriate value for fill-prefix is constructed.  Also the
;; boundaries of the current paragraph are located.  This occurs
;; only if the fill prefix is not already non-nil.
;;
;; The net result of this is that blurbs of text that are offset
;; from left margin by asterisks, dashes, and/or spaces, numbered
;; examples, included text from USENET news articles, etc.. are
;; generally filled correctly with no fuss.
;;)
;;(setq-default filladapt-mode t
;; Note that in this release Filladapt mode is a minor mode and it is
;; _off_ by default.  If you want it to be on by default, use
;;)
(defun danika-set-cpp-style ()
  "Set the current buffer's c-style to Danika's Programming Style.
Meant to be added to `c-mode-common-hook'."
  (interactive)
  ;;(c-setup-filladapt) ;; tunes the relevant variables in Filladapt for use in CC Mode.
  ;; (filladapt-mode 1)
  (c-add-style "Danika" danika-cpp-style t)) ;; (c-add-style name values use-now)
(add-hook 'c-mode-common-hook #'danika-set-cpp-style)
;; (require 'google-c-style
;;  ;; Provides the google C/C++ coding style, and `load`s it from '.el'
;; )
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent
;;  ;; If you want the RETURN key to go to the next line and space
;;  ;; over to the right place.
;; )
(add-hook 'c-mode-common-hook #'(lambda ()
 ;; (c-add-style "DANIKA" danika-cpp-style t)
 ;; (require 'cc-mode)
 ;; (add-hook 'prog-mode-hook '(lambda ()
 ;; (add-hook 'c-mode-common-hook '(lambda ()
 (c-toggle-auto-newline 1)    ;; Toggle auto-newline feature.
 ;; When the auto-newline feature is enabled (indicated by "/la" on
 ;; the mode line after the mode name) newlines are automatically
 ;; inserted after special characters such as brace, comma,
 ;; semi-colon, and colon.
 (c-toggle-hungry-state 1)       ;; Toggle hungry-delete-key feature.
 ;; When the hungry-delete-key feature is enabled (indicated by "/h"
 ;; on the mode line after the mode name) the delete key gobbles all
 ;; preceding whitespace in one fell swoop.
 (subword-mode 1)
 ;; This mode makes basic word movement commands like M-f (forward-word) and
 ;; M-b (backward-word) treat the parts of sillycapsed symbols as different
 ;; words. E.g., ‘NSGraphicsContext’ is treated as three words ‘NS’, ‘Graphics’,
 ;; and ‘Context’.
 ))
(defun c-snug-hanging-closing-curly-braces ()
  "Gather up all hanging } to one line."
  (interactive)
  (let (indentation (line 0) (column 0))
    (if (or (bolp)
            (eq ?  (char-before)))
        (c-hungry-delete-backwards)) ;; get to the last }
    (if (eq ?\; (char-before)) ;; skip class ending
        (backward-char))
    (catch 'done
      (while (eq ?} (char-before))
        (backward-char) ;; skip }
        (insert-char ?\n 1 t) ;; insert new line for guessing syntax
        (save-excursion ;; check syntax and find indentation
          (goto-char (c-langelem-pos (car (c-guess-basic-syntax))))
          (setq line (line-number-at-pos nil nil))
          (setq column (current-column))) ;; return
        (c-hungry-delete-backwards) ;; clear new line
        (if (not (eq line (line-number-at-pos nil nil))) ;; skip inline function
            (setq indentation (cons column indentation))
          (forward-char) ;; keep } of inline function
          (throw 'done t)))) ;; quit the loop
    (insert-char ?\n 1 t) ;; put "}}} on a new line
    (setq indentation (reverse indentation))
    (while indentation ;; make " } } }"
      (insert-char ?  (- (car indentation) (current-column)) t) ;; space
      (forward-char)
      (setq indentation (cdr indentation)))
    (if (eq ?\; (char-after)) ;; skip class ending
        (forward-char))
    (insert-char ?\n 1 t)))
(define-key c++-mode-map (kbd "C-c }") 'c-snug-hanging-closing-curly-braces)
;; (require 'ggtags
;;  ;; Source code navigation
;; )
;; (add-hook 'c-mode-common-hook
;;           #'(lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;                 (ggtags-mode 1))))
;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;; (define-key ggtags-mode-map (kbd "M-,")     'pop-tag-mark)
;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index
;;  ;; The Imenu facility offers a way to find the major definitions, such as
;;  ;; function definitions, variable definitions in a file by name.
;; )
;; (global-set-key (kbd "<f5>") #'(lambda ()
;;                                  (interactive)
;;                                  (setq-local compilation-read-command nil)
;;                                  (call-interactively 'compile)))
;; (require 'eimp
;;  ;; This package allows interactive image manipulation from within
;;  ;; Emacs. It uses the mogrify utility from ImageMagick to do
;;  ;; the actual transformations.
;; )
;; (autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
;; (add-hook 'image-mode-hook 'eimp-mode
;;  ;; Switch the minor mode on for all image-mode buffers.
;; )
(require 'whitespace
 ;; WhitespaceMode
 ;; whitespace mode (part of Emacs 23+) has two interesting options
 ;; you can set in variable whitespace-style:
 ;; 'lines', will highlight lines that go beyond the columns limit
 ;; defined in 'whitespace-line-column';
 ;; 'lines-tail', same as above but only the part that goes beyond the
 ;; limit of 'whitespace-line-column' gets highlighted.
 ;; 'white-line-column' default value is 80.
 ;; Example config:
)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
 ;; (global-whitespace-mode t)
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'c-mode-common-hook #'whitespace-mode)
(require 'win-switch
 ;; Now, when executing a window switch (i.e., hitting C-xo), Emacs
 ;; enters window switching mode, which lasts until either the user
 ;; exits the mode or the idle time exceeds the threshold
 ;; `win-switch-idle-time'.  During this override, selected keys move
 ;; among windows (or frames) or resize the windows. The following
 ;; keys are bound by default:
 ;;    + i select the window above the current window.
 ;;    + k select the window below the current window.
 ;;    + j select the window left of the current window.
 ;;    + l select the window right of the current window.
 ;;    + o cycle forward through the window list in the current frame.
 ;;    + p cycle backward through the window list in the current frame.
 ;;    + SPACE cycles among existing frames.
 ;;    + u (and RETURN) exit window switching mode.
 ;;    + I and K vertically enlarge and shrink the current window,
 ;;      respectively.
 ;;    + L and J horizontally enlarge and shrink the current window,
 ;;      respectively.
 ;;    + h and ; split the current window, horizontally and vertically,
 ;;      respectively.
 ;;    + ESCAPE acts as an "emergency" exit.
 ;; All other keys exit window switching mode and execute their
 ;; original function.
)
(global-set-key "\C-xo" 'win-switch-dispatch)
;;(require 'ace-window
 ;; The main function, `ace-window' is meant to replace `other-window'.
 ;; In fact, when there are only two windows present, `other-window' is
 ;; called.  If there are more, each window will have its first
 ;; character highlighted.  Pressing that character will switch to that
 ;; window.
 ;; When prefixed with one `universal-argument', instead of switching
 ;; to selected window, the selected window is swapped with current one.
 ;; When prefixed with two `universal-argument', the selected window is
 ;; deleted instead.
;; )
;;(global-set-key (kbd "C-x p") 'ace-window)
;; personal fuctions; now in "~/.emacs.d/lisp/"
;; (load "~/Documents/Samantha/LISP/hhmm.el")
;; (load "~/Documents/Samantha/LISP/nibble.el")
(require 'hhmm)
(require 'nibble)
;; (require 'w3m-load
 ;; Emacs web browser
;; )
;; (setq w3m-key-binding 'info)
(add-hook 'prog-mode-hook #'(lambda ()
  (abbrev-mode -1)                ;; turn off abbrev mode
))
(require 'move-text
 ;; It allows you to move the current line using M-up / M-down if a
 ;; region is marked, it will move the region instead.  Using the
 ;; prefix (C-u *number* or META *number*) you can predefine how
 ;; many lines move-text will travel.
)
(move-text-default-bindings)
(require 'buffer-move
 ;; This file is for lazy people wanting to swap buffers without
 ;; typing C-x b on each window.  With buffer-move, just go in
 ;; #gnus, do buf-move-left, go to #emacs (which now should be on
 ;; top right) and do buf-move-down.
)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
(require 'nov
 ;; nov.el provides a major mode for reading EPUB documents.
 ;; Features:
 ;;     Basic navigation (jump to TOC, previous/next chapter)
 ;;     Remembering and restoring the last read position
 ;;     Jump to next chapter when scrolling beyond end
 ;;     Storing and following Org links to EPUB files
 ;;     Renders EPUB2 (.ncx) and EPUB3 (<nav>) TOCs
 ;;     Hyperlinks to internal and external targets
 ;;     Supports textual and image documents
 ;;     Info-style history navigation
 ;;     View source of document files
 ;;     Metadata display
 ;;     Image rescaling
)
;; Automatically open .epub files with nov-mode.
(map-put auto-mode-alist "\\.epub$" 'nov-mode)
;; Running your own Dictionary Server
;; $ sudo apt-get install dictd dict \
;;     dict-{wn,vera,jargon,devil,gcide,foldoc}
;; $ sudo systemctl enable dictd
;; To make Emacs use your local server instead of a remote one,
;; customize dictionary-server to localhost:
(setq dictionary-server "localhost")
;; That is why I prefer M-x dictionary-lookup-definition as it looks up
;; the definition of the word at point.  I have conveniently bound that
;; to the key M-#.  It’s next to M-x ispell-word (M-$) and to me that
;; seems like as good a place as any.
(require 'osx-dictionary
 ;; Interface for OSX Dictionary.app
)
(if (eq system-type 'darwin)
    (global-set-key (kbd "C-#") 'osx-dictionary-search-word-at-point)
  (global-set-key (kbd "C-#")  'dictionary-lookup-definition))
(global-set-key (kbd "C-$") 'ispell-word)
;; replace ispell with hunspell:
;;   C-h v ispell-program-name  /usr/bin/ispell
;; (as root) aptitude purge ispell ibritish-insane ienglish-common
;;   ingerman irussian wbritish-insane wngerman wukrainian
;; check directories: /usr/share/dict (empty)
;;   /usr/share/ispell (removed)  /usr/lib/ispell (empty)
;;   /var/lib/ispell (empty)
;;   C-h v ispell-program-name "hunspell"
;; (let ((languages '("english" "german")))
;;   (setq ispell-languages-ring (make-ring (length languages)))
;;   (dolist (elem languages) (ring-insert ispell-languages-ring elem)))
;; (defun ispell-cycle-languages ()
;;   "Toggle both ispell distionary and input method with C-\\."
;;   (interactive)
;;   (let ((language (ring-ref ispell-languages-ring -1)))
;;     (ring-insert ispell-languages-ring language)
;;     (ispell-change-dictionary language)
;;     (cond
;;      ((string-match "german" language) (activate-input-method "german-postfix"))
;;      ((string-match "english" language) (deactivate-input-method)))))
;; (define-key (current-global-map) [remap toggle-input-method] 'ispell-cycle-languages)
;; (as root) aptitude install hunspell hunspell-de-de hunspell-en-gb
;;   hunspell-ru hunspell-uk
;; (as user) hunspell -D
;;   VERFÜGBARE WÖRTERBÜCHER:
;;   /usr/share/hunspell/uk_UA /usr/share/hunspell/de_DE
;;   /usr/share/hunspell/de_LU /usr/share/hunspell/de_BE
;;   /usr/share/hunspell/en_GB /usr/share/hunspell/ru_RU
;;   /usr/share/myspell/dicts/hyph_ru_RU
;;   GELADENES WÖRTERBUCH:
;;   /usr/share/hunspell/de_DE.aff /usr/share/hunspell/de_DE.dic
(let ((languages '("en_GB" "de_DE" "uk_UA" "ru_RU")))
  (setq ispell-languages-ring (make-ring (length languages)))
  (dolist (elem languages) (ring-insert ispell-languages-ring elem)))
(defun ispell-cycle-languages ()
  "Toggle both ispell distionary and input method with C-\\."
  (interactive)
  (let ((language (ring-ref ispell-languages-ring -1)))
    (ring-insert ispell-languages-ring language)
    (ispell-change-dictionary language)
    (cond
     ;; Check variable `input-method-alist`.  Use a US keyboard layout
     ;; to type characters from other languages like the german umlauts.
     ;; My preferred way to do this in Emacs is the input-method
     ;; `german-postfix`.  The input "ae" becomes "ä".  ae  -> ä  aee -> ae  oe  -> ö  oee -> oe  ue  -> ü (not after a/e/q)  uee -> ue  sz  -> ß  szz -> sz  E= -> €
     ((string-match "de_DE" language) (activate-input-method "german-postfix"))
     ;; (quail-define-rules ("`" ?`) ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=) ("q" ?й) ("w" ?ц) ("e" ?у) ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("[" ?х) ("]" ?ї) ("\\" ?ґ) ("a" ?ф) ("s" ?и) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о) ("k" ?л) ("l" ?д) (";" ?ж) ("'" ?є) ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?і) ("n" ?т) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?/) ("~" ?~) ("!" ?!) ("@" ?\") ("#" ?#) ("$" ?\%) ("%" ?:) ("^" ?,) ("&" ?\.) ("*" ?\;) ("(" ?\() (")" ?\)) ("_" ?_) ("+" ?\+) ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш) ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ї) ("|" ?Ґ) ("A" ?Ф) ("S" ?И) ("D" ?В) ("F" ?А) ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Є) ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?І) ("N" ?Т) ("M" ?Ь) ("<" ?Б) (">" ?Ю) ("?" ?\?) ("¢" ?$) ("ß" ?ы) ("‘" ?ъ) ("æ" ?э) ("Æ" ?Э) ("«" ?ё) ("’" ?Ъ) ("»" ?Ё))
     ((string-match "uk_UA" language) (activate-input-method "ukrainian-computer")) ;; "ЙЦУКЕН"
     ;; (quail-define-rules ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=) ("|" ?/) ("`" ?ё) ("q" ?й) ("w" ?ц) ("e" ?л) ("r" ?ы) ("t" ?а) ("y" ?щ) ("u" ?ш) ("i" ?д) ("o" ?ж) ("p" ?к) ("[" ?х) ("]" ?ъ) ("a" ?ф) ("s" ?в) ("d" ?п) ("f" ?у) ("g" ?е) ("h" ?р) ("j" ?н) ("k" ?т) ("l" ?г) (";" ?з) ("'" ?э) ("\\" ?\\) ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?и) ("n" ?о) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?.) ("!" ?!) ("@" ?\") ("#" ?№) ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?() (")" ?)) ("_" ?_) ("+" ?+) ("~" ?Ё) ("Q" ?Й) ("W" ?Ц) ("E" ?Л) ("R" ?Ы) ("T" ?А) ("Y" ?Щ) ("U" ?Ш) ("I" ?Д) ("O" ?Ж) ("P" ?К) ("{" ?Х) ("}" ?Ъ) ("A" ?Ф) ("S" ?В) ("D" ?П) ("F" ?У) ("G" ?Е) ("H" ?Р) ("J" ?Н) ("K" ?Т) ("L" ?Г) (":" ?З) ("\"" ?Э) ("|" ?|) ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И) ("N" ?О) ("M" ?Ь) ("<" ?Б) (">" ?Ю) ("?" ?,))
     ((string-match "ru_RU" language) (activate-input-method "russian-computer"))   ;; "ЙЦУКЕН"
     ((string-match "en_GB" language) (deactivate-input-method)))))
(define-key (current-global-map) [remap toggle-input-method] 'ispell-cycle-languages)
;; So what I gather from the thread is that a fix will be applied to
;; Emacs 28 which will make it's way up to Emacs 29.  Meanwhile, one
;; can do something like
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
(require 'password-generator
;; Generate a password and insert it in-place.  Such functions provided:
;; password-generator-numeric  - generate PIN-code or any other numeric password.
;; password-generator-simple   - simple password for most websites.
;; password-generator-phonetic - easy to remember password.
;; password-generator-strong   - strong password and still suitable for most web sites with strange password requirements to used special chars.
;; password-generator-words    - generate rememberable password from top used 1500 english words.
;; password-generator-custom   - generate custome password from your alphabete.
;; Use C-u <length> password-generator-simple to specify length of generated password.  This works with other functions too.
)
(require 'pdf-tools
;; PDF Tools is, among other things, a replacement of DocView for PDF
;; files. The key difference is that pages are not pre-rendered by, say,
;; ~ghostscript~ and stored in the file-system, but rather created
;; on-demand and stored in memory.
;; Some day you might want to update this package via ~git pull~ and
;; then reinstall it. Sometimes this may fail, especially if Lisp-Macros
;; are involved and the version hasn't changed. To avoid this kind of
;; problems, you should delete the old package via ~list-packages~,
;; restart Emacs, run ~make distclean~ and then reinstall the package.
;; | Navigation                                    |                         |
;; |-----------------------------------------------+-------------------------|
;; | Scroll Up / Down by Page-full                 | ~space~ / ~backspace~   |
;; | Scroll Up / Down by Line                      | ~C-n~ / ~C-p~           |
;; | Scroll Right / Left                           | ~C-f~ / ~C-b~           |
;; | First Page / Last Page                        | ~<~, ~M-<~ / ~>~, ~M->~ |
;; | Next Page / Previous Page                     | ~n~ / ~p~               |
;; | Incremental Search Forward / Backward         | ~C-s~ / ~C-r~           |
;; | Occur (list all lines containing a phrase)    | ~M-s o~                 |
;; | Jump to Occur Line                            | ~RETURN~                |
;; | Pick a Link and Jump                          | ~F~                     |
;; | Incremental Search in Links                   | ~f~                     |
;; | History Back / Forwards                       | ~l~ / ~r~               |
;; | Display Outline                               | ~o~                     |
;; | Jump to Section from Outline                  | ~RETURN~                |
;; | Jump to Page                                  | ~M-g g~                 |
;; | Store position / Jump to position in register | ~m~ / ~'~               |
;; |-----------------------------------------------+-------------------------|
;; |                                               |                         |
;; Note that ~pdf-tools~ renders the PDF as images inside Emacs. This
;; means that all the keybindings of ~image-mode~ work on individual PDF
;; pages as well.
;; If you decide to install pdf-tools package (also available on Melpa)
;; to view PDF files (instead of doc-view-mode), you can view the PDF in
;; "midnight mode" by using the default binding C-c C-r m or by doing
;; M-x pdf-view-midnight-minor-mode.
;; The default colors of this minor mode are,
;;     Foreground color - #839496
;;     Background color - #002b36
;; But you can customize those by customizing the variable
;; pdf-view-midnight-colors.
;; From C-h v pdf-view-midnight-colors,
;;     Colors used when `pdf-view-midnight-minor-mode' is activated.
;;     This should be a cons (FOREGROUND . BACKGROUND) of colors.
;; (pdf-tools-install) ;; Standard activation command
)
(pdf-loader-install) ;; On demand loading, leads to faster startup time
;; (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode))
;; ;; automatically turns on midnight-mode for pdfs
;; )
;; MMM Mode is a minor mode for Emacs that allows Multiple Major Modes
;; to coexist in one buffer.
;; First the package needs to be loaded, with either
;;     (require 'mmm-mode)
;; or instead, to save time during emacs startup,
;;     (require 'mmm-auto)
;; Then you will probably want to set something like this:
;;     (setq mmm-global-mode 'maybe)
;;     (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
;; The first line tells MMM Mode to load itself whenever you open an
;; appropriate file, and the second is an example which says to notice
;; PHP regions in html-mode files having a `.php' extension.  Both
;; lines are necessary.
;; User Option: mmm-mode-ext-classes-alist  This global variable
;; associates certain submode classes with major modes and/or file
;; extensions.  Its value is a list of elements of the form
;; (mode ext class).  Any buffer whose major mode is mode (a symbol)
;; and whose file name matches ext (a regular expression) will
;; automatically have the submode class class applied to it.  Elements
;; look like (MODE EXT CLASS), where MODE is a major mode, EXT
;; is a regexp to match a filename such as in ‘auto-mode-alist’, and
;; CLASS is a submode class. CLASS is activated in all buffers in mode
;; MODE (if non-nil) and whose filenames match EXT (if non-nil).  If
;; both MODE and EXT are nil, CLASS is activated in all buffers.  If
;; CLASS is the symbol t, MMM Mode is turned on in all buffers
;; matching MODE and EXT, but no classes are activated.
;; Function: mmm-add-mode-ext-class mode ext class  This function adds
;; an element to mmm-mode-ext-classes-alist, associating the submode
;; class class with the major mode mode and extension ext.
;; If you want a submode class to apply to all files in a certain major
;; mode or with a certain extension, add a line such as this to your
;; initialization file: (mmm-add-mode-ext-class mode extension class)
;; After this call, any file opened whose name matches the regular
;; expression extension and whose default mode is mode will be
;; automatically mmm-ified according to class (assuming mmm-global-mode
;; is non-nil).  If one of extension or mode is nil, a file need only
;; satisfy the other one to be mmm-ified.
;; User Option: mmm-global-mode  There are three possible (meanings of)
;; values for it: t, nil, and anything else.  When this variable is
;; neither nil nor t, MMM Mode is enabled automatically in all buffers
;; that would have associated submode classes; i.e. only if there would
;; be something for it to do.
;; You will, of course, want to change and duplicate the second line
;; according to your needs.  Either of the first two parameters can be
;; `nil', meaning not to consider that criterion.  For example, if all
;; your html files, regardless of extension, are Mason components, you
;; will want something like:
;;       (mmm-add-mode-ext-class 'html-mode nil 'mason)
;; whereas if all your files with a `.nw' extension, regardless of
;; primary mode (some may be LaTeX, others HTML, say) are Noweb, you
;; will prefer
;;       (mmm-add-mode-ext-class nil "\\.nw\\'" 'noweb)
;; Provides syntax highlighting and indentation for CMakeLists.txt and
;; *.cmake source files.
(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode
;; To switch EMacs to black, dark, midnight mode:
;; M-x customize-themes
;;   modus-vivendi
;; C-c C-r t pdf-view-themed-minor-mode
)
(put 'scroll-left 'disabled nil
;; C-x < to move frame's view left; C-x > to move it right
)
(require 'transpose-frame
;; `transpose-frame'  ...  Swap x-direction and y-direction
;;        +------------+------------+      +----------------+--------+
;;        |            |     B      |      |        A       |        |
;;        |     A      +------------+      |                |        |
;;        |            |     C      |  =>  +--------+-------+   D    |
;;        +------------+------------+      |   B    |   C   |        |
;;        |            D            |      |        |       |        |
;;        +-------------------------+      +--------+-------+--------+
;; `flip-frame'  ...  Flip vertically
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |            |     C      |
;;        +------------+------------+      |     A      +------------+
;;        |            D            |      |            |     B      |
;;        +-------------------------+      +------------+------------+
;; `flop-frame'  ...  Flop horizontally
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |     B      |            |
;;        |     A      +------------+      +------------+     A      |
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+------------+
;;        |            D            |      |            D            |
;;        +-------------------------+      +-------------------------+
;; `rotate-frame'  ...  Rotate 180 degrees
;;        +------------+------------+      +-------------------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+     A      |
;;        |            D            |      |     B      |            |
;;        +-------------------------+      +------------+------------+
;; `rotate-frame-clockwise'  ...  Rotate 90 degrees clockwise
;;        +------------+------------+      +-------+-----------------+
;;        |            |     B      |      |       |        A        |
;;        |     A      +------------+      |       |                 |
;;        |            |     C      |  =>  |   D   +--------+--------+
;;        +------------+------------+      |       |   B    |   C    |
;;        |            D            |      |       |        |        |
;;        +-------------------------+      +-------+--------+--------+
;; `rotate-frame-anticlockwise'  ...  Rotate 90 degrees anti-clockwise
;;        +------------+------------+      +--------+--------+-------+
;;        |            |     B      |      |   B    |   C    |       |
;;        |     A      +------------+      |        |        |       |
;;        |            |     C      |  =>  +--------+--------+   D   |
;;        +------------+------------+      |        A        |       |
;;        |            D            |      |                 |       |
;;        +-------------------------+      +-----------------+-------+
)
(require 'markdown-mode)
;; Neither markdown-mode nor gfm-mode do anything specifically with
;; respect to newline behavior.  If you use gfm-mode mostly to write
;; text for comments or issues on the GitHub site–where newlines are
;; significant and correspond to hard line breaks–then you may want to
;; enable visual-line-mode for line wrapping in buffers.  You can do
;; this with a gfm-mode-hook as follows:
(defun my-gfm-mode-hook ()
  "Enable line wrapping in buffers."
  (visual-line-mode 1))
(add-hook 'gfm-mode-hook 'my-gfm-mode-hook)
(require 'languagetool
;; https://github.com/PillFall/languagetool.el
;; Use languagetool as your grammar, orthography and styling checker
;; tool in Emacs.  Confirmed to work on following environment:
;;   (as user) `java --version`
;;   openjdk 17.0.10 2024-01-16
;;   OpenJDK Runtime Environment (build 17.0.10+7-Debian-1deb12u1)
;;   OpenJDK 64-Bit Server VM (build 17.0.10+7-Debian-1deb12u1, mixed mode, sharing)
;; LanguageTool can be downloaded from https://languagetool.org/download/
;;   (as user) `java -jar ~/languagetool/languagetool-commandline.jar --version`
;;   LanguageTool version 6.4 (2024-03-28 14:05:28 +0100, 0e9362b)
;; The N-gram data can be downloaded from https://languagetool.org/download/ngram-data/
;;   (as user) `ls ~/ngram-data/`
;;   de  en
)
;; Was set through `M-x customize`:
;; (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;       languagetool-console-command "~/languagetool/languagetool-commandline.jar"
;;       languagetool-server-command "~/languagetool/languagetool-server.jar"
;;       languagetool-console-arguments '("--languagemodel" "/home/danika/ngram-data") ;; cannot use ~/ngram-data
;;       languagetool-correction-language "en-GB"
;;       languagetool-mother-tongue "de-DE")
;; console mode
;; (as user) /usr/bin/java -Dfile.encoding=UTF-8 -jar /home/danika/languagetool/languagetool-commandline.jar --languagemodel /home/danika/ngram-data --encoding utf8 --json --language en-GB --mothertongue de-DE
;; (as user) /usr/bin/java -Dfile.encoding=UTF-8 -jar /home/danika/languagetool/languagetool-commandline.jar --languagemodel /home/danika/ngram-data --encoding utf8 --json --language de-DE --mothertongue de-DE
(global-set-key "\C-x4l" 'languagetool-set-language) ;; de-DE  en-GB
;; In this mode, when you start checking, the first thing you need to do
;; is call `languagetool-check`.  This will invoke LanguageTool in the
;; current region, if any, and then highlight all the suggestions made
;; by the tool.  If there is no region, the whole available portion of
;; the buffer will check.  This function is synchronous.  Therefore, it
;; blocks Emacs until LanguageTool done with your text.  This is the
;; right behaviour, as LanguageTool is a bit slow checking text in this
;; mode, so it prevents you from changing the text while checking.
(global-set-key "\C-x4w" 'languagetool-check)
;; After LanguageTool highlights all its suggestions, now you can
;; correct your text, then put your cursor on the underlined word and
;; call `languagetool-correct-at-point`, this will pop up a transient
;; minibuffer with all the suggestions, choose the one fits your needs,
;; and you are ready to go.
(global-set-key "\C-x44" 'languagetool-correct-at-point)
;; There is also a buffer wide correction function, called
;; `languagetool-correct-buffer`, you can call it if you want to check
;; all the buffer, suggestion by suggestion.
(global-set-key "\C-x4c" 'languagetool-correct-buffer)
;; If you finish, and don’t want to see any more suggestions, call
;; `languagetool-clear-suggestions` and all the highlighting will
;; disappear.
(global-set-key "\C-x4W" 'languagetool-clear-suggestions)
;;; .emacs ends here

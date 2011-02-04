;;; jtags.el --- enhanced tags functionality for Java development

;; Copyright (C) 2001-2008 Alexander Baltatzis, Johan Dykstrom

;; Author: Alexander Baltatzis <alexander@baltatzis.com>
;;	Johan Dykstrom <jody4711-sourceforge@yahoo.se>
;; Maintainer: Johan Dykstrom <jody4711-sourceforge@yahoo.se>
;; Created: May 2001
;; Version: 0.96
;; Keywords: java, tags, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; The main purpose of `jtags-mode' is to provide an improved tags lookup
;; function for Java source code, compared to the ordinary etags package.
;; While etags knows only the name of the identifier, jtags also knows the
;; context in which the identifier is used. This allows jtags to find the
;; correct declaration at once, instead of the declaration that happens to
;; appear first in the tags table file. The jtags package also contains a
;; function for completing partly typed class members, and functions for
;; managing tags table files.
;;
;; The following interactive functions are included in jtags mode:
;;
;; - jtags-member-completion:      find all completions of the partly typed
;;                                 method or variable name at point
;; - jtags-show-declaration:       look up and display the declaration of the
;;                                 indentifier at point
;; - jtags-show-documentation:     look up and display the Javadoc for the
;;                                 indentifier at point
;; - jtags-update-tags-files:      update all tags table files with the latest
;;                                 source code changes
;; - jtags-update-this-tags-file:  update the tags table file in which the
;;                                 class in the current buffer is tagged
;;
;; Throughout this file, the two terms DECLARATION and DEFINITION are used
;; repeatedly. The DECLARATION of an identifier is the place in the Java source
;; code where the identifier is declared, e.g. the class declaration. The
;; DEFINITION of an identifier is the data structure used by jtags to describe
;; the declaration, containing file name, line number etc.
;;
;; The latest version of jtags mode can always be found at
;; http://jtags.sourceforge.net.

;; Installation:

;; Place "jtags.el" in your `load-path' and place the following lines in your
;; init file:
;;
;; (autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
;; (add-hook 'java-mode-hook 'jtags-mode)

;; Configuration:

;; Add the Emacs "bin" directory to your path, and restart Emacs to make the
;; etags program available to jtags mode.
;;
;; Unzip the source code files that come with the JDK and other products you
;; use, e.g. JUnit. The etags program can only extract information from source
;; code files, and not from class files.
;;
;; Configure the tags table list in your init file. Include the directories
;; where you unzipped the external source code files, and the directories where
;; your project's source code is located.
;;
;; GNU Emacs example:
;;
;; (setq tags-table-list '("c:/java/jdk1.6.0/src"
;;                         "c:/projects/tetris/src"))
;; (setq tags-revert-without-query 't)
;;
;; XEmacs example:
;;
;; (setq tag-table-alist '(("\\.java$" . "c:/java/jdk1.6.0/src")
;;                         ("\\.java$" . "c:/projects/tetris/src")))
;; (setq tags-auto-read-changed-tag-files 't)
;;
;; Type `M-x jtags-update-tags-files' to update all of the files in the tags
;; table list. If you do not have write access to all of the tags table files,
;; e.g. in the JDK installation directory, you can copy the source code tree,
;; or ask the system administrator to create the tags table files for you. If
;; you are running Linux, you can start Emacs using the sudo command once, to
;; create the tags table files.
;;
;; If you want to use jtags mode to display Javadoc files in your web browser,
;; you need to go through some additional steps:
;;
;; Download and unzip the Javadoc files for the JDK and other products you
;; use, e.g. JUnit.
;;
;; Configure the variable `jtags-javadoc-root-list' in your init file. It
;; should be a list of directories to search for Javadoc files. The Javadoc
;; root directory is where the file "index.html" resides, for example:
;;
;; (setq jtags-javadoc-root-list '("c:/java/jdk1.6.0/docs/api"
;;                                 "c:/projects/tetris/docs/api"))
;;
;; The shell command that runs when you update tags table files is defined in
;; the variable `jtags-etags-command'. Change this variable to run a specific
;; version of etags, or to include other source code files in the tags table
;; files.
;;
;; If you want to use the jtags submenu, set `jtags-display-menu-flag' to
;; non-nil. If this variable is non-nil, the jtags submenu will be displayed
;; when jtags mode is active.
;;
;; You can customize all the variables above, as well as the faces used in
;; member completion. Type `M-x customize-group' and enter group "jtags" for
;; the jtags mode variables, or "etags" for the tags table list.
;;
;; The jtags package defines four key bindings in the `jtags-mode-map':
;;
;; - C-,   is bound to `jtags-member-completion'
;; - M-,   is bound to `jtags-show-declaration'
;; - M-f1  is bound to `jtags-show-documentation'
;; - C-c , is bound to `jtags-update-this-tags-file'

;;; Change Log:

;;  0.96  2008-10-03  Added customization of etags command. Changed load method
;;                    to autoload. Moved key bindings to jtags-mode-map. Made
;;                    jtags a minor mode. Added jtags-buffer-tag-table-list
;;                    to parse the tags table list. Improved member completion:
;;                    Members that are not visible in the current scope are now
;;                    excluded. Members in super classes are now included, but
;;                    are displayed using a different face. When completing a
;;                    member in a class (and not an object), only static
;;                    members are included. Function jtags-show-declaration
;;                    now works with pop-tag-mark. Added support for Java 5 for
;;                    loops. Improved Java code parsing.
;;  0.95  2008-02-01  Basic support for Java generics. Fixed "Unbalanced
;;                    parentheses" bug. Final and static members are displayed
;;                    using a different face during member completion. Added
;;                    function jtags-update-this-tags-file. Split function
;;                    jtags-lookup-member in two - jtags-lookup-identifier and
;;                    jtags-lookup-class-members. Renamed jtags-javadoc-root to
;;                    jtags-javadoc-root-list, it must now be a list; a string
;;                    value is not accepted. Added support for customization.
;;                    If a local variable shadows a class member, jtags finds
;;                    the local variable. Improved Java code parsing.
;;  0.94  2006-10-03  Added functions for updating tags table files. Added menu
;;                    support. Split old jtags-find-tag in two - one function
;;                    that shows the declaration, and one that shows the
;;                    Javadoc for a tag. Improved member completion. Improved
;;                    Java and tags table file parsing. Moved test code to
;;                    separate file. Ported to XEmacs 21.4.19.
;;  0.92  2002-02-27  Completing members in local variables work. Point is
;;                    placed after inserted text after completion.
;;  0.91  2001-09-10  Initial version.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cc-mode)
(require 'easymenu)
(require 'easy-mmode)
(require 'etags)

;; ----------------------------------------------------------------------------
;; Customization:
;; ----------------------------------------------------------------------------

(defgroup jtags nil
  "Enhanced tags functionality for Java development."
  :link '(emacs-library-link :tag "Source File" "jtags.el")
  :group 'tools)

;; Faces:

(defface jtags-member-face
  '((t (:bold t)))
  "*Face used to display normal class members."
  :group 'jtags)
(defvar jtags-member-face
  (make-face 'jtags-member-face))

(defface jtags-final-member-face
  '((t (:foreground "seagreen" :bold t)))
  "*Face used to display final class members."
  :group 'jtags)
(defvar jtags-final-member-face
  (make-face 'jtags-final-member-face))

(defface jtags-static-member-face
  '((t (:italic t :bold t)))
  "*Face used to display static class members."
  :group 'jtags)
(defvar jtags-static-member-face
  (make-face 'jtags-static-member-face))

(defface jtags-final-static-member-face
  '((t (:foreground "seagreen" :italic t :bold t)))
  "*Face used to display final static class members."
  :group 'jtags)
(defvar jtags-final-static-member-face
  (make-face 'jtags-final-static-member-face))

(defface jtags-inherited-member-face
  '((t nil))
  "*Face used to display inherited class members."
  :group 'jtags)
(defvar jtags-inherited-member-face
  (make-face 'jtags-inherited-member-face))

(defface jtags-inherited-final-member-face
  '((t (:foreground "seagreen")))
  "*Face used to display inherited final class members."
  :group 'jtags)
(defvar jtags-inherited-final-member-face
  (make-face 'jtags-inherited-final-member-face))

(defface jtags-inherited-static-member-face
  '((t (:italic t)))
  "*Face used to display inherited static class members."
  :group 'jtags)
(defvar jtags-inherited-static-member-face
  (make-face 'jtags-inherited-static-member-face))

(defface jtags-inherited-final-static-member-face
  '((t (:foreground "seagreen" :italic t)))
  "*Face used to display inherited final static class members."
  :group 'jtags)
(defvar jtags-inherited-final-static-member-face
  (make-face 'jtags-inherited-final-static-member-face))

;; Other stuff:

(defcustom jtags-javadoc-root-list nil
  "*List of directories to search for Javadoc files.
Make this variable a list of the root directories for the Javadoc files you
want to browse using jtags. The Javadoc root directory is the directory where
the generated \"index.html\" file resides."
  :type '(repeat file)
  :group 'jtags)

(defcustom jtags-etags-command
  (if (eq system-type 'windows-nt)
      "dir /b /s *.java | etags --declarations --members -o %f -"
    "find `pwd` | egrep java$ | etags --declarations --members -o %f -")
  "*The shell command to run when updating tags table files.
This variable allows you to customize how to update tags table files, e.g.
specify a path to the etags program. The sequence %f will be replaced by
the actual tags table file name when running the command."
  :type 'string
  :group 'jtags)

(defcustom jtags-display-menu-flag 't
  "*Non-nil means that the jtags submenu will be added to the menu bar.
Set this variable to nil if you do not want to use the jtags submenu. If
non-nil, the submenu will be displayed when jtags mode is active."
  :type 'boolean
  :group 'jtags)

(defcustom jtags-use-buffer-tag-table-list-flag nil
  "*Non-nil means use built-in function `buffer-tag-table-list' if available.
Set this variable to nil if you want to use `jtags-buffer-tag-table-list' to
parse `tag-table-alist' instead of the built-in function."
  :type 'boolean
  :group 'jtags)

(defcustom jtags-trace-flag nil
  "*Non-nil means that tracing is ON. A nil value means that tracing is OFF."
  :type 'boolean
  :group 'jtags)

;; ----------------------------------------------------------------------------
;; Generic functions:
;; ----------------------------------------------------------------------------

(defun jtags-uniqify-list (list)
  "Return a copy of LIST with all duplicates removed.
The original list is not modified. Example:

\(jtags-uniqify-list '\(1 3 1 5 3 1\)\) -> \(1 3 5\)"
  (let (unique)
    (while list
      (unless (member (car list) unique)
        (setq unique (cons (car list) unique)))
      (setq list (cdr list)))
    (nreverse unique)))

(defun jtags-filter-list (predicate list)
  "Return a list containing all elements in LIST that satisfy PREDICATE.
The original LIST is not modified. PREDICATE should be a function of one
argument that returns non-nil if the argument should be part of the result
list. Example:

\(jtags-filter-list \(lambda \(x\) \(> x 3\)\) '\(1 2 3 4 5\)\) -> \(4 5\)"
  (let (result)
    (while list
      (if (funcall predicate (car list))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))

(defsubst jtags-rotate-left (src)
  "Rotate the list SRC one position left, and return the result.
The original list is not modified."
  (if src
      (append (cdr src) (list (car src)))))

(defsubst jtags-get-line ()
  "Return the line number of the current buffer position."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defsubst jtags-line-to-point (line)
  "Convert LINE to a position in the current buffer."
  (save-excursion
    (goto-line line)
    (point)))

(defsubst jtags-point-to-line (pos)
  "Convert point POS (a position in the current buffer) to a line number."
  (save-excursion
    (goto-char pos)
    (jtags-get-line)))

(defun jtags-file-name-directory (filename)
  "Return the directory component in file name FILENAME.
Unlike the built-in function `file-name-directory', this function also
normalizes the file name. File name components such as `..' and `~' are
expanded. Back slashes are replaced with forward slashes. The returned
string always ends with a slash."
  (setq filename (file-truename filename))
  (if (string-match "^[A-Za-z]:" filename)
      (setq filename (concat (downcase (substring filename 0 1))
                             (substring filename 1))))
  (if (not (file-directory-p filename))
      (setq filename (file-name-directory filename)))
  (setq filename (file-name-as-directory filename))
  (setq filename (replace-regexp-in-string "\\\\" "/" filename)))

;; ----------------------------------------------------------------------------
;; Private variables:
;; ----------------------------------------------------------------------------

(defconst jtags-version "0.96"
  "The current version of jtags mode.")

;;                              PACKAGE                  TYPE NAME          TYPE ARGS       ARRAY OR SPACE
(defconst jtags-type-regexp "\\([A-Za-z0-9_.]+\\\.\\)*\\([A-Za-z0-9_]+\\)\\(<[^>]+>\\)?\\(\\[\\]\\|[ \t\n]\\)+"
  "Defines a regular expression that matches a Java type.
The matched string is grouped into subexpressions as described below.

subexp   description
------   -----------

1        Package name, including the last dot
2        Type name
3        Generic type arguments, including the angle brackets
4        Array square brackets and/or space")

;; This regexp is copied from "etags.el" in GNU Emacs 22.1
(defconst jtags-tag-line-regexp
  "^\\(\\([^\177]+[^-a-zA-Z0-9_+*$:\177]+\\)?\
\\([-a-zA-Z0-9_+*$?:]+\\)[^-a-zA-Z0-9_+*$?:\177]*\\)\177\
\\(\\([^\n\001]+\\)\001\\)?\\([0-9]+\\)?,\\([0-9]+\\)?\n"
  "This monster regexp matches an etags tag line.
The matched string is grouped into subexpressions as described below.

subexp   description
------   -----------

1        The string to match
2        Not interesting
3        The guessed tag name
4        Not interesting
5        The explicitly-specified tag name
6        The line to start searching at
7        The char to start searching at")

(defvar jtags-javadoc-frameset
  (let ((temp-dir (file-name-as-directory (if (featurep 'xemacs)
                                              (temp-directory)
                                            temporary-file-directory))))
    (concat temp-dir "jtags_frameset_" (user-login-name) ".html"))
  "The file name of the jtags Javadoc frameset.
Included with jtags is a frameset file. This file will be altered in order to
give the fancy Javadocs frames when viewing documentation for a Java class.")

(defvar jtags-last-possible-completions nil
  "Holds a list of the text to be replaced and possible completions.
This variable is used to circle completions.")

(defvar jtags-buffer-name "*jtags*"
  "The name of the jtags temporary buffer.")

;; ----------------------------------------------------------------------------
;; Utility functions:
;; ----------------------------------------------------------------------------

(defun jtags-message (string &rest args)
  "Print a one-line message (see `message') if `jtags-trace-flag' is non-nil.
The first argument is a format control string, and the rest are data to be
formatted under control of the string. The message is printed to the bottom of
the screen, and to the \"*Messages*\" buffer."
  (when jtags-trace-flag
    (save-excursion
      (save-match-data

        ;; Get name of calling function
        (let* ((frame-number 0)
               (function-list (backtrace-frame frame-number))
               (function-name nil))
          (while function-list
            (if (symbolp (cadr function-list))
                (setq function-name (symbol-name (cadr function-list)))
              (setq function-name "<not a symbol>"))
            (if (and (string-match "^jtags-" function-name)
                     (not (string-match "^jtags-message$" function-name)))
                (setq function-list nil)
              (setq frame-number (1+ frame-number))
              (setq function-list (backtrace-frame frame-number))))

          ;; Update argument list
          (setq args (append (list (concat "%s:\t" string) function-name) args)))

        ;; Print message
        (apply 'message args)))))

(defstruct (jtags-definition
            (:constructor jtags-make-definition (file line package class name type)))
  file line package class name type)

(defsubst jtags-class-or-interface-p (definition)
  "Return non-nil if DEFINITION is a class or interface."
  (string-match "^\\(class\\|interface\\)$" (jtags-definition-type definition)))

;; ----------------------------------------------------------------------------
;; GNU Emacs/XEmacs compatibility:
;; ----------------------------------------------------------------------------

(unless (functionp 'replace-regexp-in-string)
  (defsubst replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))

(eval-when-compile
  (when (featurep 'xemacs)
    (defsubst jtags-visit-tags-table-buffer (name)
      (set-buffer (get-tag-table-buffer name))))
  (unless (featurep 'xemacs)
    (defsubst jtags-visit-tags-table-buffer (name)
      (visit-tags-table-buffer name))))

;; This macro is copied from "cc-defs.el" in GNU Emacs 22.1
(defmacro jtags-save-buffer-state (varlist &rest body)
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
          (inhibit-read-only t) (inhibit-point-motion-hooks t)
          before-change-functions after-change-functions
          deactivate-mark
          ,@varlist)
     (unwind-protect
         (progn ,@body)
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

;; ----------------------------------------------------------------------------
;; Main functions:
;; ----------------------------------------------------------------------------

(defun jtags-show-declaration ()
  "Look up the identifier around or before point, and show its declaration.

Find the definition of the identifier in the tags table files. Load and
display the Java source file where the identifier is declared, and move
the point to the first line of the declaration.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark]."
  (interactive)
  (let ((definition (jtags-find-tag)))

    ;; If we found a definition in the tags table, load the file and go to
    ;; the first line of the declaration
    (if (null definition)
        (message "Tag not found!")

      ;; Record whence we came
      (if (featurep 'xemacs)
          (push-tag-mark)
        (ring-insert find-tag-marker-ring (point-marker)))

      (find-file (jtags-definition-file definition))
      (goto-line (jtags-definition-line definition))
      (message "Found %s in %s"
               (jtags-definition-name definition)
               (jtags-definition-file definition)))))

(defun jtags-show-documentation ()
  "Look up the identifier around or before point, and show its Javadoc.

Find the definition of the identifier in the tags table files. Find its
Javadoc documentation using the Javadoc root list, and display it in the
system browser.

See also variable `jtags-javadoc-root-list'."
  (interactive)
  (let ((definition (jtags-find-tag)))
    (if (null definition)
        (message "Tag not found!")
      (if jtags-javadoc-root-list
        (jtags-browse-url definition)))))

(defun jtags-member-completion ()
  "Look up a partly typed identifier around or before point, and complete it.

Find all the classes which the identifier can belong to, find all matching
members in those classes, and display the list of members in the echo area.
Members with certain properties, e.g. 'static', can be displayed using a
different face.

The first member in the list is selected as the default completion. Typing
\\<jtags-mode-map>\\[jtags-member-completion] again will select the next member in the list, and so on.

When completing members in a class (and not an object), only static members
will be displayed."
  (interactive)
  (let ((last-completion nil))

    ;; If first call - find the text to be completed, and all completions
    (if (not (eq last-command 'jtags-member-completion))
        (setq jtags-last-possible-completions (jtags-find-member-completions))

      ;; Not first call, cycle through the possible completions
      (when jtags-last-possible-completions
          (setq last-completion (cadr jtags-last-possible-completions))
          (setq jtags-last-possible-completions
                (cons (car jtags-last-possible-completions)
                      (jtags-rotate-left (cdr jtags-last-possible-completions))))))
    ;; (jtags-message "Possible completions=%S" jtags-last-possible-completions)

    ;; If no completion found
    (if (not jtags-last-possible-completions)
        (message "No completion!")

      ;; If not first call, remove last completion
      (if (eq last-command 'jtags-member-completion)
          (let ((region-length (- (length last-completion)
                                  (length (car jtags-last-possible-completions)))))
            (delete-region (- (point) region-length) (point))))

      ;; Insert only the part that needs to be completed
      (insert (substring (cadr jtags-last-possible-completions)
                         (length (car jtags-last-possible-completions))))

      ;; Display completions
      (if (= (length jtags-last-possible-completions) 2)
          (message "Found one completion: %s"
                   (jtags-format-members (cdr jtags-last-possible-completions)))
        (message "Found %d completions: %s"
                 (1- (length jtags-last-possible-completions))
                 (jtags-format-members (cdr jtags-last-possible-completions)))))))

(defun jtags-format-members (members)
  "Format MEMBERS, a list of class members, for printing in the echo area.
The result is a string where each member is separated by a space, and where
members with certain text properties, e.g. 'static' are fontified using a
different face. See function `jtags-get-tagged-members'."
  (let ((result))
    (dolist (member members result)
      (let ((member-face "jtags-")
            (member-name (copy-sequence member)))

        (if (get-text-property 0 'inherited member)
            (setq member-face (concat member-face "inherited-")))
        (if (get-text-property 0 'final member)
            (setq member-face (concat member-face "final-")))
        (if (get-text-property 0 'static member)
            (setq member-face (concat member-face "static-")))

        (setq member-face (concat member-face "member-face"))

        ;; Convert face name to symbol, and set face on member
        (set-text-properties 0 (length member) (list 'face (intern member-face)) member-name)

        (setq result (concat result (if result " ") member-name))))))

;; ----------------------------------------------------------------------------
;; Functions for finding which class(es) the point is in:
;; ----------------------------------------------------------------------------

(defun jtags-get-surrounding-classes (filename line)
  "Return a list of classes that the point is within.
The list starts with the innermost class, and ends with the outermost class.
FILENAME is the name of the current buffer, and LINE is the line the point is
on."
  (jtags-filter-class-list (jtags-find-classes filename) line))

(defun jtags-filter-class-list (class-list line)
  "Subfunction used by `jtags-get-surrounding-classes'.
Looks for the end of each class in CLASS-LIST and decides if the point is
within that class or not. Returns a list that contains the names of the classes
that the point is in, with the innermost class first and the outermost class
last."
  (if (null class-list)
      nil
    (let* ((class (caar class-list))
           (start-line (cdar class-list)))
      ;; (jtags-message "Class `%s' starts on line %d" class start-line)
      (goto-line start-line)
      (re-search-forward "{")

      ;; Scan buffer to find closing brace, i.e. end-of-class. A scan error
      ;; ("Unbalanced parentheses") usually means that the user is in the
      ;; middle of editing a block or an expression -> the point is in the
      ;; class -> use `point-max' as end-pos.
      (let* ((end-pos (condition-case nil
                          (scan-sexps (1- (point)) 1)
                        (error (point-max))))
             (end-line (jtags-point-to-line end-pos)))
        ;; (jtags-message "Start=%d End=%d Line=%d" start-line end-line line)

        (if (and (>= line start-line) (<= line end-line))
            (cons class (jtags-filter-class-list (cdr class-list) line))
          (jtags-filter-class-list (cdr class-list) line))))))

;; ----------------------------------------------------------------------------
;; Functions for finding base classes for a list of classes:
;; ----------------------------------------------------------------------------

(defun jtags-get-class-list (&optional class)
  "Return the list of classes that are available at point.

The list starts with the class that the point is in, continues with its base
classes, and ends with \"Object\". If the point is inside an inner class, the
list continues with the outer class, and its base classes.

If the optional argument CLASS is specified, the returned list will contain
CLASS and its base classes."
  (let ((buffer (current-buffer))
        (pos (point))
        (src (if (null class)
                 (jtags-get-surrounding-classes (buffer-file-name) (jtags-get-line))
               (list class))))
    ;; (jtags-message "Classes=%S" src)
    (let ((res (jtags-uniqify-list (jtags-get-class-list-iter src))))
      (switch-to-buffer buffer)
      (goto-char pos)
      res)))

(defun jtags-get-class-list-iter (class-list)
  "Return the class hierarchies for all classes in CLASS-LIST as a flat list.
Subfunction used by `jtags-get-class-list'. Example:

\(jtags-get-class-list-iter '(\"String\" \"Integer\")) ->
               (\"String\" \"Object\" \"Integer\" \"Number\" \"Object\")"
  (if (null class-list)
      nil
    (append (jtags-do-get-class-list (car class-list))
            (jtags-get-class-list-iter (cdr class-list)))))

(defun jtags-do-get-class-list (class &optional package-list)
  "Return the class hierarchy for CLASS, ending with class \"Object\".
If we cannot look up CLASS in the tags files, end the class list with CLASS.
The optional argument PACKAGE-LIST is a list of package names, or fully
qualified class names."
  ;; Lookup the class and if found, load its Java file
  ;; Otherwise, end the class list with this class
  (let ((definition (jtags-lookup-identifier class nil package-list))
        (regexp (concat "class\\W+"
                        class
                        "\\W+extends\\W+"
                        "\\([A-Za-z0-9_.]+\\\.\\)*\\([A-Za-z0-9_]+\\)")))
    ;; (jtags-message "Definition of `%s'=%S" class definition)
    (if (null definition)
        (list class)
      (set-buffer (find-file-noselect (jtags-definition-file definition) t))
      (goto-char (point-min))

      ;; Find out which class this class extends
      (if (re-search-forward regexp nil t)
          (let ((super-class (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                package-list)

            ;; If class included a package name
            (if (match-beginning 1)
                (setq package-list (list (concat (buffer-substring-no-properties (match-beginning 1)
                                                                                 (match-end 1))
                                                 "*")))
              (setq package-list (jtags-find-packages "import")))
            (jtags-message "Class `%s' extends class `%s'" class super-class)

            ;; Find out which class the super class extends
            (cons class (jtags-do-get-class-list super-class package-list)))

        ;; If this class does not extend any class, end the class list with
        ;; "Object" (if this class isn't "Object")
        (if (string-equal class "Object")
            (list class)
          (list class "Object"))))))

;; ----------------------------------------------------------------------------
;; Functions for looking up tags:
;; ----------------------------------------------------------------------------

(defun jtags-find-tag ()
  "Look up the identifier around or before point, and return its DEFINITION.
Parse the current buffer, other Java files, and the tags table files in
`tags-table-list' to find the declaration of the identifier. If found, the
DEFINITION of the identifier is returned, see `jtags-lookup-identifier'."
  (let* ((identifiers (jtags-parse-java-line))
         (classes (jtags-get-class-list))
         (definition (jtags-local-tags-lookup identifiers classes)))
    (jtags-message "Local definition=%s" definition)

    ;; If no local definition, find class member definition
    (when (null definition)
      (setq definition (jtags-recursive-tags-lookup identifiers classes))
      (jtags-message "Recursive definition=%s" definition))

    definition))

(defun jtags-recursive-tags-lookup (identifiers classes)
  "Look up an identifier recursively, and return its DEFINITION.
IDENTIFIERS is a list of identifiers, starting with the first identifier
in the expression, and ending with the identifier to look up. CLASSES is
a list of classes to search for the identifier. If the identifier is found,
its DEFINITION is returned, see `jtags-lookup-identifier'.

This function will not look up constructors, as it does not know if an
identifier refers to a constructor or its class. The DEFINITION of the
class will be returned instead."
  (jtags-message "Identifiers=%S, classes=%S" identifiers classes)
  (when identifiers
    (let ((definition nil)
          (looking-for-class (null classes)))

      ;; If no classes to look in, lookup first identifier as a class
      (if looking-for-class
          (setq definition (jtags-lookup-identifier (car identifiers)))

        ;; Special handling of string literals, and keywords class, super, and this
        (cond ((string-equal (car identifiers) "class")
               (setcar identifiers "Class"))
              ((string-equal (car identifiers) "#stringliteral#")
               (setcar identifiers "String"))
              ((string-equal (car identifiers) "super")
               (setcar identifiers (cadr classes)))
              ((string-equal (car identifiers) "this")
               (setcar identifiers (car classes))))

        ;; Look up the first identifier as a member in the first class, but not
        ;; if the identifier might be a constructor. We don't want to look up
        ;; and return a constructor, since we don't know here that the user
        ;; really wanted a constuctor, and not a class. The class will be found
        ;; in a later call to this function.
        (unless (equal (car classes) (car identifiers))
          (setq definition (jtags-lookup-identifier (car classes) (car identifiers)))))
      (jtags-message "Definition=%S" definition)

      ;; If we did not find the identifier, look for it in next class (if we
      ;; were not looking for a class)
      (if (null definition)
          (if (not looking-for-class)
              (jtags-recursive-tags-lookup identifiers (cdr classes)))

        ;; If this is the last identifier in the list, return the result
        (if (= (length identifiers) 1)
            definition

          ;; Otherwise, lookup the next identifier using the class (and its
          ;; base classes) returned from this lookup
          (let ((found-class (if looking-for-class
                                 (jtags-definition-class definition)
                               (jtags-definition-type definition))))
            (jtags-recursive-tags-lookup (cdr identifiers)
                                         (jtags-get-class-list found-class))))))))

(defun jtags-local-tags-lookup (identifiers classes)
  "Look up a local variable definition, and return its DEFINITION.
IDENTIFIERS is a list of identifiers, starting with the first identifier in the
expression, and ending with the identifier to look up. CLASSES is a list of
classes, but only the first one is used to search for the identifier. If the
identifier is found, its DEFINITION is returned, see `jtags-lookup-identifier'."
  (jtags-message "Identifiers=%S, classes=%S" identifiers classes)
  (if (or (null identifiers) (null classes))
      nil
    ;; Use start of class as search boundary
    (let* ((class-def (jtags-lookup-identifier (car classes)))
           (start-line (jtags-definition-line class-def))
           (local-def (jtags-find-declaration (car identifiers) start-line)))
      (jtags-message "Local tag-def=%S" local-def)
      (if local-def
          ;; If there are more identifiers, look them up. Otherwise, return the
          ;; definition of the local variable.
          (if (cdr identifiers)
              (jtags-recursive-tags-lookup (cdr identifiers)
                                           (jtags-get-class-list (car local-def)))
            (jtags-make-definition (buffer-file-name)                    ; file
                                   (cadr local-def)                      ; line
                                   (car (jtags-find-packages "package")) ; package
                                   (car classes)                         ; class
                                   (car identifiers)                     ; name
                                   (car local-def)))))))                 ; type

;; ----------------------------------------------------------------------------
;; Functions for looking up completions:
;; ----------------------------------------------------------------------------

(defun jtags-find-member-completions ()
  "Look up the identifier around point, and return a list of completions.

The returned list will start with the text to be completed, and continue
with all matching members in the class, and in its super classes. Members
that are not visible in the current scope will be discarded, and members
that are defined in a super class will be marked with text property
'inherited'. When looking up members in a class (and not an object), only
static members will be included.

Return nil if there are no matching members."
  (save-excursion
    (let* ((identifiers (reverse (jtags-parse-java-line)))
		   (to-be-completed (car identifiers))
		   (surrounding-classes (jtags-get-class-list))
           lookup-classes
           class-members
           last-identifier
		   definition
		   members)

      (if (null identifiers)
          nil

        ;; Is there some text to be completed?
        (if (looking-at "\[A-Za-z0-9_\]")
            (setq identifiers (reverse (cdr identifiers)))
          (save-excursion
            (skip-chars-backward " \t\n")
            (backward-char)

            (if (not (eq (char-after) ?\.)) ;; are we at a .
                (setq identifiers (reverse (cdr identifiers)))
              ;; we will complete the empty string
              (setq identifiers (reverse identifiers))
              (setq to-be-completed ""))))

        ;; If no identifiers left, we try to complete a member without any
        ;; preceding class or object - lookup members in "this" class
        (if (null identifiers)
            (setq identifiers (list "this")))
        (jtags-message "Identifiers=%s, to-be-completed=`%s'" identifiers to-be-completed)
        (setq last-identifier (car (reverse identifiers)))

        ;; Find definition of identifier
        (setq definition (jtags-local-tags-lookup identifiers surrounding-classes))
        (unless definition
          (setq definition (jtags-recursive-tags-lookup identifiers surrounding-classes)))
        (jtags-message "Definition=%s" definition)

        ;; If we found a definition of the identifier
        (when definition
          ;; Get list of classes to look in
          (setq lookup-classes (jtags-get-class-list (if (jtags-class-or-interface-p definition)
                                                         (jtags-definition-class definition)
                                                       (jtags-definition-type definition))))
          (jtags-message "Look in classes=%s, surrounding classes=%s"
                         lookup-classes surrounding-classes)

          ;; For each class in lookup-classes, lookup its members
          (dolist (class lookup-classes)
            (setq class-members (jtags-lookup-class-members class))
            ;; (jtags-message "All members in `%s' (%d)=%s" class (length class-members) class-members)

            ;; Keep only visible members
            (setq class-members (jtags-filter-list
                                 (lambda (x)
                                   (cond ((get-text-property 0 'private x)
                                          (equal class (car surrounding-classes)))
                                         ((get-text-property 0 'protected x)
                                          (member class surrounding-classes))
                                         (t t)))
                                 class-members))
            (jtags-message "Visible members in `%s' (%d)=%s" class (length class-members) class-members)

            ;; If looking up members in a class, and not an object, keep only static members
            ;; Special handling of string literals, and keywords class, super, and this
            (when (and (jtags-class-or-interface-p definition)
                       (not (string-match "^\\(class\\|super\\|#stringliteral#\\|this\\)$" last-identifier)))
              (setq class-members (jtags-filter-list
                                   (lambda (x) (get-text-property 0 'static x))
                                   class-members))
              (jtags-message "Static members in `%s' (%d)=%s" class (length class-members) class-members))

            ;; Add text property for inherited class members
            (unless (equal class (car lookup-classes))
              (mapc (lambda (x) (add-text-properties 0 1 '(inherited t) x)) class-members))

            (setq members (append members class-members)))

          ;; If there are any visible members
          (when members
            ;; Find matching members (case sensitive match)
            (setq case-fold-search nil)
            (setq members (jtags-filter-list (lambda (x)
                                               (string-match (concat "^" to-be-completed) x))
                                             members))
            ;; (jtags-message "Matching members (%d)=%s" (length members) members)

            ;; Remove duplicates
            (setq members (jtags-uniqify-list members))
            (jtags-message "Unique members (%d)=%s" (length members) members)

            ;; If there are any matching members, return a list beginning with
            ;; the identifier to be completed, and ending with the matching
            ;; members sorted in alphabetical order. Otherwise, return nil.
            (if members
                (cons to-be-completed (sort members 'string<)))))))))

;; ----------------------------------------------------------------------------
;; Functions that operate on the tags table files:
;; ----------------------------------------------------------------------------

;; Parts of this function are copied from "etags.el" in XEmacs 21.4.21
(defun jtags-buffer-tag-table-list ()
  "Return a list of tags tables to be used for the current buffer.
Use `tag-table-alist' if defined, otherwise `tags-table-list'.
Setting variable `jtags-use-buffer-tag-table-list-flag' to non-nil
means use the built-in function `buffer-tag-table-list' instead.

The primary difference between this function, and `buffer-tag-table-list',
is that this function does not load the tags table files in the list. This
means that you can call this function before creating the tags table files."
  (cond ((not (boundp 'tag-table-alist))
         tags-table-list)

        ((and jtags-use-buffer-tag-table-list-flag (functionp 'buffer-tag-table-list))
         (buffer-tag-table-list))

        (t
         (let* (result
                expression
                (key (or buffer-file-name (concat default-directory (buffer-name))))
                (key (replace-regexp-in-string "\\\\" "/" key)))

           ;; Add `buffer-tag-table' if defined
           (when (and (boundp 'buffer-tag-table) buffer-tag-table)
             (when (file-directory-p buffer-tag-table)
               (setq buffer-tag-table (concat (jtags-file-name-directory buffer-tag-table) "TAGS")))
             (push buffer-tag-table result))

           ;; Add matching items from `tag-table-alist'
           (dolist (item tag-table-alist)
             (setq expression (car item))
             ;; If the car of the alist item is a string, apply it as a regexp
             ;; to the buffer-file-name. Otherwise, evaluate it. If the regexp
             ;; matches, or the expression evaluates non-nil, then this item
             ;; in tag-table-alist applies to this buffer.
             (when (if (stringp expression)
                       (string-match expression key)
                     (ignore-errors (eval expression)))
               ;; Now, evaluate the cdr of the alist item to get the name of
               ;; the tag table file.
               (setq expression (ignore-errors (eval (cdr item))))
               ;; If expression is a directory name, add file name TAGS.
               (when (file-directory-p expression)
                 (setq expression (concat (jtags-file-name-directory expression) "TAGS")))
               (push expression result)))
           (or result (error "Buffer has no associated tag tables"))
           (jtags-uniqify-list (nreverse result))))))

(defun jtags-lookup-identifier (class &optional member package-list)
  "Look up an identifier in the tags table files, and return its DEFINITION.

Look up the CLASS and its MEMBER in the tags table files. If no MEMBER is
provided, just look up the CLASS. The optional argument PACKAGE-LIST is a list
of package names, or fully qualified class names. If specified, a check is made
to verify that CLASS belongs to one of the packages in the list, see function
`jtags-right-package-p'.

Looking up a constructor will work iff CLASS is equal to MEMBER, and CLASS has
a defined constructor. Default constructors will not be looked up.

The DEFINITION of an identifier is a struct type with the following elements:

'(filename     ; The name of the file containing the declaration
  line         ; The line number where the declaration starts
  package      ; The package in which the class (see below) is declared
  class        ; The class in which the identifier is declared
  name         ; The name of the identifier, i.e. the class name for classes
  type)        ; The type of the identifier, i.e.
                     - attributes = attribute type
                     - classes    = \"class\"
                     - interfaces = \"interface\"
                     - methods    = return type"
  (save-excursion
    (jtags-message "Class=`%s', member=`%s'" class member)

    (let ((tags-list (jtags-buffer-tag-table-list)))
      (block while-tags-list
        (while tags-list
          (jtags-visit-tags-table-buffer (car tags-list))
          (setq case-fold-search nil)
          (goto-char (point-min))

          ;; (jtags-message "Looking in tags file `%s'" (buffer-file-name))
          (let (class-pos
                next-class-pos
                type-line-pos
                file-name)

            ;; Find class (or interface)
            (while (re-search-forward (concat "\\(class\\|interface\\) " class "\\W") nil t)
              (setq class-pos (point))

              ;; Find next class to limit searching
              (if (re-search-forward "\\(\\|\nclass\\|\ninterface\\)" nil t)
                  (setq next-class-pos (1+ (match-beginning 0)))
                (setq next-class-pos (point-max)))

              ;; Find file name
              (setq file-name (jtags-get-tagged-file class-pos))
              (jtags-message "Found class in file `%s'" file-name)

              ;; If the package is right, find all methods and attributes
              (when (jtags-right-package-p file-name package-list)

                ;; Get member type
                (goto-char class-pos)
                (setq type-line-pos (jtags-get-tagged-type-line-pos class member next-class-pos))
                ;; (jtags-message "Type-line-pos=%S" type-line-pos)

                ;; If we found a match
                (when type-line-pos
                  (set-buffer (find-file-noselect file-name t))
                  (return-from while-tags-list
                    (jtags-make-definition file-name                             ; file
                                           (nth 1 type-line-pos)                 ; line
                                           (car (jtags-find-packages "package")) ; package
                                           class                                 ; class
                                           (if member member class)              ; name
                                           (nth 0 type-line-pos)))))             ; type

              ;; Wrong package - find another class with the same name
              (goto-char next-class-pos))

            ;; Look in the next tags file
            (setq tags-list (cdr tags-list))))))))

(defun jtags-lookup-class-members (class &optional package-list)
  "Return a list of all members in CLASS, or nil if CLASS is not found.
The optional argument PACKAGE-LIST is a list of package names, or fully
qualified class names. If specified, a check is made to verify that CLASS
belongs to one of the packages in the list, see `jtags-right-package-p'."
  (save-excursion
    (jtags-message "Class=`%s', packages=%S" class package-list)
    (let ((tags-list (jtags-buffer-tag-table-list)))
      (block while-tags-list
        (while tags-list
          (jtags-visit-tags-table-buffer (car tags-list))
          (setq case-fold-search nil)
          (goto-char (point-min))

          ;; (jtags-message "Looking in tags file `%s'" (buffer-file-name))
          (let (class-pos
                next-class-pos
                file-name)

            ;; Find class (or interface)
            (while (re-search-forward (concat "\\(class\\|interface\\) " class "\\W") nil t)
              (setq class-pos (point))

              ;; Find next class to limit searching
              (if (re-search-forward "\\(\\|\nclass\\|\ninterface\\)" nil t)
                  (setq next-class-pos (1+ (match-beginning 0)))
                (setq next-class-pos (point-max)))

              ;; Find file name
              (setq file-name (jtags-get-tagged-file class-pos))
              (jtags-message "Found class in file `%s'" file-name)

              ;; If the package is right, find all methods and attributes
              (when (jtags-right-package-p file-name package-list)
                (goto-char class-pos)
                (return-from while-tags-list (jtags-get-tagged-members class next-class-pos)))

              ;; Wrong package - find another class with the same name
              (goto-char next-class-pos))

              ;; Look in the next tags file
              (setq tags-list (cdr tags-list))))))))

(defun jtags-get-tagged-type-line-pos (class member bound)
  "Return the type, line and position of MEMBER declared in CLASS.
This function is used by `jtags-lookup-identifier'. It parses the current class
\(up to position BOUND) in the tags table file and returns the type, line and
position of the supplied MEMBER. Returns nil if MEMBER is not found."
  (if member
      (block while-tag-line
        (while (re-search-forward jtags-tag-line-regexp bound t)
          (let ((found-member (match-string 3))
                (line (match-string 6))
                (pos (match-string 7)))
            (when (string-equal member found-member)
              (jtags-message "Found member `%s' at pos %s" member pos)

              ;; If another member is declared at the same point this is an invalid tag
              (unless (jtags-next-member-same-pos-p member pos bound)

                ;; If we are looking for a constructor (member and class names are equal)
                (if (string-equal member class)
                    (return-from while-tag-line (list member
                                                      (string-to-number line)
                                                      (string-to-number pos)))

                  ;; Not constructor - find member type
                  (goto-char (match-beginning 3))

                  ;; If we can match a type name
                  (if (re-search-backward (concat "[ \t\n]+" jtags-type-regexp) nil t)
                      (return-from while-tag-line (list (match-string 2)
                                                        (string-to-number line)
                                                        (string-to-number pos))))))))))

    ;; No member supplied - we are looking for a class or interface
    (end-of-line)
    (if (re-search-backward "\\(class\\|interface\\).*[^0-9]\\([0-9]+\\),\\([0-9]+\\)" nil t)
        (list (match-string 1)
              (string-to-number (match-string 2))
              (string-to-number (match-string 3))))))

(defun jtags-next-member-same-pos-p (member pos bound)
  "Return non-nil if next member in class is declared at same POS as MEMBER.
The search is bounded by BOUND, which should be the end of the class.

This function exists because of a bug in the etags program in GNU Emacs 21."
  (when (and (not (featurep 'xemacs)) (< emacs-major-version 22))
    (save-excursion
      (save-match-data
        (if (re-search-forward jtags-tag-line-regexp bound t)
            (let ((next-member (match-string 3))
                  (next-pos (match-string 7)))
              (when (string-equal pos next-pos)
                ;; (jtags-message "Found members `%s' and `%s' at same pos, ignoring `%s'" member next-member member)
                next-member)))))))

(defun jtags-get-tagged-members (class bound)
  "Return a list of all unique members declared in CLASS.
The search is bounded by BOUND, which should be the end of the class.

This function also sets some text properties on the class members.
A member can have one or several of the following text properties:
final, private, protected, public, and static."
  (let ((member-list nil))

    (while (re-search-forward jtags-tag-line-regexp bound t)
      (let ((member (match-string 3))
            (pos (match-string 7)))
        ;; (jtags-message "Found member `%s' at pos %s" member pos)

        (unless (or (jtags-next-member-same-pos-p member pos bound)
                    (member member member-list)
                    (equal member "static")) ;; Ignore static class initializer

          ;; Set text properties to keep track of the member's Java properties
          (goto-char (match-beginning 3))
          (save-excursion
            (if (re-search-backward "[ \t\n]+final[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(final t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+static[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(static t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+private[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(private t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+protected[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(protected t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+public[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(public t) member)))

          (setq member-list (cons member member-list)))))
    member-list))

(defun jtags-get-tagged-file (class-pos)
  "Return the name of the file in which the class at CLASS-POS is declared.
Unfortunately, the function `file-of-tag' sometimes fails in XEmacs."
  (save-excursion
    (goto-char class-pos)
    (re-search-backward "" nil t)
    (forward-line 1)
    (let ((start-pos (point)))
      (re-search-forward "\\(\\\.java\\)," nil t)
      (let ((end-pos (match-end 1)))
	(buffer-substring start-pos end-pos)))))

(defun jtags-right-package-p (file-name package-list)
  "Return non-nil if the package in FILE-NAME matches one in PACKAGE-LIST.

FILE-NAME is the absolute file name of the class/package to check.
PACKAGE-LIST is a list of package names, e.g. \"java.io.*\", or fully qualified
class names, e.g. \"java.io.InputStream\". Package names must end with the wild
card character \"*\"."
  (if (null package-list)
      't
    (block while-package-list
      ;; (jtags-message "Comparing package in file `%s'" file-name)
      (while package-list
        (let ((package (car package-list)))

          ;; Replace * with [A-Za-z0-9_]+ in package
          (setq package (replace-regexp-in-string "\\*" "[A-Za-z0-9_]+" package))
          (setq package (concat package "\\\.java$"))
          ;; (jtags-message "Comparing to package `%s'" package)

          ;; If packages match, (return-from while-package-list 't)
          (if (string-match package file-name)
              (return-from while-package-list 't)))
        (setq package-list (cdr package-list))))))

(defun jtags-find-classes (file)
  "Find classes in FILE and return list of (class . line) in the given file."
  (save-excursion
    (setq file (replace-regexp-in-string "\\\\" "/" file))
    (jtags-message "Finding classes in file `%s'" file)
    (let* ((p (jtags-buffer-tag-table-list))
           result
           boundary
           (continue-search t)
           class
           line)
      ;; for each tag table buffer
      (while (stringp (car p))
        (jtags-visit-tags-table-buffer (car p))

        ;; Set case insensitive for the file name search
        (setq case-fold-search t)

        ;; look for file in tags
        (goto-char 1)
        (when (search-forward file nil t)
          (setq case-fold-search nil)

          ;; set boundary to limit search
          (save-excursion
            (setq boundary (search-forward "" nil t)))
          ;; (jtags-message "Search limits = [%s, %s]" (point) boundary)

          ;; lookup classes
          (while continue-search
            ;; Find class name
            (setq continue-search (re-search-forward "class \\([A-Za-z0-9_]+\\)" boundary t))
;;             (jtags-message "Found `%s' on line %s in tags file"
;;                            (if continue-search (match-string 1))
;;                            (jtags-get-line))

            ;; If we found a match, save class name and find line of declaration
            (when continue-search
              (setq class (match-string 1))
              ;; Find line of declaration
              (re-search-forward "\\([0-9]+\\),[0-9]+" boundary t)
              (setq line (string-to-number (match-string 1)))
              (jtags-message "Class=`%s', line=%d" class line)
              (setq result (cons (cons class line) result)))))
        (setq p (cdr p)))
      result)))

;; ----------------------------------------------------------------------------
;; Functions that update tags table files:
;; ----------------------------------------------------------------------------

(defun jtags-update-tags-files ()
  "Update all tags table files listed in `tags-table-list'.
An element that is a directory means the file \"TAGS\" in that directory.

Run the etags program and update all tags table files in `tags-table-list'
after first querying the user about each file."
  (interactive)
  (let ((tags-list (jtags-buffer-tag-table-list)))
    (set-buffer (get-buffer-create jtags-buffer-name))
    (map-y-or-n-p "Update %s? " 'jtags-update-tags-file tags-list)))

(defun jtags-update-this-tags-file ()
  "Update the tags table file in which this class is tagged.

Use `tags-table-list' to find the tags table file in which the class
in the current buffer is tagged. Update the tags table file using the
etags program."
  (interactive)
  (let ((norm-default-dir (jtags-file-name-directory default-directory)))
    (jtags-message "Normalized default directory=`%s'" norm-default-dir)
    (dolist (tags-file (jtags-buffer-tag-table-list))
      (let* ((norm-tags-dir (jtags-file-name-directory tags-file)))
        (jtags-message "Checking tags file `%s'" norm-tags-dir)

        ;; If we have found the right tags file, update it
        (when (string-match norm-tags-dir norm-default-dir)
          (jtags-update-tags-file tags-file)
          (return))))))

(defun jtags-update-tags-file (tags-file)
  "Update the tags table file specified by TAGS-FILE.
An argument that is a directory means the file \"TAGS\" in that directory."

  ;; Default file and directory name
  (let ((file-name "TAGS")
        (dir-name tags-file))

    ;; If tags-file is a file, get file and directory name
    (if (file-directory-p dir-name)
        (setq dir-name (file-name-as-directory dir-name))
      (setq file-name (file-name-nondirectory tags-file))
      (setq dir-name (file-name-directory tags-file)))

    (jtags-run-etags file-name dir-name)))

(defun jtags-run-etags (file-name dir-name)
  "Run shell command in `jtags-etags-command' to update a tags table file.
Replace the sequence %f with FILE-NAME in `jtags-etags-command', and run the
resulting shell command in directory DIR-NAME. This normally includes running
the etags program, so the Emacs \"bin\" directory must be in your path."
  (let ((command (replace-regexp-in-string "%f" file-name jtags-etags-command))
        (original-directory default-directory))
    (jtags-message "Shell command=%s" command)
    (cd dir-name)

    ;; Don't try to update if file is not writable
    (if (not (file-writable-p file-name))
        (error "Cannot update tags file: permission denied, %s" dir-name)
      (message "Updating tags file in %s..." dir-name)
      (shell-command command jtags-buffer-name)
      (message "Updating tags file in %s...done" dir-name))

    (cd original-directory)))

;; ----------------------------------------------------------------------------
;; Functions that parse Java files:
;; ----------------------------------------------------------------------------

(defun jtags-find-identifier-backward ()
  "Return identifier before point in an expression.
Search backward for an identifier in the expression around point, set point to
the first character in the identifier, and return the identifier. If a constant
string, e.g. \"foo\" is found, the string \"#stringliteral#\" is returned, and
the point is set to the end of the string. Return nil if no identifier can be
found."
  (skip-chars-backward " \t\n")
  (backward-char)
  (jtags-message "Looking at `%s'" (char-to-string (char-after)))

  (cond ((eq (char-after) ?\.)                          ; dot?
         (jtags-find-identifier-backward))

        ((eq (char-after) ?\")                          ; string?
         "#stringliteral#")

        ((eq (char-after) ?\))                          ; parenthesis?
         (goto-char (scan-sexps (1+ (point)) -1))
         (jtags-find-identifier-backward))

        ((eq (char-after) ?\])                          ; square bracket?
         (goto-char (scan-sexps (1+ (point)) -1))
         (jtags-find-identifier-backward))

        ((looking-at "\[A-Za-z0-9_\]")                  ; keyword?
         (let ((end-pos (1+ (point))))
           (re-search-backward "\[^A-Za-z0-9_\]" nil t)
           (forward-char)
           (buffer-substring-no-properties (point) end-pos)))))

(defun jtags-parse-java-line ()
  "Parse the Java expression around point, and return a list of call order.
Return a list of identifiers that starts with the first identifier in the
expression, and ends with the last identifier, i.e. the one we want to lookup.
Return nil if no identifier can be found. Example:

toString().substring(1).length() -> (\"toString\" \"substring\" \"length\")

Some of the identifiers returned might be attributes and not methods, and the
first one might actually be a class name. Example:

Class.forName(\"Foo\") -> (\"Class\" \"forName\")"
  (save-excursion
    (let ((list nil)
          (continue 't)
          (identifier nil)
          (parse-in-literal (c-in-literal)))

      ;; If point is in identifier, goto end of identifier
      (skip-chars-forward "[A-Za-z0-9_]")

      (while continue

        ;; Find previous identifier
        (setq identifier (jtags-find-identifier-backward))

        ;; If no identifier found, stop searching
        (if (null identifier)
            (setq continue nil)

          ;; Otherwise, add identifier to list
          (setq list (cons identifier list))
          (jtags-message "List so far=%S" list)
          ;; (jtags-message "1. Looking at `%s'" (char-to-string (char-after)))

          ;; If we found a constant string, stop searching
          (if (eq (char-after) ?\")
              (setq continue nil))

          (skip-chars-backward " \t\n")
          (backward-char)
          ;; (jtags-message "2. Looking at `%s'" (char-to-string (char-after)))

          ;; If in comment now, but not from the beginning, stop searching
          (if (and (c-in-literal) (not parse-in-literal))
              (setq continue nil))

          ;; If not a dot, stop searching (identifiers must be separated by dots)
          (if (not (eq (char-after) ?\.))
              (setq continue nil))))
      list)))

(defun jtags-beginning-of-method (&optional bound)
  "Move backward to the beginning of the current method.
Return non-nil unless search stops due to beginning of buffer. The
search is bounded by BOUND, which should be the start of the class."
  (interactive)
  (let (start-pos     ;;                 RETURN TYPE           METHOD NAME               ARGS
        (start-regexp (concat "[ \t\n]+" jtags-type-regexp "\\([A-Za-z0-9_]+\\)[ \t\n]*\([^)]*\)")))
    (save-excursion
      (jtags-save-buffer-state ()

        ;; Make sure we are not in the middle of a method declaration
        (c-end-of-statement)

        ;; Search backward for method declaration
        (while (and (null start-pos) (re-search-backward start-regexp bound t))
          (let ((return-type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                (method (buffer-substring-no-properties (match-beginning 5) (match-end 5))))
            (jtags-message "Found method `%s' with return type `%s'" method return-type)

            ;; Ignore invalid return types, and declarations in comments and strings
            (goto-char (match-beginning 2))
            (unless (or (string-match "^new$\\|^return$\\|^throw$" return-type)
                        (c-in-literal))

              ;; Go to the beginning of the method declaration
              (c-end-of-statement)
              (c-beginning-of-statement-1)
              (setq start-pos (point)))))))

    (if (and start-pos (< start-pos (point)))
        (goto-char start-pos))))

(defun jtags-find-declaration (var class-start-line)
  "Look backwards for a declaration of VAR, and return its declaration info.
The search is bounded by CLASS-START-LINE. If VAR is found, return a list
with the following elements:

'(type        ; The type of VAR
  line        ; The line number where the declaration starts
  pos)        ; The position where the declaration starts"
  (save-excursion
    (jtags-message "Finding declaration of `%s'" var)
    (let* ((class-start-pos (jtags-line-to-point class-start-line))
           (method-start-pos (save-excursion (jtags-beginning-of-method class-start-pos)))
           (decl-regexp (concat "[ \t\n\(]"
                                jtags-type-regexp
                                ;;  IDENT BEFORE      ARRAY OR SPACE
                                "\\([A-Za-z0-9_]+\\(\\[\\]\\|[ \t\n]\\)*,[ \t\n]*\\)*"
                                ;; SEARCH IDENT
                                var "[ \t\n]*[,=:;[\)]"))
           result)

      ;; If point is in identifier, goto end of identifier
      (skip-chars-forward "[A-Za-z0-9_]")

      ;; Find declaration (if point is in a method)
      (while (and method-start-pos
                  (null result)
                  (re-search-backward decl-regexp method-start-pos t))
        (let ((type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
              (line (jtags-get-line))
              (pos (match-beginning 2)))
          (jtags-message "Found type `%s' for `%s'" type var)

          ;; Ignore invalid types, and declarations in comments and strings
          (unless (or (string-match "^\\(instanceof\\|new\\|return\\|static\\|throws?\\)$" type)
                      (c-in-literal))

            ;; We found a valid declaration
            (jtags-message "Found declaration on line %s" line)
            (setq result (list type line pos)))))

      ;; (jtags-message "Type-line-pos=%S" result)
      result)))

(defun jtags-find-packages (kind &optional bound)
  "Find import or package statements, and return a list of package names.
The search starts at the beginning of the current buffer. The string KIND
specifies what to look for, and should be either \"import\" or \"package\".
The optional argument BOUND bounds the search; it is a buffer position."
  (save-excursion
    (let (list
          orig-pos
          end-pos)

      ;; If looking for imports, start the list with package "java.lang", and
      ;; the current package, since they are imported by default
      (when (string-equal kind "import")
        (setq list (list "java.lang.*"))
        (let ((this-package (car (jtags-find-packages "package"))))
          (if this-package
              (setq list (cons (concat this-package ".*") list)))))

      (goto-char (point-min))
      (while (re-search-forward (concat kind "[ \t\n]+\\\([^*\n;]*\\\)[ \t\n]*[*]?[ \t\n]*;") bound t)
        (setq orig-pos (match-beginning 1))
        (goto-char (match-end 0))
        (re-search-backward "[a-zA-Z0-9_*]" orig-pos t)
        (setq end-pos (match-end 0))

        ;; Ignore statement in comments and strings
        (unless (c-in-literal)
          (let* ((package-name (buffer-substring-no-properties orig-pos end-pos))
                 (clean-name (replace-regexp-in-string "[ \t\n]" "" package-name)))
;;             (jtags-message "Line=%s, substring=`%s', cleaned=`%s'"
;;                            (jtags-get-line)
;;                            package-name
;;                            clean-name)
            (setq list (cons clean-name list)))))
      (jtags-uniqify-list list))))

;; ----------------------------------------------------------------------------
;; HTML functions:
;; ----------------------------------------------------------------------------

(defun jtags-find-javadoc (class package javadoc-root-list)
  "Find the Javadoc file for CLASS in PACKAGE.
Search all directories in JAVADOC-ROOT-LIST for a HTML file that matches CLASS
and PACKAGE, and return (file name . root directory) for the first one found."
  (dolist (javadoc-root javadoc-root-list)
    (setq javadoc-root (jtags-file-name-directory javadoc-root))
    (jtags-message "Normalized javadoc-root: `%s'" javadoc-root)

    (let ((javadoc-file (concat javadoc-root package class ".html")))
      (jtags-message "Looking for Javadoc file `%s'" javadoc-file)
      (if (file-regular-p javadoc-file)
          (return (cons javadoc-file javadoc-root))))))

(defun jtags-browse-url (definition)
  "Show Javadoc for the class in DEFINITION in the system browser.
Use variable `jtags-javadoc-root-list' to find Javadoc for the class, generate
a frameset file with links to the Javadoc file found, and show the frameset
file in the system browser."
  (jtags-message "Definition=%S" definition)
  (let ((package-name (jtags-definition-package definition)))

    (if (null package-name)
        (setq package-name "")
      (setq package-name (concat (replace-regexp-in-string "\\\." "/" package-name) "/")))

    (let ((found-javadoc (jtags-find-javadoc (jtags-definition-class definition)
                                             package-name
                                             jtags-javadoc-root-list)))
      (if (not found-javadoc)
          (message "Documentation not found!")
        (jtags-message "Launching browser with file `%s'" (car found-javadoc))
        (jtags-prepare-javadoc-frameset (car found-javadoc)
                                        (cdr found-javadoc))
        (browse-url jtags-javadoc-frameset)))))

(defun jtags-prepare-javadoc-frameset (java-file javadoc-root)
  "Prepare the Javadoc frameset file `jtags-javadoc-frameset'.
Insert link to Javadoc page for the Java identifier the user is interested in."
  (save-excursion
    (if (not (file-writable-p jtags-javadoc-frameset))
        (error "Cannot create Javadoc frameset file: permission denied, %s"
               jtags-javadoc-frameset)

      ;; Initialize Javadoc frameset file
      (jtags-init-javadoc-frameset javadoc-root)
      (goto-char (point-min))

      (if (not (string-match "^/" java-file))
          (setq java-file (concat "/" java-file)))

      (search-forward "</FRAMESET>" nil t)
      (if (re-search-forward "src=\".+\" name" nil t)
          (replace-match (concat "src=\"file://" java-file "\" name")))
      (if (re-search-forward "<A HREF=\".+\">Non" nil t)
          (replace-match (concat " <A HREF=\"file://" java-file "\">Non")))
      (save-buffer)
      (kill-buffer (current-buffer))
      (jtags-message "Successfully created `%s'" jtags-javadoc-frameset))))

(defun jtags-init-javadoc-frameset (javadoc-root)
  "Initalize the Javadoc frameset file using the given JAVADOC-ROOT.
Javadoc usually creates a file index.html that refers to the files
overview-frame.html, allclasses-frame.html, and overview-summary.html."
  (let ((source-file (concat javadoc-root "index.html")))
    (if (not (file-exists-p source-file))
        (error "Cannot set up Javadoc frameset file: file not found, %s" source-file)

      (copy-file source-file jtags-javadoc-frameset 'OK-IF-ALREADY-EXISTS)

      ;; Load frameset file if not loaded. Otherwise, revert frameset buffer.
      (let ((frameset-buffer (get-buffer (file-name-nondirectory jtags-javadoc-frameset))))
        (if (null frameset-buffer)
            (set-buffer (find-file-noselect jtags-javadoc-frameset t))
          (set-buffer frameset-buffer)
          (revert-buffer t t)))
      (goto-char (point-min))

      (if (not (string-match "^/" javadoc-root))
          (setq javadoc-root (concat "/" javadoc-root)))

      ;; Insert full path to frames
      (search-forward "<FRAME src=\"" nil t)
      (insert (concat "file://" javadoc-root))
      (search-forward "<FRAME src=\"" nil t)
      (insert (concat "file://" javadoc-root)))))

;; ----------------------------------------------------------------------------
;; Initialization:
;; ----------------------------------------------------------------------------

(defvar jtags-mode-map nil
  "Keymap used when jtags mode is enabled.")

(unless jtags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ,)]     'jtags-member-completion)
    (define-key map [(meta ,)]        'jtags-show-declaration)
    (define-key map [(meta f1)]       'jtags-show-documentation)
    (define-key map [(control c) ?\,] 'jtags-update-this-tags-file)
    (setq jtags-mode-map map)))

(defvar jtags-menu-list
  (list "JTags"
        ["Member completion" jtags-member-completion t]
        ["Show declaration" jtags-show-declaration t]
        ["Show documentation" jtags-show-documentation t]
        "--"
        ["Update this tags file" jtags-update-this-tags-file t]
        ["Update all tags files" jtags-update-tags-files t])
  "JTags submenu definition.")

;; Define a menu, and put it in the `jtags-mode-map'.
(easy-menu-define jtags-menu jtags-mode-map
  "Provides menu items for accessing jtags functionality."
  jtags-menu-list)

;;;###autoload (add-hook 'java-mode-hook 'jtags-mode)

;;;###autoload
(define-minor-mode jtags-mode
  "Toggle jtags mode.
With arg, turn jtags mode on iff arg is positive.

When jtags mode is enabled, a number of improved tags lookup commands are
available, as shown below. jtags mode provides commands for looking up the
identifier before or around point, completing partly typed identifiers, and
managing tags table files.

\\{jtags-mode-map}"
  nil
  nil
  jtags-mode-map
  (if jtags-mode
      (if jtags-display-menu-flag
          (easy-menu-add jtags-menu-list jtags-mode-map))
    (if jtags-display-menu-flag
        (easy-menu-remove jtags-menu-list))))

;; ----------------------------------------------------------------------------

(provide 'jtags)

;;; jtags.el ends here

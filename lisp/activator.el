;;; -*- Mode: Emacs-Lisp; -*-

;;;;;;; Activator: an init.d style config manager
;; 
;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2009, Andrew Gwozdziewycz <web@apgwoz.com>
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version. This is
;; distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details. You
;; should have received a copy of the GNU General Public License along with 
;; Emacs; see the file `COPYING'. If not, write to the Free Software 
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.


;; INTRODUCTION:
;; 
;;   Activator adds an init.d style loader for configuration files in GNU Emacs
;;
;; COMPATIBILITY:
;; 
;;   GNU Emacs 22 -- Only tested version. 
;;   Other versions -- There's no reason it wouldn't work...
;;
;; INSTALLATION:
;; 
;;   To install, put this file, `activator.el', somewhere in your `load-path' 
;;   and add the following lines to your `.emacs' file:
;;     
;;       (require 'activator)
;;       (activator-start)
;; 
;;   If you don't know what your load path is....
;;
;; HOW TO USE:
;; 
;;   One installed, the best way is to create a directory to add your config 
;;   files to. For example, on my machines, I use `~/.emacs.d/activator.d'. This
;;   is the default file path for where your configuration files will be loaded
;;   from.
;;   
;;   To change this path, set the variable `activator-load-path' to your desired
;;   path like so:
;;
;;      (setq activator-load-path "~/path/to/activator/config/files")
;;
;;   Activator supports the ability to override the filename format for the 
;;   files that will be loaded from the filepath. By default this value is set
;;   to include files of the form `00something.el', where `00' represents the
;;   order in which to include the file. 
;;  
;;   You can change this by setting the variable `activator-filename-pattern' to
;;   your desired pattern like so:
;;
;;      (setq activator-filename-pattern "^\[0-9\]\[0-9\].*\.el$")
;;
;; 


(defconst activator-copyright "Copyright (C) 2009 Andrew Gwozdziewycz")
(defconst activator-version 0.01)
(defconst activator-author-name "Andrew Gwozdziewycz")
(defconst activator-author-email "web@apgwoz.com")
(defconst activator-web-page "http://www.apgwoz.com/activator/")
(defconst activator-license "GNU General Public License")

(defgroup activator nil
  "Activator is used for creating an organized startup for emacs"
  :prefix "activator-"
  :link '(url-link "http://www.apgwoz.com/"))

(defcustom activator-load-path "~/.emacs.d/activator.d/"
  "Load path for the files you want to include when activator is started"
  :group 'activator
  :type 'string)

(defcustom activator-filename-pattern "^\[0-9\]\[0-9\].*\.el$"
  "Regular Expression specifying what activator file names should look like."
  :group 'activator
  :type 'string)

(defun activator-get-files (&optional path pattern)
  "Gets files from path specified by `activator-load-path', or from the 
optional path"
  (let ((path (or path activator-load-path))
        (pattern (or pattern activator-filename-pattern)))
    (message path)
    (message pattern)
    (directory-files path t pattern)))

(defun activator-load-file (file)
  (load-file file))

(defun activator-start ()
  "Starts activator, thereby running all the files in `activator-load-path' that
match the `activator-filename-pattern`"
  (interactive)
  (if (not (boundp 'activator-load-path))
      (error "Please set `activator-load-path`")
    (mapcar 'activator-load-file (activator-get-files))))

(provide 'activator)

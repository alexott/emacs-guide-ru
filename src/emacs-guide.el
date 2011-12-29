;;; emacs-guide.el --- Tools for java programming

;; Copyright (C) 2011  Alt (Alexander Tihonruk)

;; Author: Alexander Tihonruk <a.tihonruk@gmail.com>
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Contains functions for java code generation:
;; * accessors (getter and setter) for given field
;; * mandatory constructor for final fields
;; * javadoc comment for method

;;; Code:

(require 'semantic)

(defun alt-current-class ()
  (semantic-current-tag-of-class 'type))


(defun alt-class-vars (class)
  (let ((members (semantic-tag-get-attribute class :members)))
    (semantic-find-tags-by-class 'variable members)))


(defun alt-java-accessors (tag)
  "Writes conventional getter and setter for given class field."
  (interactive
   (let* ((tags (alt-class-vars (alt-current-class)))
	  (name (semantic-read-variable "Field name: " nil tags)))
     (list (semantic-find-first-tag-by-name name tags))))
  (let* ((type (semantic-tag-type tag))
	 (name (semantic-tag-name tag))
	 (cname (upcase-initials name)))
    (save-excursion
      (let ((start (point)))
	(insert "\n public " type " get" cname "() {\nreturn " name ";\n}\n")
	(insert "\n public void set" cname "(" type " " name ") {\nthis." name " = " name ";\n}\n")
	(indent-region start (point))))))

  
(defun alt-java-comment-function ()
  "Inserts javadoc-style comment stub for function at current position."
  (interactive)
  (let ((tag (last (semantic-find-tag-by-overlay))))
    (if (semantic-tag-of-class-p tag 'function)
	(let ((start (semantic-tag-start tag))
	      (args (semantic-tag-function-arguments tag))
	      (throws (semantic-tag-function-throws tag))
	      (type (semantic-tag-type tag)))
	  (goto-char start)
	  (insert "/**\n* \n*\n")
	  (dolist (arg args)
	    (insert "* @param " (semantic-tag-name arg)) "\n")
	  (dolist (ex throws)
	    (insert "* @throws " ex "\n"))
	  (unless (equal type "void") (insert "* @return \n"))
	  (insert "*/\n")
	  (indent-region start (+ 1 (point)))
	  (goto-char start)
	  (end-of-line 2))
      (error "Point cursor to a function you want to comment"))))


(defun alt-java-constructor ()
  "Writes constructor for all non-static final fields of current class."
  (interactive)
  (let* ((startpos (point))
	(class (alt-current-class))
	(classname (semantic-tag-name class))
	(vars 
	 (delq nil
	       (mapcar 
		'(lambda (tag)
		   (let ((modifiers (semantic-tag-modifiers tag)))
		     (when (and (member "final" modifiers)
				(not (member "static" modifiers)))
		       (cons (semantic-tag-name tag) (semantic-tag-type tag)))))
		(alt-class-vars class))))
	(paramlist 
	 (mapconcat '(lambda (v) (concat (cdr v) " " (car v))) vars ", ")))
    (insert classname "(" paramlist ") {\n")
    (dolist (var vars)
      (insert "this." (car var) " = " (car var) ";\n"))
    (insert "}\n")
    (indent-region startpos (point))
    (goto-char startpos)))


(provide 'emacs-guide)
;;; emacs-guide.el ends here


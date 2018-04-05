;;; ----------------------------------------------------------------------------
;;; atdoc.lisp
;;;
;;; Functions for generating the documentation for the Library Pango
;;;
;;; The documentation has been copied from the Pango Reference Manual
;;; for Pango 1.29.5. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(asdf:load-system :atdoc)
(asdf:load-system :cl-cffi-gtk-pango)

;; Load the documentation of the Libraray Pango
(load "atdoc-pango.lisp")

(defpackage :atdoc-pango
  (:use :pango :common-lisp)
  (:export #:generate-html
           #:generate-html-single-page
           #:generate-latex
           #:generate-info))

(in-package :atdoc-pango)

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk-pango)))
         (output-directory (merge-pathnames "atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:pango)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk-pango API documentation"
      :heading "cl-cffi-gtk-pango"
      :css "crategus.css"
      :logo nil
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk-pango)))
         (output-directory (merge-pathnames "atdoc/single-page/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:pango)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk-pango API documentation"
      :heading "cl-cffi-gtk-pango"
      :css "crategus.css"
      :logo nil
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(generate-html)
(generate-html-single-page)

;;; --- End of file atdoc.lisp -------------------------------------------------

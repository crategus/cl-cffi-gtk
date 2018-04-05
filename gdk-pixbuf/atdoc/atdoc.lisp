;;; ----------------------------------------------------------------------------
;;; atdoc.lisp
;;;
;;; Functions for generating the documentation for the Library GDK-PixBuf
;;;
;;; The documentation has been copied from the GDK-PixBuf Reference Manual
;;; Version 2.26.1. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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

#-cl-cffi-gtk-documentation
(push :cl-cffi-gtk-documentation *features*)

(asdf:load-system :atdoc)
(asdf:load-system :cl-cffi-gtk-gdk-pixbuf)

(defpackage :atdoc-gdk-pixbuf
  (:use :gdk-pixbuf :common-lisp)
  (:export #:generate-html
           #:generate-html-single-page
           #:generate-latex
           #:generate-info))

(in-package :atdoc-gdk-pixbuf)

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk-gdk-pixbuf)))
         (output-directory (merge-pathnames "atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:gdk-pixbuf)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk-gdk-pixbuf API documentation"
      :heading "cl-cffi-gtk-gdk-pixbuf"
      :css "crategus.css"
      :logo nil
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk-gdk-pixbuf)))
         (output-directory (merge-pathnames "atdoc/single-page/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:gdk-pixbuf)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk-gdk-pixbuf API documentation (single page)"
      :heading "cl-cffi-gtk-gdk-pixbuf"
      :css "crategus.css"
      :logo nil
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(generate-html)
(generate-html-single-page)

;;; --- End of file atdoc.lisp -------------------------------------------------

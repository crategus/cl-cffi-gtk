;;; ----------------------------------------------------------------------------
;;; atdoc.lisp
;;;
;;; Copyright (C) 2012, 2019 Dieter Kaiser
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
(asdf:load-system :cl-cffi-gtk)

(defpackage :atdoc-gtk
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :common-lisp)
  (:export #:generate-html
           #:generate-html-single-page
           #:generate-latex
           #:generate-info))

(in-package :atdoc-gtk)

;; Unexport symbols we do not want to document.

(unexport 'glib:allocate-stable-pointer :glib)
(unexport 'glib:stable-pointer-destroy-notify-cb :glib)
(unexport 'glib:get-stable-pointer-value :glib)
(unexport 'glib:with-stable-pointer :glib)
(unexport 'glib:with-catching-to-g-error :glib)
(unexport 'glib:with-g-error :glib)

(unexport 'gobject:*debug-gc* :gobject)
(unexport 'gobject:*debug-subclass* :gobject)
(unexport 'gobject:*gobject-debug* :gobject)
(unexport 'gobject:*lisp-name-exceptions* :gobject)

(unexport 'gobject:g-boxed-foreign :gobject)
(unexport 'gobject:boxed-related-symbols :gobject)
(unexport 'gobject:copy-boxed-slots-to-foreign :gobject)
(unexport 'gobject:create-fn-ref :gobject)
(unexport 'gobject:define-boxed-opaque-accessor :gobject)
(unexport 'gobject:define-cb-methods :gobject)
(unexport 'gobject:define-g-boxed-cstruct :gobject)
(unexport 'gobject:define-g-boxed-opaque :gobject)
(unexport 'gobject:define-g-boxed-variant-cstruct :gobject)
(unexport 'gobject:define-g-enum :gobject)
(unexport 'gobject:define-g-flags :gobject)
(unexport 'gobject:define-g-interface :gobject)
(unexport 'gobject:define-g-object-class :gobject)
(unexport 'gobject:define-vtable :gobject)
(unexport 'gobject:get-g-type-definition :gobject)
(unexport 'gobject:gobject-class-direct-g-type-name :gobject)
(unexport 'gobject:gobject-class-g-type-initializer :gobject)
(unexport 'gobject:gobject-class-g-type-name :gobject)
(unexport 'gobject:gobject-class-interface-p :gobject)
(unexport 'gobject:list-signals :gobject)
(unexport 'gobject:parse-g-param-spec :gobject)
(unexport 'gobject:parse-g-value :gobject)
(unexport 'gobject:register-object-type :gobject)
(unexport 'gobject:register-object-type-implementation :gobject)
(unexport 'gobject:registered-object-type-by-name :gobject)
(unexport 'gobject:set-g-value :gobject)
(unexport 'gobject:signal-info :gobject)
(unexport 'gobject:using* :gobject)
(unexport 'gobject:with-foreign-boxed-array :gobject)

(unexport 'gobject:param-spec-type :gobject)
(unexport 'gobject:param-spec-owner-type :gobject)
(unexport 'gobject:param-spec-name :gobject)
(unexport 'gobject:param-spec-readable :gobject)
(unexport 'gobject:param-spec-writable :gobject)
(unexport 'gobject:param-spec-constructor :gobject)
(unexport 'gobject:param-spec-constructor-only :gobject)

(unexport 'gobject:lisp-closure :gobject)

(unexport 'gobject:gtype :gobject)
(unexport 'gobject:gtype-from-id :gobject)
(unexport 'gobject:gtype-from-name :gobject)
(unexport 'gobject:gtype-id :gobject)
(unexport 'gobject:gtype-name :gobject)

(unexport 'gdk:gdk-atom-as-string :gdk)

#-windows
(progn
  (unexport 'gdk:gdk-x11-device-manager-core :gdk)
  (unexport 'gdk:gdk-x11-device-manager-xi2 :gdk)
  (unexport 'gdk:gdk-x11-device-xi2 :gdk)
  (unexport 'gdk:gdk-x11-device-xi2-device-id :gdk))

(unexport 'gtk:atk-implementor-iface :gtk)
(unexport 'gtk:ensure-gtk-main :gtk)
#+ubuntu
(unexport 'gtk:gtk-window-ubuntu-no-proxy :gtk)
(unexport 'gtk:with-text-buffer-user-action :gtk)


(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk)))
         (output-directory (merge-pathnames "../atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk API documentation"
      :heading "cl-cffi-gtk"
      :css "crategus.css"
      :logo nil
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :cl-cffi-gtk)))
         (output-directory (merge-pathnames "../atdoc/single-page/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "cl-cffi-gtk API documentation (single page)"
      :heading "cl-cffi-gtk"
      :css "crategus.css"
      :logo nil
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(generate-html-single-page)
(generate-html)
(room)

;;; --- End of file atdoc.lisp -------------------------------------------------

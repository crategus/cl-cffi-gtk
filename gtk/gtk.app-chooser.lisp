;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2020 Dieter Kaiser
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
;;;
;;; GtkAppChooser
;;;
;;;     Interface implemented by widgets for choosing an application
;;;
;;; Types and Values
;;;
;;;     GtkAppChooser
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_get_app_info
;;;     gtk_app_chooser_get_content_type
;;;     gtk_app_chooser_refresh
;;;
;;; Properties
;;;
;;;     gchar*   content-type    Read / Write / Construct Only
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkAppChooser
;;;
;;; Prerequisites
;;;
;;;     GtkAppChooser requires GtkWidget.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAppChooser
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkAppChooser" gtk-app-chooser
  (:export t
   :type-initializer "gtk_app_chooser_get_type")
  (content-type
   gtk-app-chooser-content-type
   "content-type" "gchararray" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-app-chooser 'type)
 "@version{2020-5-21}
  @begin{short}
    @sym{gtk-app-chooser} is an interface that can be implemented by widgets
    which allow the user to choose an application, typically for the purpose of
    opening a file.
  @end{short}
  The main objects that implement this interface are
  @class{gtk-app-chooser-widget}, @class{gtk-app-chooser-dialog} and
  @class{gtk-app-chooser-button}.

  Applications are represented by GIO @class{g-app-info} objects here. GIO has
  a concept of recommended and fallback applications for a given content type.
  Recommended applications are those that claim to handle the content type
  itself, while fallback also includes applications that handle a more generic
  content type. GIO also knows the default and last-used application for a
  given content type. The @class{gtk-app-chooser-widget} provides detailed
  control over whether the shown list of applications should include default,
  recommended or fallback applications.

  To obtain the application that has been selected in a @sym{gtk-app-chooser},
  use the function @fun{gtk-app-chooser-app-info}.
  @see-slot{gtk-app-chooser-content-type}
  @see-class{g-app-info}
  @see-class{gtk-app-chooser-widget}
  @see-class{gtk-app-chooser-dialog}
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-app-info}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "content-type"
                                               'gtk-app-chooser) 't)
 "The @code{content-type} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The content type of the @sym{gtk-app-chooser} object.
  See @code{GContentType} for more information about content types. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-content-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-content-type 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-content-type object) => content-type}
  @argument[object]{a @class{gtk-app-chooser} object}
  @argument[content-type]{a string with the content type}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser]{content-type} slot of the
    @class{gtk-app-chooser} interface.
  @end{short}

  The slot access function @sym{gtk-app-chooser-content-type} returns the
  current value of the content type.
  @see-class{gtk-app-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_get_app_info () -> gtk-app-chooser-app-info
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_get_app_info" gtk-app-chooser-app-info)
    (g-object g-app-info)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[object]{a @class{gtk-app-chooser} object}
  @begin{return}
    A @class{g-app-info} object for the currently selected application, or
    @code{nil} if none is selected.
  @end{return}
  @begin{short}
    Returns the currently selected application.
  @end{short}
  @see-class{gtk-app-chooser}"
  (object (g-object gtk-app-chooser)))

(export 'gtk-app-chooser-app-info)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_refresh ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_refresh" gtk-app-chooser-refresh) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[object]{a @class{gtk-app-chooser} object}
  @short{Reloads the list of applications.}
  @see-class{gtk-app-chooser}"
  (object (g-object gtk-app-chooser)))

(export 'gtk-app-chooser-refresh)

;;; --- End of file gtk.app-chooser.lisp ---------------------------------------

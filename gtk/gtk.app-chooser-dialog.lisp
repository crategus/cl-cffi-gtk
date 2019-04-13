;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2019 Dieter Kaiser
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
;;; GtkAppChooserDialog
;;;
;;;     An application chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget
;;;     gtk_app_chooser_dialog_set_heading                 Accessor
;;;     gtk_app_chooser_dialog_get_heading                 Accessor
;;;
;;;
;;; Properties
;;;     GFile*  gfile      Read / Write / Construct Only
;;;     gchar*  heading    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkAppChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAppChooserDialog implements AtkImplementorIface, GtkBuildable and
;;;     GtkAppChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAppChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAppChooserDialog" gtk-app-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_dialog_get_type")
  ((gfile
    gtk-app-chooser-dialog-gfile
    "gfile" "GFile" t t)
   (heading
    gtk-app-chooser-dialog-heading
    "heading" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-app-chooser-dialog 'type)
 "@version{2013-11-10}
  @begin{short}
    @sym{gtk-app-chooser-dialog} shows a @class{gtk-app-chooser-widget} inside
    a @class{gtk-dialog}.
  @end{short}

  Note that @sym{gtk-app-chooser-dialog} does not have any interesting methods
  of its own. Instead, you should get the embedded
  @class{gtk-app-chooser-widget} using the function
  @fun{gtk-app-chooser-dialog-get-widget} and call its methods if the generic
  @class{gtk-app-chooser} interface is not sufficient for your needs.

  To set the heading that is shown above the @class{gtk-app-chooser-widget},
  use the function @fun{gtk-app-chooser-dialog-set-heading}.
  @see-slot{gtk-app-chooser-dialog-gfile}
  @see-slot{gtk-app-chooser-dialog-heading}
  @see-class{gtk-dialog}
  @see-class{gtk-app-chooser}
  @see-function{gtk-app-chooser-dialog-get-widget}
  @see-function{gtk-app-chooser-dialog-set-heading}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-app-chooser-dialog-gfile -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gfile"
                                               'gtk-app-chooser-dialog) 't)
 "The @code{gfile} property of type @class{g-file}
  (Read / Write / Construct Only) @br{}
  The @class{g-file} used by the @sym{gtk-app-chooser-dialog}. The dialog's
  @class{gtk-app-chooser-widget} content type will be guessed from the file, if
  present.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-dialog-gfile atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-dialog-gfile 'function)
 "@version{2013-11-1}
  Accessor of the slot @slot[gtk-app-chooser-dialog]{gfile} of the
  @class{gtk-app-chooser-dialog} class.
  @see-class{gtk-app-chooser-dialog}")

;;; --- gtk-app-chooser-dialog-heading -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "heading"
                                               'gtk-app-chooser-dialog) 't)
 "The @code{heading} property of type @code{:string} (Read / Write) @br{}
  The text to show at the top of the dialog. The string may contain Pango
  markup. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-dialog-heading atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-dialog-heading 'function)
 "@version{2013-11-1}
  Accessor of the slot @slot[gtk-app-chooser-dialog]{heading} of the
  @class{gtk-app-chooser-dialog} class.
  @see-class{gtk-app-chooser-dialog}
  @see-function{gtk-app-chooser-dialog-get-heading}
  @see-function{gtk-app-chooser-dialog-set-heading}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_dialog_new" gtk-app-chooser-dialog-new)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[parent]{a @class{gtk-window}, or @code{nil}}
  @argument[flags]{flags of type @symbol{gtk-dialog-flags} for this dialog}
  @argument[file]{a @class{g-file} object}
  @return{A newly created @class{gtk-app-chooser-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk-app-chooser-dialog} widget for the provided
    @class{g-file} object, to allow the user to select an application for it.
  @end{short}
  @see-class{gtk-app-chooser-dialog}
  @see-class{gtk-window}
  @see-class{g-file}
  @see-symbol{gtk-dialog-flags}"
  (parent (g-object gtk-window))
  (flags gtk-dialog-flags)
  (file (g-object g-file)))

(export 'gtk-app-chooser-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new_for_content_type ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_dialog_new_for_content_type"
           gtk-app-chooser-dialog-new-for-content-type) (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[parent]{a @class{gtk-window}, or @code{nil}}
  @argument[flags]{flags of type @symbol{gtk-dialog-flags} for this dialog}
  @argument[content-type]{a content type string}
  @return{A newly created @class{gtk-app-chooser-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk-app-chooser-dialog} widget for the provided content
    type, to allow the user to select an application for it.
  @end{short}
  @see-class{gtk-app-chooser-dialog}
  @see-class{gtk-window}
  @see-symbol{gtk-dialog-flags}
  @see-function{gtk-app-chooser-dialog-new-for-content-type}"
  (parent (g-object gtk-window))
  (flags gtk-dialog-flags)
  (content-type :string))

(export 'gtk-app-chooser-dialog-new-for-content-type)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_get_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_dialog_get_widget" gtk-app-chooser-dialog-get-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-11}
  @argument[self]{a @class{gtk-app-chooser-dialog} widget}
  @return{The @class{gtk-app-chooser-widget} of @arg{self}.}
  @begin{short}
    Returns the @class{gtk-app-chooser-widget} of this dialog.
  @end{short}
  @see-class{gtk-app-chooser-dialog}
  @see-class{gtk-app-chooser-widget}"
  (self (g-object gtk-app-chooser-dialog)))

(export 'gtk-app-chooser-dialog-get-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_set_heading ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-dialog-set-heading))

(defun gtk-app-chooser-dialog-set-heading (self heading)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-dialog} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Sets the text to display at the top of the dialog.
  @end{short}
  If the heading is not set, the dialog displays a default text.
  @see-class{gtk-app-chooser-dialog}
  @see-function{gtk-app-chooser-dialog-get-heading}"
  (setf (gtk-app-chooser-dialog-heading self) heading))

(export 'gtk-app-chooser-dialog-set-heading)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_get_heading ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-dialog-get-heading))

(defun gtk-app-chooser-dialog-get-heading (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-dialog} widget}
  @begin{return}
    The text to display at the top of the dialog, or @code{nil}, in which case a
    default text is displayed.
  @end{return}
  Returns the text to display at the top of the dialog.
  @see-class{gtk-app-chooser-dialog}
  @see-function{gtk-app-chooser-dialog-set-heading}"
  (gtk-app-chooser-dialog-heading self))

(export 'gtk-app-chooser-dialog-get-heading)

;;; --- End of file gtk.app-chooser-dialog.lisp --------------------------------

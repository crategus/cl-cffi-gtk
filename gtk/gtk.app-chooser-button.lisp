;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; GtkAppChooserButton
;;;
;;; A button to launch an application chooser dialog
;;;
;;; Synopsis
;;;
;;;     GtkAppChooserButton
;;;
;;;     gtk_app_chooser_button_new
;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item
;;;     gtk_app_chooser_button_get_show_default_item
;;;     gtk_app_chooser_button_set_show_default_item
;;;     gtk_app_chooser_button_get_show_dialog_item
;;;     gtk_app_chooser_button_set_show_dialog_item
;;;     gtk_app_chooser_button_get_heading
;;;     gtk_app_chooser_button_set_heading
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkComboBox
;;;                                  +----GtkAppChooserButton
;;;
;;; Implemented Interfaces
;;;
;;; GtkAppChooserButton implements AtkImplementorIface, GtkBuildable,
;;; GtkCellLayout, GtkCellEditable and GtkAppChooser.
;;;
;;; Properties
;;;
;;;   "heading"                  gchar*               : Read / Write
;;;   "show-default-item"        gboolean             : Read / Write / Construct
;;;   "show-dialog-item"         gboolean             : Read / Write / Construct
;;;
;;; Signals
;;;
;;;   "custom-item-activated"                         : Has Details
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAppChooserButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkAppChooserButton" gtk-app-chooser-button
  (:superclass gtk-combo-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
;                "GtkCellLayout"
;                "GtkCellEditable"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_button_get_type")
  ((heading
    gtk-app-chooser-button-heading
    "heading" "gchararray" t t)
   (show-default-item
    gtk-app-chooser-show-default-item
    "show-default-item" "gboolean" t t)
   (show-dialog-item
    gtk-app-chooser-button-show-dialog-item
    "show-dialog-item" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-app-chooser-button 'type)
 "@version{2013-10-31}
  @begin{short}
    The @sym{gtk-app-chooser-button} is a widget that lets the user select an
    application. It implements the @class{gtk-app-chooser} interface.
  @end{short}

  Initially, a @sym{gtk-app-chooser-button} selects the first application in its
  list, which will either be the most-recently used application or, if
  @code{\"show-default-item\"} is @em{true}, the default application.

  The list of applications shown in a @sym{gtk-app-chooser-button} includes the
  recommended applications for the given content type. When
  @code{\"show-default-item\"} is set, the default application is also included.
  To let the user chooser other applications, you can set the
  @code{\"show-dialog-item\"} property, which allows to open a full
  @class{gtk-app-chooser-dialog}.

  It is possible to add custom items to the list, using the function
  @fun{gtk-app-chooser-button-append-custom-item}. These items cause the
  \"custom-item-activated\" signal to be emitted when they are selected.

  To track changes in the selected application, use the \"changed\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"custom-item-activated\" signal}
      @begin{pre}
 lambda (self item-name)   : Has Details
      @end{pre}
      Emitted when a custom item, previously added with the function
      @fun{gtk-app-chooser-button-append-custom-item}, is activated from the
      dropdown menu.
      @begin[code]{table}
        @entry[self]{The object which received the signal.}
        @entry[item-name]{The name of the activated item.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-app-chooser-button-heading}
  @see-slot{gtk-app-chooser-button-show-default-item}
  @see-slot{gtk-app-chooser-button-show-dialog-item}
  @see-class{gtk-app-chooser}
  @see-class{gtk-app-chooser-dialog}
  @see-function{gtk-app-chooser-button-append-custom-item}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "heading"
                                               'gtk-app-chooser-button) 't)
 "The @code{\"heading\"} property of @code{:string} (Read / Write) @br{}
  The text to show at the top of the dialog that can be opened from the
  button. The string may contain Pango markup. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-default-item"
                                               'gtk-app-chooser-button) 't)
 "The @code{\"show-default-item\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-default-item\"} property determines whether the dropdown menu
  should show the default application on top for the provided content type.
  @br{}
  Default value: @code{nil} @br{}
  Since 3.2")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-dialog-item"
                                               'gtk-app-chooser-button) 't)
 "The @code{\"show-dialog-item\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The @code{\"show-dialog-item\"}\ property determines whether the dropdown menu
  should show an item that triggers a @sym{gtk-app-chooser-dialog} when clicked.
  @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-heading atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-heading 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"heading\"} of the @class{gtk-app-chooser-button}
  class.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-get-heading}
  @see-function{gtk-app-chooser-button-set-heading}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-show-default-item
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-show-default-item 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-default-item\"} of the
  @class{gtk-app-chooser-button} class.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-get-show-default-item}
  @see-function{gtk-app-chooser-button-set-show-default-item}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-show-dialog-item
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-show-dialog-item 'function)
 "@version{2013-11-1}
  Accessor of the slot @code{\"show-dialog-item\"} of the
  @class{gtk-app-chooser-button} class.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-get-show-dialog-item}
  @see-function{gtk-app-chooser-button-set-show-dialog-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-new))

(defun gtk-app-chooser-button-new (content-type)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[content-type]{the content type to show applications for}
  @return{A newly created @class{gtk-app-chooser-button} widget.}
  @begin{short}
    Creates a new @class{gtk-app-chooser-button} widget for applications that
    can handle content of the given type.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-button}"
  (make-instance 'gtk-app-chooser-button-new
                 :content-type content-type))

(export 'gtk-app-chooser-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_custom_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_append_custom_item"
           gtk-app-chooser-button-append-custom-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @argument[name]{the name of the custom item}
  @argument[label]{the label for the custom item}
  @argument[icon]{the icon of type @class{g-icon} for the custom item}
  @begin{short}
    Appends a custom item to the list of applications that is shown in the
    popup; the item name must be unique per-widget.
  @end{short}
  Clients can use the provided name as a detail for the
  \"custom-item-activated\" signal, to add a callback for the activation of a
  particular custom item in the list. See also the function
  @fun{gtk-app-chooser-button-append-separator}.

  Since 3.0
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-append-separator}"
  (self (g-object gtk-app-chooser-button))
  (name :string)
  (label :string)
  (icon (g-object g-icon)))

(export 'gtk-app-chooser-button-append-custom-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_separator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_append_separator"
           gtk-app-chooser-button-append-separator) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @begin{short}
    Appends a separator to the list of applications that is shown in the popup.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-button-append-separator}"
  (self (g-object gtk-app-chooser-button)))

(export 'gtk-app-chooser-button-append-separator)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_active_custom_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_set_active_custom_item"
           gtk-app-chooser-button-set-active-custom-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @argument[name]{the name of the custom item}
  @begin{short}
    Selects a custom item previously added with the function
    @fun{gtk-app-chooser-button-append-custom-item}.
  @end{short}

  Use the function @fun{gtk-app-chooser-refresh} to bring the selection to its
  initial state.

  Since 3.0
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-refresh}
  @see-function{gtk-app-chooser-button-append-custom-item}"
  (self (g-object gtk-app-chooser-button))
  (name :string))

(export 'gtk-app-chooser-button-set-active-custom-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_get_show_default_item ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-get-show-default-item))

(defun gtk-app-chooser-button-get-show-default-item (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @return{The value of the @code{\"show-default-item\"} property.}
  @begin{short}
    Returns the current value of the @code{\"show-default-item\"} property.
  @end{short}

  Since 3.2
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-set-show-default-item}"
  (gtk-app-chooser-button-show-default-item self))

(export 'gtk-app-chooser-button-get-show-default-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_show_default_item ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-set-show-default-item))

(defun gtk-app-chooser-button-set-show-default-item (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @argument[setting]{the new value for the @code{\"show-default-item\"}
    property}
  @begin{short}
    Sets whether the dropdown menu of this button should show the default
    application for the given content type at top.
  @end{short}

  Since 3.2
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-get-show-default-item}"
  (setf (gtk-app-chooser-button-show-default-item self) setting))

(export 'gtk-app-chooser-button-set-show-default-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_get_show_dialog_item ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-get-show-dialog-item))

(defun gtk-app-chooser-button-get-show-dialog-item (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @return{The value of the @code{\"show-dialog-item\"} property.}
  @begin{short}
    Returns the current value of the @code{\"show-dialog-item\"} property.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-set-show-dialog-item}"
  (gtk-app-chooser-button-show-dialog-item self))

(export 'gtk-app-chooser-button-get-show-dialog-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_show_dialog_item ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-set-show-dialog-item))

(defun gtk-app-chooser-button-set-show-dialog-item (self setting)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @argument[setting]{the new value for the @code{\"show-dialog-item\"} property}
  @begin{short}
    Sets whether the dropdown menu of this button should show an entry to
    trigger a @class{gtk-app-chooser-dialog}.
  @end{short}

  Since 3.0
  @see-class{gtk-app-chooser-button}
  @see-class{gtk-app-chooser-dialog}"
  (setf (gtk-app-chooser-button-show-dialog-item self) setting))

(export 'gtk-app-chooser-button-set-show-dialog-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_get_heading ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-get-heading))

(defun gtk-app-chooser-button-get-heading (self)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @begin{return}
    The text to display at the top of the dialog, or @code{nil}, in which case a
    default text is displayed.
  @end{return}
  @short{Returns the text to display at the top of the dialog.}
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-set-heading}"
  (gtk-app-chooser-button-heading self))

(export 'gtk-app-chooser-button-get-heading)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_heading ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-app-chooser-button-set-heading))

(defun gtk-app-chooser-button-set-heading (self heading)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-1}
  @argument[self]{a @class{gtk-app-chooser-button} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Sets the text to display at the top of the dialog.
  @end{short}
  If the heading is not set, the dialog displays a default text.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-get-heading}"
  (setf (gtk-app-chooser-button-heading self) heading))

(export 'gtk-app-chooser-button-set-heading)

;;; --- End of file gtk.app-chooser-button.lisp --------------------------------

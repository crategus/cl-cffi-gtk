;;; ----------------------------------------------------------------------------
;;; gtk.app-chooser-button.lisp
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
;;; GtkAppChooserButton
;;;
;;;     A button to launch an application chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserButton
;;;
;;; Functions
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
;;; Properties
;;;
;;;        gchar*   heading                   Read / Write
;;;     gboolean    show-default-item         Read / Write / Construct
;;;     gboolean    show-dialog-item          Read / Write / Construct
;;;
;;; Signals
;;;
;;;         void    custom-item-activated     Has Details
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkComboBox
;;;                         ╰── GtkAppChooserButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAppChooserButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkCellLayout, GtkCellEditable and GtkAppChooser.
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
 "@version{2020-5-21}
  @begin{short}
    The @sym{gtk-app-chooser-button} is a widget that lets the user select an
    application. It implements the @class{gtk-app-chooser} interface.
  @end{short}

  Initially, a @sym{gtk-app-chooser-button} selects the first application in
  its list, which will either be the most-recently used application or, if the
  @code{show-default-item} property is @em{true}, the default application.

  The list of applications shown in a @sym{gtk-app-chooser-button} includes
  the recommended applications for the given content type. When the
  @code{show-default-item} property is set, the default application is also
  included. To let the user chooser other applications, you can set the
  @code{show-dialog-item} property, which allows to open a full
  @class{gtk-app-chooser-dialog}.

  It is possible to add custom items to the list, using the function
  @fun{gtk-app-chooser-button-append-custom-item}. These items cause the
  \"custom-item-activated\" signal to be emitted when they are selected.

  To track changes in the selected application, use the \"changed\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"custom-item-activated\" signal}
      @begin{pre}
 lambda (widget item-name)    : Has Details
      @end{pre}
      Emitted when a custom item, previously added with the function
      @fun{gtk-app-chooser-button-append-custom-item}, is activated from the
      dropdown menu.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-app-chooser-button} widget which received
          the signal.}
        @entry[item-name]{A string with the name of the activated item.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-app-chooser-button-heading}
  @see-slot{gtk-app-chooser-button-show-default-item}
  @see-slot{gtk-app-chooser-button-show-dialog-item}
  @see-class{gtk-app-chooser}
  @see-class{gtk-app-chooser-dialog}
  @see-function{gtk-app-chooser-button-append-custom-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-app-chooser-button-heading -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "heading"
                                               'gtk-app-chooser-button) 't)
 "The @code{heading} property of @code{g-string} (Read / Write) @br{}
  The text to show at the top of the dialog that can be opened from the
  button. The string may contain Pango markup. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-heading atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-heading 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-button-heading object) => heading}
  @syntax[]{(setf (gtk-app-chooser-button-heading object) heading)}
  @argument[object]{a @class{gtk-app-chooser-button} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-button]{heading} slot of the
    @class{gtk-app-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-button-heading} returns the
  text to display at the top of the dialog. The
  @sym{(setf gtk-app-chooser-button-heading)} sets the text to display at the
  top of the dialog.

  If the heading is not set, the dialog displays a default text.
  @see-class{gtk-app-chooser-button}")

;;; --- gtk-app-chooser-button-show-default-item -------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-default-item"
                                               'gtk-app-chooser-button) 't)
 "The @code{show-default-item} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the dropdown menu should show the default application on top for the
  provided content type. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-show-default-item
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-show-default-item 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-button-show-default-item object) => setting}
  @syntax[]{(setf (gtk-app-chooser-button-show-default-item object) setting)}
  @argument[object]{a @class{gtk-app-chooser-button} widget}
  @argument[setting]{a boolean whether the dropdown menu should show the default
    application}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-button]{show-default-item} slot of
    the @class{gtk-app-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-button-show-default-item}
  returns whether the dropdown menu of the button should show the default
  application. The slot access function
  @sym{(setf gtk-app-chooser-button-show-default-item)} sets whether the
  dropdown menu of the button should show the default application for the given
  content type at top.
  @see-class{gtk-app-chooser-button}")

;;; --- gtk-app-chooser-button-show-dialog-item --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-dialog-item"
                                               'gtk-app-chooser-button) 't)
 "The @code{show-dialog-item} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the dropdown menu should show an item that triggers a
  @sym{gtk-app-chooser-dialog} when clicked. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-app-chooser-button-show-dialog-item
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-app-chooser-button-show-dialog-item 'function)
 "@version{2020-5-21}
  @syntax[]{(gtk-app-chooser-button-show-dialog-item object) => setting}
  @syntax[]{(setf (gtk-app-chooser-button-show-dialog-item object) setting)}
  @argument[object]{a @class{gtk-app-chooser-button} widget}
  @argument[setting]{a boolean whether the dropdown menu shoult show a
    @class{gtk-app-chooser-dialog} widget}
  @begin{short}
    Accessor of the @slot[gtk-app-chooser-button]{show-dialog-item} slot of
    the @class{gtk-app-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-app-chooser-button-show-dialog-item} returns
  whether the dropdown menu of the button should show an entry to trigger a
  @class{gtk-app-chooser-dialog} widget. The slot access function
  @sym{(setf gtk-app-chooser-button-show-dialog-item)} sets the property.
  @see-class{gtk-app-chooser-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-app-chooser-button-new (content-type)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[content-type]{a string with the content type to show applications
    for}
  @return{A newly created @class{gtk-app-chooser-button} widget.}
  @begin{short}
    Creates a new application chooser button for applications that can handle
    content of the given type.
  @end{short}
  @see-class{gtk-app-chooser-button}"
  (make-instance 'gtk-app-chooser-button
                 :content-type (if content-type
                                   content-type
                                   (null-pointer))))

(export 'gtk-app-chooser-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_custom_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_append_custom_item"
           gtk-app-chooser-button-append-custom-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[widget]{a @class{gtk-app-chooser-button} widget}
  @argument[name]{a string with the name of the custom item}
  @argument[label]{a string with the label for the custom item}
  @argument[icon]{the icon of type @class{g-icon} for the custom item}
  @begin{short}
    Appends a custom item to the list of applications that is shown in the
    popup.
  @end{short}
  The item name must be unique per-widget.

  Clients can use the provided name as a detail for the
  \"custom-item-activated\" signal, to add a callback for the activation of a
  particular custom item in the list. See also the function
  @fun{gtk-app-chooser-button-append-separator}.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-button-append-separator}"
  (widget (g-object gtk-app-chooser-button))
  (name g-string)
  (label g-string)
  (icon (g-object g-icon)))

(export 'gtk-app-chooser-button-append-custom-item)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_append_separator ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_append_separator"
           gtk-app-chooser-button-append-separator) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[widget]{a @class{gtk-app-chooser-button} widget}
  @begin{short}
    Appends a separator to the list of applications that is shown in the popup.
  @end{short}
  @see-class{gtk-app-chooser-button}"
  (widget (g-object gtk-app-chooser-button)))

(export 'gtk-app-chooser-button-append-separator)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_button_set_active_custom_item ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_app_chooser_button_set_active_custom_item"
           gtk-app-chooser-button-set-active-custom-item) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-21}
  @argument[widget]{a @class{gtk-app-chooser-button} widget}
  @argument[name]{a string with the name of the custom item}
  @begin{short}
    Selects a custom item previously added with the function
    @fun{gtk-app-chooser-button-append-custom-item}.
  @end{short}

  Use the function @fun{gtk-app-chooser-refresh} to bring the selection to its
  initial state.
  @see-class{gtk-app-chooser-button}
  @see-function{gtk-app-chooser-refresh}
  @see-function{gtk-app-chooser-button-append-custom-item}"
  (widget (g-object gtk-app-chooser-button))
  (name g-string))

(export 'gtk-app-chooser-button-set-active-custom-item)

;;; --- End of file gtk.app-chooser-button.lisp --------------------------------

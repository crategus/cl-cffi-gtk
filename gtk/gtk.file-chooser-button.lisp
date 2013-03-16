;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;; GtkFileChooserButton
;;;
;;; A button to launch a file selection dialog
;;;
;;; Synopsis
;;;
;;;     GtkFileChooserButton
;;;
;;;     gtk_file_chooser_button_new
;;;     gtk_file_chooser_button_new_with_dialog
;;;     gtk_file_chooser_button_get_title
;;;     gtk_file_chooser_button_set_title
;;;     gtk_file_chooser_button_get_width_chars
;;;     gtk_file_chooser_button_set_width_chars
;;;     gtk_file_chooser_button_get_focus_on_click
;;;     gtk_file_chooser_button_set_focus_on_click
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFileChooserButton
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkFileChooserButton" 'gtk-file-chooser-button))

(define-g-object-class "GtkFileChooserButton" gtk-file-chooser-button
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFileChooser")
   :type-initializer "gtk_file_chooser_button_get_type")
  ((dialog
    gtk-file-chooser-button-dialog
    "dialog" "GtkFileChooser" nil nil)
   (focus-on-click
    gtk-file-chooser-button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (title
    gtk-file-chooser-button-title
    "title" "gchararray" t t)
   (width-chars
    gtk-file-chooser-button-width-chars
    "width-chars" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-chooser-button 'type)
 "@version{2013-3-3}
  @begin{short}
    The GtkFileChooserButton is a widget that lets the user select a file. It
    implements the GtkFileChooser interface. Visually, it is a file name with a
    button to bring up a GtkFileChooserDialog. The user can then use that dialog
    to change the file associated with that button. This widget does not support
    setting the \"select-multiple\" property to TRUE.
  @end{short}
  Example 88. Create a button to let the user select a file in /etc
  @begin{pre}
   {
     GtkWidget *button;

     button = gtk_file_chooser_button_new (_(\"Select a file\"),
                                           GTK_FILE_CHOOSER_ACTION_OPEN);
     gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (button),
                                          \"/etc\");
   @}
  @end{pre}
  The GtkFileChooserButton supports the GtkFileChooserActions
  GTK_FILE_CHOOSER_ACTION_OPEN and GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER.

  @subheading{Important}
  The GtkFileChooserButton will ellipsize the label, and thus will request
  little horizontal space. To give the button more space, you should
  call gtk_widget_get_preferred_size(),
  gtk_file_chooser_button_set_width_chars(), or pack the button in such a way
  that other interface elements give space to the widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"file-set\" signal}
      The @code{\"file-set\"} signal is emitted when the user selects a file.
      Note that this signal is only emitted when the user changes the file.
      @begin{pre}
 void user_function (GtkFileChooserButton *widget,
                     gpointer              user_data)      : Run First
      @end{pre}
      @begin[code]{table}
        @entry[widget]{the object which received the signal.}
        @entry[user_data]{user data set when the signal handler was connected.}
      @end{table}
      Since 2.12
  @end{dictionary}
  @see-slot{gtk-file-chooser-dialog}
  @see-slot{gtk-file-chooser-focus-on-click}
  @see-slot{gtk-file-chooser-title}
  @see-slot{gtk-file-chooser-width-chars}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "dialog" 'gtk-file-chooser-button) 't)
 "The @code{\"dialog\"} property of type @code{GtkFileChooser*}
  (Write / Construct Only)@br{}
  Instance of the GtkFileChooserDialog associated with the button.@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click" 'gtk-file-chooser-button) 't)
 "The @code{\"focus-on-click\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Whether the GtkFileChooserButton button grabs focus when it is clicked with
  the mouse.@br{}
  Default value: TRUE@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title" 'gtk-file-chooser-button) 't)
 "The @code{\"title\"} property of type @code{gchar*} (Read / Write)@br{}
  Title to put on the GtkFileChooserDialog associated with the button.@br{}
  Default value: \"Select a File\"@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars" 'gtk-file-chooser-button) 't)
 "The @code{\"width-chars\"} property of type @code{gint} (Read / Write)@br{}
  The width of the entry and label inside the button, in characters.@br{}
  Allowed values: >= G_MAXULONG@br{}
  Default value: -1@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-dialog atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-dialog 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the slot @code{\"dialog\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-focus-on-click atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-focus-on-click 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the slot @code{\"focus-on-click\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-title 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the slot @code{\"title\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-width-chars 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the slot @code{\"width-chars\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-expand 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-fill 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-padding 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-pack-type 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-position 'function)
 "@version{2013-3-3}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the
    @class{gtk-file-chooser-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-new))

(defun gtk-file-chooser-button-new (title action)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[title]{the title of the browse dialog.}
  @argument[action]{the open mode for the widget.}
  @return{a new button widget.}
  @begin{short}
    Creates a new file-selecting button widget.
  @end{short}

  Since 2.6"
  (make-instance 'gtk-file-chooser-button
                 :title title
                 :action action))

(export 'gtk-file-chooser-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new_with_dialog ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-new-with-dialog))

(defun gtk-file-chooser-button-new-with-dialog (dialog)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[dialog]{the widget to use as dialog}
  @return{a new button widget.}
  @begin{short}
    Creates a GtkFileChooserButton widget which uses dialog as its file-picking
    window.
  @end{short}

  Note that dialog must be a GtkDialog (or subclass) which implements the
  GtkFileChooser interface and must not have GTK_DIALOG_DESTROY_WITH_PARENT
  set.

  Also note that the dialog needs to have its confirmative button added with
  response GTK_RESPONSE_ACCEPT or GTK_RESPONSE_OK in order for the button to
  take over the file selected in the dialog.

  Since 2.6"
  (make-instance 'gtk-file-chooser-button
                 :dialog dialog))

(export 'gtk-file-chooser-button-new-with-dialog)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-get-title))

(defun gtk-file-chooser-button-get-title (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{the button widget to examine.}
  @return{a pointer to the browse dialog's title.}
  @begin{short}
    Retrieves the title of the browse dialog used by button. The returned value
    should not be modified or freed.
  @end{short}

  Since 2.6"
  (gtk-file-chooser-button-title button))

(export 'gtk-file-chooser-button-get-title)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_title ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-set-title))

(defun gtk-file-chooser-button-set-title (button title)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{the button widget to modify.}
  @argument[title]{the new browse dialog title.}
  @begin{short}
    Modifies the title of the browse dialog used by button.
  @end{short}

  Since 2.6"
  (setf (gtk-file-chooser-button-title button) title))

(export 'gtk-file-chooser-button-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-get-width-chars))

(defun gtk-file-chooser-button-get-width-chars (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{the button widget to examine.}
  @begin{return}
    an integer width (in characters) that the button will use to size itself.
  @end{return}
  @begin{short}
    Retrieves the width in characters of the button widget's entry and/or label.
  @end{short}

  Since 2.6"
  (gtk-file-chooser-button-width-chars button))

(export 'gtk-file-chooser-button-get-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-set-width-chars))

(defun gtk-file-chooser-button-set-width-chars (button n-chars)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{the button widget to examine.}
  @argument[n_chars]{the new width, in characters.}
  @begin{short}
    Sets the width (in characters) that button will use to n_chars.
  @end{short}

  Since 2.6"
  (setf (gtk-file-chooser-button-width-chars button) n-chars))

(export 'gtk-file-chooser-button-set-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_get_focus_on_click ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-get-focus-on-click))

(defun gtk-file-chooser-button-get-focus-on-click (button)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{a GtkFileChooserButton}
  @return{TRUE if the button grabs focus when it is clicked with the mouse.}
  @begin{short}
    Returns whether the button grabs focus when it is clicked with the mouse.
  @end{short}
  See gtk_file_chooser_button_set_focus_on_click().

  Since 2.10"
  (gtk-file-chooser-button-focus-on-click button))

(export 'gtk-file-chooser-button-get-focus-on-click)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_set_focus_on_click ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-set-focus-on-click))

(defun gtk-file-chooser-button-set-focus-on-click (button focus-on-click)
 #+cl-cffi-gtk-documentation
 "@version{2013-3-3}
  @argument[button]{a GtkFileChooserButton}
  @argument[focus_on_click]{whether the button grabs focus when clicked with
    the mouse}
  @begin{short}
    Sets whether the button will grab focus when it is clicked with the mouse.
  @end{short}
  Making mouse clicks not grab focus is useful in places like toolbars where
  you don't want the keyboard focus removed from the main area of the
  application.

  Since 2.10"
  (setf (gtk-file-chooser-button-focus-on-click button) focus-on-click))

(export 'gtk-file-chooser-button-set-focus-on-click)

;;; --- End of file gtk.file-chooser-button.lisp -------------------------------

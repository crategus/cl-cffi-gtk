;;; ----------------------------------------------------------------------------
;;; gtk.file-chooser-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;;     A button to launch a file selection dialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserButton
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_button_new
;;;     gtk_file_chooser_button_new_with_dialog
;;;     gtk_file_chooser_button_get_title                  Accessor
;;;     gtk_file_chooser_button_set_title                  Accessor
;;;     gtk_file_chooser_button_get_width_chars            Accessor
;;;     gtk_file_chooser_button_set_width_chars            Accessor
;;;     gtk_file_chooser_button_get_focus_on_click         Accessor
;;;     gtk_file_chooser_button_set_focus_on_click         Accessor
;;;
;;; Properties
;;;
;;;     GtkFileChooser*   dialog            Write / Construct Only
;;;           gboolean    focus-on-click    Read / Write
;;;              gchar*   title             Read / Write
;;;               gint    width-chars       Read / Write
;;;
;;; Signals
;;;
;;;               void    file-set          Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFileChooserButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooserButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkFileChooser.
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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-file-chooser-button 'type)
 "@version{2020-6-4}
  @begin{short}
    The @sym{gtk-file-chooser-button} is a widget that lets the user select a
    file.
  @end{short}

  @image[file-chooser-button]{}

  It implements the @class{gtk-file-chooser} interface. Visually, it is a file
  name with a button to bring up a @class{gtk-file-chooser-dialog}. The user
  can then use that dialog to change the file associated with that button. This
  widget does not support setting the @slot[gtk-file-chooser]{select-multiple}
  property to @em{true}.

  @b{Example:} Create a button to let the user select a file in /etc
  @begin{pre}
   {
     GtkWidget *button;

     button = gtk_file_chooser_button_new (_(\"Select a file\"),
                                           GTK_FILE_CHOOSER_ACTION_OPEN);
     gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (button),
                                          \"/etc\");
   @}
  @end{pre}
  The @sym{gtk-file-chooser-button} supports the
  @symbol{gtk-file-chooser-action}'s @code{:open} and @code{:select-folder}.

  @subheading{Important}
    The @sym{gtk-file-chooser-button} will ellipsize the label, and thus will
    request little horizontal space. To give the button more space, you should
    call the functions @fun{gtk-widget-preferred-size},
    @fun{gtk-file-chooser-button-width-chars}, or pack the button
    in such a way that other interface elements give space to the widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"file-set\" signal}
      @begin{pre}
 lambda (widget)    : Run First
      @end{pre}
      The \"file-set\" signal is emitted when the user selects a file. Note
      that this signal is only emitted when the user changes the file.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk-file-chooser-button} widget which received
          the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-file-chooser-button-dialog}
  @see-slot{gtk-file-chooser-button-focus-on-click}
  @see-slot{gtk-file-chooser-button-title}
  @see-slot{gtk-file-chooser-button-width-chars}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-file-chooser-button-dialog -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "dialog"
                                               'gtk-file-chooser-button) 't)
 "The @code{dialog} property of type @class{gtk-file-chooser}
  (Write / Construct Only) @br{}
  Instance of the @class{gtk-file-chooser-dialog} associated with the
  button.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-dialog atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-dialog 'function)
 "@version{2020-6-4}
  @syntax[]{(gtk-file-chooser-button-dialog object) => dialog}
  @syntax[]{(setf (gtk-file-chooser-button-dialog object) dialog)}
  @argument[object]{a @class{gtk-file-chooser-button} widget}
  @argument[dialog]{a @class{gtk-file-chooser-dialog} widget}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-button]{dialog} slot of the
    @class{gtk-file-chooser-button} class.
  @end{short}

  Instance of the file chooser dialog associated with the button.
  @see-class{gtk-file-chooser-button}
  @see-class{gtk-file-chooser-dialog}")

;;; --- gtk-file-chooser-button-focus-on-click ---------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click"
                                               'gtk-file-chooser-button) 't)
 "The @code{focus-on-click} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the file chooser button grabs focus when it is clicked with the
  mouse. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-focus-on-click
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-focus-on-click 'function)
 "@version{2020-6-4}
  @syntax[]{(gtk-file-chooser-button-focus-on-click object) => focus-on-click}
  @syntax[]{(setf (gtk-file-chooser-button-focus-on-click object) focus-on-click)}
  @argument[object]{a @class{gtk-file-chooser-button} widget to modify}
  @argument[focus-on-click]{a boolean whether the button grabs focus when
    clicked with the mouse}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-button]{focus-on-click} slot of the
    @class{gtk-file-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-file-chooser-button-focus-on-click} returns
  whether the button grabs focus when it is clicked with the mouse. The slot
  access function @sym{(setf gtk-file-chooser-button-focus-on-click)} sets
  whether the button will grab focus when it is clicked with the mouse.

  Making mouse clicks not grab focus is useful in places like toolbars where
  you do not want the keyboard focus removed from the main area of the
  application.
  @begin[Warning]{dictionary}
    The function @sym{gtk-file-chooser-button-focus-on-click} has been
    deprecated since version 3.20 and should not be used in newly-written code.
    Use the function @fun{gtk-widget-focus-on-click} instead.
  @end{dictionary}
  @see-class{gtk-file-chooser-button}
  @see-function{gtk-widget-focus-on-click}")

;;; --- gtk-file-chooser-button-title ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "title"
                                               'gtk-file-chooser-button) 't)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  Title to put on the @class{gtk-file-chooser-dialog} associated with the
  button. @br{}
  Default value: \"Select a File\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-title 'function)
 "@version{2020-6-4}
  @syntax[]{(gtk-file-chooser-button-title object) => title}
  @syntax[]{(setf (gtk-file-chooser-button-title object) title)}
  @argument[object]{a @class{gtk-file-chooser-button} widget to modify}
  @argument[title]{a string with the browse dialog title}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-button]{title} slot of the
    @class{gtk-file-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-file-chooser-button-title} retrieves the
  title of the browse dialog used by the the file chooser button. The slot
  access function @sym{(setf gtk-file-chooser-button-title)} modifies the title
  of the browse dialog used by the file chooser button.
  @see-class{gtk-file-chooser-button}")

;;; --- gtk-file-chooser-button-width-chars ------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "width-chars"
                                               'gtk-file-chooser-button) 't)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The width of the entry and label inside the button, in characters. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-width-chars 'function)
 "@version{2020-6-4}
  @syntax[]{(gtk-file-chooser-button-width-chars object) => n-chars}
  @syntax[]{(setf (gtk-file-chooser-button-width-chars object) n-chars)}
  @argument[object]{a @class{gtk-file-chooser-button} widget to modify}
  @argument[n-chars]{an integer with the width, in characters}
  @begin{short}
    Accessor of the @slot[gtk-file-chooser-button]{width-chars} slot of the
    @class{gtk-file-chooser-button} class.
  @end{short}

  The slot access function @sym{gtk-file-chooser-button-width-chars} retrieves
  the width in characters of the file chooser button's entry and/or label. The
  slot access function @sym{(setf gtk-file-chooser-button-width-chars)} sets the
  width, in characters, that the file chooser button will use to @arg{n-chars}.
  @see-class{gtk-file-chooser-button}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;; TODO: GtkFileChooserButton has no documented child properties
;;       We remove the implementation of the child properties.

#|

;;; --- gtk-file-chooser-button-child-expand -----------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-expand 'function)
 "@version{2013-8-27}
  Accessor of the @code{expand} child property of the
  @class{gtk-file-chooser-button} class.
  @see-class{gtk-file-chooser-button}")

;;; --- gtk-file-chooser-button-child-fill -------------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-fill 'function)
 "@version{2013-8-27}
  Accessor of the @code{fill} child property of the
  @class{gtk-file-chooser-button} class.
  @see-class{gtk-file-chooser-button}")

;;; --- gtk-file-chooser-button-child-padding ----------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-padding
                       "padding" "guint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-padding 'function)
 "@version{2013-8-27}
  Accessor of the @code{padding} child property of the
  @class{gtk-file-chooser-button} class.
  @see-class{gtk-file-chooser-button}")

;;; --- gtk-file-chooser-button-child-pack-type --------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-pack-type 'function)
 "@version{2013-8-27}
  Accessor of the @code{pack-type} child property of the
  @class{gtk-file-chooser-button} class.
  @see-class{gtk-file-chooser-button}")

;;; --- gtk-file-chooser-button-child-position ---------------------------------

(define-child-property "GtkFileChooserButton"
                       gtk-file-chooser-button-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-file-chooser-button-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-file-chooser-button-child-position 'function)
 "@version{2013-8-27}
  Accessor of the @code{position} child property of the
  @class{gtk-file-chooser-button} class.
  @see-class{gtk-file-chooser-button}")

|#

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-file-chooser-button-new))

(defun gtk-file-chooser-button-new (title action)
 #+cl-cffi-gtk-documentation
 "@version{2020-6-4}
  @argument[title]{a string with the title of the browse dialog}
  @argument[action]{the open mode for the widget of type
    @symbol{gtk-file-chooser-action}}
  @return{A new @class{gtk-file-chooser-button} widget.}
  @begin{short}
    Creates a new file selecting file chooser button widget.
  @end{short}
  @see-class{gtk-file-chooser-button}"
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
 "@version{2020-6-4}
  @argument[dialog]{a @class{gtk-dialog} widget to use as dialog}
  @return{A new @class{gtk-file-chooser} widget.}
  @begin{short}
    Creates a file chooser button which uses @arg{dialog} as its file picking
    window.
  @end{short}

  Note that dialog must be a @class{gtk-dialog}, or subclass, which implements
  the @class{gtk-file-chooser} interface and must not have
  @code{:destroy-with-parent} set.

  Also note that the dialog needs to have its confirmative button added with
  response @code{:accept} or @code{:ok} in order for the button to take over the
  file selected in the dialog.
  @see-class{gtk-file-chooser-button}"
  (make-instance 'gtk-file-chooser-button
                 :dialog dialog))

(export 'gtk-file-chooser-button-new-with-dialog)

;;; --- End of file gtk.file-chooser-button.lisp -------------------------------

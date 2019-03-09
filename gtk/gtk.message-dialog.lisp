;;; ----------------------------------------------------------------------------
;;; gtk.message-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkMessageDialog
;;;
;;;     A convenient message window
;;;
;;; Synopsis
;;;
;;;     GtkMessageDialog
;;;     GtkMessageType
;;;     GtkButtonsType
;;;
;;;     gtk_message_dialog_new
;;;     gtk_message_dialog_new_with_markup
;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_set_image
;;;     gtk_message_dialog_get_image
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup
;;;     gtk_message_dialog_get_message_area
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMessageDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMessageDialog" gtk-message-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_message_dialog_get_type")
  ((buttons
    gtk-message-dialog-buttons
    "buttons" "GtkButtonsType" nil nil)
   (image
    gtk-message-dialog-image
    "image" "GtkWidget" t t)
   (message-area
    gtk-message-dialog-message-area
    "message-area" "GtkWidget" t nil)
   (message-type
    gtk-message-dialog-message-type
    "message-type" "GtkMessageType" t t)
   (secondary-text
    gtk-message-dialog-secondary-text
    "secondary-text" "gchararray" t t)
   (secondary-use-markup
    gtk-message-dialog-secondary-use-markup
    "secondary-use-markup" "gboolean" t t)
   (text
    gtk-message-dialog-text
    "text" "gchararray" t t)
   (use-markup
    gtk-message-dialog-use-markup
    "use-markup" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-message-dialog 'type)
 "@version{2013-12-30}
  @begin{short}
    A @sym{gtk-message-dialog} window presents a dialog with an image
    representing the type of message, Error, Question, etc., alongside some
    message text.
  @end{short}
  It is simply a convenience widget; you could construct the equivalent of a
  @sym{gtk-message-dialog} window from a @class{gtk-dialog} window without too
  much effort, but @sym{gtk-message-dialog} saves typing.

  One difference from @class{gtk-dialog} is that @sym{gtk-message-dialog} sets
  the @code{\"skip-taskbar-hint\"} property to @em{true}, so that the dialog is
  hidden from the taskbar by default.

  The easiest way to do a modal message dialog is to use the function
  @fun{gtk-dialog-run}, though you can also pass in the @code{:modal} flag of
  type @symbol{gtk-dialog-flags}, the function @fun{gtk-dialog-run}
  automatically makes the dialog modal and waits for the user to respond to it.
  The function @fun{gtk-dialog-run} returns when any dialog button is clicked.

  @b{Example:} A modal dialog.
  @begin{pre}
(let ((dialog (gtk-message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      :close
                                      \"Error loading file ~s\"
                                      filename)))
  (gtk-dialog-run dialog)
  (gtk-widget-destroy dialog))
  @end{pre}
  You might do a non-modal @sym{gtk-message-dialog} as follows:

  @b{Example:} A non-modal dialog.
  @begin{pre}
(let ((dialog (gtk-message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      :close
                                      \"Error loading file ~s\"
                                      filename)))
  ;; Destroy the dialog when the user responds to it
  (g-signal-connect dialog \"response\"
                    (lambda (dialog response-id)
                      (declare (ignore response-id))
                      (gtk-widget-destroy dialog)))
  ... )
  @end{pre}
  @subheading{GtkMessageDialog as GtkBuildable}
    The @sym{gtk-message-dialog} implementation of the @class{gtk-buildable}
    interface exposes the message area as an internal child with the name
    \"message_area\".
  @begin[Style Properties]{dictionary}
    @subheading{The \"message-border\" style property}
      @code{\"message-border\"} of type @code{:int} (Read) @br{}
      Width of border around the label and image in the message dialog. @br{}
      Allowed values: >= 0 @br{}
      Default value: 12
  @end{dictionary}
  @see-slot{gtk-message-dialog-buttons}
  @see-slot{gtk-message-dialog-image}
  @see-slot{gtk-message-dialog-message-area}
  @see-slot{gtk-message-dialog-message-type}
  @see-slot{gtk-message-dialog-secondary-text}
  @see-slot{gtk-message-dialog-secondary-use-markup}
  @see-slot{gtk-message-dialog-text}
  @see-slot{gtk-message-dialog-use-markup}
  @see-class{gtk-dialog}
  @see-symbol{gtk-dialog-flags}
  @see-function{gtk-dialog-run}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-message-dialog-buttons ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "buttons"
                                               'gtk-message-dialog) 't)
 "The @code{\"buttons\"} property of type @symbol{gtk-buttons-type}
  (Write / Construct Only) @br{}
  The buttons shown in the message dialog. @br{}
  @b{Warning:} This property is not accessible from the Lisp binding. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-buttons atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-buttons 'function)
 "@version{2014-3-4}
  Accessor of the slot @slot[gtk-message-dialog]{buttons} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}")

;;; --- gtk-message-dialog-image -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "image" 'gtk-message-dialog) 't)
 "The @code{\"image\"} property of type @class{gtk-widget} (Read / Write) @br{}
  The image for this dialog. @br{}
  @b{Warning:} @code{\"image\"} has been deprecated since version 3.12 and
  should not be used in newly-written code. Use @class{gtk-dialog} to create
  dialogs with images. @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-image atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-image 'function)
 "@version{2014-2-12}
  @argument[object]{a @class{gtk-message-dialog} window}
  @argument[image]{the image}
  @syntax[]{(gtk-message-dialog-image object) => image}
  @syntax[]{(setf (gtk-message-dialog-image object) image)}
  @begin{short}
    Accessor of the slot @slot[gtk-message-dialog]{image} of the
    @class{gtk-message-dialog} class.
  @end{short}

  The generic function @sym{gtk-message-dialog-image} returns the dialog's
  image.

  The generic function @sym{(setf gtk-message-dialog-image)} sets the dialog's
  image to @arg{image}.
  @begin[Warning]{dictionary}
    The generic function @sym{gtk-message-dialog-image} has been deprecated
    since version 3.12 and should not be used in newly-written code.
    Use @class{gtk-dialog} to create dialogs with images.
  @end{dictionary}

  Since 2.14
  @see-class{gtk-message-dialog}")

;;; --- gtk-message-dialog-message-area ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "message-area"
                                               'gtk-message-dialog) 't)
 "The @code{\"message-area\"} property of type @class{gtk-widget} (Read) @br{}
  The @class{gtk-box} widget of orientation @code{:vertical} that corresponds
  to the message area of this dialog. See the generic function
  @fun{gtk-message-dialog-message-area} for a detailed description of this
  area. @br{}
  Since 2.22")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-message-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-message-area 'function)
 "@version{2014-3-4}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @syntax[]{(gtk-message-dialog-message-area object) => message-area}
  @begin{short}
    Accessor of the slot @slot[gtk-message-dialog]{message-area} of the
    @class{gtk-message-dialog} class.
  @end{short}

  The generic function @sym{gtk-message-dialog-message-area} returns the
  @class{gtk-box} widget with orientation @code{:vertical} corresponding to
  the \"message area\" in the message @arg{dialog}. This is the box where the
  @arg{dialog}'s primary and secondary labels are packed.

  You can add your own extra content to that box and it will appear below those
  labels, on the right side of the dialog's image, or on the left for
  right-to-left languages. See the function @fun{gtk-dialog-get-content-area}
  for the corresponding function in the parent @class{gtk-dialog} class.

  Since 2.22
  @see-class{gtk-message-dialog}
  @see-class{gtk-dialog}
  @see-function{gtk-dialog-get-content-area}")

;;; --- gtk-message-dialog-message-type ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "message-type"
                                               'gtk-message-dialog) 't)
 "The @code{\"message-type\"} property of type @symbol{gtk-message-type}
  (Read / Write / Construct) @br{}
  The type of the message. The type is used to determine the image that is
  shown in the dialog, unless the image is explicitly set by the
  @code{\"image\"} property. @br{}
  Default value: @code{:info}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-message-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-message-type 'function)
 "@version{2014-3-4}
  Accessor of the slot @slot[gtk-message-dialog]{message-type} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}")

;;; --- gtk-message-dialog-secondary-text --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-text"
                                               'gtk-message-dialog) 't)
 "The @code{\"secondary-text\"} property of type @code{:string}
  (Read / Write) @br{}
  The secondary text of the message dialog. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-secondary-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-secondary-text 'function)
 "@version{2014-3-5}
  Accessor of the slot @slot[gtk-message-dialog]{secondary-text} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}
  @see-function{gtk-message-dialog-format-secondary-text}")

;;; --- gtk-message-dialog-secondary-use-markup --------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "secondary-use-markup"
                                               'gtk-message-dialog) 't)
 "The @code{\"secondary-use-markup\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the secondary text of the dialog includes Pango markup. See
  the function @fun{pango-parse-markup}. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-secondary-use-markup
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-secondary-use-markup 'function)
 "@version{2014-3-5}
  Accessor of the slot @slog[gtk-message-dialog]{secondary-use-markup} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}
  @see-function{gtk-message-dialog-format-secondary-markup}")

;;; --- gtk-message-dialog-text ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-message-dialog) 't)
 "The @code{\"text\"} property of type @code{:string} (Read / Write) @br{}
  The primary text of the message dialog. If the dialog has a secondary text,
  this will appear as the title. @br{}
  Default value: \"\" @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-text 'function)
 "@version{2014-3-5}
  Accessor of the slot @slot[gtk-message-dialog]{text} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}")

;;; --- gtk-message-dialog-use-markup ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup"
                                               'gtk-message-dialog) 't)
 "The @code{\"use-markup\"} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the primary text of the dialog includes Pango markup. See
  the function @fun{pango-parse-markup}. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-dialog-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-message-dialog-use-markup 'function)
 "@version{2014-3-5}
  Accessor of the slot @slot[gtk-message-dialog]{use-markup} of the
  @class{gtk-message-dialog} class.
  @see-class{gtk-message-dialog}")

;;; ----------------------------------------------------------------------------
;;; enum GtkMessageType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkMessageType" gtk-message-type
  (:export t
   :type-initializer "gtk_message_type_get_type")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-message-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-message-type atdoc:*external-symbols*)
 "@version{2013-9-10}
  @begin{short}
    The type of message being displayed in the dialog.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkMessageType\" gtk-message-type
  (:export t
   :type-initializer \"gtk_message_type_get_type\")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))
  @end{pre}
  @begin[code]{table}
    @entry[:info]{Informational message.}
    @entry[:warning]{Nonfatal warning message.}
    @entry[:question]{Question requiring a choice.}
    @entry[:error]{Fatal error message.}
    @entry[:other]{None of the above, does not get an icon.}
  @end{table}
  @see-class{gtk-message-dialog}")

;;; ----------------------------------------------------------------------------
;;; enum GtkButtonsType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkButtonsType" gtk-buttons-type
  (:export t
   :type-initializer "gtk_buttons_type_get_type")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-buttons-type atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-buttons-type atdoc:*external-symbols*)
 "@version{2013-9-10}
  @begin{short}
    Prebuilt sets of buttons for the dialog. If none of these choices are
    appropriate, simply use @code{:none} and call the function
    @fun{gtk-dialog-add-buttons} to add your own buttons.
  @end{short}

  @subheading{Note}
    Please note that @code{:ok}, @code{:yes-no} and @code{:ok-cancel} are
    discouraged by the Gnome Human Interface Guidelines.
  @begin{pre}
(define-g-enum \"GtkButtonsType\" gtk-buttons-type
  (:export t
   :type-initializer \"gtk_buttons_type_get_type\")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No buttons at all.}
    @entry[:ok]{An OK button.}
    @entry[:close]{A Close button.}
    @entry[:cancel]{A Cancel button.}
    @entry[:yes-no]{Yes and No buttons.}
    @entry[:ok-cancel]{OK and Cancel buttons.}
  @end{table}
  @see-class{gtk-message-dialog}
  @see-function{gtk-dialog-add-buttons}")

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-message-dialog-new (parent flags type buttons message &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[parent]{transient parent, or @code{nil} for none}
  @argument[flags]{flags of type @symbol{gtk-dialog-flags}}
  @argument[type]{type of message of type @symbol{gtk-message-type}}
  @argument[buttons]{set of buttons to use of type @symbol{gtk-buttons-type}}
  @argument[message]{format string, or @code{nil}}
  @argument[args]{the arguments for @arg{message}}
  @return{A new @class{gtk-message-dialog} window.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with an icon
    indicating the dialog type (error, warning, etc.) and some text the user may
    want to see.
  @end{short}
  When the user clicks a button a \"response\" signal is emitted with response
  IDs from the @symbol{gtk-response-type} enumeration. See @class{gtk-dialog}
  for more details.
  @see-class{gtk-message-dialog}
  @see-class{gtk-dialog}
  @see-symbol{gtk-dialog-flags}
  @see-symbol{gtk-message-type}
  @see-symbol{gtk-buttons-type}
  @see-symbol{gtk-response-type}"
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (gtk-message-dialog-text dialog)
        (apply #'format (cons nil (cons message args)))))
    (if parent
        (setf (gtk-window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (gtk-window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (gtk-window-destroy-with-parent dialog) t))
    dialog))

(export 'gtk-message-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new_with_markup ()
;;; ----------------------------------------------------------------------------

(defun gtk-message-dialog-new-with-markup (parent flags type buttons message
                                           &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-2}
  @argument[parent]{transient parent, or @code{nil} for none}
  @argument[flags]{flags of type @symbol{gtk-dialog-flags}}
  @argument[type]{type of message of type @symbol{gtk-message-type}}
  @argument[buttons]{set of buttons to use of type @symbol{gtk-buttons-type}}
  @argument[message]{format string, or @code{nil}}
  @argument[args]{the arguments for @arg{message}}
  @return{A new @class{gtk-message-dialog} window.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with an icon
    indicating the dialog type (error, warning, etc.) and some text which is
    marked up with the Pango text markup language.
  @end{short}
  When the user clicks a button a \"response\" signal is emitted with response
  IDs from the @symbol{gtk-response-type} enumeration. See
  @class{gtk-dialog} for more details.

  Special XML characters in the message arguments passed to this function will
  automatically be escaped as necessary. Usually this is what you want, but if
  you have an existing Pango markup string that you want to use literally as the
  label, then you need to use the function @fun{gtk-message-dialog-set-markup}
  instead, since you cannot pass the markup string either as the format, it
  might contain '%' characters, or as a string argument.
  @begin{pre}
(let ((dialog (gtk-message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      close
                                      nil)))
  (gtk-message-dialog-set-markup dialog markup)
  ... )
  @end{pre}
  Since 2.4
  @see-class{gtk-message-dialog}
  @see-symbol{gtk-dialog-flags}
  @see-symbol{gtk-message-type}
  @see-symbol{gtk-buttons-type}
  @see-symbol{gtk-response-type}
  @see-function{gtk-message-dialog-set-markup}"
  (let ((dialog (make-instance 'gtk-message-dialog
                               :use-markup t
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (gtk-message-dialog-text dialog)
        (apply #'format (cons nil (cons message args)))))
    (if parent
        (setf (gtk-window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (gtk-window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (gtk-window-destroy-with-parent dialog) t))
    dialog))

(export 'gtk-message-dialog-new-with-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_markup ()
;;; ----------------------------------------------------------------------------

(defun gtk-message-dialog-set-markup (dialog text)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @argument[text]{markup string, see Pango markup format}
  @begin{short}
    Sets the text of the message @arg{dialog} to be @arg{text}, which is marked
    up with the Pango text markup language.
  @end{short}

  Since 2.4
  @see-class{gtk-message-dialog}"
  (setf (gtk-message-dialog-use-markup dialog) t
        (gtk-message-dialog-text dialog) text))

(export 'gtk-message-dialog-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-format-secondary-text))

(defun gtk-message-dialog-format-secondary-text (dialog message &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @argument[message]{format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text of the message dialog to be @arg{message} with
    the arguments @arg{args}.
  @end{short}

  Note that setting a secondary text makes the primary text become bold,
  unless you have provided explicit markup.

  Since 2.6
  @see-class{gtk-message-dialog}
  @see-function{gtk-message-dialog-format-secondary-markup}"
  (setf (gtk-message-dialog-secondary-text dialog)
        (apply #'format (cons nil (cons message args)))))

(export 'gtk-message-dialog-format-secondary-text)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-message-dialog-format-secondary-markup))

(defun gtk-message-dialog-format-secondary-markup (dialog message &rest args)
 #+cl-cffi-gtk-documentation
 "@version{2013-9-10}
  @argument[dialog]{a @class{gtk-message-dialog} window}
  @argument[message]{markup string, see Pango markup format, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text of the message dialog to be @arg{message} with
    the arguments @arg{args}, which is marked up with the Pango text markup
    language.
  @end{short}

  Note that setting a secondary text makes the primary text become bold,
  unless you have provided explicit markup.

  Due to an oversight in the C implementation, this function does not escape
  special XML characters like the function
  @fun{gtk-message-dialog-new-with-markup} does.

  Since 2.6
  @see-class{gtk-message-dialog}
  @see-function{gtk-message-dialog-new-with-markup}
  @see-function{gtk-message-dialog-format-secondary-text}"
  (setf (gtk-message-dialog-secondary-use-markup dialog) t
        (gtk-message-dialog-secondary-text dialog)
        (apply #'format (cons nil (cons message args)))))

(export 'gtk-message-dialog-format-secondary-markup)

;;; --- End of file gtk.message-dialog.lisp ------------------------------------

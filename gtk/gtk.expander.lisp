;;; ----------------------------------------------------------------------------
;;; gtk.expander.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkExpander
;;; 
;;; A container which can hide its child
;;;     
;;; Synopsis
;;; 
;;;     GtkExpander
;;;     
;;;     gtk_expander_new
;;;     gtk_expander_new_with_mnemonic
;;;     gtk_expander_set_expanded
;;;     gtk_expander_get_expanded
;;;     gtk_expander_set_spacing
;;;     gtk_expander_get_spacing
;;;     gtk_expander_set_label
;;;     gtk_expander_get_label
;;;     gtk_expander_set_use_underline
;;;     gtk_expander_get_use_underline
;;;     gtk_expander_set_use_markup
;;;     gtk_expander_get_use_markup
;;;     gtk_expander_set_label_widget
;;;     gtk_expander_get_label_widget
;;;     gtk_expander_set_label_fill
;;;     gtk_expander_get_label_fill
;;;     gtk_expander_set_resize_toplevel
;;;     gtk_expander_get_resize_toplevel
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkExpander
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkExpander implements AtkImplementorIface and GtkBuildable.
;;; 
;;; Style Properties
;;; 
;;;   "expander-size"            gint                 : Read
;;;   "expander-spacing"         gint                 : Read
;;; 
;;; Signals
;;; 
;;;   "activate"                                      : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkExpander
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkExpander" gtk-expander
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_expander_get_type")
  ((expanded
    gtk-expander-expanded
    "expanded" "gboolean" t t)
   (label
    gtk-expander-label
    "label" "gchararray" t t)
   (label-fill
    gtk-expander-label-fill
    "label-fill" "gboolean" t t)
   (label-widget
    gtk-expander-label-widget
    "label-widget" "GtkWidget" t t)
   (resize-toplevel
    gtk-expander-resize-toplevel
    "resize-toplevel" "gboolean" t t)
   (spacing
    gtk-expander-spacing
    "spacing" "gint" t t)
   (use-markup
    gtk-expander-use-markup
    "use-markup" "gboolean" t t)
   (use-underline
    gtk-expander-use-underline
    "use-underline" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-expander 'type)
 "@version{2013-5-10}
  @begin{short}
    A @sym{gtk-expander} allows the user to hide or show its child by clicking
    on an expander triangle similar to the triangles used in a
    @class{gtk-tree-view}.
  @end{short}

  Normally you use an expander as you would use any other descendant of
  @class{gtk-bin}; you create the child widget and use the function
  @fun{gtk-container-add} to add it to the expander. When the expander is
  toggled, it will take care of showing and hiding the child automatically.
 
  @subheading{Special Usage}
    There are situations in which you may prefer to show and hide the expanded
    widget yourself, such as when you want to actually create the widget at
    expansion time. In this case, create a @sym{gtk-expander} but do not add a
    child to it. The expander widget has an \"expanded\" property which can be
    used to monitor its expansion state. You should watch this property with a
    signal connection as follows:
    @begin{pre}
 expander = gtk_expander_new_with_mnemonic (\"_More Options\");
 g_signal_connect (expander, \"notify::expanded\",
                   G_CALLBACK (expander_callback), NULL);

 ...

 static void
 expander_callback (GObject    *object,
                    GParamSpec *param_spec,
                    gpointer    user_data)
 {
   GtkExpander *expander;

   expander = GTK_EXPANDER (object);

   if (gtk_expander_get_expanded (expander))
     {
       /* Show or create widgets */
     @}
   else
     {
       /* Hide or destroy widgets */
     @}
 @}
    @end{pre}
  @subheading{GtkExpander as GtkBuildable}
    The @sym{gtk-expander} implementation of the @class{gtk-buildable}
    interface supports placing a child in the label position by specifying
    \"label\" as the \"type\" attribute of a <child> element. A normal content
    child can be specified without specifying a <child> type attribute.

    @b{Example:} A UI definition fragment with @sym{gtk-expander}
    @begin{pre}
 <object class=\"GtkExpander\">
   <child type=\"label\">
     <object class=\"GtkLabel\" id=\"expander-label\"/>
   </child>
   <child>
     <object class=\"GtkEntry\" id=\"expander-content\"/>
   </child>
 </object>
    @end{pre}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"expander-size\" style property}
      @code{\"expander-size\"} of type @code{:int} (Read) @br{}
      Size of the expander arrow. @br{}
      Allowed values: >= 0 @br{}
      Default value: 10

    @subheading{The \"expander-spacing\" style property}
      @code{\"expander-spacing\"} of type @code{:int} (Read) @br{}
      Spacing around expander arrow. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
 lambda (expander)   : Action
      @end{pre}
  @end{dictionary}
  @see-slot{gtk-expander-expanded}
  @see-slot{gtk-expander-label}
  @see-slot{gtk-expander-label-fill}
  @see-slot{gtk-expander-label-widget}
  @see-slot{gtk-expander-resize-toplevel}
  @see-slot{gtk-expander-spacing}
  @see-slot{gtk-expander-use-markup}
  @see-slot{gtk-expander-use-underline}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expanded" 'gtk-expander) 't)
 "The @code{\"expanded\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the expander has been opened to reveal the child widget. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-expander) 't)
 "The @code{\"label\"} property of type @code{:string}
  (Read / Write / Construct) @br{}
  Text of the expander's label. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-fill" 'gtk-expander) 't)
 "The @code{\"label-fill\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the label widget should fill all available horizontal space. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget" 'gtk-expander) 't)
 "The @code{\"label-widget\"} property of type @code{gtk-widget}
  (Read / Write) @br{}
  A widget to display in place of the usual expander label.")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resize-toplevel"
                                               'gtk-expander) 't)
 "The @code{\"resize-toplevel\"} property of type @code{:boolean}
  (Read / Write) @br{}
  When this property is @em{true}, the expander will resize the toplevel widget
  containing the expander upon expanding and collapsing. @br{}
  Default value: @code{nil} @br{}
  Since 3.2")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-expander) 't)
 "The @code{\"spacing\"} property of type @code{:int} (Read / Write) @br{}
  Space to put between the label and the child. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-expander) 't)
 "The @code{\"use-markup\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The text of the label includes XML markup. See the function
  @fun{pango-parse-markup}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline"
                                               'gtk-expander) 't)
 "The @code{\"use-underline\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-expanded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-expanded 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"expanded\"} of the @class{gtk-expander} class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-expanded}
  @see-function{gtk-expander-set-expanded}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"label\"} of the @class{gtk-expander} class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-label}
  @see-function{gtk-expander-set-label}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label-fill 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"label-fill\"} of the @class{gtk-expander} class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-label-fill}
  @see-function{gtk-expander-set-label-fill}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label-widget 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"label-widget\"} of the @class{gtk-expander}
  class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-label-widget}
  @see-function{gtk-expander-set-label-widget}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-resize-toplevel atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-resize-toplevel 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"resize-toplevel\"} of the @class{gtk-expander}
  class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-resize-toplevel}
  @see-function{gtk-expander-set-resize-toplevel}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-spacing 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"spacing\"} of the @class{gtk-expander} class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-spacing}
  @see-function{gtk-expander-set-spacing}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-use-markup 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"use-markup\"} of the @class{gtk-expander} class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-use-markup}
  @see-function{gtk-expander-set-use-markup}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-use-underline 'function)
 "@version{2013-11-6}
  Accessor of the slot @code{\"use-underline\"} of the @class{gtk-expander}
  class.
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-use-underline}
  @see-function{gtk-expander-set-use-underline}")

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-new))

(defun gtk-expander-new (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[label]{the text of the label}
  @return{A new @class{gtk-expander} container.}
  @begin{short}
    Creates a new expander using @arg{label} as the text of the label.
  @end{short}

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-new-with-mnemonic}"
  (make-instance 'gtk-expander
                 :label label))

(export 'gtk-expander-new)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-new-with-mnemonic))

(defun gtk-expander-new-with-mnemonic (label)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[label]{the text of the label with an underscore in front of the
    mnemonic character}
  @return{A new @class{gtk-expander} widget.}
  @begin{short}
    Creates a new expander using @arg{label} as the text of the label.
  @end{short}
  If characters in label are preceded by an underscore, they are underlined. If
  you need a literal underscore character in a label, use two underscores '__'.
  The first underlined character represents a keyboard accelerator called a
  mnemonic. Pressing Alt and that key activates the button.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-new}"
  (make-instance 'gtk-expander
                 :label label
                 :use-underline t))

(export 'gtk-expander-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_expanded ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-expanded))

(defun gtk-expander-set-expanded (expander expanded)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[expanded]{whether the child widget is revealed}
  @begin{short}
    Sets the state of the expander.
  @end{short}
  Set to @em{true}, if you want the child widget to be revealed, and @code{nil}
  if you want the child widget to be hidden.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-expanded}"
  (setf (gtk-expander-expanded expander) expanded))

(export 'gtk-expander-set-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_expanded ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-expanded))

(defun gtk-expander-get-expanded (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @return{The current state of the expander.}
  @begin{short}
    Queries a @class{gtk-expander} and returns its current state.
  @end{short}
  Returns @em{true} if the child widget is revealed.

  See the function @class{gtk-expander-set-expanded}.

  Since 2.4
  @see-class{gtk-expander}
  @class{gtk-expander-set-expanded}"
  (gtk-expander-expanded expander))

(export 'gtk-expander-get-expanded)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-spacing))

(defun gtk-expander-set-spacing (expander spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[spacing]{distance between the expander and child in pixels}
  @begin{short}
    Sets the spacing field of @arg{expander}, which is the number of pixels to
    place between expander and the child.
  @end{short}

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-spacing}"
  (setf (gtk-expander-spacing expander) spacing))

(export 'gtk-expander-set-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-spacing))

(defun gtk-expander-get-spacing (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @return{Spacing between the expander and child.}
  @begin{short}
    Gets the value set by @fun{gtk-expander-set-spacing}.
  @end{short}

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-spacing}"
  (gtk-expander-spacing expander))

(export 'gtk-expander-get-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-label))

(defun gtk-expander-set-label (expander label)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[label]{a string}
  @begin{short}
    Sets the text of the label of the expander to @arg{label}.
  @end{short}

  This will also clear any previously set labels.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-label}"
  (setf (gtk-expander-label expander) label))

(export 'gtk-expander-set-label)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-label))

(defun gtk-expander-get-label (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @begin{return}
    The text of the label widget. This string is owned by the widget and
    must not be modified or freed.
  @end{return}
  @begin{short}
    Fetches the text from a label widget including any embedded underlines
    indicating mnemonics and Pango markup, as set by the function
    @fun{gtk-expander-set-label}.
  @end{short}
  If the label text has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the function
  @fun{gtk-button-new} to use as a container.

  Note that this function behaved differently in versions prior to 2.14 and
  used to return the label text stripped of embedded underlines indicating
  mnemonics and Pango markup. This problem can be avoided by fetching the
  label text directly from the label widget.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-label}
  @see-function{gtk-button-new}"
  (gtk-expander-label expander))

(export 'gtk-expander-get-label)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-use-underline))

(defun gtk-expander-set-use-underline (expander use-underline)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[use-underline]{@em{true} if underlines in the text indicate
    mnemonics}
  @begin{short}
    If @em{true}, an underline in the text of the expander label indicates the
    next character should be used for the mnemonic accelerator key.
  @end{short}

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-use-underline}"
  (setf (gtk-expander-use-underline expander) use-underline))

(export 'gtk-expander-set-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_use_underline ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-use-underline))

(defun gtk-expander-get-use-underline (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @begin{return}
    @em{True} if an embedded underline in the expander label indicates the
    mnemonic accelerator keys.
  @end{return}
  @begin{short}
    Returns whether an embedded underline in the expander label indicates a
    mnemonic.
  @end{short}
  See the function @fun{gtk-expander-set-use-underline}.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-use-underline}"
  (gtk-expander-use-underline expander))

(export 'gtk-expander-get-use-underline)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_use_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-use-markup))

(defun gtk-expander-set-use-markup (expander use-markup)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[use-markup]{@em{true} if the label's text should be parsed for
    markup}
  @begin{short}
    Sets whether the text of the label contains markup in Pango's text markup
    language.
  @end{short}
  See the function @fun{gtk-label-set-markup}.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-use-markup}
  @see-function{gtk-label-set-markup}"
  (setf (gtk-expander-use-markup expander) use-markup))

(export 'gtk-expander-set-use-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_use_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-use-markup))

(defun gtk-expander-get-use-markup (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @return{@em{True} if the label's text will be parsed for markup.}
  @begin{short}
    Returns whether the label's text is interpreted as marked up with the Pango
    text markup language.
  @end{short}
  See the function @fun{gtk-expander-set-use-markup}.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-use-markup}"
  (gtk-expander-use-markup expander))

(export 'gtk-expander-get-use-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-label-widget))

(defun gtk-expander-set-label-widget (expander label-widget)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[label-widget]{the new label widget}
  @begin{short}
    Set the label widget for the expander.
  @end{short}
  This is the widget that will appear embedded alongside the expander arrow.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-label-widget}"
  (setf (gtk-expander-label-widget expander) label-widget))

(export 'gtk-expander-set-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label_widget ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-label-widget))

(defun gtk-expander-get-label-widget (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @return{The label widget, or @code{nil} if there is none.}
  @begin{short}
    Retrieves the label widget for the frame.
  @end{short}
  See the function @fun{gtk-expander-set-label-widget}.

  Since 2.4
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-label-widget}"
  (gtk-expander-label-widget expander))

(export 'gtk-expander-get-label-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label_fill ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-label-fill))

(defun gtk-expander-set-label-fill (expander label-fill)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[label-fill]{@em{true} if the label should should fill all available
    horizontal space}
  @begin{short}
    Sets whether the label widget should fill all available horizontal space
    allocated to expander.
  @end{short}

  Since 2.22
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-label-fill}"
  (setf (gtk-expander-label-fill expander) label-fill))

(export 'gtk-expander-set-label-fill)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label_fill ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-label-fill))

(defun gtk-expander-get-label-fill (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @begin{return}
    @em{True} if the label widget will fill all available horizontal space.
  @end{return}
  @begin{short}
    Returns whether the label widget will fill all available horizontal space
    allocated to expander.
  @end{short}

  Since 2.22
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-label-fill}"
  (gtk-expander-label-fill expander))

(export 'gtk-expander-get-label-fill)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_resize_toplevel ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-set-resize-toplevel))

(defun gtk-expander-set-resize-toplevel (expander resize-toplevel)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @argument[resize-toplevel]{whether to resize the toplevel}
  @begin{short}
    Sets whether the expander will resize the toplevel widget containing the
    expander upon resizing and collpasing.
  @end{short}

  Since 3.2
  @see-class{gtk-expander}
  @see-function{gtk-expander-get-resize-toplevel}"
  (setf (gtk-expander-resize-toplevel expander) resize-toplevel))

(export 'gtk-expander-set-resize-toplevel)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_resize_toplevel ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-expander-get-resize-toplevel))

(defun gtk-expander-get-resize-toplevel (expander)
 #+cl-cffi-gtk-documentation
 "@version{2013-11-6}
  @argument[expander]{a @class{gtk-expander} widget}
  @return{The \"resize toplevel\" setting.}
  @begin{short}
    Returns whether the expander will resize the toplevel widget containing the
    expander upon resizing and collpasing.
  @end{short}

  Since 3.2
  @see-class{gtk-expander}
  @see-function{gtk-expander-set-resize-toplevel}"
  (gtk-expander-resize-toplevel expander))

(export 'gtk-expander-get-resize-toplevel)

;;; --- End of file gtk.expander.lisp ------------------------------------------

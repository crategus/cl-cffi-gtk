;;; ----------------------------------------------------------------------------
;;; gtk.expander.lisp
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
;;; GtkExpander
;;;
;;;     A container which can hide its child
;;;
;;; Types and Values
;;;
;;;     GtkExpander
;;;
;;; Functions
;;;
;;;     gtk_expander_new
;;;     gtk_expander_new_with_mnemonic
;;;     gtk_expander_set_expanded                          Accessor
;;;     gtk_expander_get_expanded                          Accessor
;;;     gtk_expander_set_spacing                           Accessor
;;;     gtk_expander_get_spacing                           Accessor
;;;     gtk_expander_set_label                             Accessor
;;;     gtk_expander_get_label                             Accessor
;;;     gtk_expander_set_use_underline                     Accessor
;;;     gtk_expander_get_use_underline                     Accessor
;;;     gtk_expander_set_use_markup                        Accessor
;;;     gtk_expander_get_use_markup                        Accessor
;;;     gtk_expander_set_label_widget                      Accessor
;;;     gtk_expander_get_label_widget                      Accessor
;;;     gtk_expander_set_label_fill                        Accessor
;;;     gtk_expander_get_label_fill                        Accessor
;;;     gtk_expander_set_resize_toplevel                   Accessor
;;;     gtk_expander_get_resize_toplevel                   Accessor
;;;
;;; Properties
;;;
;;;       gboolean  expanded          Read / Write / Construct
;;;        gchar *  label             Read / Write / Construct
;;;       gboolean  label-fill        Read / Write / Construct
;;;     GtkWidget*  label-widget      Read / Write
;;;       gboolean  resize-toplevel   Read / Write
;;;           gint  spacing           Read / Write
;;;       gboolean  use-markup        Read / Write / Construct
;;;       gboolean  use-underline     Read / Write / Construct
;;;
;;; Style Properties
;;;
;;;           gint  expander-size     Read
;;;           gint  expander-spacing  Read
;;;
;;; Signals
;;;
;;;           void  activate  Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;        ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkExpander
;;;
;;; Implemented Interfaces
;;;
;;;     GtkExpander implements AtkImplementorIface and GtkBuildable.
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
 "@version{2019-3-16}
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
    child to it. The expander widget has an @slot[gtk-expander]{expanded}
    property which can be used to monitor its expansion state. You should watch
    this property with a signal connection as follows:
    @begin{pre}
  (let ((expander (gtk-expander-new-with-mnemonic \"_More Options\")))
    (g-signal-connect expander \"notify::expanded\"
                      (lambda (object param)
                        (if (gtk-expander-expanded object)
                            ;; Show or create widgets
                            ...
                            ;; Hide or destroy widgets
                            ... )))
    ... )
    @end{pre}
  @begin[GtkExpander as GtkBuildable]{dictionary}
    The @sym{gtk-expander} implementation of the @class{gtk-buildable}
    interface supports placing a child in the label position by specifying
    @code{\"label\"} as the @code{\"type\"} attribute of a @code{<child>}
    element. A normal content child can be specified without specifying a
    @code{<child>} type attribute.
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
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
  expander
  ├── title
  │   ├── arrow
  │   ╰── <label widget>
  ╰── <child>
    @end{pre}
    @sym{gtk-expander} has three CSS nodes, the main node with the name
    expander, a subnode with name title and node below it with name arrow. The
    arrow of an expander that is showing its child gets the :checked pseudoclass
    added to it.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[expander-size]{entry}
        The @code{expander-size} style property of type @code{:int} (Read) @br{}
        Size of the expander arrow. @br{}
        @em{Warning:} @code{expander-size} has been deprecated since version
        3.20 and should not be used in newly-written code. Use CSS min-width and
        min-height instead. @br{}
        Allowed values: >= 0 @br{}
        Default value: 10
      @end{entry}
      @begin[expander-spacing]{entry}
        The @code{expander-spacing} style property of type @code{:int}
        (Read) @br{}
        Spacing around expander arrow. @br{}
        @em{Warning:} @code{expander-spacing} has been deprecated since version
        3.20 and should not be used in newly-written code. Use CSS margins
        instead, the value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
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
  @see-slot{gtk-expander-use-underline}
  @see-class{gtk-bin}
  @see-class{gtk-tree-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-expander-expanded --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "expanded" 'gtk-expander) 't)
 "The @code{expanded} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the expander has been opened to reveal the child widget. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-expanded atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-expanded 'function)
 "@version{2014-8-22}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[expanded]{whether the child widget is revealed}
  @syntax[]{(gtk-expander-expanded object) => expanded}
  @syntax[]{(setf (gtk-expander-expanded object) expanded)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{expanded} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-expanded} queries a
  @class{gtk-expander} and returns its current state. Returns @em{true} if the
  child widget is revealed.

  Set to @em{true}, if you want the child widget to be revealed, and @code{nil}
  if you want the child widget to be hidden.
  @see-class{gtk-expander}")

;;; --- gtk-expander-label -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label" 'gtk-expander) 't)
 "The @code{label} property of type @code{:string}
  (Read / Write / Construct) @br{}
  Text of the expander's label. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label 'function)
 "@version{2014-9-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[label]{a string}
  @syntax[]{(gtk-expander-label object) => label}
  @syntax[]{(setf (gtk-expander-label object) label)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{label} of the @class{gtk-expander}
    class.
  @end{short}

  The generic function @sym{gtk-expander-label} fetches the text from a label
  widget including any embedded underlines indicating mnemonics and Pango
  markup, as set by the generic function @sym{(setf gtk-expander-label)}.

  If the label text has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the function
  @fun{gtk-button-new} to use as a container.

  Note that this function behaved differently in versions prior to 2.14 and
  used to return the label text stripped of embedded underlines indicating
  mnemonics and Pango markup. This problem can be avoided by fetching the
  label text directly from the label widget.


  The generic function @sym{(setf gtk-expander-label)} sets the text of the
  label of the expander to @arg{label}.

  This will also clear any previously set labels.
  @see-class{gtk-expander}
  @see-function{gtk-button-new}")

;;; --- gtk-expander-label-fill ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-fill" 'gtk-expander) 't)
 "The @code{label-fill} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the label widget should fill all available horizontal space. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label-fill 'function)
 "@version{2014-9-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[label-fill]{@em{true} if the label should should fill all available
    horizontal space}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{label-fill} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-label-fill} returns whether the label
  widget will fill all available horizontal space allocated to expander.

  The generic function @sym{(setf gtk-expander-label-fill)} sets whether the
  label widget should fill all available horizontal space allocated to expander.
  @see-class{gtk-expander}")

;;; --- gtk-expander-label-widget ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget" 'gtk-expander) 't)
 "The @code{label-widget} property of type @code{gtk-widget}
  (Read / Write) @br{}
  A widget to display in place of the usual expander label.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-label-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-label-widget 'function)
 "@version{2014-9-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[label-widget]{the new label widget}
  @syntax[]{(gtk-expander-label-widget object) => label-widget}
  @syntax[]{(setf gtk-expander-label-widget object) label-widget)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{label-widget} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-label-widget} retrieves the label
  widget for the frame.

  The generic function @sym{(setf gtk-expander-label-widget)} sets the label
  widget for the expander. This is the widget that will appear embedded
  alongside the expander arrow.
  @see-class{gtk-expander}")

;;; --- gtk-expander-resize-toplevel -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "resize-toplevel"
                                               'gtk-expander) 't)
 "The @code{resize-toplevel} property of type @code{:boolean}
  (Read / Write) @br{}
  When this property is @em{true}, the expander will resize the toplevel widget
  containing the expander upon expanding and collapsing. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-resize-toplevel atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-resize-toplevel 'function)
 "@version{2014-9-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[resize-toplevel]{whether to resize the toplevel}
  @syntax[]{(gtk-expander-resize-toplevel object) => resize-toplevel}
  @syntax[]{(setf (gtk-expander-resize-toplevel object) resize-toplevel)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{resize-toplevel} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-resize-toplevel} returns whether the
  expander will resize the toplevel widget containing the expander upon
  resizing and collpasing.

  The generic function @sym{(setf gtk-expander-resize-toplevel)} sets whether
  the expander will resize the toplevel widget containing the expander upon
  resizing and collpasing.
  @see-class{gtk-expander}")

;;; --- gtk-expander-spacing ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-expander) 't)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  Space to put between the label and the child. @br{}
  @B{Warning:} @code{spacing} has been deprecated since version 3.20 and should
  not be used in newly-written code. This property is deprecated and ignored.
  Use margins on the child instead. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-spacing 'function)
 "@version{2014-9-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[spacing]{distance between the expander and child in pixels}
  @syntax[]{(gtk-expander-spacing object) => spacing}
  @syntax[]{(setf (gtk-expand-spacing object) spacing)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{spacing} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-spacing} returns the spacing between
  the expander and child.

  The generic function @sym{(setf gtk-expander-spacing)} sets the spacing field
  of the expander, which is the number of pixels to place between the expander
  and the child.
  @begin[Warning]{dictionary}
    The function @sym{gtk-expander-spacing} has been deprecated since version
    3.20 and should not be used in newly-written code. Use margins on the child
    instead.
  @end{dictionary}
  @see-class{gtk-expander}")

;;; --- gtk-expander-use-markup ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-markup" 'gtk-expander) 't)
 "The @code{use-markup} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The text of the label includes XML markup. See the function
  @fun{pango-parse-markup}. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-use-markup atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-use-markup 'function)
 "@version{2014-9-7}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[use-markup]{@em{true} if the label's text should be parsed for
    markup}
  @syntax[]{(gtk-expander-use-markup object) => use-markup}
  @syntax[]{(setf (gtk-expander-use-markup object) use-markup)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{use-markup} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-use-markup} returns whether the label's
  text is interpreted as marked up with the Pango text markup language.

  The generic function @sym{(setf gtk-expander-use-markup)} sets whether the
  text of the label contains markup in Pango's text markup language. See the
  function @fun{gtk-label-set-markup}.
  @see-class{gtk-expander}
  @see-function{gtk-label-set-markup}")

;;; --- gtk-expander-use-underline ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-underline"
                                               'gtk-expander) 't)
 "The @code{use-underline} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-expander-use-underline atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-expander-use-underline 'function)
 "@version{2013-11-6}
  @argument[object]{a @class{gtk-expander} widget}
  @argument[use-underline]{@em{true} if underlines in the text indicate
    mnemonics}
  @syntax[]{(gtk-expander-use-underline object) => use-underline}
  @syntax[]{(setf (gtk-expander-use-underline object) use-underline)}
  @begin{short}
    Accessor of the slot @slot[gtk-expander]{use-underline} of the
    @class{gtk-expander} class.
  @end{short}

  The generic function @sym{gtk-expander-use-underline} returns whether an
  embedded underline in the expander label indicates a mnemonic.

  If @em{true}, an underline in the text of the expander label indicates the
  next character should be used for the mnemonic accelerator key.
  @see-class{gtk-expander}")

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
  @see-class{gtk-expander}
  @see-function{gtk-expander-new}"
  (make-instance 'gtk-expander
                 :label label
                 :use-underline t))

(export 'gtk-expander-new-with-mnemonic)

;;; --- End of file gtk.expander.lisp ------------------------------------------

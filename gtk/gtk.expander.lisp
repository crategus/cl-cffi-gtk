;;; ----------------------------------------------------------------------------
;;; gtk.expander.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; Properties
;;; 
;;;   "expanded"                 gboolean             : Read / Write / Construct
;;;   "label"                    gchar*               : Read / Write / Construct
;;;   "label-fill"               gboolean             : Read / Write / Construct
;;;   "label-widget"             GtkWidget*           : Read / Write
;;;   "resize-toplevel"          gboolean             : Read / Write
;;;   "spacing"                  gint                 : Read / Write
;;;   "use-markup"               gboolean             : Read / Write / Construct
;;;   "use-underline"            gboolean             : Read / Write / Construct
;;; 
;;; Style Properties
;;; 
;;;   "expander-size"            gint                 : Read
;;;   "expander-spacing"         gint                 : Read
;;; 
;;; Signals
;;; 
;;;   "activate"                                      : Action
;;; 
;;; Description
;;; 
;;; A GtkExpander allows the user to hide or show its child by clicking on an
;;; expander triangle similar to the triangles used in a GtkTreeView.
;;; 
;;; Normally you use an expander as you would use any other descendant of
;;; GtkBin; you create the child widget and use gtk_container_add() to add it
;;; to the expander. When the expander is toggled, it will take care of showing
;;; and hiding the child automatically.
;;; 
;;; Special Usage
;;; 
;;; There are situations in which you may prefer to show and hide the expanded
;;; widget yourself, such as when you want to actually create the widget at
;;; expansion time. In this case, create a GtkExpander but do not add a child
;;; to it. The expander widget has an "expanded" property which can be used to
;;; monitor its expansion state. You should watch this property with a signal
;;; connection as follows:
;;; 
;;; expander = gtk_expander_new_with_mnemonic ("_More Options");
;;; g_signal_connect (expander, "notify::expanded",
;;;                   G_CALLBACK (expander_callback), NULL);
;;; 
;;; ...
;;; 
;;; static void
;;; expander_callback (GObject    *object,
;;;                    GParamSpec *param_spec,
;;;                    gpointer    user_data)
;;; {
;;;   GtkExpander *expander;
;;; 
;;;   expander = GTK_EXPANDER (object);
;;; 
;;;   if (gtk_expander_get_expanded (expander))
;;;     {
;;;       /* Show or create widgets */
;;;     }
;;;   else
;;;     {
;;;       /* Hide or destroy widgets */
;;;     }
;;; }
;;; 
;;; GtkExpander as GtkBuildable
;;; 
;;; The GtkExpander implementation of the GtkBuildable interface supports
;;; placing a child in the label position by specifying "label" as the "type"
;;; attribute of a <child> element. A normal content child can be specified
;;; without specifying a <child> type attribute.
;;; 
;;; Example 93. A UI definition fragment with GtkExpander
;;; 
;;; <object class="GtkExpander">
;;;   <child type="label">
;;;     <object class="GtkLabel" id="expander-label"/>
;;;   </child>
;;;   <child>
;;;     <object class="GtkEntry" id="expander-content"/>
;;;   </child>
;;; </object>
;;; 
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "expanded" property
;;; 
;;;   "expanded"                 gboolean             : Read / Write / Construct
;;; 
;;; Whether the expander has been opened to reveal the child widget.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "label" property
;;; 
;;;   "label"                    gchar*               : Read / Write / Construct
;;; 
;;; Text of the expander's label.
;;; 
;;; Default value: NULL
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-fill" property
;;; 
;;;   "label-fill"               gboolean             : Read / Write / Construct
;;; 
;;; Whether the label widget should fill all available horizontal space.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "label-widget" property
;;; 
;;;   "label-widget"             GtkWidget*           : Read / Write
;;; 
;;; A widget to display in place of the usual expander label.
;;;
;;; ----------------------------------------------------------------------------
;;; The "resize-toplevel" property
;;; 
;;;   "resize-toplevel"          gboolean             : Read / Write
;;; 
;;; When this property is TRUE, the expander will resize the toplevel widget
;;; containing the expander upon expanding and collapsing.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "spacing" property
;;; 
;;;   "spacing"                  gint                  : Read / Write
;;; 
;;; Space to put between the label and the child.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-markup" property
;;; 
;;;   "use-markup"               gboolean             : Read / Write / Construct
;;; 
;;; The text of the label includes XML markup. See pango_parse_markup().
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "use-underline" property
;;; 
;;;   "use-underline"            gboolean             : Read / Write / Construct
;;; 
;;; If set, an underline in the text indicates the next character should be
;;; used for the mnemonic accelerator key.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "expander-size" style property
;;; 
;;;   "expander-size"            gint                  : Read
;;; 
;;; Size of the expander arrow.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 10
;;;
;;; ----------------------------------------------------------------------------
;;; The "expander-spacing" style property
;;; 
;;;   "expander-spacing"         gint                  : Read
;;; 
;;; Spacing around expander arrow.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "activate" signal
;;; 
;;; void user_function (GtkExpander *expander,
;;;                     gpointer     user_data)      : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkExpander
;;; 
;;; struct GtkExpander;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkExpander" gtk-expander
  (:superclass gtk-bin
    :export t
    :interfaces ("AtkImplementorIface" "GtkBuildable")
    :type-initializer "gtk_expander_get_type")
  ((expanded gtk-expander-expanded
    "expanded" "gboolean" t t)
   (label gtk-expander-label
    "label" "gchararray" t t)
   (label-widget gtk-expander-label-widget
    "label-widget" "GtkWidget" t t)
   (spacing gtk-expander-spacing
    "spacing" "gint" t t)
   (use-markup gtk-expander-use-markup
    "use-markup" "gboolean" t t)
   (use-underline gtk-expander-use-underline
    "use-underline" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new ()
;;; 
;;; GtkWidget * gtk_expander_new (const gchar *label);
;;; 
;;; Creates a new expander using label as the text of the label.
;;; 
;;; label :
;;;     the text of the label
;;; 
;;; Returns :
;;;     a new GtkExpander widget.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_expander_new_with_mnemonic (const gchar *label);
;;; 
;;; Creates a new expander using label as the text of the label. If characters
;;; in label are preceded by an underscore, they are underlined. If you need a
;;; literal underscore character in a label, use '__' (two underscores). The
;;; first underlined character represents a keyboard accelerator called a
;;; mnemonic. Pressing Alt and that key activates the button.
;;; 
;;; label :
;;;     the text of the label with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     a new GtkExpander widget.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_expanded ()
;;; 
;;; void gtk_expander_set_expanded (GtkExpander *expander, gboolean expanded);
;;; 
;;; Sets the state of the expander. Set to TRUE, if you want the child widget
;;; to be revealed, and FALSE if you want the child widget to be hidden.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; expanded :
;;;     whether the child widget is revealed
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_expanded ()
;;; 
;;; gboolean gtk_expander_get_expanded (GtkExpander *expander);
;;; 
;;; Queries a GtkExpander and returns its current state. Returns TRUE if the
;;; child widget is revealed.
;;; 
;;; See gtk_expander_set_expanded().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     the current state of the expander
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_spacing ()
;;; 
;;; void gtk_expander_set_spacing (GtkExpander *expander, gint spacing);
;;; 
;;; Sets the spacing field of expander, which is the number of pixels to place
;;; between expander and the child.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; spacing :
;;;     distance between the expander and child in pixels
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_spacing ()
;;; 
;;; gint gtk_expander_get_spacing (GtkExpander *expander);
;;; 
;;; Gets the value set by gtk_expander_set_spacing().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     spacing between the expander and child
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label ()
;;; 
;;; void gtk_expander_set_label (GtkExpander *expander, const gchar *label);
;;; 
;;; Sets the text of the label of the expander to label.
;;; 
;;; This will also clear any previously set labels.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; label :
;;;     a string. [allow-none]
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label ()
;;; 
;;; const gchar * gtk_expander_get_label (GtkExpander *expander);
;;; 
;;; Fetches the text from a label widget including any embedded underlines
;;; indicating mnemonics and Pango markup, as set by gtk_expander_set_label().
;;; If the label text has not been set the return value will be NULL. This will
;;; be the case if you create an empty button with gtk_button_new() to use as a
;;; container.
;;; 
;;; Note that this function behaved differently in versions prior to 2.14 and
;;; used to return the label text stripped of embedded underlines indicating
;;; mnemonics and Pango markup. This problem can be avoided by fetching the
;;; label text directly from the label widget.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     The text of the label widget. This string is owned by the widget and
;;;     must not be modified or freed.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_use_underline ()
;;; 
;;; void gtk_expander_set_use_underline (GtkExpander *expander,
;;;                                      gboolean use_underline);
;;; 
;;; If true, an underline in the text of the expander label indicates the next
;;; character should be used for the mnemonic accelerator key.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; use_underline :
;;;     TRUE if underlines in the text indicate mnemonics
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_use_underline ()
;;; 
;;; gboolean gtk_expander_get_use_underline (GtkExpander *expander);
;;; 
;;; Returns whether an embedded underline in the expander label indicates a
;;; mnemonic. See gtk_expander_set_use_underline().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     TRUE if an embedded underline in the expander label indicates the
;;;     mnemonic accelerator keys
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_use_markup ()
;;; 
;;; void gtk_expander_set_use_markup (GtkExpander *expander,
;;;                                   gboolean use_markup);
;;; 
;;; Sets whether the text of the label contains markup in Pango's text markup
;;; language. See gtk_label_set_markup().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; use_markup :
;;;     TRUE if the label's text should be parsed for markup
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_use_markup ()
;;; 
;;; gboolean gtk_expander_get_use_markup (GtkExpander *expander);
;;; 
;;; Returns whether the label's text is interpreted as marked up with the Pango
;;; text markup language. See gtk_expander_set_use_markup().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     TRUE if the label's text will be parsed for markup
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label_widget ()
;;; 
;;; void gtk_expander_set_label_widget (GtkExpander *expander,
;;;                                     GtkWidget *label_widget);
;;; 
;;; Set the label widget for the expander. This is the widget that will appear
;;; embedded alongside the expander arrow.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; label_widget :
;;;     the new label widget
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label_widget ()
;;; 
;;; GtkWidget * gtk_expander_get_label_widget (GtkExpander *expander);
;;; 
;;; Retrieves the label widget for the frame.
;;; See gtk_expander_set_label_widget().
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     the label widget, or NULL if there is none. [transfer none]
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_label_fill ()
;;; 
;;; void gtk_expander_set_label_fill (GtkExpander *expander,
;;;                                   gboolean label_fill);
;;; 
;;; Sets whether the label widget should fill all available horizontal space
;;; allocated to expander.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; label_fill :
;;;     TRUE if the label should should fill all available horizontal space
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_label_fill ()
;;; 
;;; gboolean gtk_expander_get_label_fill (GtkExpander *expander);
;;; 
;;; Returns whether the label widget will fill all available horizontal space
;;; allocated to expander.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     TRUE if the label widget will fill all available horizontal space
;;; 
;;; Since 2.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_set_resize_toplevel ()
;;; 
;;; void gtk_expander_set_resize_toplevel (GtkExpander *expander,
;;;                                        gboolean resize_toplevel);
;;; 
;;; Sets whether the expander will resize the toplevel widget containing the
;;; expander upon resizing and collpasing.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; resize_toplevel :
;;;     whether to resize the toplevel
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_expander_get_resize_toplevel ()
;;; 
;;; gboolean gtk_expander_get_resize_toplevel (GtkExpander *expander);
;;; 
;;; Returns whether the expander will resize the toplevel widget containing the
;;; expander upon resizing and collpasing.
;;; 
;;; expander :
;;;     a GtkExpander
;;; 
;;; Returns :
;;;     the "resize toplevel" setting.
;;; 
;;; Since 3.2
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.expander.lisp ------------------------------------------

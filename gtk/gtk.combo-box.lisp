;;; ----------------------------------------------------------------------------
;;; gtk.combo-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
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
;;; GtkComboBox
;;;
;;; A widget used to choose from a list of items
;;;
;;; Synopsis
;;;
;;;     GtkComboBox
;;;
;;;     gtk_combo_box_new
;;;     gtk_combo_box_new_with_entry
;;;     gtk_combo_box_new_with_model
;;;     gtk_combo_box_new_with_model_and_entry
;;;     gtk_combo_box_new_with_area
;;;     gtk_combo_box_new_with_area_and_entry
;;;     gtk_combo_box_get_wrap_width
;;;     gtk_combo_box_set_wrap_width
;;;     gtk_combo_box_get_row_span_column
;;;     gtk_combo_box_set_row_span_column
;;;     gtk_combo_box_get_column_span_column
;;;     gtk_combo_box_set_column_span_column
;;;     gtk_combo_box_get_active
;;;     gtk_combo_box_set_active
;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_get_id_column
;;;     gtk_combo_box_set_id_column
;;;     gtk_combo_box_get_active_id
;;;     gtk_combo_box_set_active_id
;;;     gtk_combo_box_get_model
;;;     gtk_combo_box_set_model
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_popup_accessible
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func
;;;     gtk_combo_box_set_add_tearoffs
;;;     gtk_combo_box_get_add_tearoffs
;;;     gtk_combo_box_set_title
;;;     gtk_combo_box_get_title
;;;     gtk_combo_box_set_focus_on_click
;;;     gtk_combo_box_get_focus_on_click
;;;     gtk_combo_box_set_button_sensitivity
;;;     gtk_combo_box_get_button_sensitivity
;;;     gtk_combo_box_get_has_entry
;;;     gtk_combo_box_set_entry_text_column
;;;     gtk_combo_box_get_entry_text_column
;;;     gtk_combo_box_set_popup_fixed_width
;;;     gtk_combo_box_get_popup_fixed_width
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
;;;                                  +----GtkComboBoxText
;;;
;;; Implemented Interfaces
;;;
;;; GtkComboBox implements AtkImplementorIface, GtkBuildable, GtkCellLayout and
;;; GtkCellEditable.
;;;
;;;
;;; Style Properties
;;;
;;;   "appears-as-list"          gboolean             : Read
;;;   "arrow-scaling"            gfloat               : Read
;;;   "arrow-size"               gint                 : Read
;;;   "shadow-type"              GtkShadowType        : Read
;;;
;;; Signals
;;;
;;;   "changed"                                       : Run Last
;;;   "format-entry-text"                             : Run Last
;;;   "move-active"                                   : Action
;;;   "popdown"                                       : Action
;;;   "popup"                                         : Action
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBox" gtk-combo-box
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_get_type")
  ((active
    gtk-combo-box-active
    "active" "gint" t t)
   (active-id
    gtk-combo-box-active-id
    "active-id" "gchar" t t)
   (add-tearoffs
    gtk-combo-box-add-tearoffs
    "add-tearoffs" "gboolean" t t)
   (button-sensitivity
    gtk-combo-box-button-sensitivity
    "button-sensitivity" "GtkSensitivityType" t t)
   (cell-area
    gtk-combo-box-cell-area
    "cell-area" "GtkCellArea" t t)
   (column-span-column
    gtk-combo-box-column-span-column
    "column-span-column" "gint" t t)
   (entry-text-column
    gtk-combo-box-entry-text-column
    "entry-text-column" "gint" t t)
   (focus-on-click
    gtk-combo-box-focus-on-click
    "focus-on-click" "gboolean" t t)
   (has-entry
    gtk-combo-box-has-entry
    "has-entry" "gboolean" t nil)
   (has-frame
    gtk-combo-box-has-frame
    "has-frame" "gboolean" t t)
   (id-column
    gtk-combo-box-id-column
    "id-column" "gint" t t)
   (model
    gtk-combo-box-model
    "model" "GtkTreeModel" t t)
   (popup-fixed-width
    gtk-popup-fixed-width
    "popup-fixed-width" "gboolean" t t)
   (popup-shown
    gtk-combo-box-popup-shown
    "popup-shown" "gboolean" t nil)
   (row-span-column
    gtk-combo-box-row-span-column
    "row-span-column" "gint" t t)
   (tearoff-title
    gtk-combo-box-tearoff-title
    "tearoff-title" "gchararray" t t)
   (wrap-width
    gtk-combo-box-wrap-width
    "wrap-width" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-combo-box 'type)
 "@version{2013-5-31}
  @begin{short}
    A @sym{gtk-combo-box} is a widget that allows the user to choose from a list
    of valid choices. The @sym{gtk-combo-box} displays the selected choice. When
    activated, the @sym{gtk-combo-box} displays a popup which allows the user to
    make a new choice. The style in which the selected value is displayed, and
    the style of the popup is determined by the current theme. It may be similar
    to a Windows-style combo box.
  @end{short}

  The @sym{gtk-combo-box} uses the model-view pattern; the list of valid choices
  is specified in the form of a tree model, and the display of the choices can
  be adapted to the data in the model by using cell renderers, as you would in a
  tree view. This is possible since @sym{gtk-combo-box} implements the
  @class{gtk-cell-layout} interface. The tree model holding the valid choices is
  not restricted to a flat list, it can be a real tree, and the popup will
  reflect the tree structure.

  To allow the user to enter values not in the model, the @code{\"has-entry\"}
  property allows the @sym{gtk-combo-box} to contain a @class{gtk-entry}. This
  entry can be accessed by calling the @fun{gtk-bin-get-child} function on the
  combo box.

  For a simple list of textual choices, the model-view API of
  @sym{gtk-combo-box} can be a bit overwhelming. In this case,
  @class{gtk-combo-box-text} offers a simple alternative. Both
  @sym{gtk-combo-box} and @class{gtk-combo-box-text} can contain an entry.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"appears-as-list\" style property}
      @code{\"appears-as-list\"} of type @code{:boolean} (Read) @br{}
      Whether dropdowns should look like lists rather than menus. @br{}
      Default value: @code{nil}

    @subheading{The \"arrow-scaling\" style property}
      @code{\"arrow-scaling\"} @code{:float} (Read) @br{}
      Sets the amount of space used up by the combo box arrow, proportional to
      the font size. @br{}
      Allowed values: [0,2] @br{}
      Default value: 1 @br{}
      Since 3.2

    @subheading{The \"arrow-size\" style property}
      @code{\"arrow-size\"} of type @code{:int} (Read) @br{}
      Sets the minimum size of the arrow in the combo box. Note that the arrow
      size is coupled to the font size, so in case a larger font is used, the
      arrow will be larger than set by arrow size. @br{}
      Allowed values: >= 0 @br{}
      Default value: 15 @br{}
      Since 2.12

    @subheading{The \"shadow-type\" style property}
      @code{\"shadow-type\"} of type @symbol{gtk-shadow-type} (Read) @br{}
      Which kind of shadow to draw around the combo box. @br{}
      Default value: @code{:none} @br{}
      Since 2.12
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (widget)   : Run Last
      @end{pre}
      The changed signal is emitted when the active item is changed. The can be
      due to the user selecting a different item from the list, or due to a call
      to the @fun{gtk-combo-box-set-active-iter} function. It will also be
      emitted while typing into the entry of a combo box with an entry.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
      Since 2.4

    @subheading{The \"format-entry-text\" signal}
      @begin{pre}
 lambda (combo path)   : Run Last
      @end{pre}
      For combo boxes that are created with an entry (See the
      @code{\"has-entry\"}).
      A signal which allows you to change how the text displayed in a combo
      box's entry is displayed.
      Connect a signal handler which returns an allocated string representing
      path. That string will then be used to set the text in the combo box's
      entry. The default signal handler uses the text from the
      @code{\"entry-text-column\"} property model column.
      Here is an example signal handler which fetches data from the model and
      displays it in the entry.
      @begin{pre}
 static gchar*
 format_entry_text_callback (GtkComboBox *combo,
                             const gchar *path,
                             gpointer     user_data)
 {
   GtkTreeIter iter;
   GtkTreeModel model;
   gdouble      value;

   model = gtk_combo_box_get_model (combo);

   gtk_tree_model_get_iter_from_string (model, &iter, path);
   gtk_tree_model_get (model, &iter,
                       THE_DOUBLE_VALUE_COLUMN, &value,
                       -1);

   return g_strdup_printf (\"%g\", value);
 @}
      @end{pre}
      @begin[code]{table}
        @entry[combo]{The object which received the signal.}
        @entry[path]{The @class{gtk-tree-path} object string from the combo
          box's current model to format text for.}
        @entry[Returns]{A newly allocated string representing path for the
          current @sym{gtk-combo-box} model.}
      @end{table}
      Since 3.4

    @subheading{The \"move-active\" signal}
      @begin{pre}
 lambda (widget scroll-type)   : Action
      @end{pre}
      The \"move-active\" signal is a keybinding signal which gets emitted to
      move the active selection.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
        @entry[scroll-type]{A @symbol{gtk-scroll-type}.}
      @end{table}
      Since 2.12

    @subheading{The \"popdown\" signal}
      @begin{pre}
 lambda (button)   ; Action
      @end{pre}
      The \"popdown\" signal is a keybinding signal which gets emitted to
      popdown the combo box list.
      The default bindings for this signal are Alt+Up and Escape.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
      @end{table}
      Since 2.12

    @subheading{The \"popup\" signal}
      @begin{pre}
 lambda (widget)   : Action
      @end{pre}
      The \"popup\" signal is a keybinding signal which gets emitted to popup
      the combo box list.
      The default binding for this signal is Alt+Down.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
      @end{table}
      Since 2.12
  @end{dictionary}
  @see-slot{gtk-combo-box-active}
  @see-slot{gtk-combo-box-active-id}
  @see-slot{gtk-combo-box-add-tearoffs}
  @see-slot{gtk-combo-box-button-sensitivity}
  @see-slot{gtk-combo-box-cell-area}
  @see-slot{gtk-combo-box-column-span-column}
  @see-slot{gtk-combo-box-entry-text-column}
  @see-slot{gtk-combo-box-focus-on-click}
  @see-slot{gtk-combo-box-has-entry}
  @see-slot{gtk-combo-box-has-frame}
  @see-slot{gtk-combo-box-id-column}
  @see-slot{gtk-combo-box-model}
  @see-slot{gtk-combo-box-popup-fixed-width}
  @see-slot{gtk-combo-box-popup-shown}
  @see-slot{gtk-combo-box-row-span-column}
  @see-slot{gtk-combo-box-tearoff-title}
  @see-slot{gtk-combo-box-wrap-width}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-combo-box) 't)
 "The @code{\"active\"} property of type @code{:int} (Read / Write) @br{}
  The item which is currently active. If the model is a non-flat treemodel,
  and the active item is not an immediate child of the root of the tree, this
  property has the value @code{gtk_tree_path_get_indices (path)[0]}, where
  @arg{path} is the @class{gtk-tree-path} of the active item. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active-id" 'gtk-combo-box) 't)
 "The @code{\"active-id\"} property of type @code{:string} (Read / Write) @br{}
  The value of the ID column of the active row. @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "add-tearoffs" 'gtk-combo-box) 't)
 "The @code{\"add-tearoffs\"} property of type @code{:boolean}
  (Read / Write) @br{}
  The @code{\"add-tearoffs\"} property controls whether generated menus have
  tearoff menu items.
  Note that this only affects menu style combo boxes. @br{}
  Default value: @code{nil} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "button-sensitivity"
                                               'gtk-combo-box) 't)
 "The @code{\"button-sensitivity\"} property of type
  @symbol{gtk-sensitivity-type} (Read / Write) @br{}
  Whether the dropdown button is sensitive when the model is empty. @br{}
  Default value: @code{:auto} @br{}
  Since 2.14")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area" 'gtk-combo-box) 't)
 "The @code{\"cell-area\"} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The @class{gtk-cell-area} used to layout cell renderers for this combo box.
  If no area is specified when creating the combo box with the
  @fun{gtk-combo-box-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} will be used. @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-span-column"
                                               'gtk-combo-box) 't)
 "The @code{\"column-span-column\"} property of type @code{:int}
  (Read / Write) @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type @variable{+g-type-int+} in the model.
  The values of that column are used to determine how many columns a value in
  the list will span. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "entry-text-column"
                                               'gtk-combo-box) 't)
 "The @code{\"entry-text-column\"} property of type @code{:int}
  (Read / Write) @br{}
  The column in the combo box's model to associate with strings from the entry
  if the combo was created with \"has-entry\" = @em{true}. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.24")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click"
                                               'gtk-combo-box) 't)
 "The @code{\"focus-on-click\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the combo box grabs focus when it is clicked with the mouse. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-entry" 'gtk-combo-box) 't)
 "The @code{\"has-entry\"} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the combo box has an entry. @br{}
  Default value: @code{nil} @br{}
  Since 2.24")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-frame" 'gtk-combo-box) 't)
 "The @code{\"has-frame\"} property of type @code{:boolean} (Read / Write) @br{}
  The has-frame property controls whether a frame is drawn around the
  entry. @br{}
  Default value: @em{true} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "id-column" 'gtk-combo-box) 't)
 "The @code{\"id-column\"} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model that provides string IDs for the values
  in the model, if != -1. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-combo-box) 't)
 "The @code{\"model\"} property of type @class{gtk-tree-model}
  (Read / Write) @br{}
  The model from which the combo box takes the values shown in the list. @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-fixed-width"
                                               'gtk-combo-box) 't)
 "The @code{\"popup-fixed-width\"} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the popup's width should be a fixed width matching the allocated
  width of the combo box. @br{}
  Default value: @em{true} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-shown" 'gtk-combo-box) 't)
 "The @code{\"popup-shown\"} property of type @code{:boolean} (Read) @br{}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the \"notify::popup-shown\"
  signal. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-span-column"
                                               'gtk-combo-box) 't)
 "The @code{\"row-span-column\"} property of type @code{:int}
  (Read / Write) @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type @variable{+g-type-int+} in the model.
  The values of that column are used to determine how many rows a value in the
  list will span. Therefore, the values in the model column pointed to by this
  property must be greater than zero and not larger than wrap-width. @br{}
  Allowed values: >= @code{G_MAXULONG} @br{}
  Default value: -1 @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-title"
                                               'gtk-combo-box) 't)
 "The @code{\"tearoff-title\"} property of type @code{:string}
  (Read / Write) @br{}
  A title that may be displayed by the window manager when the popup is
  torn-off. @br{}
  Default value: @code{nil} @br{}
  Since 2.10")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-width" 'gtk-combo-box) 't)
 "The @code{\"wrap-width\"} property of type @code{:int} (Read / Write) @br{}
  If wrap-width is set to a positive value, the list will be displayed in
  multiple columns, the number of columns is determined by wrap-width. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-active 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"active\"} of the @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-active-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-active-id 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"active-id\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-add-tearoffs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-add-tearoffs 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"add-tearoffs\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-button-sensitivity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-button-sensitivity 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"button-sensitivity\"} of the
    @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-cell-area 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"cell-area\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-column-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-column-span-column 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"column-span-column\"} of the
    @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-entry-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-entry-text-column 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"entry-text-column\"} of the
    @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-focus-on-click atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-focus-on-click 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"focus-on-click\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-entry 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"has-entry\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-frame atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-frame 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"has-frame\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-id-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-id-column 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"id-column\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-model 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"model\"} of the @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-fixed-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-fixed-width 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"popup-fixed-width\"} of the
    @class{gtk-combo-box} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-shown atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-shown 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"popup-shown\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-row-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-row-span-column 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"row-span-column\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-tearoff-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-tearoff-title 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"tearoff-title\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-wrap-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-wrap-width 'function)
 "@version{2013-2-26}
  @begin{short}
    Accessor of the slot @code{\"wrap-width\"} of the @class{gtk-combo-box}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new))

(defun gtk-combo-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @return{A new @class{gtk-combo-box} widget.}
  @short{Creates a new empty @class{gtk-combo-box} widget.}

  Since 2.4"
  (make-instance 'gtk-combo-box))

(export 'gtk-combo-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-entry))

(defun gtk-combo-box-new-with-entry ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @return{A new @class{gtk-combo-box} widget.}
  Creates a new empty @class{gtk-combo-box} widget with an entry."
  (make-instance 'gtk-combo-box
                 :has-entry t))

(export 'gtk-combox-box-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model ()
;;;
;;; GtkWidget * gtk_combo_box_new_with_model (GtkTreeModel *model);
;;;
;;; Creates a new GtkComboBox with the model initialized to model.
;;;
;;; model :
;;;     A GtkTreeModel.
;;;
;;; Returns :
;;;     A new GtkComboBox.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model_and_entry ()
;;;
;;; GtkWidget * gtk_combo_box_new_with_model_and_entry (GtkTreeModel *model);
;;;
;;; Creates a new empty GtkComboBox with an entry and with the model initialized
;;; to model.
;;;
;;; model :
;;;     A GtkTreeModel
;;;
;;; Returns :
;;;     A new GtkComboBox
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area ()
;;;
;;; GtkWidget * gtk_combo_box_new_with_area (GtkCellArea *area);
;;;
;;; Creates a new empty GtkComboBox using area to layout cells.
;;;
;;; area :
;;;     the GtkCellArea to use to layout cell renderers
;;;
;;; Returns :
;;;     A new GtkComboBox.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area_and_entry ()
;;;
;;; GtkWidget * gtk_combo_box_new_with_area_and_entry (GtkCellArea *area);
;;;
;;; Creates a new empty GtkComboBox with an entry.
;;;
;;; The new combo box will use area to layout cells.
;;;
;;; area :
;;;     the GtkCellArea to use to layout cell renderers
;;;
;;; Returns :
;;;     A new GtkComboBox.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_wrap_width ()
;;;
;;; gint gtk_combo_box_get_wrap_width (GtkComboBox *combo_box);
;;;
;;; Returns the wrap width which is used to determine the number of columns for
;;; the popup menu. If the wrap width is larger than 1, the combo box is in
;;; table mode.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; Returns :
;;;     the wrap width.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_wrap_width ()
;;;
;;; void gtk_combo_box_set_wrap_width (GtkComboBox *combo_box, gint width);
;;;
;;; Sets the wrap width of combo_box to be width. The wrap width is basically
;;; the preferred number of columns when you want the popup to be layed out in a
;;; table.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; width :
;;;     Preferred number of columns
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_span_column ()
;;;
;;; gint gtk_combo_box_get_row_span_column (GtkComboBox *combo_box);
;;;
;;; Returns the column with row span information for combo_box.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; Returns :
;;;     the row span column.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_row_span_column ()
;;;
;;; void gtk_combo_box_set_row_span_column (GtkComboBox *combo_box,
;;;                                         gint row_span);
;;;
;;; Sets the column with row span information for combo_box to be row_span. The
;;; row span column contains integers which indicate how many rows an item
;;; should span.
;;;
;;; combo_box :
;;;     A GtkComboBox.
;;;
;;; row_span :
;;;     A column in the model passed during construction.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_column_span_column ()
;;;
;;; gint gtk_combo_box_get_column_span_column (GtkComboBox *combo_box);
;;;
;;; Returns the column with column span information for combo_box.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; Returns :
;;;     the column span column.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_column_span_column ()
;;;
;;; void gtk_combo_box_set_column_span_column (GtkComboBox *combo_box,
;;;                                            gint column_span);
;;;
;;; Sets the column with column span information for combo_box to be
;;; column_span. The column span column contains integers which indicate how
;;; many columns an item should span.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; column_span :
;;;     A column in the model passed during construction
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-active))

(defun gtk-combo-box-get-active (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @begin{return}
    An integer which is the index of the currently active item, or -1 if
    there is no active item.
  @end{return}
  @begin{short}
    Returns the index of the currently active item, or -1 if there is no active
    item.
  @end{short}
  If the model is a non-flat treemodel, and the active item is not an immediate
  child of the root of the tree, this function returns
  @code{gtk_tree_path_get_indices (path)[0]}, where path is the
  @class{gtk-tree-path} of the active item.

  Since 2.4"
  (gtk-combo-box-active combo-box))

(export 'gtk-combo-box-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-active))

(defun gtk-combo-box-set-active (combo-box index)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[index]{an index in the model passed during construction, or -1 to
    have no active item}
  @begin{short}
    Sets the active item of @arg{combo-box} to be the item at @arg{index}.
  @end{short}

  Since 2.4"
  (setf (gtk-combo-box-active combo-box) index))

(export 'gtk-combo-box-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_active_iter" %gtk-combo-box-get-active-iter)
    :boolean
  (combo-box g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-combo-box-get-active-iter (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{the iter, or @code{nil}}
  @begin{short}
    Returns iter to point to the current active item, if it exists.
  @end{short}

  Since 2.4"
  (let ((iter (make-instance 'gtk-tree-iter)))
    (when (%gtk-combo-box-get-active-iter combo-box iter)
      iter)))

(export 'gtk-combo-box-get-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active_iter ()
;;;
;;; void gtk_combo_box_set_active_iter (GtkComboBox *combo_box,
;;;                                     GtkTreeIter *iter);
;;;
;;; Sets the current active item to be the one referenced by iter, or unsets the
;;; active item if iter is NULL.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; iter :
;;;     The GtkTreeIter, or NULL.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_id_column ()
;;;
;;; gint gtk_combo_box_get_id_column (GtkComboBox *combo_box);
;;;
;;; Returns the column which combo_box is using to get string IDs for values
;;; from.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; Returns :
;;;     A column in the data source model of combo_box.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_id_column ()
;;;
;;; void gtk_combo_box_set_id_column (GtkComboBox *combo_box, gint id_column);
;;;
;;; Sets the model column which combo_box should use to get string IDs for
;;; values from. The column id_column in the model of combo_box must be of type
;;; G_TYPE_STRING.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; id_column :
;;;     A column in model to get string IDs for values from
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_id ()
;;;
;;; const gchar * gtk_combo_box_get_active_id (GtkComboBox *combo_box);
;;;
;;; Returns the ID of the active row of combo_box. This value is taken from the
;;; active row and the column specified by the "id-column" property of combo_box
;;; (see gtk_combo_box_set_id_column()).
;;;
;;; The returned value is an interned string which means that you can compare
;;; the pointer by value to other interned strings and that you must not free
;;; it.
;;;
;;; If the "id-column" property of combo_box is not set, or if no row is active,
;;; or if the active row has a NULL ID value, then NULL is returned.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     the ID of the active row, or NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active_id ()
;;;
;;; gboolean gtk_combo_box_set_active_id (GtkComboBox *combo_box,
;;;                                       const gchar *active_id);
;;;
;;; Changes the active row of combo_box to the one that has an ID equal to
;;; active_id, or unsets the active row if active_id is NULL. Rows having a NULL
;;; ID string cannot be made active by this function.
;;;
;;; If the "id-column" property of combo_box is unset or if no row has the given
;;; ID then the function does nothing and returns FALSE.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; active_id :
;;;     the ID of the row to select, or NULL.
;;;
;;; Returns :
;;;     TRUE if a row with a matching ID was found. If a NULL active_id was
;;;     given to unset the active row, the function always returns TRUE.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_model ()
;;;
;;; GtkTreeModel * gtk_combo_box_get_model (GtkComboBox *combo_box);
;;;
;;; Returns the GtkTreeModel which is acting as data source for combo_box.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; Returns :
;;;     A GtkTreeModel which was passed during construction.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_model ()
;;;
;;; void gtk_combo_box_set_model (GtkComboBox *combo_box, GtkTreeModel *model);
;;;
;;; Sets the model used by combo_box to be model. Will unset a previously set
;;; model (if applicable). If model is NULL, then it will unset the model.
;;;
;;; Note that this function does not clear the cell renderers, you have to call
;;; gtk_cell_layout_clear() yourself if you need to set up different cell
;;; renderers for the new model.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; model :
;;;     A GtkTreeModel.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup_for_device ()
;;;
;;; void gtk_combo_box_popup_for_device (GtkComboBox *combo_box,
;;;                                      GdkDevice *device);
;;;
;;; Pops up the menu or dropdown list of combo_box, the popup window will be
;;; grabbed so only device and its associated pointer/keyboard are the only
;;; GdkDevices able to send events to it.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; device :
;;;     a GdkDevice
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_popup" gtk-combo-box-popup) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @begin{short}
    Pops up the menu or dropdown list of @arg{combo-box}.
  @end{short}

  This function is mostly intended for use by accessibility technologies;
  applications should have little use for it.

  Since 2.4"
  (combo-box g-object))

(export 'gtk-combo-box-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popdown ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_popdown" gtk-combo-box-popdown) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @begin{short}
    Hides the menu or dropdown list of @arg{combo-box}.
  @end{short}

  This function is mostly intended for use by accessibility technologies;
  applications should have little use for it.

  Since 2.4"
  (combo-box g-object))

(export 'gtk-combo-box-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_popup_accessible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_popup_accessible"
          gtk-combo-box-get-popup-accessible) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-2-26}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{the accessible object corresponding to the combo box's popup}
  @begin{short}
    Gets the accessible object corresponding to the combo box's popup.
  @end{short}

  This function is mostly intended for use by accessibility technologies;
  applications should have little use for it.

  Since 2.6"
  (combo-box g-object))

(export 'gtk-combo-box-get-popup-accessible)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_separator_func ()
;;;
;;; GtkTreeViewRowSeparatorFunc gtk_combo_box_get_row_separator_func
;;;                                                    (GtkComboBox *combo_box);
;;;
;;; Returns the current row separator function.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     the current row separator function.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_row_separator_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_set_row_separator_func"
          %gtk-combo-box-set-row-separator-func) :void
  (combo-box g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-combo-box-set-row-separator-func (combo-box func)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-31}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[func]{a @code{GtkTreeViewRowSeparatorFunc}}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator.
  @end{short}
  If the row separator function is @code{nil}, no separators are drawn. This is
  the default value.

  Since 2.6"
  (%gtk-combo-box-set-row-separator-func
                            combo-box
                            (callback gtk-tree-view-row-separator-func-callback)
                            (glib::allocate-stable-pointer func)
                            (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-combo-box-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_add_tearoffs ()
;;;
;;; void gtk_combo_box_set_add_tearoffs (GtkComboBox *combo_box,
;;;                                      gboolean add_tearoffs);
;;;
;;; Sets whether the popup menu should have a tearoff menu item.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; add_tearoffs :
;;;     TRUE to add tearoff menu items
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_add_tearoffs ()
;;;
;;; gboolean gtk_combo_box_get_add_tearoffs (GtkComboBox *combo_box);
;;;
;;; Gets the current value of the :add-tearoffs property.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     the current value of the :add-tearoffs property.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_title ()
;;;
;;; void gtk_combo_box_set_title (GtkComboBox *combo_box, const gchar *title);
;;;
;;; Sets the menu's title in tearoff mode.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; title :
;;;     a title for the menu in tearoff mode
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_title ()
;;;
;;; const gchar * gtk_combo_box_get_title (GtkComboBox *combo_box);
;;;
;;; Gets the current title of the menu in tearoff mode. See
;;; gtk_combo_box_set_add_tearoffs().
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     the menu's title in tearoff mode. This is an internal copy of the string
;;;     which must not be freed.
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_focus_on_click ()
;;;
;;; void gtk_combo_box_set_focus_on_click (GtkComboBox *combo,
;;;                                        gboolean focus_on_click);
;;;
;;; Sets whether the combo box will grab focus when it is clicked with the
;;; mouse. Making mouse clicks not grab focus is useful in places like toolbars
;;; where you don't want the keyboard focus removed from the main area of the
;;; application.
;;;
;;; combo :
;;;     a GtkComboBox
;;;
;;; focus_on_click :
;;;     whether the combo box grabs focus when clicked with the mouse
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_focus_on_click ()
;;;
;;; gboolean gtk_combo_box_get_focus_on_click (GtkComboBox *combo);
;;;
;;; Returns whether the combo box grabs focus when it is clicked with the mouse.
;;; See gtk_combo_box_set_focus_on_click().
;;;
;;; combo :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     TRUE if the combo box grabs focus when it is clicked with the mouse.
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_button_sensitivity ()
;;;
;;; void gtk_combo_box_set_button_sensitivity (GtkComboBox *combo_box,
;;;                                            GtkSensitivityType sensitivity);
;;;
;;; Sets whether the dropdown button of the combo box should be always sensitive
;;; (GTK_SENSITIVITY_ON), never sensitive (GTK_SENSITIVITY_OFF) or only if there
;;; is at least one item to display (GTK_SENSITIVITY_AUTO).
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; sensitivity :
;;;     specify the sensitivity of the dropdown button
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_button_sensitivity ()
;;;
;;; GtkSensitivityType gtk_combo_box_get_button_sensitivity
;;;                                                    (GtkComboBox *combo_box);
;;;
;;; Returns whether the combo box sets the dropdown button sensitive or not when
;;; there are no items in the model.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     GTK_SENSITIVITY_ON if the dropdown button is sensitive when the model is
;;;     empty, GTK_SENSITIVITY_OFF if the button is always insensitive or
;;;     GTK_SENSITIVITY_AUTO if it is only sensitive as long as the model has
;;;     one item to be selected.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_has_entry ()
;;;
;;; gboolean gtk_combo_box_get_has_entry (GtkComboBox *combo_box);
;;;
;;; Returns whether the combo box has an entry.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     whether there is an entry in combo_box.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_entry_text_column ()
;;;
;;; void gtk_combo_box_set_entry_text_column (GtkComboBox *combo_box,
;;;                                           gint text_column);
;;;
;;; Sets the model column which combo_box should use to get strings from to be
;;; text_column. The column text_column in the model of combo_box must be of
;;; type G_TYPE_STRING.
;;;
;;; This is only relevant if combo_box has been created with "has-entry" as
;;; TRUE.
;;;
;;; combo_box :
;;;     A GtkComboBox
;;;
;;; text_column :
;;;     A column in model to get the strings from for the internal entry
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_entry_text_column ()
;;;
;;; gint gtk_combo_box_get_entry_text_column (GtkComboBox *combo_box);
;;;
;;; Returns the column which combo_box is using to get the strings from to
;;; display in the internal entry.
;;;
;;; combo_box :
;;;     A GtkComboBox.
;;;
;;; Returns :
;;;     A column in the data source model of combo_box.
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_popup_fixed_width ()
;;;
;;; void gtk_combo_box_set_popup_fixed_width (GtkComboBox *combo_box,
;;;                                           gboolean fixed);
;;;
;;; Specifies whether the popup's width should be a fixed width matching the
;;; allocated width of the combo box.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; fixed :
;;;     whether to use a fixed popup width
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_popup_fixed_width ()
;;;
;;; gboolean gtk_combo_box_get_popup_fixed_width (GtkComboBox *combo_box);
;;;
;;; Gets whether the popup uses a fixed width matching the allocated width of
;;; the combo box.
;;;
;;; combo_box :
;;;     a GtkComboBox
;;;
;;; Returns :
;;;     TRUE if the popup uses a fixed width
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.combo-box.lisp -----------------------------------------

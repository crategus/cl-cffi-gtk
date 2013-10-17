;;; ----------------------------------------------------------------------------
;;; gtk.combo-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GDK library.
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
;;; Properties
;;;
;;;   "active"                   gint                 : Read / Write
;;;   "active-id"                gchar*               : Read / Write
;;;   "add-tearoffs"             gboolean             : Read / Write
;;;   "button-sensitivity"       GtkSensitivityType   : Read / Write
;;;   "cell-area"                GtkCellArea*         : Read / Write / Construct
;;;   "column-span-column"       gint                 : Read / Write
;;;   "entry-text-column"        gint                 : Read / Write
;;;   "focus-on-click"           gboolean             : Read / Write
;;;   "has-entry"                gboolean             : Read / Write / Construct
;;;   "has-frame"                gboolean             : Read / Write
;;;   "id-column"                gint                 : Read / Write
;;;   "model"                    GtkTreeModel*        : Read / Write
;;;   "popup-fixed-width"        gboolean             : Read / Write
;;;   "popup-shown"              gboolean             : Read
;;;   "row-span-column"          gint                 : Read / Write
;;;   "tearoff-title"            gchar*               : Read / Write
;;;   "wrap-width"               gint                 : Read / Write
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
      For combo boxes that are created with an entry. See the
      @code{\"has-entry\"} property.
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
  tearoff menu items. Note that this only affects menu style combo boxes. @br{}
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
  if the combo was created with @code{\"has-entry\"} = @em{true}. @br{}
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
  property must be greater than zero and not larger than
  @code{\"wrap-width\"}. @br{}
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
 "@version{2013-10-16}
  Accessor of the slot @code{\"active\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active}
  @see-function{gtk-combo-box-set-active}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-active-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-active-id 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"active-id\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active-id}
  @see-function{gtk-combo-box-set-active-id}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-add-tearoffs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-add-tearoffs 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"add-tearoffs\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-add-tearoffs}
  @see-function{gtk-combo-box-set-add-tearoffs}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-button-sensitivity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-button-sensitivity 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"button-sensitivity\"} of the
  @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-button-sensitivity}
  @see-function{gtk-combo-box-set-button-sensitivity}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-cell-area 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"cell-area\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-column-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-column-span-column 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"column-span-column\"} of the
  @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-column-span-column}
  @see-function{gtk-combo-box-set-column-span-column}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-entry-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-entry-text-column 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"entry-text-column\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-entry-text-column}
  @see-function{gtk-combo-box-set-entry-text-column}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-focus-on-click atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-focus-on-click 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"focus-on-click\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-focus-on-click}
  @see-function{gtk-combo-box-set-focus-on-click}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-entry 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"has-entry\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-has-entry}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-frame atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-frame 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"has-frame\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-id-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-id-column 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"id-column\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-id-column}
  @see-function{gtk-combo-box-set-id-column}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-model 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"model\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-model}
  @see-function{gtk-combo-box-set-model}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-fixed-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-fixed-width 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"popup-fixed-width\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-popup-fixed-width}
  @see-function{gtk-combo-box-set-popup-fixed-width}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-shown atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-shown 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"popup-shown\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-row-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-row-span-column 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"row-span-column\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-row-span-column}
  @see-function{gtk-combo-box-set-row-span-column}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-tearoff-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-tearoff-title 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"tearoff-title\"} of the @class{gtk-combo-box}
  class.
  @see-class{gtk-combo-box}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-wrap-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-wrap-width 'function)
 "@version{2013-10-16}
  Accessor of the slot @code{\"wrap-width\"} of the @class{gtk-combo-box} class.
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-wrap-width}
  @see-function{gtk-combo-box-set-wrap-width}")

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

(export 'gtk-combo-box-new-with-entry)

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-area))

(defun gtk-combo-box-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-8-3}
  @argument[area]{the @class{gtk-cell-area} to use to layout cell renderers}
  @return{A new @class{gtk-combo-box} widget.}
  Creates a new empty @class{gtk-combo-box} using @arg{area} to layout cells.
  @see-class{gtk-combo-box}"
  (make-instance 'gtk-combo-box
                 :cell-area area))

(export 'gtk-combo-box-new-with-area)

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-wrap-width))

(defun gtk-combo-box-get-wrap-width (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The wrap width.}
  @begin{short}
    Returns the wrap width which is used to determine the number of columns for
    the popup menu.
  @end{short}
  If the wrap width is larger than 1, the combo box is in table mode.

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-wrap-width}"
  (gtk-combo-box-wrap-width combo-box))

(export 'gtk-combo-box-get-wrap-width)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_wrap_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-wrap-width))

(defun gtk-combo-box-set-wrap-width (combo-box width)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[width]{preferred number of columns}
  @begin{short}
    Sets the wrap width of @arg{combo-box} to be @arg{width}.
  @end{short}
  The wrap width is basically the preferred number of columns when you want the
  popup to be layed out in a table.

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-wrap-width}"
  (setf (gtk-combo-box-wrap-width combo-box) width))

(export 'gtk-combo-box-set-wrap-width)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_span_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-row-span-column))

(defun gtk-combo-box-get-row-span-column (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The row span column.}
  @begin{short}
    Returns the column with row span information for @arg{combo-box}.
  @end{short}

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-row-span-column}"
  (gtk-combo-box-row-span-column combo-box))

(export 'gtk-combo-box-get-row-span-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_row_span_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-row-span-column))

(defun gtk-combo-box-set-row-span-column (combo-box row-span)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[row-span]{a column in the model passed during construction}
  @begin{short}
    Sets the column with row span information for @arg{combo-box} to be
    @arg{row-span}.
  @end{short}
  The row span column contains integers which indicate how many rows an item
  should span.

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-row-span-column}"
  (setf (gtk-combo-box-row-span-column combo-box) row-span))

(export 'gtk-combo-box-set-row-span-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_column_span_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-column-span-column))

(defun gtk-combo-box-get-column-span-column (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The column span column.}
  @begin{short}
    Returns the column with column span information for @arg{combo-box}.
  @end{short}

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-column-span-column}"
  (gtk-combo-box-column-span-column combo-box))

(export 'gtk-combo-box-get-column-span-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_column_span_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-column-span-column))

(defun gtk-combo-box-set-column-span-column (combo-box column-span)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[column-span]{a column in the model passed during construction}
  @begin{short}
    Sets the column with column span information for @arg{combo-box} to be
    @arg{column-span}.
  @end{short}
  The column span column contains integers which indicate how many columns an
  item should span.

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-column-span-column}"
  (setf (gtk-combo-box-column-span-column combo-box) column-span))

(export 'gtk-combo-box-set-column-span-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-active))

(defun gtk-combo-box-get-active (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
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

  Since 2.4
  @see-class{gtk-combo-box}
  @see-class{gtk-tree-path}
  @see-function{gtk-combo-box-set-active}"
  (gtk-combo-box-active combo-box))

(export 'gtk-combo-box-get-active)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-active))

(defun gtk-combo-box-set-active (combo-box index)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[index]{an index in the model passed during construction, or -1 to
    have no active item}
  @begin{short}
    Sets the active item of @arg{combo-box} to be the item at @arg{index}.
  @end{short}

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active}"
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
 "@version{2013-8-3}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The @arg{iter}, or @code{nil}.}
  @begin{short}
    Returns @arg{iter} to point to the current active item, if it exists.
  @end{short}

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-active-iter}"
  (let ((iter (make-instance 'gtk-tree-iter)))
    (when (%gtk-combo-box-get-active-iter combo-box iter)
      iter)))

(export 'gtk-combo-box-get-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active_iter ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_set_active_iter" gtk-combo-box-set-active-iter) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-8-3}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[iter]{the @class{gtk-tree-iter}, or @code{nil}}
  @begin{short}
    Sets the current active item to be the one referenced by @arg{iter}, or
    unsets the active item if @arg{iter} is @code{nil}.
  @end{short}

  Since 2.4
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active-iter}"
  (combo-box (g-object gtk-combo-box))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-combo-box-set-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_id_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-id-column))

(defun gtk-combo-box-get-id-column (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{A column in the data source model of @arg{combo-box}.}
  @begin{short}
    Returns the column which @arg{combo-box} is using to get string IDs for
    values from.
  @end{short}

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-id-column}"
  (gtk-combo-box-id-column combo-box))

(export 'gtk-combo-box-get-id-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_id_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-id-column))

(defun gtk-combo-box-set-id-column (combo-box id-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[id-column]{a column in model to get string IDs for values from}
  @begin{short}
    Sets the model column which @arg{combo-box} should use to get string IDs for
    values from.
  @end{short}
  The column @arg{id-column} in the model of @arg{combo-box} must be of type
  @code{\"gchararray\"}.

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-id-column}"
  (setf (gtk-combo-box-id-column combo-box) id-column))

(export 'gtk-combo-box-set-id-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-active-id))

(defun gtk-combo-box-get-active-id (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The ID of the active row, or @code{nil}.}
  @begin{short}
    Returns the ID of the active row of @arg{combo-box}.
  @end{short}
  This value is taken from the active row and the column specified by the
  @code{\"id-column\"} property of @arg{combo-box}; see the function
  @fun{gtk-combo-box-set-id-column}.

  If the @code{\"id-column\"} property of @arg{combo-box} is not set, or if no
  row is active, or if the active row has a @code{nil} ID value, then @code{nil}
  is returned.

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-id-column}
  @see-function{gtk-combo-box-set-active-id}"
  (gtk-combo-box-active-id combo-box))

(export 'gtk-combo-box-get-active-id)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active_id ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-active-id))

(defun gtk-combo-box-set-active-id (combo-box active-id)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[active-id]{the ID of the row to select, or @code{nil}}
  @begin{return}
    @em{True} if a row with a matching ID was found. If a @code{nil}
    @arg{active-id} was given to unset the active row, the function always
    returns @em{true}.
  @end{return}
  @begin{short}
    Changes the active row of @arg{combo-box} to the one that has an ID equal to
    @arg{active-id}, or unsets the active row if @arg{active-id} is @code{nil}.
  @end{short}
  Rows having a @code{nil} ID string cannot be made active by this function.

  If the @code{\"id-column\"} property of @arg{combo-box} is unset or if no row
  has the given ID then the function does nothing and returns @code{nil}.

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active-id}"
  (setf (gtk-combo-box-active-id combo-box) active-id))

(export 'gtk-combo-box-set-active-id)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-model))

(defun gtk-combo-box-get-model (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{A @class{gtk-tree-model} object which was passed during construction.}
  @begin{short}
    Returns the @class{gtk-tree-model} object which is acting as data source for
    @arg{combo-box}.
  @end{short}

  Since 2.4
  @see-class{gtk-combo-box}
  @see-class{gtk-tree-model}
  @see-function{gtk-combo-box-set-model}"
  (gtk-combo-box-model combo-box))

(export 'gtk-combo-box-model)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-model))

(defun gtk-combo-box-set-model (combo-box model)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-15}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[model]{a @class{gtk-tree-model} object}
  @begin{short}
    Sets the model used by @arg{combo-box} to be @arg{model}.
  @end{short}
  Will unset a previously set model (if applicable). If @arg{model} is
  @code{nil}, then it will unset the model.

  Note that this function does not clear the cell renderers, you have to call
  the function @fun{gtk-cell-layout-clear} yourself if you need to set up
  different cell renderers for the new model.

  Since 2.4
  @see-class{gtk-combo-box}
  @see-class{gtk-tree-model}
  @see-function{gtk-combo-box-get-model}
  @see-function{gtk-cell-layout-clear}"
  (setf (gtk-combo-box-model combo-box) model))

(export 'gtk-combo-box-set-model)

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-add-tearoffs))

(defun gtk-combo-box-set-add-tearoffs (combo-box add-tearoffs)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[add-tearoffs]{@em{true} to add tearoff menu items}
  @begin{short}
    Sets whether the popup menu should have a tearoff menu item.
  @end{short}

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-add-tearoffs}"
  (setf (gtk-combo-box-add-tearoffs combo-box) add-tearoffs))

(export 'gtk-combo-box-set-add-tearoffs)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_add_tearoffs ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-add-tearoffs))

(defun gtk-combo-box-get-add-tearoffs (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The current value of the @code{\"add-tearoffs\"} property.}
  @short{Gets the current value of the @code{\"add-tearoffs\"} property.}
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-add-tearoffs}"
  (gtk-combo-box-add-tearoffs combo-box))

(export 'gtk-combo-box-get-add-tearoffs)

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
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-focus-on-click))

(defun gtk-combo-box-set-focus-on-click (combo focus-on-click)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo]{a @class{gtk-combo-box} widget}
  @argument[focus-on-click]{whether the combo box grabs focus when clicked with
    the mouse}
  @begin{short}
    Sets whether the combo box will grab focus when it is clicked with the
    mouse.
  @end{short}
  Making mouse clicks not grab focus is useful in places like toolbars
  where you do not want the keyboard focus removed from the main area of the
  application.

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-focus-on-click}"
  (setf (gtk-combo-box-focus-on-click combo) focus-on-click))

(export 'gtk-combo-box-set-focus-on-click)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_focus_on_click ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-focus-on-click))

(defun gtk-combo-box-get-focus-on-click (combo)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo]{a @class{gtk-combo-box} widget}
  @begin{return}
    @em{True} if the combo box grabs focus when it is clicked with the mouse.
  @end{return}
  @begin{short}
    Returns whether the combo box grabs focus when it is clicked with the mouse.
  @end{short}
  See the function @fun{gtk-combo-box-set-focus-on-click}.

  Since 2.6
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-focus-on-click}"
  (gtk-combo-box-focus-on-click combo))

(export 'gtk-combo-box-get-focus-on-click)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_button_sensitivity ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-button-sensitivity))

(defun gtk-combo-box-set-button-sensitivity (combo-box sensitivity)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[sensitivity]{specify the sensitivity of type
    @symbol{gtk-sensitivity-type} of the dropdown button}
  @begin{short}
    Sets whether the dropdown button of the combo box should be always sensitive
    @code{:on}, never sensitive @code{:off} or only if there is at least one
    item to display @code{:auto}.
  @end{short}

  Since 2.14
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-button-sensitivity}"
  (setf (gtk-combo-box-button-sensitivity combo-box) sensitivity))

(export 'gtk-combo-box-set-button-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_button_sensitivity ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-button-sensitivity))

(defun gtk-combo-box-get-button-sensitivity (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @begin{return}
    @code{:on} if the dropdown button is sensitive when the model is
    empty, @code{:off} if the button is always insensitive or @code{:auto} if it
    is only sensitive as long as the model has one item to be selected.
  @end{return}
  @begin{short}
    Returns whether the combo box sets the dropdown button sensitive or not when
    there are no items in the model.
  @end{short}

  Since 2.14
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-button-sensitivity}"
  (gtk-combo-box-button-sensitivity combo-box))

(export 'gtk-combo-box-get-button-sensitivity)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_has_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-has-entry))

(defun gtk-combo-box-get-has-entry (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{Whether there is an entry in @arg{combo-box}.}
  @short{Returns whether the combo box has an entry.}

  Since 2.24
  @see-class{gtk-combo-box}"
  (gtk-combo-box-has-entry combo-box))

(export 'gtk-combo-box-get-has-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_entry_text_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-entry-text-column))

(defun gtk-combo-box-set-entry-text-column (combo-box text-column)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[text-column]{a column in model to get the strings from for the
    internal entry}
  @begin{short}
    Sets the model column which @arg{combo-box} should use to get strings from
    to be @arg{text-column}.
  @end{short}
  The column @arg{text-column} in the model of @arg{combo-box} must be of type
  @code{\"gchararray\"}.

  This is only relevant if @arg{combo-box} has been created with
  @code{\"has-entry\"} as @em{true}.

  Since 2.24
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-entry-column}"
  (setf (gtk-combo-box-entry-text-column combo-box) text-column))

(export 'gtk-combo-box-set-entry-text-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_entry_text_column ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-entry-text-column))

(defun gtk-combo-box-get-entry-text-column (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{A column in the data source model of @arg{combo-box}.}
  @begin{short}
    Returns the column which @arg{combo-box} is using to get the strings from to
    display in the internal entry.
  @end{short}

  Since 2.24
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-entry-text-column}"
  (gtk-combo-box-entry-text-column combo-box))

(export 'gtk-combo-box-get-entry-text-column)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_popup_fixed_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-set-popup-fixed-width))

(defun gtk-combo-box-set-popup-fixed-width (combo-box fixed)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[fixed]{whether to use a fixed popup width}
  @begin{short}
    Specifies whether the popup's width should be a fixed width matching the
    allocated width of the combo box.
  @end{short}

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-popup-fixed-width}"
  (setf (gtk-combo-box-popup-fixed-width combo-box) fixed))

(export 'gtk-combo-box-set-popup-fixed-width)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_popup_fixed_width ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-get-popup-fixed-width))

(defun gtk-combo-box-get-popup-fixed-width (combo-box)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-16}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{@em{True} if the popup uses a fixed width.}
  @begin{short}
    Gets whether the popup uses a fixed width matching the allocated width of
    the combo box.
  @end{short}

  Since 3.0
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-popup-fixed-width}"
  (gtk-combo-box-popup-fixed-width combo-box))

(export 'gtk-combo-box-get-popup-fixed-width)

;;; --- End of file gtk.combo-box.lisp -----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.combo-box.lisp
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
;;; GtkComboBox
;;;
;;;     A widget used to choose from a list of items
;;;
;;; Types and Values
;;;
;;;     GtkComboBox
;;;
;;; Functions
;;;
;;;     gtk_combo_box_new
;;;     gtk_combo_box_new_with_entry
;;;     gtk_combo_box_new_with_model
;;;     gtk_combo_box_new_with_model_and_entry
;;;     gtk_combo_box_new_with_area
;;;     gtk_combo_box_new_with_area_and_entry
;;;     gtk_combo_box_get_wrap_width                       Accessor
;;;     gtk_combo_box_set_wrap_width                       Accessor
;;;     gtk_combo_box_get_row_span_column                  Accessor
;;;     gtk_combo_box_set_row_span_column                  Accessor
;;;     gtk_combo_box_get_column_span_column               Accessor
;;;     gtk_combo_box_set_column_span_column               Accessor
;;;     gtk_combo_box_get_active                           Accessor
;;;     gtk_combo_box_set_active                           Accessor
;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_get_id_column                        Accessor
;;;     gtk_combo_box_set_id_column                        Accessor
;;;     gtk_combo_box_get_active_id                        Accessor
;;;     gtk_combo_box_set_active_id                        Accessor
;;;     gtk_combo_box_get_model                            Accessor
;;;     gtk_combo_box_set_model                            Accessor
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_popup_accessible
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func
;;;     gtk_combo_box_set_add_tearoffs                     Accessor
;;;     gtk_combo_box_get_add_tearoffs                     Accessor
;;;     gtk_combo_box_set_title
;;;     gtk_combo_box_get_title
;;;     gtk_combo_box_set_focus_on_click
;;;     gtk_combo_box_get_focus_on_click
;;;     gtk_combo_box_set_button_sensitivity               Accessor
;;;     gtk_combo_box_get_button_sensitivity               Accessor
;;;     gtk_combo_box_get_has_entry                        Accessor
;;;     gtk_combo_box_set_entry_text_column                Accessor
;;;     gtk_combo_box_get_entry_text_column                Accessor
;;;     gtk_combo_box_set_popup_fixed_width                Accessor
;;;     gtk_combo_box_get_popup_fixed_width                Accessor
;;;
;;; Properties
;;;
;;;                   gint   active                Read / Write
;;;                  gchar*  active-id             Read / Write
;;;               gboolean   add-tearoffs          Read / Write
;;;     GtkSensitivityType   button-sensitivity    Read / Write
;;;            GtkCellArea*  cell-area             Read / Write / Construct Only
;;;                   gint   column-span-column    Read / Write
;;;                   gint   entry-text-column     Read / Write
;;;               gboolean   has-entry             Read / Write / Construct Only
;;;               gboolean   has-frame             Read / Write
;;;                   gint   id-column             Read / Write
;;;           GtkTreeModel*  model                 Read / Write
;;;               gboolean   popup-fixed-width     Read / Write
;;;               gboolean   popup-shown           Read
;;;                   gint   row-span-column       Read / Write
;;;                  gchar*  tearoff-title         Read / Write
;;;                   gint   wrap-width            Read / Write
;;;
;;; Style Properties
;;;
;;;          gboolean  appears-as-list    Read
;;;            gfloat  arrow-scaling      Read
;;;              gint  arrow-size         Read
;;;     GtkShadowType  shadow-type        Read
;;;
;;; Signals
;;;
;;;         void   changed              Run Last
;;;        gchar*  format-entry-text    Run Last
;;;         void   move-active          Action
;;;     gboolean   popdown              Action
;;;         void   popup                Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkComboBox
;;;                         ├── GtkAppChooserButton
;;;                         ╰── GtkComboBoxText
;;;
;;; Implemented Interfaces
;;;
;;;     GtkComboBox implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;;     and GtkCellEditable.
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
    "active-id" "gchararray" t t)
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

  To allow the user to enter values not in the model, the @code{has-entry}
  property allows the @sym{gtk-combo-box} to contain a @class{gtk-entry}. This
  entry can be accessed by calling the @fun{gtk-bin-get-child} function on the
  combo box.

  For a simple list of textual choices, the model-view API of
  @sym{gtk-combo-box} can be a bit overwhelming. In this case,
  @class{gtk-combo-box-text} offers a simple alternative. Both
  @sym{gtk-combo-box} and @class{gtk-combo-box-text} can contain an entry.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 CSS nodes
 combobox
 ├── box.linked
 │   ╰── button.combo
 │       ╰── box
 │           ├── cellview
 │           ╰── arrow
 ╰── window.popup
    @end{pre}
    A normal @code{combobox} contains a @code{box} with the @code{.linked}
    class, a @code{button} with the @code{.combo} class and inside those
    buttons, there are a @code{cellview} and an @code{arrow}.
    @begin{pre}
 combobox
 ├── box.linked
 │   ├── entry.combo
 │   ╰── button.combo
 │       ╰── box
 │           ╰── arrow
 ╰── window.popup
    @end{pre}
    A @sym{gtk-combo-box} with an entry has a single CSS node with name
    @code{combobox}. It contains a @code{box} with the @code{.linked} class.
    That box contains an @code{entry} and a @code{button}, both with the
    @code{.combo} class added. The button also contains another node with name
    @code{arrow}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[appears-as-list]{entry}
        The @code{appears-as-list} style property of type @code{:boolean}
        (Read) @br{}
        Whether dropdowns should look like lists rather than menus. @br{}
        Default value: @code{nil}{
      @end{entry}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of @code{:float} (Read) @br{}
        Sets the amount of space used up by the combo box arrow, proportional to
        the font size. @br{}
        @b{Warning:} @code{arrow-scaling} has been deprecated since version 3.20
        and should not be used in newly-written code. use the standard
        min-width/min-height CSS properties on the arrow node; the value of this
        style property is ignored. @br{}
        Allowed values: [0,2] @br{}
        Default value: 1 @br{}
      @end{entry}
      @begin[arrow-size]{entry}
        The @code{arrow-size} style property of type @code{:int} (Read) @br{}
        Sets the minimum size of the arrow in the combo box. Note that the arrow
        size is coupled to the font size, so in case a larger font is used, the
        arrow will be larger than set by arrow size. @br{}
        @b{Warning:} @code{arrow-size} has been deprecated since version 3.20
        and should not be used in newly-written code. Use the standard
        min-width/min-height CSS properties on the arrow node; the value of
        this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 15 @br{}
      @end{entry}
      @begin[shadow-type]{entry}
        The @code{shadow-type} style property of type @symbol{gtk-shadow-type}
        (Read) @br{}
        Which kind of shadow to draw around the combo box. @br{}
        @b{Warning:} @code{shadow-type} has been deprecated since version 3.20
        and should not be used in newly-written code. Use CSS styling to change
        the appearance of the combobox frame; the value of this style property
        is ignored. @br{}
        Default value: @code{:none} @br{}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
 lambda (widget)    : Run Last
      @end{pre}
      The changed signal is emitted when the active item is changed. The can be
      due to the user selecting a different item from the list, or due to a call
      to the @fun{gtk-combo-box-set-active-iter} function. It will also be
      emitted while typing into the entry of a combo box with an entry.
      @begin[code]{table}
        @entry[widget]{The object which received the signal.}
      @end{table}
    @subheading{The \"format-entry-text\" signal}
      @begin{pre}
 lambda (combo path)    : Run Last
      @end{pre}
      For combo boxes that are created with an entry. See the
      @code{has-entry} property.
      A signal which allows you to change how the text displayed in a combo
      box's entry is displayed.
      Connect a signal handler which returns an allocated string representing
      path. That string will then be used to set the text in the combo box's
      entry. The default signal handler uses the text from the
      @code{entry-text-column} property model column.
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
    @subheading{The \"move-active\" signal}
      @begin{pre}
 lambda (widget scroll-type)    : Action
      @end{pre}
      The \"move-active\" signal is a keybinding signal which gets emitted to
      move the active selection.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
        @entry[scroll-type]{A @symbol{gtk-scroll-type}.}
      @end{table}
    @subheading{The \"popdown\" signal}
      @begin{pre}
 lambda (button)    : Action
      @end{pre}
      The \"popdown\" signal is a keybinding signal which gets emitted to
      popdown the combo box list.
      The default bindings for this signal are Alt+Up and Escape.
      @begin[code]{table}
        @entry[button]{The object which received the signal.}
      @end{table}
    @subheading{The \"popup\" signal}
      @begin{pre}
 lambda (widget)    : Action
      @end{pre}
      The \"popup\" signal is a keybinding signal which gets emitted to popup
      the combo box list.
      The default binding for this signal is Alt+Down.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
      @end{table}
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
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-combo-box-active ---------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active" 'gtk-combo-box) 't)
 "The @code{active} property of type @code{:int} (Read / Write) @br{}
  The item which is currently active. If the model is a non-flat treemodel,
  and the active item is not an immediate child of the root of the tree, this
  property has the value @code{gtk_tree_path_get_indices (path)[0]}, where
  @arg{path} is the @class{gtk-tree-path} of the active item. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-active atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-active 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-active object) => index}
  @syntax[]{(setf (gtk-combo-box-active object) index)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[index]{an index in the model passed during construction, or -1 to
    have no active item}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{active} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-active} slot access function
  returns the index of the currently active item, or -1 if there is no active
  item.

  If the model is a non-flat tree model, and the active item is not an immediate
  child of the root of the tree, this function returns
  @code{gtk_tree_path_get_indices (path)[0]}, where path is the
  @class{gtk-tree-path} of the active item.

  The @sym{(setf gtk-combo-box-active)} slot access function
  sets the active item of @arg{combo-box} to be the item at @arg{index}.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-active-id ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "active-id" 'gtk-combo-box) 't)
 "The @code{active-id} property of type @code{:string} (Read / Write) @br{}
  The value of the ID column of the active row. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-active-id atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-active-id 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-active-id object) => active-id}
  @syntax[]{(setf (gtk-combo-box-active-id object) active-id)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[active-id]{the ID of the row to select, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{active-id} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-active-id} slot access function
  returns the ID of the active row of the combo box.

  This value is taken from the active row and the column specified by the
  @code{id-column} property of the combo box.

  If the @code{id-column} property of the combo box is not set, or if no
  row is active, or if the active row has a @code{nil} ID value, then @code{nil}
  is returned.

  The @sym{(setf gtk-combo-box-active-id)} slot access function
  changes the active row of the combo box to the one that has an ID equal to
  @arg{active-id}, or unsets the active row if @arg{active-id} is @code{nil}.

  Rows having a @code{nil} ID string cannot be made active by this function.

  If the @code{id-column} property of @arg{combo-box} is unset or if no row
  has the given ID then the function does nothing and returns @code{nil}.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-add-tearoffs ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "add-tearoffs" 'gtk-combo-box) 't)
 "The @code{add-tearoffs} property of type @code{:boolean}
  (Read / Write) @br{}
  The @code{add-tearoffs} property controls whether generated menus have
  tearoff menu items. Note that this only affects menu style combo boxes. @br{}
  @em{Warning:} @code{add-tearoffs} has been deprecated since version 3.10 and
  should not be used in newly-written code. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-add-tearoffs atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-add-tearoffs 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-add-tearoffs) => active-id}
  @syntax[]{(setf (gtk-combo-box-add-tearoffs object) active-id)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[add-tearoffs]{@em{true} to add tearoff menu items}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{add-tearoffs} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-add-tearoffs} slot access function
  gets the current value of the @code{add-tearoffs} property.

  The @sym{(setf gtk-combo-box-add-tearoffs)} slot access function
  sets whether the popup menu should have a tearoff menu item.
  @begin[Warning]{dictionary}
    The @sym{gtk-combo-box-add-tearoffs} function has been deprecated since
    version 3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-button-sensitivity ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "button-sensitivity"
                                               'gtk-combo-box) 't)
 "The @code{button-sensitivity} property of type
  @symbol{gtk-sensitivity-type} (Read / Write) @br{}
  Whether the dropdown button is sensitive when the model is empty. @br{}
  Default value: @code{:auto}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-button-sensitivity atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-button-sensitivity 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-button-sensitivity) => sensitivity}
  @syntax[]{(setf (gtk-combo-box-button-sensitivity object) sensitivity)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[sensitivity]{specify the sensitivity of type
    @symbol{gtk-sensitivity-type} of the dropdown button}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{button-sensitivity} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-button-sensitivity} slot access function
  returns whether the combo box sets the dropdown button sensitive or not when
  there are no items in the model.

  @code{:on} if the dropdown button is sensitive when the model is empty,
  @code{:off} if the button is always insensitive or @code{:auto} if it
  is only sensitive as long as the model has one item to be selected.

  The @sym{(setf gtk-combo-box-button-sensitivity)} slot access function
  sets whether the dropdown button of the combo box should be always sensitive
  @code{:on}, never sensitive @code{:off} or only if there is at least one
  item to display @code{:auto}.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-cell-area ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "cell-area" 'gtk-combo-box) 't)
 "The @code{cell-area} property of type @class{gtk-cell-area}
  (Read / Write / Construct) @br{}
  The @class{gtk-cell-area} used to layout cell renderers for this combo box.
  If no area is specified when creating the combo box with the
  @fun{gtk-combo-box-new-with-area} a horizontally oriented
  @class{gtk-cell-area-box} will be used.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-cell-area atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-cell-area 'function)
 "@version{2013-10-16}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{cell-area} slot of the
    @class{gtk-combo-box} class.
  @end{short}
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-column-span-column ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "column-span-column"
                                               'gtk-combo-box) 't)
 "The @code{column-span-column} property of type @code{:int}
  (Read / Write) @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type @variable{+g-type-int+} in the model.
  The values of that column are used to determine how many columns a value in
  the list will span. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-column-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-column-span-column 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-column-span-column) => column-span}
  @syntax[]{(setf (gtk-combo-box-column-span-column object) column-span)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[column-span]{a column in the model passed during construction}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{column-span-column} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-column-span-column} slot access function
  returns the column with column span information for the combo box.

  The @sym{(setf gtk-combo-box-column-span-column)} slot access function
  sets the column with column span information for the combo box to be
  @arg{column-span}. The column span column contains integers which indicate
  how many columns an item should span.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-entry-text-column ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "entry-text-column"
                                               'gtk-combo-box) 't)
 "The @code{entry-text-column} property of type @code{:int}
  (Read / Write) @br{}
  The column in the combo box's model to associate with strings from the entry
  if the combo was created with @code{has-entry} = @em{true}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-entry-text-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-entry-text-column 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-entry-text-column) => text-column}
  @syntax[]{(setf (gtk-combo-box-entry-text-column object) text-column)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[text-column]{a column in model to get the strings from for the
    internal entry}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{entry-text-column} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-entry-text-column} slot access function
  returns the column which the combo box is using to get the strings from to
  display in the internal entry.

  The @sym{(setf gtk-combo-box-entry-text-column)} slot access function
  sets the model column which the combo box should use to get strings from
  to be @arg{text-column}.

  The column @arg{text-column} in the model of the combo box must be of type
  @code{\"gchararray\"}. This is only relevant if the combo box has been created
  with the @code{has-entry} property as @em{true}.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-focus-on-click -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "focus-on-click"
                                               'gtk-combo-box) 't)
 "The @code{focus-on-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the combo box grabs focus when it is clicked with the mouse. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-focus-on-click atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-focus-on-click 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-focus-on-click) => focus-on-click}
  @syntax[]{(setf (gtk-combo-box-focus-on-click object) focus-on-click)}
  @argument[combo]{a @class{gtk-combo-box} widget}
  @argument[focus-on-click]{whether the combo box grabs focus when clicked with
    the mouse}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{focus-on-click} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-focus-on-click} slot access function
  returns whether the combo box grabs focus when it is clicked with the mouse.

  The @sym{(setf gtk-combo-box-focus-on-click)} slot access function
  sets whether the combo box will grab focus when it is clicked with the mouse.

  Making mouse clicks not grab focus is useful in places like toolbars
  where you do not want the keyboard focus removed from the main area of the
  application.
  @begin[Warning]{dictionary}
    The @sym{gtk-combo-box-focus-on-click} function has been deprecated
    since version 3.20 and should not be used in newly-written code. Use the
    @fun{gtk-widget-focus-on-click} function instead.
  @end{dictionary}
  @see-class{gtk-combo-box}
  @see-function{gtk-widget-focus-on-click}")

;;; --- gtk-combo-box-has-entry ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-entry" 'gtk-combo-box) 't)
 "The @code{has-entry} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the combo box has an entry. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-entry atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-entry 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-has-entry) => has-entry}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{has-entry} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-has-entry} slot access function
  returns whether the combo box has an entry.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-has-frame ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "has-frame" 'gtk-combo-box) 't)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  The has-frame property controls whether a frame is drawn around the
  entry. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-has-frame atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-has-frame 'function)
 "@version{2013-10-16}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{has-frame} slot of the
    @class{gtk-combo-box} class.
  @end{short}
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-id-column ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "id-column" 'gtk-combo-box) 't)
 "The @code{id-column} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model that provides string IDs for the values
  in the model, if != -1. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-id-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-id-column 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-id-column) => id-column}
  @syntax[]{(setf (gtk-combo-box-id-column object) id-column)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[id-column]{a column in model to get string IDs for values from}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{id-column} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-id-column} slot access function
  returns the column which combo box is using to get string IDs for values
  from.

  The @sym{(setf gtk-combo-box-id-column} slot access function
  sets the model column which the combo box should use to get string IDs for
  values from. The column @arg{id-column} in the model of the combo box must be
  of type @code{gchararray}.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-model ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "model" 'gtk-combo-box) 't)
 "The @code{model} property of type @class{gtk-tree-model}
  (Read / Write) @br{}
  The model from which the combo box takes the values shown in the list.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-model atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-model 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-model) => model}
  @syntax[]{(setf (gtk-combo-box-model object) model)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[model]{a @class{gtk-tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{model} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-model} slot access function
  returns the @class{gtk-tree-model} object which is acting as data source for
  the combo box.

  The @sym{(setf gtk-combo-box-model)} slot access function
  sets the model used by the combo box to be @arg{model}. Will unset a
  previously set model, if applicable. If @arg{model} is @code{nil}, then it
  will unset the model.

  Note that this function does not clear the cell renderers, you have to call
  the @fun{gtk-cell-layout-clear} function yourself if you need to set up
  different cell renderers for the new model.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-popup-fixed-width ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-fixed-width"
                                               'gtk-combo-box) 't)
 "The @code{popup-fixed-width} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the popup's width should be a fixed width matching the allocated
  width of the combo box. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-fixed-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-fixed-width 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-popup-fixed-width) => fixed}
  @syntax[]{(setf (gtk-combo-box-popup-fixed-width object) fixed)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[fixed]{whether to use a fixed popup width}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{popup-fixed-width} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-model} slot access function
  gets whether the popup uses a fixed width matching the allocated width of
  the combo box.

  The @sym{(setf gtk-combo-box-model)} slot access function
  specifies whether the popup's width should be a fixed width matching the
  allocated width of the combo box.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-popup-shown ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "popup-shown" 'gtk-combo-box) 't)
 "The @code{popup-shown} property of type @code{:boolean} (Read) @br{}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the \"notify::popup-shown\"
  signal. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-popup-shown atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-popup-shown 'function)
 "@version{2013-10-16}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{popup-shown} slot of the
    @class{gtk-combo-box} class.
  @end{short}
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-row-span-column ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "row-span-column"
                                               'gtk-combo-box) 't)
 "The @code{row-span-column} property of type @code{:int}
  (Read / Write) @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type @variable{+g-type-int+} in the model.
  The values of that column are used to determine how many rows a value in the
  list will span. Therefore, the values in the model column pointed to by this
  property must be greater than zero and not larger than
  @code{wrap-width}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-row-span-column atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-row-span-column 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-row-span-column) => row-spaned}
  @syntax[]{(setf (gtk-combo-box-row-span-column object) row-spaned)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[row-span]{a column in the model passed during construction}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{row-span-column} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-row-span-column} slot access function
  returns the column with row span information for the combo box.

  The @sym{(setf gtk-combo-box-row-span-column)} slot access function
  sets the column with row span information for the combo box to be
  @arg{row-span}. The row span column contains integers which indicate how many
  rows an item should span.
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-tearoff-title --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "tearoff-title"
                                               'gtk-combo-box) 't)
 "The @code{tearoff-title} property of type @code{:string}
  (Read / Write) @br{}
  A title that may be displayed by the window manager when the popup is
  torn-off. @br{}
  @b{Warning:} @code{tearoff-title} has been deprecated since version 3.10 and
  should not be used in newly-written code. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-tearoff-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-tearoff-title 'function)
 "@version{2013-10-16}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{tearoff-title} slot of the
    @class{gtk-combo-box} class.
  @end{short}
  @see-class{gtk-combo-box}")

;;; --- gtk-combo-box-wrap-width -----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "wrap-width" 'gtk-combo-box) 't)
 "The @code{wrap-width} property of type @code{:int} (Read / Write) @br{}
  If wrap-width is set to a positive value, the list will be displayed in
  multiple columns, the number of columns is determined by wrap-width. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-combo-box-wrap-width atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-combo-box-wrap-width 'function)
 "@version{2013-10-16}
  @syntax[]{(gtk-combo-box-wrap-width) => width}
  @syntax[]{(setf (gtk-combo-box-wrap-width object) width)}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[width]{preferred number of columns}
  @begin{short}
    Accessor of the @slot[gtk-combo-box]{wrap-width} slot of the
    @class{gtk-combo-box} class.
  @end{short}

  The @sym{gtk-combo-box-row-wrap-width} slot access function
  returns the wrap width which is used to determine the number of columns for
  the popup menu. If the wrap width is larger than 1, the combo box is in table
  mode.

  The @sym{(setf gtk-combo-box-row-wrap-width)} slot access function
  sets the wrap width of the combo box to be @arg{width}. The wrap width is
  basically the preferred number of columns when you want the popup to be layed
  out in a table.
  @see-class{gtk-combo-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new))

(defun gtk-combo-box-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @return{A new @class{gtk-combo-box} widget.}
  @short{Creates a new empty @class{gtk-combo-box} widget.}
  @see-class{gtk-combo-box}"
  (make-instance 'gtk-combo-box))

(export 'gtk-combo-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-entry))

(defun gtk-combo-box-new-with-entry ()
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @return{A new @class{gtk-combo-box} widget.}
  Creates a new empty @class{gtk-combo-box} widget with an entry.
  @see-class{gtk-combo-box}"
  (make-instance 'gtk-combo-box
                 :has-entry t))

(export 'gtk-combo-box-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-model))

(defun gtk-combo-box-new-with-model (model)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{A new @class{gtk-combo-box} widget.}
  @begin{short}
    Creates a new @class{gtk-combo-box} widget with the model initialized to
    @arg{model}.
  @end{short}
  @see-class{gtk-combo-box}
  @see-class{gtk-tree-model}"
  (make-instance 'gtk-combo-box
                 :model model))

(export 'gtk-combo-box-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model_and_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-model-and-entry))

(defun gtk-combo-box-new-with-model-and-entry (model)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[model]{a @class{gtk-tree-model} object}
  @return{A new @class{gtk-combo-box} widget}
  Creates a new empty @class{gtk-combo-box} widget with an entry and with the
  model initialized to @arg{model}.
  @see-class{gtk-combo-box}
  @see-class{gtk-tree-model}"
  (make-instance 'gtk-combo-box
                 :model model
                 :has-entry t))

(export 'gtk-combo-box-new-with-model-and-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-area))

(defun gtk-combo-box-new-with-area (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[area]{the @class{gtk-cell-area} to use to layout cell renderers}
  @return{A new @class{gtk-combo-box} widget.}
  Creates a new empty @class{gtk-combo-box} using @arg{area} to layout cells.
  @see-class{gtk-combo-box}
  @see-class{gtk-cell-area}"
  (make-instance 'gtk-combo-box
                 :cell-area area))

(export 'gtk-combo-box-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area_and_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-combo-box-new-with-area-and-entry))

(defun gtk-combo-box-new-with-area-and-entry (area)
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[area]{the @class{gtk-cell-area} object to use to layout cell
    renderers}
  @return{A new @class{gtk-combo-box} widget.}
  @begin{short}
    Creates a new empty @class{gtk-combo-box} widget with an entry.
  @end{short}

  The new combo box will use area to layout cells.
  @see-class{gtk-combo-box}
  @see-class{gtk-cell-area}"
  (make-instance 'gtk-combo-box
                 :cell-area area
                 :has-entry t))

(export 'gtk-combo-box-new-with-area-and-entry)

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
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-active-iter}"
  (combo-box (g-object gtk-combo-box))
  (iter (g-boxed-foreign gtk-tree-iter)))

(export 'gtk-combo-box-set-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_popup_for_device" gtk-combo-box-popup-for-device) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[device]{a @class{gdk-device} object}
  @begin{short}
    Pops up the menu or dropdown list of @arg{combo-box}, the popup window will
    be grabbed so only @arg{device} and its associated pointer/keyboard are the
    only @class{gdk-device}s able to send events to it.
  @end{short}
  @see-class{gtk-combo-box}
  @see-class{gdk-device}"
  (combo-box (g-object gtk-combo-box))
  (device (g-object gdk-device)))

(export 'gtk-combo-box-popup-for-device)

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
  @see-class{gtk-combo-box}"
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
  @see-clas{gtk-combo-box}"
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
  @see-class{gtk-combo-box}"
  (combo-box g-object))

(export 'gtk-combo-box-get-popup-accessible)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_separator_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_row_separator_func"
           gtk-combo-box-get-row-separator-func) :pointer
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The current row separator function.}
  @short{Returns the current row separator function.}
  @see-class{gtk-combo-box}"
  (combo-box (g-object gtk-combo-box)))

(export 'gtk-combo-box-get-row-separator-func)

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
  @see-class{gtk-combo-box}"
  (%gtk-combo-box-set-row-separator-func
                            combo-box
                            (callback gtk-tree-view-row-separator-func-callback)
                            (glib::allocate-stable-pointer func)
                            (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-combo-box-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_set_title" gtk-combo-box-set-title) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @argument[title]{a title for the menu in tearoff mode}
  @begin{short}
    Sets the menu's title in tearoff mode.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk-combo-box-set-title} function has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-get-title}"
  (combo-box (g-object gtk-combo-box))
  (title :string))

(export 'gtk-combo-box-set-title)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_title" gtk-combo-box-get-title) :string
 #+cl-cffi-gtk-documentation
 "@version{2013-10-17}
  @argument[combo-box]{a @class{gtk-combo-box} widget}
  @return{The menu's title in tearoff mode.}
  @begin{short}
    Gets the current title of the menu in tearoff mode.
  @end{short}
  See the @fun{gtk-combo-box-set-add-tearoffs} function.
  @begin[Warning]{dictionary}
    The @sym{gtk-combo-box-get-title} function has been deprecated since version
    3.10 and should not be used in newly-written code.
  @end{dictionary}
  @see-class{gtk-combo-box}
  @see-function{gtk-combo-box-set-title}
  @see-function{gtk-combo-box-set-add-tearoffs}"
  (combo-box (g-object gtk-combo-box)))

(export 'gtk-combo-box-get-title)

;;; --- End of file gtk.combo-box.lisp -----------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.combo-box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; GtkComboBox implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;; and GtkCellEditable.
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
;;;   "appears-as-list"          gboolean              : Read
;;;   "arrow-scaling"            gfloat                : Read
;;;   "arrow-size"               gint                  : Read
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Signals
;;; 
;;;   "changed"                                        : Run Last
;;;   "move-active"                                    : Action
;;;   "popdown"                                        : Action
;;;   "popup"                                          : Action
;;; 
;;; Description
;;; 
;;; A GtkComboBox is a widget that allows the user to choose from a list of
;;; valid choices. The GtkComboBox displays the selected choice. When activated,
;;; the GtkComboBox displays a popup which allows the user to make a new choice.
;;; The style in which the selected value is displayed, and the style of the
;;; popup is determined by the current theme. It may be similar to a
;;; Windows-style combo box.
;;; 
;;; The GtkComboBox uses the model-view pattern; the list of valid choices is
;;; specified in the form of a tree model, and the display of the choices can be
;;; adapted to the data in the model by using cell renderers, as you would in a
;;; tree view. This is possible since GtkComboBox implements the GtkCellLayout
;;; interface. The tree model holding the valid choices is not restricted to a
;;; flat list, it can be a real tree, and the popup will reflect the tree
;;; structure.
;;; 
;;; To allow the user to enter values not in the model, the 'has-entry' property
;;; allows the GtkComboBox to contain a GtkEntry. This entry can be accessed by
;;; calling gtk_bin_get_child() on the combo box.
;;; 
;;; For a simple list of textual choices, the model-view API of GtkComboBox can
;;; be a bit overwhelming. In this case, GtkComboBoxText offers a simple
;;; alternative. Both GtkComboBox and GtkComboBoxText can contain an entry.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gint                  : Read / Write
;;; 
;;; The item which is currently active. If the model is a non-flat treemodel,
;;; and the active item is not an immediate child of the root of the tree, this
;;; property has the value gtk_tree_path_get_indices (path)[0], where path is
;;; the GtkTreePath of the active item.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "active-id" property
;;; 
;;;   "active-id"                gchar*                : Read / Write
;;; 
;;; The value of the ID column of the active row.
;;; 
;;; Default value: NULL
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "add-tearoffs" property
;;; 
;;;   "add-tearoffs"             gboolean              : Read / Write
;;; 
;;; The add-tearoffs property controls whether generated menus have tearoff
;;; menu items.
;;; 
;;; Note that this only affects menu style combo boxes.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "button-sensitivity" property
;;; 
;;;   "button-sensitivity"       GtkSensitivityType    : Read / Write
;;; 
;;; Whether the dropdown button is sensitive when the model is empty.
;;; 
;;; Default value: GTK_SENSITIVITY_AUTO
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "cell-area" property
;;; 
;;;   "cell-area"                GtkCellArea*    : Read / Write / Construct Only
;;; 
;;; The GtkCellArea used to layout cell renderers for this combo box.
;;; 
;;; If no area is specified when creating the combo box with
;;; gtk_combo_box_new_with_area() a horizontally oriented GtkCellAreaBox will
;;; be used.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "column-span-column" property
;;; 
;;;   "column-span-column"       gint                  : Read / Write
;;; 
;;; If this is set to a non-negative value, it must be the index of a column of
;;; type G_TYPE_INT in the model.
;;; 
;;; The values of that column are used to determine how many columns a value in
;;; the list will span.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "entry-text-column" property
;;; 
;;;   "entry-text-column"        gint                  : Read / Write
;;; 
;;; The column in the combo box's model to associate with strings from the
;;; entry if the combo was created with "has-entry" = TRUE.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.24
;;;
;;; ----------------------------------------------------------------------------
;;; The "focus-on-click" property
;;; 
;;;   "focus-on-click"           gboolean              : Read / Write
;;; 
;;; Whether the combo box grabs focus when it is clicked with the mouse.
;;; 
;;; Default value: TRUE
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-entry" property
;;; 
;;;   "has-entry"                gboolean        : Read / Write / Construct Only
;;; 
;;; Whether the combo box has an entry.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.24
;;;
;;; ----------------------------------------------------------------------------
;;; The "has-frame" property
;;; 
;;;   "has-frame"                gboolean              : Read / Write
;;; 
;;; The has-frame property controls whether a frame is drawn around the entry.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.6
;;;
;;; ----------------------------------------------------------------------------
;;; The "id-column" property
;;; 
;;;   "id-column"                gint                  : Read / Write
;;; 
;;; The column in the combo box's model that provides string IDs for the values
;;; in the model, if != -1.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "model" property
;;; 
;;;   "model"                    GtkTreeModel*         : Read / Write
;;; 
;;; The model from which the combo box takes the values shown in the list.
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-fixed-width" property
;;; 
;;;   "popup-fixed-width"        gboolean              : Read / Write
;;; 
;;; Whether the popup's width should be a fixed width matching the allocated
;;; width of the combo box.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup-shown" property
;;; 
;;;   "popup-shown"              gboolean              : Read
;;; 
;;; Whether the combo boxes dropdown is popped up. Note that this property is
;;; mainly useful, because it allows you to connect to notify::popup-shown.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "row-span-column" property
;;; 
;;;   "row-span-column"          gint                  : Read / Write
;;; 
;;; If this is set to a non-negative value, it must be the index of a column
;;; of type G_TYPE_INT in the model.
;;; 
;;; The values of that column are used to determine how many rows a value in
;;; the list will span. Therefore, the values in the model column pointed to by
;;; this property must be greater than zero and not larger than wrap-width.
;;; 
;;; Allowed values: >= G_MAXULONG
;;; 
;;; Default value: -1
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "tearoff-title" property
;;; 
;;;   "tearoff-title"            gchar*                : Read / Write
;;; 
;;; A title that may be displayed by the window manager when the popup is
;;; torn-off.
;;; 
;;; Default value: NULL
;;; 
;;; Since 2.10
;;;
;;; ----------------------------------------------------------------------------
;;; The "wrap-width" property
;;; 
;;;   "wrap-width"               gint                  : Read / Write
;;; 
;;; If wrap-width is set to a positive value, the list will be displayed in
;;; multiple columns, the number of columns is determined by wrap-width.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 0
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "appears-as-list" style property
;;; 
;;;   "appears-as-list"          gboolean              : Read
;;; 
;;; Whether dropdowns should look like lists rather than menus.
;;; 
;;; Default value: FALSE
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-scaling" style property
;;; 
;;;   "arrow-scaling"            gfloat                : Read
;;; 
;;; Sets the amount of space used up by the combobox arrow, proportional to the
;;; font size.
;;; 
;;; Allowed values: [0,2]
;;; 
;;; Default value: 1
;;; 
;;; Since 3.2
;;;
;;; ----------------------------------------------------------------------------
;;; The "arrow-size" style property
;;; 
;;;   "arrow-size"               gint                  : Read
;;; 
;;; Sets the minimum size of the arrow in the combo box. Note that the arrow
;;; size is coupled to the font size, so in case a larger font is used, the
;;; arrow will be larger than set by arrow size.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 15
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "shadow-type" style property
;;; 
;;;   "shadow-type"              GtkShadowType         : Read
;;; 
;;; Which kind of shadow to draw around the combo box.
;;; 
;;; Default value: GTK_SHADOW_NONE
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "changed" signal
;;; 
;;; void user_function (GtkComboBox *widget,
;;;                     gpointer     user_data)      : Run Last
;;; 
;;; The changed signal is emitted when the active item is changed. The can be
;;; due to the user selecting a different item from the list, or due to a call
;;; to gtk_combo_box_set_active_iter(). It will also be emitted while typing
;;; into the entry of a combo box with an entry.
;;; 
;;; widget :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "move-active" signal
;;; 
;;; void user_function (GtkComboBox  *widget,
;;;                     GtkScrollType scroll_type,
;;;                     gpointer      user_data)        : Action
;;; 
;;; The ::move-active signal is a keybinding signal which gets emitted to move
;;; the active selection.
;;; 
;;; widget :
;;;     the object that received the signal
;;; 
;;; scroll_type :
;;;     a GtkScrollType
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "popdown" signal
;;; 
;;; gboolean user_function (GtkComboBox *button,
;;;                         gpointer     user_data)      : Action
;;; 
;;; The ::popdown signal is a keybinding signal which gets emitted to popdown
;;; the combo box list.
;;; 
;;; The default bindings for this signal are Alt+Up and Escape.
;;; 
;;; button :
;;;     the object which received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;;
;;; ----------------------------------------------------------------------------
;;; The "popup" signal
;;; 
;;; void user_function (GtkComboBox *widget,
;;;                     gpointer     user_data)      : Action
;;; 
;;; The ::popup signal is a keybinding signal which gets emitted to popup the
;;; combo box list.
;;; 
;;; The default binding for this signal is Alt+Down.
;;; 
;;; widget :
;;;     the object that received the signal
;;; 
;;; user_data :
;;;     user data set when the signal handler was connected.
;;; 
;;; Since 2.12
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(defcfun ("gtk_combo_box_get_active_text" gtk-combo-box-get-active-text)
    (:string :free-from-foreign t)
  (combo-box g-object))

(export 'gtk-combo-box-get-active-text)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBox
;;; 
;;; struct GtkComboBox;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBox" gtk-combo-box
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_get_type")
  ((active
    gtk-combo-box-active
    "active" "gint" t t)
   (add-tearoffs
    gtk-combo-box-add-tearoffs
    "add-tearoffs" "gboolean" t t)
   (button-sensitivity
    gtk-combo-box-button-sensitivity
    "button-sensitivity" "GtkSensitivityType" t t)
   (column-span-column
    gtk-combo-box-column-span-column
    "column-span-column" "gint" t t)
   (focus-on-click
    gtk-combo-box-focus-on-click
    "focus-on-click" "gboolean" t t)
   (has-frame
    gtk-combo-box-has-frame
    "has-frame" "gboolean" t t)
   (model
    gtk-combo-box-model
    "model" "GtkTreeModel" t t)
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
    "wrap-width" "gint" t t)
   (:cffi active-iter
          gtk-combo-box-active-iter (g-boxed-foreign gtk-tree-iter)
          combo-box-get-active-iter "gtk_combo_box_set_active_iter")
   (:cffi row-separator-func
          gtk-combo-box-separator-func nil
          nil combo-box-set-separator-func)
   (:cffi title
          gtk-combo-box-title
          (:string :free-from-foreign nil :free-to-foreign t)
          "gtk_combo_box_get_title" "gtk_combo_box_set_title")))

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new ()
;;; 
;;; GtkWidget * gtk_combo_box_new (void);
;;; 
;;; Creates a new empty GtkComboBox.
;;; 
;;; Returns :
;;;     A new GtkComboBox.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_entry ()
;;; 
;;; GtkWidget * gtk_combo_box_new_with_entry (void);
;;; 
;;; Creates a new empty GtkComboBox with an entry.
;;; 
;;; Returns :
;;;     A new GtkComboBox.
;;; ----------------------------------------------------------------------------

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
;;; GtkWidget * gtk_combo_box_new_with_model_and_entry (GtkTreeModel *model)
;;; 
;;; Creates a new empty GtkComboBox with an entry and with the model
;;; initialized to model.
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
;;; the preferred number of columns when you want the popup to be layed out in
;;; a table.
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
;;; Sets the column with row span information for combo_box to be row_span.
;;; The row span column contains integers which indicate how many rows an item
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
;;; 
;;; gint gtk_combo_box_get_active (GtkComboBox *combo_box);
;;; 
;;; Returns the index of the currently active item, or -1 if there's no active
;;; item. If the model is a non-flat treemodel, and the active item is not an
;;; immediate child of the root of the tree, this function returns
;;; gtk_tree_path_get_indices (path)[0], where path is the GtkTreePath of the
;;; active item.
;;; 
;;; combo_box :
;;;     A GtkComboBox
;;; 
;;; Returns :
;;;     An integer which is the index of the currently active item, or -1 if
;;;     there's no active item.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active ()
;;; 
;;; void gtk_combo_box_set_active (GtkComboBox *combo_box, gint index_);
;;; 
;;; Sets the active item of combo_box to be the item at index.
;;; 
;;; combo_box :
;;;     A GtkComboBox
;;; 
;;; index_ :
;;;     An index in the model passed during construction, or -1 to have no
;;;     active item
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_set_active" gtk-combo-box-set-active) :void
  (combo-box (g-object gtk-combo-box))
  (index :int))

(export 'gtk-combo-box-set-active)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_iter ()
;;; 
;;; gboolean gtk_combo_box_get_active_iter (GtkComboBox *combo_box,
;;;                                         GtkTreeIter *iter);
;;; 
;;; Sets iter to point to the current active item, if it exists.
;;; 
;;; combo_box :
;;;     A GtkComboBox
;;; 
;;; iter :
;;;     The uninitialized GtkTreeIter.
;;; 
;;; Returns :
;;;     TRUE, if iter was set
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_active_iter" %gtk-combo-box-get-active-iter)
    :boolean
  (combo-box g-object)
  (iter (g-boxed-foreign gtk-tree-iter)))

(defun gtk-combo-box-get-active-iter (combo-box)
  (let ((i (make-instance 'gtk-tree-iter)))
    (when (%gtk-combo-box-get-active-iter combo-box i)
      i)))

(export 'gtk-combo-box-get-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_active_iter ()
;;; 
;;; void gtk_combo_box_set_active_iter (GtkComboBox *combo_box,
;;;                                     GtkTreeIter *iter);
;;; 
;;; Sets the current active item to be the one referenced by iter, or unsets
;;; the active item if iter is NULL.
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
;;; void gtk_combo_box_set_id_column (GtkComboBox *combo_box, gint id_column)
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
;;; active_id, or unsets the active row if active_id is NULL. Rows having a
;;; NULL ID string cannot be made active by this function.
;;; 
;;; If the "id-column" property of combo_box is unset or if no row has the
;;; given ID then the function does nothing and returns FALSE.
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; active_id :
;;;     the ID of the row to select, or NULL. [allow-none]
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
;;; void gtk_combo_box_set_model (GtkComboBox *combo_box,
;;;                               GtkTreeModel *model);
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
;;; 
;;; void gtk_combo_box_popup (GtkComboBox *combo_box);
;;; 
;;; Pops up the menu or dropdown list of combo_box.
;;; 
;;; This function is mostly intended for use by accessibility technologies;
;;; applications should have little use for it.
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_popup" gtk-combo-box-popup) :void
  (combo-box g-object))

(export 'gtk-combo-box-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popdown ()
;;; 
;;; void gtk_combo_box_popdown (GtkComboBox *combo_box);
;;; 
;;; Hides the menu or dropdown list of combo_box.
;;; 
;;; This function is mostly intended for use by accessibility technologies;
;;; applications should have little use for it.
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_popdown" gtk-combo-box-popdown) :void
  (combo-box g-object))

(export 'gtk-combo-box-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_popup_accessible ()
;;; 
;;; AtkObject * gtk_combo_box_get_popup_accessible (GtkComboBox *combo_box);
;;; 
;;; Gets the accessible object corresponding to the combo box's popup.
;;; 
;;; This function is mostly intended for use by accessibility technologies;
;;; applications should have little use for it.
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; Returns :
;;;     the accessible object corresponding to the combo box's popup
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_get_popup_accessible"
          gtk-combo-box-get-popup-accessible) g-object
  (combo-box g-object))

(export 'gtk-combo-box-get-popup-accessible)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_separator_func ()
;;; 
;;; GtkTreeViewRowSeparatorFunc gtk_combo_box_get_row_separator_func
;;;                                                     (GtkComboBox *combo_box)
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
;;; 
;;; void gtk_combo_box_set_row_separator_func (GtkComboBox *combo_box,
;;;                                            GtkTreeViewRowSeparatorFunc func,
;;;                                            gpointer data,
;;;                                            GDestroyNotify destroy);
;;; 
;;; Sets the row separator function, which is used to determine whether a row
;;; should be drawn as a separator. If the row separator function is NULL, no
;;; separators are drawn. This is the default value.
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; func :
;;;     a GtkTreeViewRowSeparatorFunc
;;; 
;;; data :
;;;     user data to pass to func, or NULL
;;; 
;;; destroy :
;;;     destroy notifier for data, or NULL
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_set_row_separator_func"
          %gtk-combo-box-set-row-separator-func) :void
  (combo-box g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-combo-box-set-row-separator-func (combo-box func)
  (%gtk-combo-box-set-row-separator-func
                            combo-box
                            (callback gtk-tree-view-row-separator-func-callback)
                            (allocate-stable-pointer func)
                            (callback stable-pointer-free-destroy-notify-cb)))

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
;;;     the current value of the :add-tearoffs property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_title ()
;;; 
;;; void gtk_combo_box_set_title (GtkComboBox *combo_box, const gchar *title)
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
;;; Gets the current title of the menu in tearoff mode.
;;; See gtk_combo_box_set_add_tearoffs().
;;; 
;;; combo_box :
;;;     a GtkComboBox
;;; 
;;; Returns :
;;;     the menu's title in tearoff mode. This is an internal copy of the
;;;     string which must not be freed.
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
;;;                                                     (GtkComboBox *combo_box)
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

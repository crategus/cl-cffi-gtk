;;; ----------------------------------------------------------------------------
;;; gtk.tool-item-group.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GtkToolItemGroup
;;;
;;;     A sub container used in a tool palette
;;;
;;; Types and Values
;;;
;;;     GtkToolItemGroup
;;;
;;; Functions
;;;
;;;     gtk_tool_item_group_get_collapsed                  Accessor
;;;     gtk_tool_item_group_get_drop_item
;;;     gtk_tool_item_group_get_ellipsize                  Accessor
;;;     gtk_tool_item_group_get_item_position
;;;     gtk_tool_item_group_get_n_items
;;;     gtk_tool_item_group_get_label                      Accessor
;;;     gtk_tool_item_group_get_label_widget               Accessor
;;;     gtk_tool_item_group_get_nth_item
;;;     gtk_tool_item_group_get_header_relief              Accessor
;;;     gtk_tool_item_group_insert
;;;     gtk_tool_item_group_new
;;;     gtk_tool_item_group_set_collapsed                  Accessor
;;;     gtk_tool_item_group_set_ellipsize                  Accessor
;;;     gtk_tool_item_group_set_item_position
;;;     gtk_tool_item_group_set_label                      Accessor
;;;     gtk_tool_item_group_set_label_widget               Accessor
;;;     gtk_tool_item_group_set_header_relief              Accessor
;;;
;;; Properties
;;;
;;;               gboolean    collapsed         Read / Write
;;;     PangoEllipsizeMode    ellipsize         Read / Write
;;;         GtkReliefStyle    header-relief     Read / Write
;;;                  gchar*   label             Read / Write
;;;              GtkWidget*   label-widget      Read / Write
;;;
;;; Child Properties
;;;
;;;               gboolean    expand            Read / Write
;;;               gboolean    fill              Read / Write
;;;               gboolean    homogeneous       Read / Write
;;;               gboolean    new-row           Read / Write
;;;                   gint    position          Read / Write
;;;
;;; Style Properties
;;;
;;;                   gint    expander-size     Read
;;;                   gint    header-spacing    Read
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkToolItemGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolItemGroup implements AtkImplementorIface, GtkBuildable and
;;;     GtkToolShell.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItemGroup
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkToolItemGroup" 'gtk-tool-item-group))

(define-g-object-class "GtkToolItemGroup" gtk-tool-item-group
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkToolShell")
   :type-initializer "gtk_tool_item_group_get_type")
  ((collapsed
    gtk-tool-item-group-collapsed
    "collapsed" "gboolean" t t)
   (ellipsize
    gtk-tool-item-group-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (header-relief
    gtk-tool-item-group-header-relief
    "header-relief" "GtkReliefStyle" t t)
   (label
    gtk-tool-item-group-label
    "label" "gchararray" t t)
   (label-widget
    gtk-tool-item-group-label-widget
    "label-widget" "GtkWidget" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-tool-item-group 'type)
 "@version{*2021-3-14}
  @begin{short}
    A @sym{gtk-tool-item-group} widget is used together with a
    @class{gtk-tool-palette} widget to add @class{gtk-tool-item} widgets to a
    palette like container with different categories and drag and drop support.
  @end{short}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-tool-item-group} class has a single CSS node named
    @code{toolitemgroup}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should receive extra space when the group grows. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[fill]{entry}
        The @code{fill} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should fill the available space. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[homogenous]{entry}
        The @code{homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item should be the same size as other homogeneous items.
        @br{}
        Default value: @em{true}
      @end{entry}
      @begin[new-row]{entry}
        The @code{new-row} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should start a new row. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        Position of the item within this group. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[expander-size]{entry}
        The @code{expander-size} style property of type @code{:int} (Read) @br{}
        Size of the expander arrow. @br{}
        Allowed values: >= 0 @br{}
        Default value: 16
      @end{entry}
      @begin[header-spacing]{entry}
        The @code{header-spacing} style property of type @code{:int} (Read)
        @br{}
        Spacing between expander arrow and caption. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-tool-item-group-collapsed}
  @see-slot{gtk-tool-item-group-ellipsize}
  @see-slot{gtk-tool-item-group-header-relief}
  @see-slot{gtk-tool-item-group-label}
  @see-slot{gtk-tool-item-group-label-widget}
  @see-class{gtk-tool-item}
  @see-class{gtk-tool-palette}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-tool-item-group-collapsed ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "collapsed"
                                               'gtk-tool-item-group) 't)
 "The @code{collapsed} property of type @code{:boolean} (Read / Write) @br{}
  Whether the group has been collapsed and items are hidden. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-collapsed atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-collapsed 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-collapsed object) => collapsed}
  @syntax[]{(setf (gtk-tool-item-group-collapsed object) collapsed)}
  @argument[object]{a @class{gtk-tool-item-group} widget}
  @argument[collapsed]{a boolean whether the group should be collapsed
    or expanded}
  @begin{short}
    Accessor of the @slot[gtk-tool-item-group]{collapsed} slot of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-group-collapsed} gets whether the
  tool item group is collapsed or expanded. The slot access function
  @sym{(setf gtk-tool-item-group-collapsed)} sets whether the tool item group
  should be collapsed or expanded.
  @see-class{gtk-tool-item-group}")

;;; --- gtk-tool-item-group-ellipsize ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize"
                                               'gtk-tool-item-group) 't)
 "The @code{ellipsize} property of type @symbol{pango-ellipsize-mode}
  (Read / Write) @br{}
  Ellipsize for item group headers. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-ellipsize 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-ellipsize object) => ellipsize}
  @syntax[]{(setf (gtk-tool-item-group-ellipsize object) ellipsize)}
  @argument[object]{a @class{gtk-tool-item-group} widget}
  @argument[ellipsize]{the @symbol{pango-ellipsize-mode} labels in @arg{group}}
  @begin{short}
    Accessor of the @slot[gtk-tool-item-group]{ellipsize} slot of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-group-ellipsize} gets the
  ellipsization mode of the tool item group. The slot access function
  @sym{(setf gtk-tool-item-group-ellipsize)} sets the ellipsization mode which
  should be used by labels in the tool item group.
  @see-class{gtk-tool-item-group}
  @see-symbol{pango-ellipsize-mode}")

;;; --- gtk-tool-item-group-header-relief --------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "header-relief"
                                               'gtk-tool-item-group) 't)
 "The @code{header-relief} property of type @symbol{gtk-relief-style}
  (Read / Write) @br{}
  Relief of the group header button. @br{}
  Default value: @code{:normal}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-header-relief atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-header-relief 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-header-relief object) => style}
  @syntax[]{(setf (gtk-tool-item-group-header-relief object) style)}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[object]{a value of the @symbol{gtk-relief-style} enumeration}
  @begin{short}
    Accessor of the @slot[gtk-tool-item-group]{header-relief} slot of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The slot acces function @sym{gtk-tool-item-group-header-relief} gets the
  relief mode of the header button of the tool item group. The slot access
  function @sym{(setf gtk-tool-item-group-header-relief)} sets the button
  relief of the group header.
  @see-class{gtk-tool-item-group}
  @see-symbol{gtk-relief-style}")

;;; --- gtk-tool-item-group-label ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label"
                                               'gtk-tool-item-group) 't)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The human-readable title of this item group. @br{}
  Default value: \"\"")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-label atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-label 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-label object) => label}
  @syntax[]{(setf (gtk-tool-item-group-label object) label)}
  @argument[object]{a @class{gtk-tool-item-group} widget}
  @argument[label]{a @code{:string} with the new human-readable label of of the
    group}
  @begin{short}
    Accessor of the @slot[gtk-tool-item-group]{label} slot of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-group-label} gets the label of
  the tool item group. The slot access function
  @sym{(setf gtk-tool-item-group-label)} sets the label of the tool item group.

  The label is displayed in the header of the group. Note that @code{nil} is
  returned if a custom label has been set with the function
  @fun{gtk-tool-item-group-label-widget}.
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-item-group-label-widget}")

;;; --- gtk-tool-item-group-label-widget ---------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "label-widget"
                                               'gtk-tool-item-group) 't)
 "The @code{label-widget} property of type @class{gtk-widget} (Read / Write)
  @br{}
  A widget to display in place of the usual label.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-label-widget atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-label-widget 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-label-widget object) => label-widget}
  @syntax[]{(setf (gtk-tool-item-group-label-widget object) label-widget)}
  @argument[object]{a @class{gtk-tool-item-group} widget}
  @argument[label-widget]{the @class{gtk-widget} to be displayed in place of
    the usual label}
  @begin{short}
    Accessor of the @slot[gtk-tool-item-group]{label-widget} slot of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The slot access function @sym{gtk-tool-item-group-label-widget} gets the
  label widget of the tool item group. The slot access function
  @sym{(setf gtk-tool-item-group-label-widget)} sets the label of the tool item
  group.

  The label widget is displayed in the header of the group, in place of the
  usual label.
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-item-group-label}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-tool-item-group-child-expand ---------------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-expand 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-child-expand container child) => expand}
  @syntax[]{(setf (gtk-tool-item-group-child-expand container child) expand)}
  @argument[container]{a @class{gtk-tool-item-group} widget}
  @argument[child]{a @class{gtk-widget} child object}
  @argument[expand]{a boolean whether the item should receive extra space when
    the group grows}
  @begin{short}
    Accessor of the @code{expand} child property of the
    @class{gtk-tool-item-group} class.
  @end{short}
  @see-class{gtk-tool-item-group}")

;;; --- gtk-tool-item-group-child-fill -----------------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-fill 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-child-fill container child) => fill}
  @syntax[]{(setf (gtk-tool-item-group-child-fill container child) fill)}
  @argument[container]{a @class{gtk-tool-item-group} widget}
  @argument[child]{a @class{gtk-widget} child object}
  @argument[fill]{a boolean whether the item should fill the available
    space}
  @begin{short}
    Accessor of the @code{fill} child property of the
    @class{gtk-tool-item-group} class.
  @end{short}
  @see-class{gtk-tool-item-group}")

;;; --- gtk-tool-item-group-child-homogeneous ----------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-homogeneous
                       "homogeneous" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-homogeneous
               atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-homogeneous 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-child-homogeneous container child) => homogeneous}
  @syntax[]{(setf (gtk-tool-item-group-child-homogeneous container child) homogeneous)}
  @argument[container]{a @class{gtk-tool-item-group} widget}
  @argument[child]{a @class{gtk-widget} child object}
  @argument[fill]{a boolean whether the item should be the same size
    as other homogeneous items}
  @begin{short}
    Accessor of the @code{homogeneous} child property of the
    @class{gtk-tool-item-group} class.
  @end{short}
  @see-class{gtk-tool-item-group}")

;;; --- gtk-tool-item-group-child-new-row --------------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-new-row
                       "new-row" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-new-row atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-new-row 'function)
 "@version{2020-9-5}
  @syntax[]{(gtk-tool-item-group-child-new-row container child) => new-row}
  @syntax[]{(setf (gtk-tool-item-group-child-new-row container child) new-row)}
  @argument[container]{a @class{gtk-tool-item-group} widget}
  @argument[child]{a @class{gtk-widget} child object}
  @argument[new-row]{a boolean whether the item should start a new row}
  @begin{short}
    Accessor of the @code{new-row} child property of the
    @class{gtk-tool-item-group} class.
  @end{short}
  @see-class{gtk-tool-item-group}")

;;; --- gtk-tool-item-group-child-position -------------------------------------

(define-child-property "GtkToolItemGroup"
                       gtk-tool-item-group-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-tool-item-group-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-tool-item-group-child-position 'function)
 "@version{*2021-3-14}
  @syntax[]{(gtk-tool-item-group-child-position container child) => position}
  @syntax[]{(setf (gtk-tool-item-group-child-position container child) position)}
  @argument[container]{a @class{gtk-tool-item-group} widget}
  @argument[child]{a @class{gtk-tool-item} child object}
  @argument[position]{an integer with the position of the item within the group}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk-tool-item-group} class.
  @end{short}

  The function @sym{gtk-tool-item-group-child-position} gets the position of
  @arg{item} in the list of children of @arg{container}, or -1 if @arg{item}
  is no child of @arg{container}. The function
  @sym{gtk-tool-item-group-child-position} sets the position, starting with 0,
  the position -1 means end of list.
  @see-class{gtk-tool-item-group}
  @see-class{gtk-tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_drop_item () -> gtk-tool-item-group-drop-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_get_drop_item" gtk-tool-item-group-drop-item)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-14}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @return{The @class{gtk-tool-item} widget at position (@arg{x}, @arg{y}).}
  @short{Gets the tool item at position (@arg{x}, @arg{y}).}
  @see-class{gtk-tool-item-group}
  @see-class{gtk-tool-item}"
  (group (g-object gtk-tool-item-group))
  (x :int)
  (y :int))

(export 'gtk-tool-item-group-drop-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_item_position ()
;;; ----------------------------------------------------------------------------

;; Implemented as gtk-tool-item-group-child-position

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_n_items () -> gtk-tool-item-n-items
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_get_n_items" gtk-tool-item-group-n-items) :uint
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @return{A @code{:uint} with the number of tool items in @arg{group}.}
  @begin{short}
    Gets the number of tool items in the tool item group.
  @end{short}
  @see-class{gtk-tool-item-group}
  @see-function{gtk-tool-item-group-nth-item}"
  (group (g-object gtk-tool-item-group)))

(export 'gtk-tool-item-group-n-items)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_nth_item () -> gtk-tool-item-group-nth-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_get_nth_item" gtk-tool-item-group-nth-item)
    (g-object gtk-tool-item)
 #+cl-cffi-gtk-documentation
 "@version{2020-9-5}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[index]{a @code{:unit} with the index}
  @return{The @class{gtk-tool-item} widget at @arg{index}.}
  @begin{short}
    Gets the tool item at @arg{index} in the tool item group.
  @end{short}
  @see-class{gtk-tool-item-group}
  @see-class{gtk-tool-item}"
  (group (g-object gtk-tool-item-group))
  (index :uint))

(export 'gtk-tool-item-group-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_item_group_insert" gtk-tool-item-group-insert) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-14}
  @argument[group]{a @class{gtk-tool-item-group} widget}
  @argument[item]{the @class{gtk-tool-item} widget to insert into group}
  @argument[position]{an integer with the position of @arg{item} in
    @arg{group}, starting with 0, the position -1 means end of list}
  @begin{short}
    Inserts @arg{item} at @arg{position} in the list of children of the
    tool item group.
  @end{short}
  @see-class{gtk-tool-item-group}
  @see-class{gtk-tool-item}"
  (group (g-object gtk-tool-item-group))
  (item (g-object gtk-tool-item))
  (position :int))

(export 'gtk-tool-item-group-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-tool-item-group-new))

(defun gtk-tool-item-group-new (label)
 #+cl-cffi-gtk-documentation
 "@version{*2021-3-14}
  @argument[label]{a string with the label of the new group}
  @return{A new @class{gtk-tool-item-group} widget.}
  @begin{short}
    Creates a new tool item group with label @arg{label}.
  @end{short}
  @see-class{gtk-tool-item-group}"
  (make-instance 'gtk-tool-item-group
                 :label label))

(export 'gtk-tool-item-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_item_position ()
;;; ----------------------------------------------------------------------------

;; Implemented as (setf gtk-tool-item-group-child-position

;;; --- End of file gtk.tool-item-group.lisp -----------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.box.lisp
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
;;; GtkBox
;;;
;;;     A container for packing widgets in a single row or column
;;;
;;; Functions
;;;
;;;     gtk_box_new
;;;     gtk_box_pack_start
;;;     gtk_box_pack_end
;;;     gtk_box_get_homogeneous                            Accessor
;;;     gtk_box_set_homogeneous                            Accessor
;;;     gtk_box_get_spacing                                Accessor
;;;     gtk_box_set_spacing                                Accessor
;;;     gtk_box_reorder_child
;;;     gtk_box_query_child_packing
;;;     gtk_box_set_child_packing
;;;     gtk_box_get_baseline_position                      Accessor
;;;     gtk_box_set_baseline_position                      Accessor
;;;     gtk_box_get_center_widget
;;;     gtk_box_set_center_widget
;;;
;;; Properties
;;;
;;;     GtkBaselinePosition  baseline-position  Read / Write
;;;     gboolean             homogeneous        Read / Write
;;;     gint                 spacing            Read / Write
;;;
;;; Child Properties
;;;
;;;     gboolean     expand     Read / Write
;;;     gboolean     fill       Read / Write
;;;     GtkPackType  pack-type  Read / Write
;;;     guint        padding    Read / Write
;;;     gint         position   Read / Write
;;;
;;; Types and Values
;;;
;;;     struct  GtkBox
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ├── GtkAppChooserWidget
;;;                     ├── GtkButtonBox
;;;                     ├── GtkColorChooserWidget
;;;                     ├── GtkColorSelection
;;;                     ├── GtkFileChooserButton
;;;                     ├── GtkFileChooserWidget
;;;                     ├── GtkFontChooserWidget
;;;                     ├── GtkFontSelection
;;;                     ├── GtkHBox
;;;                     ├── GtkInfoBar
;;;                     ├── GtkRecentChooserWidget
;;;                     ├── GtkShortcutsSection
;;;                     ├── GtkShortcutsGroup
;;;                     ├── GtkShortcutsShortcut
;;;                     ├── GtkStackSwitcher
;;;                     ├── GtkStatusbar
;;;                     ╰── GtkVBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkBox
;;; ----------------------------------------------------------------------------

;; There is a problem, if the Lisp name of the class corresponds to the
;; name of the C class. For this case the name is not registered. That causes
;; further problems.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkBox" 'gtk-box))

(define-g-object-class "GtkBox" gtk-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_box_get_type")
  (#+gtk-3-10
   (baseline-position
    gtk-box-base-line-position
    "baseline-position" "GtkBaselinePosition" t t)
   (homogeneous
    gtk-box-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    gtk-box-spacing
    "spacing" "gint" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-box 'type)
 "@version{2019-3-13}
  @begin{short}
    The @sym{gtk-box} widget arranges child widgets into a single row or column,
    depending upon the value @code{:horizontal} or @code{:vertical} of its
    inherited @slot[gtk-orientable]{orientation} property.
  @end{short}
  Within the other dimension, all children are allocated the same size. Of 
  course, the @slot[gtk-widget]{halign} and @slot[gtk-widget]{valign}
  properties can be used on the children to influence their allocation.

  @sym{gtk-box} uses a notion of packing. Packing refers to adding widgets with 
  reference to a particular position in a @class{gtk-container}. For a 
  @sym{gtk-box}, there are two reference positions: the start and the end of 
  the box. For a vertical @sym{gtk-box}, the start is defined as the top of 
  the box and the end is defined as the bottom. For a horizontal @sym{gtk-box}
  the start is defined as the left side and the end is defined as the right 
  side.

  Use repeated calls to the function @fun{gtk-box-pack-start} to pack widgets 
  into a @sym{gtk-box} from start to end. Use the function 
  @fun{gtk-box-pack-end} to add widgets from end to start. You may intersperse 
  these calls and add widgets from both ends of the same @sym{gtk-box}.

  Because @sym{gtk-box} is a @class{gtk-container}, you may also use the 
  function @fun{gtk-container-add} to insert widgets into the box, and they will
  be packed with the default values for expand and fill child properties. Use 
  the function @fun{gtk-container-remove} to remove widgets from the 
  @sym{gtk-box}.

  Use the slot access function @fun{gtk-box-homogeneous} to specify whether or
  not all children of the @sym{gtk-box} are forced to get the same amount of
  space.

  Use the slot access function @fun{gtk-box-spacing} to determine how much space 
  will be minimally placed between all children in the @sym{gtk-box}. Note that 
  spacing is added between the children, while padding added by the functions
  @fun{gtk-box-pack-start} or @fun{gtk-box-pack-end} is added on either side of 
  the widget it belongs to.

  Use the function @fun{gtk-box-reorder-child} to move a @sym{gtk-box} child to 
  a different place in the box.

  Use the function @fun{gtk-box-set-child-packing} to reset the @code{expand},
  @code{fill} and @code{padding} child properties. Use the function 
  @fun{gtk-box-query-child-packing} to query these fields.
  @begin[CSS nodes]{dictionary}
    @sym{gtk-box} uses a single CSS node with name @code{box}.

    In horizontal orientation, the nodes of the children are always arranged 
    from left to right. So @code{:first-child} will always select the leftmost
    child, regardless of text direction.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the child should receive extra space when the parent grows.
        Note that the default value for this property is @code{nil} for
        @sym{gtk-box}. Note that the @slot[gtk-widget]{halign},
        @slot[gtk-widget]{valign}, @slot[gtk-widget]{hexpand} and
        @slot[gtk-widget]{vexpand} properties are the preferred way to
        influence child size allocation in containers. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[fill]{entry}
        The @code{fill} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the child should receive extra space when the parent grows.
        Note that the @slot[gtk-widget]{halign}, @slot[gtk-widget]{valign},
        @slot[gtk-widget]{hexpand} and @slot[gtk-widget]{vexpand} properties
        are the preferred way to influence child size allocation in containers.
        @br{}
        Default value: @em{true}
      @end{entry}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk-pack-type} 
        (Read / Write) @br{}
        A @symbol{gtk-pack-type} indicating whether the child is packed with
        reference to the start or end of the parent. @br{}
        Default value: @code{:start}
      @end{entry}
      @begin[padding]{entry}
        The @code{padding} child property of type @code{:uint} (Read / Write)
        @br{}
        Extra space to put between the child and its neighbors, in pixels. @br{}
        Default value: 0
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child in the parent. @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-box-baseline-position}
  @see-slot{gtk-box-homogeneous}
  @see-slot{gtk-box-spacing}
  @see-class{gtk-orientable}
  @see-symbol{gtk-orientation}
  @see-symbol{gtk-pack-type}
  @see-class{gtk-container}
  @see-function{gtk-container-add}
  @see-function{gtk-container-remove}
  @see-function{gtk-box-pack-start}
  @see-function{gtk-box-pack-end}
  @see-function{gtk-box-reorder-child}
  @see-function{gtk-box-set-child-packing}
  @see-function{gtk-box-query-child-packing}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-box-baseline-position ----------------------------------------------

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (documentation (atdoc:get-slot-from-name "baseline-position" 'gtk-box) 't)
 "The @code{baseline-position} property of type
  @symbol{gtk-baseline-position} (Read / Write) @br{}
  The position of the baseline aligned widgets if extra space is available.
  @br{}
  Default value: @code{:center} @br{}
  Since 3.10")

#+(and gtk-3-10 cl-cffi-gtk-documentation)
(setf (gethash 'gtk-box-baseline-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-baseline-position 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-baseline-position object) => position)}
  @syntax[]{(setf (gtk-box-baseline-position object) position)}
  @argument[object]{a @class{gtk-box} container}
  @argument[position]{a baseline position of type 
    @symbol{gtk-baseline-position}}
  @begin{short}
    Accessor of the slot @slot[gtk-box]{baseline-position} of the
    @class{gtk-box} class.
  @end{short}

  The slot access function @sym{gtk-box-baseline-position} 
  gets the baseline position of a box. 
  
  The slot access function @sym{(setf gtk-box-baseline-position)}
  sets the baseline position of a box. This affects only horizontal boxes with
  at least one baseline aligned child. If there is more vertical space available
  than requested, and the baseline is not allocated by the parent then
  @arg{position} is used to allocate the baseline wrt the extra space available.

  Since 3.10
  @see-class{gtk-box}
  @see-symbol{gtk-baseline-position}")

;;; --- gtk-box-homogeneous ----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous" 'gtk-box) 't)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Whether the children should all be the same size. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-homogeneous 'function)
 "@version{2014-2-10}
  @syntax[]{(gtk-box-homogeneous object) => homogeneous)}
  @syntax[]{(setf (gtk-box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-box} container}
  @argument[homogeneous]{@emph{true} to create equal allotments, @code{nil}
    for variable allotments}
  @begin{short}
    Accessor of the slot @slot[gtk-box]{homogeneous} of the @class{gtk-box}
    class.
  @end{short}

  The slot access function @sym{gtk-box-homogeneous} returns whether the box is
  homogeneous, that is, all children are the same size.

  The slot access function @sym{(setf gtk-box-homogeneous)} sets the
  @slot[gtk-box]{homogeneous} property of the box, controlling whether
  or not all children of the box are given equal space in the box.
  @see-class{gtk-box}")

;;; --- gtk-box-spacing --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-box) 't)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-spacing 'function)
 "@version{2014-2-11}
  @syntax[]{(gtk-box-spacing object) => spacing)}
  @syntax[]{(setf (gtk-box-spacing object) spacing)}
  @argument[object]{a @class{gtk-box} container}
  @argument[spacing]{the number of pixels to put between children}
  @begin{short}
    Accessor of the slot @slot[gtk-box]{spacing} of the @class{gtk-box} class.
  @end{short}

  The slot access function @sym{gtk-box-spacing} returns the spacing between
  children.

  The slot access function @sym{(setf gtk-box-spacing)} sets the
  @slot[gtk-box]{spacing} property of the box, which is the number of pixels
  to place between children of the box.
  @see-class{gtk-box}")

;;; ----------------------------------------------------------------------------
;;; Child Property Implementation
;;; ----------------------------------------------------------------------------

;;; --- gtk-box-child-expand ---------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-expand
                       "expand" "gboolean" t t t)
                       
#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-expand 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-child-expand container child) => expand)}
  @syntax[]{(setf (gtk-box-child-expand container child) expand)}
  @argument[container]{a @class{gtk-box} container}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[expand]{@em{true}, if @arg{child} is to be given extra space}
  Accessor of the @code{expand} child property of the @class{gtk-box} class.
  @see-class{gtk-box}")

;;; --- gtk-box-child-fill -----------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-fill 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-child-expand container child) => fill)}
  @syntax[]{(setf (gtk-box-child-expand container child) fill)}  
  @argument[container]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[fill]{@em{true}, if space given to @arg{child} by the expand option}
  Accessor of the @code{fill} child property of the @class{gtk-box} class.
  @see-class{gtk-box}")

;;; --- gtk-box-child-padding --------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-padding
                       "padding" "guint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-padding 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-child-padding container child) => padding)}
  @syntax[]{(setf (gtk-box-child-padding container child) padding)} 
  @argument[container]{a @class{gtk-box} container}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[padding]{extra space in pixels to put between childs} 
  Accessor of the @code{padding} child property of the @class{gtk-box} class.
  @see-class{gtk-box}")

;;; --- gtk-box-child-pack-type ------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-pack-type 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-child-pack-type container child) => pack-type)}
  @syntax[]{(setf (gtk-box-child-pack-type container child) pack-type)} 
  @argument[container]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} child widget}
  @argument[pack-type]{the pack type of type @symbol{gtk-pack-type}}  
  Accessor of the @code{pack-type} child property of the @class{gtk-box} class.
  @see-class{gtk-box}
  @see-symbol{gtk-pack-type}")

;;; --- gtk-box-child-position -------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-position 'function)
 "@version{2019-3-15}
  @syntax[]{(gtk-box-child-position container child) => position)}
  @syntax[]{(setf (gtk-box-child-position container child) position)} 
  @argument[container]{a @class{gtk-box} container}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[position]{the position of @arg{child} in a box} 
  Accessor of the @code{position} child property of the @class{gtk-box} class.
  @see-class{gtk-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-new))

(defun gtk-box-new (orientation spacing)
 #+cl-cffi-gtk-documentation
 "@version{2019-3-15}
  @argument[orientation]{the box's orientation of type @symbol{gtk-orientation}}
  @argument[spacing]{the number of pixels to place by default between children}
  @return{A new @class{gtk-box} container.}
  @short{Creates a new @class{gtk-box} container.}

  Since 3.0
  @see-class{gtk-box}
  @see-symbol{gtk-orientation}
  @see-function{gtk-box-spacing}"
  (make-instance 'gtk-box
                 :orientation orientation
                 :spacing spacing))

(export 'gtk-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_box_pack_start" %gtk-box-pack-start) :void
  (box (g-object gtk-box))
  (child (g-object gtk-widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun gtk-box-pack-start (box child &key (expand t) (fill t) (padding 0))
 #+cl-cffi-gtk-documentation
 "@version{2014-2-21}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} to be added to @arg{box}}
  @argument[expand]{@arg{true} if the new @arg{child} is to be given extra space
    allocated to @arg{box}. The extra space will be divided evenly between all
    children that use this option. The default value is @em{true}.}
  @argument[fill]{@arg{true} if space given to @arg{child} by the expand option
    is actually allocated to @arg{child}, rather than just padding it. This
    parameter has no effect if @arg{expand} is set to @code{nil}. A child is
    always allocated the full height of a horizontal @class{gtk-box} and the
    full width of a vertical @class{gtk-box}. This option affects the other
    dimension. The default value is @em{true}.}
  @argument[padding]{extra space in pixels to put between this @arg{child} and
    its neighbors, over and above the global amount specified by
    @slot[gtk-box]{spacing} property. If @arg{child} is a widget at one of the
    reference ends of @arg{box}, then padding pixels are also put between
    @arg{child} and the reference edge of @arg{box}. The default value is 0.}
  @begin{short}
    Adds @arg{child} to @arg{box}, packed with reference to the start of
    @arg{box}.
  @end{short}
  The child is packed after any other child packed with reference to the
  start of @arg{box}.
  @begin[Note]{dictionary}
    In the Lisp binding the arguments @arg{expand}, @arg{fill}, and
    @arg{padding} are keyword arguments, which have default values. The default
    value of the arguments @arg{expand} and @arg{fill} is @em{true}. The default
    value of the argument @arg{padding} is 0.
  @end{dictionary}
  @see-class{gtk-box}
  @see-function{gtk-box-pack-end}"
  (%gtk-box-pack-start box child expand fill padding))

(export 'gtk-box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_box_pack_end" %gtk-box-pack-end) :void
  (box (g-object gtk-box))
  (child (g-object gtk-widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun gtk-box-pack-end (box child &key (expand t) (fill t) (padding 0))
 #+cl-cffi-gtk-documentation
 "@version{2014-2-21}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} to be added to @arg{box}}
  @argument[expand]{@arg{true} if the new @arg{child} is to be given extra space
    allocated to @arg{box}. The extra space will be divided evenly between all
    children of @arg{box} that use this option.}
  @argument[fill]{@arg{true} if space given to @arg{child} by the @arg{expand}
    option is actually allocated to @arg{child}, rather than just padding it.
    This parameter has no effect if @arg{expand} is set to @code{nil}. A child
    is always allocated the full height of a horizontal @class{gtk-box} and the
    full width of a vertical @class{gtk-box} container. This option affects the
    other dimension.}
  @argument[padding]{extra space in pixels to put between this @arg{child} and
    its neighbors, over and above the global amount specified by the
    @slot[gtk-box]{spacing} property. If @arg{child} is a widget at one of the
    reference ends of @arg{box}, then padding pixels are also put between
    @arg{child} and the reference edge of @arg{box}.}
  @begin{short}
    Adds @arg{child} to @arg{box}, packed with reference to the end of
    @arg{box}.
  @end{short}
  The @arg{child} is packed after (away from end of) any other child packed with
  reference to the end of @arg{box}.
  @see-class{gtk-box}
  @see-function{gtk-box-pack-start}"
  (%gtk-box-pack-end box child expand fill padding))

(export 'gtk-box-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_box_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_box_reorder_child" gtk-box-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-22}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} to move}
  @argument[position]{the new position for @arg{child} in the list of children
    of @arg{box}, starting from 0. If negative, indicates the end of the
    list}
  @begin{short}
    Moves @arg{child} to a new position in the list of @arg{box} children.
  @end{short}
  The list is the children field of @class{gtk-box}, and contains both widgets
  packed @code{:start} as well as widgets packed @code{:end}, in the order that
  these widgets were added to @arg{box}.

  A widget's position in the box children list determines where the widget is
  packed into @arg{box}. A child widget at some position in the list will be
  packed just after all other widgets of the same packing type that appear
  earlier in the list.
  @see-class{gtk-box}"
  (box (g-object gtk-box))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-box-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_box_query_child_packing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-query-child-packing))

(defun gtk-box-query-child-packing (box child)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-22}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} of the child to query}
  @return{@code{expand} -- the @code{expand} child property @br{}
          @code{fill} -- the @code{fill} child property @br{}
          @code{padding} -- the @code{padding} child property @br{}
          @code{pack-type} -- the @code{pack-type} child property}
  @begin{short}
    Obtains information about how @arg{child} is packed into @arg{box}.
  @end{short}
  @see-class{gtk-box}
  @see-function{gtk-box-set-child-packing}"
  (values (gtk-box-child-expand box child)
          (gtk-box-child-fill box child)
          (gtk-box-child-padding box child)
          (gtk-box-child-pack-type box child)))

(export 'gtk-box-query-child-packing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_child_packing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-set-child-packing))

(defun gtk-box-set-child-packing (box child expand fill padding pack-type)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-22}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} of the child to set}
  @argument[expand]{the new value of the @code{expand} child property}
  @argument[fill]{the new value of the @code{fill} child property}
  @argument[padding]{the new value of the @code{padding} child property}
  @argument[pack-type]{the new value of the @code{pack-type} child property}
  @begin{short}
    Sets the way @arg{child} is packed into @arg{box}.
  @end{short}
  @see-class{gtk-box}
  @see-function{gtk-box-query-child-packing}"
  (setf (gtk-box-child-expand box child) expand
        (gtk-box-child-fill box child) fill
        (gtk-box-child-padding box child) padding
        (gtk-box-child-pack-type box child) pack-type))

(export 'gtk-box-set-child-packing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_center_widget ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_box_get_center_widget" gtk-box-get-center-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2016-1-19}
  @argument[box]{a @class{gtk-box} container}
  @return{The center @class{gtk-widget} widget.}
  @begin{short}
    Retrieves the center widget of the box.
  @end{short}

  Since 3.12
  @see-class{gtk-box}
  @see-function{gtk-box-set-center-widget}"
  (box (g-object gtk-box)))

#+gtk-3-12
(export 'gtk-box-get-center-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_center_widget ()
;;; ----------------------------------------------------------------------------

#+gtk-3-12
(defcfun ("gtk_box_set_center_widget" gtk-box-set-center-widget) :void
 #+cl-cffi-gtk-documentation
 "@version{2019-3-19}
  @argument[box]{a @class{gtk-box} container}
  @argument[widget]{a @class{gtk-widget} to center}
  @begin{short}
    Sets a center widget; that is a child widget that will be centered with
    respect to the full width of the box, even if the children at either side
    take up different amounts of space.
  @end{short}

  Since 3.12
  @see-class{gtk-box}
  @see-function{gtk-box-get-center-widget}"
  (box (g-object gtk-box))
  (widget (g-object gtk-widget)))

#+gtk-3-12
(export 'gtk-box-set-center-widget)

;;; ----------------------------------------------------------------------------
;;; GtkHBox
;;;
;;;     A horizontal container box
;;;
;;; Synopsis
;;;
;;;     GtkHBox
;;;
;;;     gtk_hbox_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHBox
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHBox" 'gtk-hbox)
  (setf *lisp-name-exceptions*
        (append '(("GtkHBox" GTK-HBOX)) *lisp-name-exceptions*)))

(define-g-object-class "GtkHBox" gtk-hbox
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hbox_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hbox 'type)
 "@version{2014-2-22}
  @begin{short}
    @sym{gtk-hbox} is a container that organizes child widgets into a single
    row.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, width, and alignment of @sym{gtk-hbox} children.

  All children are allocated the same height.
  @begin[Warning]{dictionary}
    @sym{gtk-hbox} has been deprecated. You can use @class{gtk-box} instead,
    which is a very quick and easy change. If you have derived your own classes
    from @sym{gtk-hbox}, you can simply change the inheritance to derive
    directly from @class{gtk-box}. No further changes are needed, since the
    default value of the @slot[gtk-orientable]{orientation} property is
    @code{:horizontal}. If you want your code to be future-proof, the
    recommendation is to switch to @class{gtk-grid}, since @class{gtk-box} is
    going to be deprecated in favor of the more flexible grid widget eventually.
    For more information about migrating to @class{gtk-grid}, see Migrating
    from other containers to @class{gtk-grid}.
  @end{dictionary}
  @see-class{gtk-box}
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-hbox-child-expand --------------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-expand 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{expand} of the @class{gtk-hbox} class.
  @see-class{gtk-box}
  @see-class{gtk-hbox}")

;;; --- gtk-hbox-child-fill ----------------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-fill 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{fill} of the @class{gtk-hbox} class.
  @see-class{gtk-box}
  @see-class{gtk-hbox}")

;;; --- gtk-hbox-child-padding -------------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-padding
                       "padding" "guint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-padding 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{padding} of the @class{gtk-hbox} class.
  @see-class{gtk-box}
  @see-class{gtk-hbox}")

;;; --- gtk-hbox-child-pack-type -----------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-pack-type 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{pack-type} of the @class{gtk-hbox} class.
  @see-class{gtk-box}
  @see-class{gtk-hbox}")

;;; --- gth-hbox-child-position ------------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-position 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{position} of the @class{gtk-hbox} class.
  @see-class{gtk-box}
  @see-class{gtk-hbox}")

;;; ----------------------------------------------------------------------------
;;; gtk_hbox_new ()
;;; ----------------------------------------------------------------------------

;; Because GtkHBox is deprecated, gtk-hbox-new creates an instance of GtkBox
;; with an orientation :horizontal.

(defun gtk-hbox-new (homogeneous spacing)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-22}
  @argument[homogeneous]{@arg{true} if all children are to be given equal space
    allotments}
  @argument[spacing]{the number of pixels to place by default between children}
  @return{A new @class{gtk-hbox} container.}
  @short{Creates a new @class{gtk-hbox} container.}
  @begin[Warning]{dictionary}
    The function @sym{gtk-hbox-new} has been deprecated since version 3.2 and
    should not be used in newly-written code. You can use the function
    @fun{gtk-box-new} with @code{:horizontal} instead, which is a quick and easy
    change. But the recommendation is to switch to @class{gtk-grid}, since
    @class{gtk-box} is going to go away eventually. See Migrating from other
    containers to.
  @end{dictionary}
  @see-class{gtk-box}
  @class{gtk-grid}.
  @see-function{gtk-box-new}"
  (make-instance 'gtk-box
                 :orientation :horizontal
                 :homogeneous homogeneous
                 :spacing spacing))

(export 'gtk-hbox-new)

;;; ----------------------------------------------------------------------------
;;; GtkVBox
;;;
;;; A vertical container box
;;;
;;; Synopsis
;;;
;;;     GtkVBox
;;;
;;;     gtk_vbox_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVBox
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVBox" 'gtk-vbox)
  (setf *lisp-name-exceptions*
        (append '(("GtkVBox" GTK-VBOX)) *lisp-name-exceptions*)))

(define-g-object-class "GtkVBox" gtk-vbox
  (:superclass gtk-box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_vbox_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vbox 'type)
 "@version{2014-2-22}
  @begin{short}
    A @sym{gtk-vbox} is a container that organizes child widgets into a single
    column.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, height, and alignment of @sym{gtk-vbox} children.

  All children are allocated the same width.
  @begin[Warning]{dictionary}
    @sym{gtk-vbox} has been deprecated. You can use @class{gtk-box} instead,
    which is a very quick and easy change. If you have derived your own classes
    from @sym{gtk-vbox}, you can simply change the inheritance to derive
    directly from @class{gtk-box}, and set the
    @slot[gtk-orientable]{orientation} property to @code{:vertical} in your
    instance init function, with a call like:
    @begin{pre}
 (setf (gtk-orientable-orientation object) :vertical)
    @end{pre}
    If you want your code to be future-proof, the recommendation is to switch
    to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in
    favor of the more flexible grid widget eventually. For more information
    about migrating to @class{gtk-grid}, see Migrating from other containers to
    @class{gtk-grid}.
  @end{dictionary}
  @see-class{gtk-box}
  @see-class{gtk-grid}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk-vbox-child-expand --------------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-expand
                       "expand" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-expand 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{expand} of the @class{gtk-vbox} class.
  @see-class{gtk-box}
  @see-class{gtk-vbox}")

;;; --- gtk-vbox-child-fill ----------------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-fill
                       "fill" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-fill 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{fill} of the @class{gtk-vbox} class.
  @see-class{gtk-box}
  @see-class{gtk-vbox}")

;;; --- gtk-vbox-child-padding -------------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-padding
                       "padding" "guint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-padding 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{padding} of the @class{gtk-vbox} class.
  @see-class{gtk-box}
  @see-class{gtk-vbox}")

;;; --- gtk-vbox-child-pack-type -----------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-pack-type 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{pack-type} of the @class{gtk-vbox} class.
  @see-class{gtk-box}
  @see-class{gtk-vbox}")

;;; --- gtk-vbox-child-position ------------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-position 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{position} of the @class{gtk-vbox} class.
  @see-class{gtk-box}
  @see-class{gtk-vbox}")

;;; ----------------------------------------------------------------------------
;;; gtk_vbox_new ()
;;; ----------------------------------------------------------------------------

;; Because GtkVBox is deprecated, gtk-vbox-new creates an instance of GtkBox
;; with an orientation :vertical.

(declaim (inline gtk-vbox-new))

(defun gtk-vbox-new (homogeneous spacing)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-22}
  @begin{short}
    A @sym{gtk-vbox} is a container that organizes child widgets into a single
    column.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, height, and alignment of @sym{gtk-vbox} children.

  All children are allocated the same width.
  @begin[Warning]{dictionary}
    @sym{gtk-vbox} has been deprecated. You can use @class{gtk-box} instead,
    which is a very quick and easy change. If you have derived your own classes
    from @sym{gtk-vbox}, you can simply change the inheritance to derive
    directly from @class{gtk-box}, and set the
    @slot[gtk-orientable]{orientation} property to @code{:vertical} in your
    instance init function, with a call like:
    @begin{pre}
 (setf (gtk-orientable-orientation object) :vertical)
    @end{pre}
    If you want your code to be future-proof, the recommendation is to switch
    to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in
    favor of the more flexible grid widget eventually. For more information
    about migrating to @class{gtk-grid}, see Migrating from other containers to
    @class{gtk-grid}.
  @end{dictionary}
  @see-class{gtk-box}
  @see-class{gtk-grid}"
  (make-instance 'gtk-box
                 :orientation :vertical
                 :homogeneous homogeneous
                 :spacing spacing))

(export 'gtk-vbox-new)

;;; --- End of file gtk.box.lisp -----------------------------------------------

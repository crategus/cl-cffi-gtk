;;; ----------------------------------------------------------------------------
;;; gtk.box.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; GtkBox
;;;
;;; A container box
;;;
;;; Synopsis
;;;
;;;     GtkBox
;;;
;;;     gtk_box_new
;;;     gtk_box_pack_start
;;;     gtk_box_pack_end
;;;     gtk_box_get_homogeneous
;;;     gtk_box_set_homogeneous
;;;     gtk_box_get_spacing
;;;     gtk_box_set_spacing
;;;     gtk_box_reorder_child
;;;     gtk_box_query_child_packing
;;;     gtk_box_set_child_packing
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
  ((homogeneous
    gtk-box-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    gtk-box-spacing
    "spacing" "gint" t t)))

;;; --- gtk-box ----------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-box 'type)
 "@version{2013-5-18}
  @begin{short}
    The @sym{gtk-box} widget organizes child widgets into a rectangular area.
  @end{short}

  The rectangular area of a @sym{gtk-box} is organized into either a single row
  or a single column of child widgets depending upon the orientation. Thus, all
  children of a @sym{gtk-box} are allocated one dimension in common, which is
  the height of a row, or the width of a column.

  @sym{gtk-box} uses a notion of packing. Packing refers to adding widgets with
  reference to a particular position in a @class{gtk-container}. For a
  @sym{gtk-box}, there are two reference positions: the start and the end of the
  box. For a vertical @sym{gtk-box}, the start is defined as the top of the box
  and the end is defined as the bottom. For a horizontal @sym{gtk-box} the start
  is defined as the left side and the end is defined as the right side.

  Use repeated calls to @fun{gtk-box-pack-start} to pack widgets into a
  @sym{gtk-box} from start to end. Use @fun{gtk-box-pack-end} to add widgets
  from end to start. You may intersperse these calls and add widgets from both
  ends of the same @sym{gtk-box}.

  Because @sym{gtk-box} is a @class{gtk-container}, you may also use
  @fun{gtk-container-add} to insert widgets into the box, and they will be
  packed with the default values for @arg{\"expand\"} and @arg{\"fill\"}. Use
  @fun{gtk-container-remove} to remove widgets from the @sym{gtk-box}.

  Use @fun{gtk-box-set-homogeneous} to specify whether or not all children of
  the @sym{gtk-box} are forced to get the same amount of space.

  Use @fun{gtk-box-set-spacing} to determine how much space will be minimally
  placed between all children in the @sym{gtk-box}. Note that spacing is added
  between the children, while padding added by @fun{gtk-box-pack-start} or
  @fun{gtk-box-pack-end} is added on either side of the widget it belongs to.

  Use @fun{gtk-box-reorder-child} to move a @sym{gtk-box} child to a different
  place in the box.

  Use @fun{gtk-box-set-child-packing} to reset the @arg{\"expand\"}, 
  @arg{\"fill\"} and @arg{\"padding\"} child properties. Use
  @fun{gtk-box-query-child-packing} to query these fields.
  @subheading{Note]}
    Note that a single-row or single-column @class{gtk-grid} provides exactly
    the same functionality as @sym{gtk-box}.
  @begin[Child Property Details]{dictionary}
    @subheading{The \"expand\" child property}
      @code{\"expand\"} of type @code{:boolean} (Read / Write)@br{}
      Whether the child should receive extra space when the parent grows.
      Note that the default value for this property is @code{nil} for
      @sym{gtk-box}, but @class{gtk-hbox}, @class{gtk-vbox} and other subclasses
      use the old default of @arg{true}.
      Note that the @code{\"halign\"}, @code{\"valign\"}, @code{\"hexpand\"} and
      @code{\"vexpand\"} properties are the preferred way to influence child size
      allocation in containers.@br{}
      Default value: @code{nil}

    @subheading{The \"fill\" child property}
      @code{\"fill\"} of type @code{gboolean} (Read / Write)@br{}
      Whether the child should receive extra space when the parent grows.
      Note that the @code{\"halign\"}, @code{\"valign\"}, @code{\"hexpand\"} and
      @code{\"vexpand\"} properties are the preferred way to influence child
      size allocation in containers.@br{}
      Default value: @em{true}
  
    @subheading{The \"pack-type\" child property}
      @code{\"pack-type\"} of type @symbol{gtk-pack-type} (Read / Write)@br{}
      A @symbol{gtk-pack-type} indicating whether the child is packed with
      reference to the start or end of the parent.@br{}
      Default value: @code{:start}

    @subheading{The \"padding\" child property}
      @code{\"padding\"} of type @code{:uint} (Read / Write)@br{}
      Extra space to put between the child and its neighbors, in pixels.@br{}
      Allowed values: <= @code{G_MAXINT}@br{}
      Default value: 0

    @subheading{The \"position\" child property}
      @code{\"position\"} of type @code{:int} (Read / Write)@br{}
      The index of the child in the parent.@br{}
      Allowed values: >= @code{G_MAXULONG}@br{}
      Default value: 0
  @end{dictionary}
  @see-slot{gtk-box-homogeneous}
  @see-slot{gtk-box-spacing}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous" 'gtk-box) 't)
 "The @code{\"homogeneous\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether the children should all be the same size.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "spacing" 'gtk-box) 't)
 "The @code{\"spacing\"} property of type @code{:int} (Read / Write)@br{}
  The amount of space between children.@br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-homogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-homogeneous 'function)
 "@version{2013-1-20}
  @begin{short}
    Accessor of the slot @code{\"homogeneous\"} of the @class{gtk-box} class.
  @end{short}
  @see-function{gtk-box-get-homogeneous}
  @see-function{gtk-box-set-homogeneous}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-spacing atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-spacing 'function)
 "@version{2013-1-20}
  @begin{short}
    Accessor of the slot @code{\"spacing\"} of the @class{gtk-box} class.
  @end{short}
  @see-function{gtk-box-get-spacing}
  @see-function{gtk-box-set-spacing}")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkBox"
                       gtk-box-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkBox"
                       gtk-box-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkBox"
                       gtk-box-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkBox"
                       gtk-box-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkBox"
                       gtk-box-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-expand 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"expand\"} of the @class{gtk-box}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-fill 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"fill\"} of the @class{gtk-box}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-padding 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"padding\"} of the @class{gtk-box}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-pack-type 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"pack-type\"} of the @class{gtk-box}
  class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-box-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-box-child-position 'function)
 "@version{2013-2-13}
  Accessor of the child property @code{\"position\"} of the @class{gtk-box}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-new))

(defun gtk-box-new (orientation spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-24}
  @argument[orientation]{the box's orientation}
  @argument[spacing]{the number of pixels to place by default between children}
  @return{A new @class{gtk-box} container.}
  @short{Creates a new @class{gtk-box} container.}

  Since 3.0
  @see-class{gtk-box}"
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
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} to be added to @arg{box}}
  @argument[expand]{@arg{true} if the new @arg{child} is to be given extra space
    allocated to @arg{box}. The extra space will be divided evenly between all
    children that use this option.}
  @argument[fill]{@arg{true} if space given to @arg{child} by the expand option
    is actually allocated to @arg{child}, rather than just padding it. This
    parameter has no effect if @arg{expand} is set to @code{nil}. A @arg{child}
    is always allocated the full height of a horizontal @class{gtk-box} and the
    full width of a vertical @class{gtk-box}. This option affects the other
    dimension.}
  @argument[padding]{extra space in pixels to put between this @arg{child} and
    its neighbors, over and above the global amount specified by
    @arg{\"spacing\"} property. If @arg{child} is a widget at one of the
    reference ends of @arg{box}, then padding pixels are also put between
    @arg{child} and the reference edge of @arg{box}.}
  @begin{short}
    Adds @arg{child} to @arg{box}, packed with reference to the start of
    @arg{box}.
  @end{short}
  The @arg{child} is packed after any other child packed with reference to the
  start of @arg{box}.
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
 "@version{2013-5-18}
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
    its neighbors, over and above the global amount specified by
    @arg{\"spacing\"} property. If @arg{child} is a widget at one of the
    reference ends of @arg{box}, then padding pixels are also put between
    @arg{child} and the reference edge of @arg{box}.}
  @begin{short}
    Adds @arg{child} to @arg{box}, packed with reference to the end of
    @arg{box}.
  @end{short}
  The @arg{child} is packed after (away from end of) any other child packed with
  reference to the end of @arg{box}.
  @see-function{gtk-box-pack-start}"
  (%gtk-box-pack-end box child expand fill padding))

(export 'gtk-box-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-get-homogeneous))

(defun gtk-box-get-homogeneous (box)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @return{@arg{true} if the @arg{box} is homogeneous}
  @begin{short}
    Returns whether the @arg{box} is homogeneous (all children are the same
    size).
  @end{short}
  See the function @fun{gtk-box-set-homogeneous}.
  @see-function{gtk-box-set-homogeneous}"
  (gtk-box-homogeneous box))

(export 'gtk-box-get-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_homogeneous ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-set-homogeneous))

(defun gtk-box-set-homogeneous (box homogeneous)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @argument[homogeneous]{a boolean value, @arg{true} to create equal allotments,
    @code{nil} for variable allotments}
  @begin{short}
    Sets the @code{\"homogeneous\"} property of @arg{box}, controlling whether
    or not all children of @arg{box} are given equal space in the @arg{box}.
  @end{short}
  @see-function{gtk-box-get-homogeneous}"
  (setf (gtk-box-homogeneous box) homogeneous))

(export 'gtk-box-set-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-get-spacing))

(defun gtk-box-get-spacing (box)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @return{spacing between children}
  @short{Gets the value set by @fun{gtk-box-set-spacing}.}
  @see-function{gtk-box-set-spacing}"
  (gtk-box-spacing box))

(export 'gtk-box-get-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_spacing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-set-spacing))

(defun gtk-box-set-spacing (box spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @argument[spacing]{the number of pixels to put between children}
  @begin{short}
    Sets the @arg{\"spacing\"} property of @arg{box}, which is the number of
    pixels to place between children of @arg{box}.
  @end{short}
  @see-function{gtk-box-get-spacing}"
  (setf (gtk-box-spacing box) spacing))

(export 'gtk-box-set-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_reorder_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_box_reorder_child" gtk-box-reorder-child) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
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

  A widget's position in the @arg{box} children list determines where the widget
  is packed into @arg{box}. A child widget at some position in the list will be
  packed just after all other widgets of the same packing type that appear
  earlier in the list."
  (box g-object)
  (child g-object)
  (position :int))

(export 'gtk-box-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_box_query_child_packing ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-box-query-child-packing))

(defun gtk-box-query-child-packing (box child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} of the @arg{child} to query}
  @return{@code{expand} -- @code{\"expand\"} child property@br{}
          @code{fill} -- @code{\"fill\"} child property@br{}
          @code{padding} -- @code{\"padding\"} child property@br{}
          @code{pack-type} -- @code{\"pack-type\"} child property}
  @begin{short}
    Obtains information about how @arg{child} is packed into @arg{box}.
  @end{short}
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
 "@version{2013-5-18}
  @argument[box]{a @class{gtk-box} container}
  @argument[child]{the @class{gtk-widget} of the @arg{child} to set}
  @argument[expand]{the new value of the @code{\"expand\"} child property}
  @argument[fill]{the new value of the @code{\"fill\"} child property}
  @argument[padding]{the new value of the @code{\"padding\"} child property}
  @argument[pack-type]{the new value of the @code{\"pack-type\"} child property}
  @begin{short}
    Sets the way @arg{child} is packed into @arg{box}.
  @end{short}
  @see-function{gtk-box-query-child-packing}"
  (setf (gtk-box-child-expand box child) expand
        (gtk-box-child-fill box child) fill
        (gtk-box-child-padding box child) padding
        (gtk-box-child-pack-type box child) pack-type))

(export 'gtk-box-set-child-packing)

;;; ----------------------------------------------------------------------------
;;; GtkHBox
;;;
;;; A horizontal container box
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hbox 'type)
 "@version{2013-5-18}
  @begin{short}
    @sym{gtk-hbox} is a container that organizes child widgets into a single
    row.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, width, and alignment of @sym{gtk-hbox} children.

  All children are allocated the same height.

  @sym{gtk-hbox} has been deprecated. You can use @class{gtk-box} instead, which
  is a very quick and easy change. If you have derived your own classes from
  @sym{gtk-hbox}, you can simply change the inheritance to derive directly from
  @class{gtk-box}. No further changes are needed, since the default value of the
  @code{\"orientation\"} property is @code{:horizontal}. If you want your code
  to be future-proof, the recommendation is to switch to @class{gtk-grid}, since
  @class{gtk-box} is going to be deprecated in favor of the more flexible grid
  widget eventually. For more information about migrating to @class{gtk-grid},
  see Migrating from other containers to @class{gtk-grid}.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkHBox"
                       gtk-hbox-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkHBox"
                       gtk-hbox-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkHBox"
                       gtk-hbox-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkHBox"
                       gtk-hbox-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkHBox"
                       gtk-hbox-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-expand 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the @class{gtk-hbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-fill 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the @class{gtk-hbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-padding 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the @class{gtk-hbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-pack-type 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the @class{gtk-hbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hbox-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hbox-child-position 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the @class{gtk-hbox}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_hbox_new ()
;;; ----------------------------------------------------------------------------

;; Because GtkHBox is deprecated, gtk-hbox-new creates an instance of GtkBox
;; with an orientation :horizontal.

(defun gtk-hbox-new (homogeneous spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[homogeneous]{@arg{true} if all children are to be given equal space
    allotments}
  @argument[spacing]{the number of pixels to place by default between children}
  @return{A new @class{gtk-hbox} container.}
  @short{Creates a new @class{gtk-hbox} container.}

  @subheading{Warning}
    The function @sym{gtk-hbox-new} has been deprecated since version 3.2 and
    should not be used in newly-written code. You can use the function
    @fun{gtk-box-new} with @code{:horizontal} instead, which is a quick and easy
    change. But the recommendation is to switch to @class{gtk-grid}, since
    @class{gtk-box} is going to go away eventually. See Migrating from other
    containers to.
  @class{gtk-grid}.
  @see-function{gtk-box-new}
  @see-class{gtk-grid}
  @see-class{gtk-box}"
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

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vbox 'type)
 "@version{2013-5-18}
  @begin{short}
    A @sym{gtk-vbox} is a container that organizes child widgets into a single
    column.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, height, and alignment of @sym{gtk-vbox} children.

  All children are allocated the same width.

  @sym{gtk-vbox} has been deprecated. You can use @class{gtk-box} instead, which
  is a very quick and easy change. If you have derived your own classes from
  @sym{gtk-vbox}, you can simply change the inheritance to derive directly from
  @class{gtk-box}, and set the @code{\"orientation\"} property to
  @code{:vertical} in your instance init function, with a call like:
  @begin{pre}
 (@fun{gtk-orientable-set-orientation} object :vertical)
  @end{pre}
  If you want your code to be future-proof, the recommendation is to switch
  to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in favor
  of the more flexible grid widget eventually. For more information about
  migrating to @class{gtk-grid}, see Migrating from other containers to
  @class{gtk-grid}.")

;;; ----------------------------------------------------------------------------

(define-child-property "GtkVBox"
                       gtk-vbox-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkVBox"
                       gtk-vbox-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkVBox"
                       gtk-vbox-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkVBox"
                       gtk-vbox-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkVBox"
                       gtk-vbox-child-position
                       "position" "gint" t t t)

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-expand atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-expand 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"expand\"} of the @class{gtk-vbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-fill atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-fill 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"fill\"} of the @class{gtk-vbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-padding atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-padding 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"padding\"} of the @class{gtk-vbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-pack-type 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"pack-type\"} of the @class{gtk-vbox}
    class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vbox-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vbox-child-position 'function)
 "@version{2013-2-13}
  @begin{short}
    Accessor of the child property @code{\"position\"} of the @class{gtk-vbox}
    class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_vbox_new ()
;;; ----------------------------------------------------------------------------

;; Because GtkVBox is deprecated, gtk-vbox-new creates an instance of GtkBox
;; with an orientation :vertical.

(declaim (inline gtk-vbox-new))

(defun gtk-vbox-new (homogeneous spacing)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @begin{short}
    A @sym{gtk-vbox} is a container that organizes child widgets into a single
    column.
  @end{short}

  Use the @class{gtk-box} packing interface to determine the arrangement,
  spacing, height, and alignment of @sym{gtk-vbox} children.

  All children are allocated the same width.

  @sym{gtk-vbox} has been deprecated. You can use @class{gtk-box} instead, which
  is a very quick and easy change. If you have derived your own classes from
  @sym{gtk-vbox}, you can simply change the inheritance to derive directly from
  @class{gtk-box}, and set the @code{\"orientation\"} property to
  @code{:vertical} in your instance init function, with a call like:
  @begin{pre}
 (@fun{gtk-orientable-set-orientation} object :vertical)
  @end{pre}
  If you want your code to be future-proof, the recommendation is to switch
  to @class{gtk-grid}, since @class{gtk-box} is going to be deprecated in favor
  of the more flexible grid widget eventually. For more information about
  migrating to @class{gtk-grid}, see Migrating from other containers to
  @class{gtk-grid}."
  (make-instance 'gtk-box
                 :orientation :vertical
                 :homogeneous homogeneous
                 :spacing spacing))

(export 'gtk-vbox-new)

;;; --- End of file gtk.box.lisp -----------------------------------------------

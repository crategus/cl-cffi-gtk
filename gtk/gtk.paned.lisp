;;; ----------------------------------------------------------------------------
;;; gtk.paned.lisp
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
;;; GtkPaned
;;;
;;;     A widget with two adjustable panes
;;;
;;; Types and Values
;;;
;;;     GtkPaned
;;;
;;; Functions
;;;
;;;     gtk_paned_new
;;;     gtk_paned_add1
;;;     gtk_paned_add2
;;;     gtk_paned_pack1
;;;     gtk_paned_pack2
;;;     gtk_paned_get_child1
;;;     gtk_paned_get_child2
;;;     gtk_paned_set_position
;;;     gtk_paned_get_position
;;;     gtk_paned_get_handle_window
;;;     gtk_paned_set_wide_handle
;;;     gtk_paned_get_wide_handle
;;;
;;; Properties
;;;
;;;         gint  max-position         Read
;;;         gint  min-position         Read
;;;         gint  position             Read / Write
;;;     gboolean  position-set         Read / Write
;;;     gboolean  wide-handle          Read / Write
;;;
;;; Child Properties
;;;
;;;     gboolean  resize	           Read / Write
;;;     gboolean  shrink	           Read / Write
;;;
;;; Style Properties
;;;
;;;         gint  handle-size          Read
;;;
;;; Signals
;;;
;;;     gboolean  accept-position      Action
;;;     gboolean  cancel-position      Action
;;;     gboolean  cycle-child-focus    Action
;;;     gboolean  cycle-handle-focus   Action
;;;     gboolean  move-handle          Action
;;;     gboolean  toggle-handle-focus  Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkPaned
;;;                     ├── GtkHPaned
;;;                     ╰── GtkVPaned
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPaned implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPaned
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkPaned" 'gtk-paned))

(define-g-object-class "GtkPaned" gtk-paned
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_paned_get_type")
  ((max-position
    gtk-paned-max-position
    "max-position" "gint" t nil)
   (min-position
    gtk-paned-min-position
    "min-position" "gint" t nil)
   (position
    gtk-paned-position
    "position" "gint" t t)
   (position-set
    gtk-paned-position-set
    "position-set" "gboolean" t t)
   (wide-handle
    gtk-pandes-wide-handle
    "wide-handle" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-paned 'type)
 "@version{2013-5-18}
  @begin{short}
    @sym{gtk-paned} has two panes, arranged either horizontally or vertically.
    The division between the two panes is adjustable by the user by dragging a
    handle.
  @end{short}

  Child widgets are added to the panes of the widget with the functions
  @fun{gtk-paned-pack1} and @fun{gtk-paned-pack2}. The division between the two
  children is set by default from the size requests of the children, but it can
  be adjusted by the user.

  A paned widget draws a separator between the two child widgets and a small
  handle that the user can drag to adjust the division. It does not draw any
  relief around the children or around the separator. (The space in which the
  separator is called the gutter.) Often, it is useful to put each child
  inside a @class{gtk-frame} with the shadow type set to @code{:in} so that the
  gutter appears as a ridge. No separator is drawn if one of the children is
  missing.

  Each child has two options that can be set, @code{\"resize\"} and
  @code{\"shrink\"}. If resize is @em{true}, then when the @sym{gtk-paned} is
  resized, that child will expand or shrink along with the paned widget. If
  @code{\"shrink\"} is @em{true}, then that child can be made smaller than its
  requisition by the user. Setting shrink to @code{nil} allows the application
  to set a minimum size. If @code{\"resize\"} is @code{nil} for both children,
  then this is treated as if @code{\"resize\"} is @em{true} for both children.

  The application can set the position of the slider as if it were set by the
  user, by calling the generic function @fun{gtk-paned-position}.

  @begin[Example]{dictionary}
    Creating a paned widget with minimum sizes.
    @begin{pre}
  (let ((paned (make-instance 'gtk-paned
                              :orientation :horizontal
                              :width-request 250
                              :height-request 150))
        (frame1 (make-instance 'gtk-frame :shadow-type :in
                                          :width-request 100))
        (frame2 (make-instance 'gtk-frame :shadow-type :in
                                          :width-request 50)))
      (gtk-paned-pack1 paned frame1 :resize t :shrink nil)
      (gtk-paned-pack2 paned frame2 :resize nil :shrink nil)
      ... )
    @end{pre}
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @subheading{The @code{\"resize\"} child property}
      @code{\"resize\"} of type @code{:boolean} (Read / Write) @br{}
      The @code{\"resize\"} child property determines whether the child expands
      and shrinks along with the paned widget. @br{}
      Default value: @em{true} @br{}
      Since 2.4

    @subheading{The @code{\"shrink\"} child property}
      @code{\"shrink\"} of type @code{:boolean} (Read / Write) @br{}
      The @code{\"shrink\"} child property determines whether the child can be
      made smaller than its requisition. @br{}
      Default value: @em{true} @br{}
      Since 2.4
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"handle-size\" style property}
      @code{\"handle-size\"} of type @code{:int} (Read) @br{}
      Width of handle. @br{}
      Allowed values: >= 0 @br{}
      Default value: 5
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"accept-position\" signal}
      @begin{pre}
 lambda (widget)   : Action
      @end{pre}
      The \"accept-position\" signal is a keybinding signal which gets emitted
      to accept the current position of the handle when moving it using key
      bindings.
      The default binding for this signal is Return or Space.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
      @end{table}
      Since 2.0

    @subheading{The \"cancel-position\" signal}
      @begin{pre}
 lambda (widget)   : Action
      @end{pre}
      The \"cancel-position\" signal is a keybinding signal which gets emitted
      to cancel moving the position of the handle using key bindings. The
      position of the handle will be reset to the value prior to moving it.
      The default binding for this signal is Escape.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
      @end{table}
      Since 2.0

    @subheading{The \"cycle-child-focus\" signal}
      @begin{pre}
 lambda (widget reversed)   : Action
      @end{pre}
      The \"cycle-child-focus\" signal is a keybinding signal which gets emitted
      to cycle the focus between the children of the paned.
      The default binding is f6.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
        @entry[reversed]{Whether cycling backward or forward.}
      @end{table}
      Since 2.0

    @subheading{The \"cycle-handle-focus\" signal}
      @begin{pre}
 lambda (widget reversed)   : Action
      @end{pre}
      The \"cycle-handle-focus\" signal is a keybinding signal which gets
      emitted to cycle whether the paned should grab focus to allow the user
      to change position of the handle by using key bindings.
      The default binding for this signal is f8.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
        @entry[reversed]{Whether cycling backward or forward.}
      @end{table}
      Since 2.0

    @subheading{The \"move-handle\" signal}
      @begin{pre}
 lambda (widget scroll-type)   : Action
      @end{pre}
      The \"move-handle\" signal is a keybinding signal which gets emitted to
      move the handle when the user is using key bindings to move it.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
        @entry[scroll-type]{A @symbol{gtk-scroll-type}.}
      @end{table}
      Since 2.0

    @subheading{The \"toggle-handle-focus\" signal}
      @begin{pre}
 lambda (widget)   : Action
      @end{pre}
      The \"toggle-handle-focus\" is a keybinding signal which gets emitted to
      accept the current position of the handle and then move focus to the next
      widget in the focus chain.
      The default binding is Tab.
      @begin[code]{table}
        @entry[widget]{The object that received the signal.}
      @end{table}
      Since 2.0
  @end{dictionary}
  @see-slot{gtk-paned-max-position}
  @see-slot{gtk-paned-min-position}
  @see-slot{gtk-paned-position}
  @see-slot{gtk-paned-position-set}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-paned-max-position -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "max-position" 'gtk-paned) 't)
 "The @code{\"max-position\"} property of type @code{:int} (Read)@br{}
  The largest possible value for the position property. This property is
  derived from the size and shrinkability of the widget's children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 2147483647 @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-max-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-max-position 'function)
 "@version{2014-7-27}
  Accessor of the slot @slot[gtk-paned]{max-position} of the @class{gtk-paned}
  class.")

;;; --- gtk-paned-min-position -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "min-position" 'gtk-paned) 't)
 "The @code{\"min-position\"} property of type @code{:int} (Read)@br{}
  The smallest possible value for the position property. This property is
  derived from the size and shrinkability of the widget's children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0 @br{}
  Since 2.4")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-min-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-min-position 'function)
 "@version{2014-7-27}
  Accessor of the slot @slot[gtk-panded]{min-position} of the @class{gtk-paned}
  class.")

;;; --- gtk-paned-position -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "position" 'gtk-paned) 't)
 "The @code{\"position\"} property of type @code{:int} (Read / Write)@br{}
  Position of paned separator in pixels (0 means all the way to the
  left/top). @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-position 'function)
 "@version{2014-7-27}
  @argument[object]{a @class{gtk-paned} container}
  @argument[position]{pixel position of divider, a negative value means that
    the position is unset}
  @syntax[]{(gtk-paned-position object) => position}
  @syntax[]{(setf (gtk-panded-position position) position)}
  @begin{short}
    Accessor of the slot @slot[gtk-paned]{position} of the @class{gtk-paned}
    class.
  @end{short}

  The generic function @sym{gtk-paned} obtains the position of the divider
  between the two panes.

  The generic function @sym{(setf gtk-paned-position)} sets the position of the
  divider between the two panes.
  @see-class{gtk-paned}")

;;; --- gtk-paned-position-set -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "position-set" 'gtk-paned) 't)
 "The @code{\"position-set\"} property of type @code{:boolean}
  (Read / Write)@br{}
  @em{True} if the @code{\"position\"} property should be used. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-position-set atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-position-set 'function)
 "@version{2014-7-27}
  Accessor of the slot @slot[gtk-paned]{position-set} of the @class{gtk-paned}
  class.")
  
;;; --- gtk-panded-wide-handled ------------------------------------------------





   

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

(define-child-property "GtkPaned"
                       gtk-paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkPaned"
                       gtk-paned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-child-resize 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"resize\"} of the @class{gtk-paned}
  class.
  @see-class{gtk-paned}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-paned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-paned-child-shrink 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"shrink\"} of the @class{gtk-paned}
  class.
  @see-class{gtk-paned}")

;;; ----------------------------------------------------------------------------
;;; gtk_paned_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-paned-new))

(defun gtk-paned-new (orientation)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[orientation]{the paned's orientation.}
  @return{A new @class{gtk-paned} containter.}
  @short{Creates a new @class{gtk-paned} container.}

  Since 3.0"
  (make-instance 'gtk-paned
                 :orientation orientation))

(export 'gtk-paned-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add1 ()
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add1 (paned child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a paned container}
  @argument[child]{the child to add}
  Adds a child to the top or left pane with default parameters. This is
  equivalent to @code{(gtk-paned-pack1 paned child nil t)}."
  (gtk-paned-pack1 paned child :resize nil :shrink t))

(export 'gtk-paned-add1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add2 ()
;;; ----------------------------------------------------------------------------

(defun gtk-paned-add2 (paned child)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a paned container}
  @argument[child]{the child to add}
  Adds a child to the bottom or right pane with default parameters. This is
  equivalent to @code{(gtk-paned-pack2 paned child t t)}."
  (gtk-paned-pack2 paned child :resize t :shrink t))

(export 'gtk-paned-add2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack1 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack1" %gtk-paned-pack1) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack1 (paned child &key (resize nil) (shrink t))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a paned container}
  @argument[child]{the child to add}
  @argument[resize]{should this child expand when the paned widget is resized}
  @argument[shrink]{can this child be made smaller than its requisition}
  Adds a child to the top or left pane."
  (%gtk-paned-pack1 paned child resize shrink))

(export 'gtk-paned-pack1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack2 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_pack2" %gtk-paned-pack2) :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun gtk-paned-pack2 (paned child &key (resize t) (shrink t))
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a paned container}
  @argument[child]{the child to add}
  @argument[resize]{should this child expand when the paned widget is resized}
  @argument[shrink]{can this child be made smaller than its requisition}
  Adds a child to the bottom or right pane."
  (%gtk-paned-pack2 paned child resize shrink))

(export 'gtk-paned-pack2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child1 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child1" gtk-paned-get-child1) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a @class{gtk-paned} container}
  @return{First child, or @code{nil} if it is not set.}
  @short{Obtains the first child of the paned container.}

  Since 2.4"
  (paned g-object))

(export 'gtk-paned-get-child1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child2 ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_child2" gtk-paned-get-child2) g-object
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a @class{gtk-paned} container}
  @return{Second child, or @code{nil} if it is not set.}
  @short{Obtains the second child of the paned container.}

  Since 2.4"
  (paned g-object))

(export 'gtk-paned-get-child2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_handle_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paned_get_handle_window" gtk-paned-get-handle-window)
    (g-object gdk-window)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @argument[paned]{a @class{gtk-paned} container}
  @return{The paned's handle window.}
  @begin{short}
    Returns the @class{gdk-window} object of the handle. This function is useful
    when handling button or motion events because it enables the callback to
    distinguish between the window of the paned, a child and the handle.
  @end{short}

  Since 2.20"
  (paned (g-object gtk-paned)))

(export 'gtk-paned-get-handle-window)

;;; ----------------------------------------------------------------------------
;;; GtkHPaned
;;;
;;; A container with two panes arranged horizontally
;;;
;;; Synopsis
;;;
;;;     GtkHPaned
;;;
;;;     gtk_hpaned_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkHPaned
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHPaned" 'gtk-hpaned))

(define-g-object-class "GtkHPaned" gtk-hpaned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_hpaned_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-hpaned 'type)
 "@version{2013-5-18}
  @begin{short}
    The @sym{gtk-hpaned} widget is a container widget with two children arranged
    horizontally. The division between the two panes is adjustable by the user
    by dragging a handle. See @class{gtk-paned} for details.
  @end{short}

  @sym{gtk-hpaned} has been deprecated, use @class{gtk-paned} instead.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkHPaned"
                       gtk-hpaned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkHPaned"
                       gtk-hpaned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hpaned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hpaned-child-resize 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"resize\"} of the @class{gtk-hpaned}
  class.
  @see-class{gtk-paned}
  @see-class{gtk-hpaned}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-hpaned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-hpaned-child-shrink 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"shrink\"} of the @class{gtk-hpaned}
  class.
  @see-class{gtk-paned}
  @see-class{gtk-hpaned}")

;;; ----------------------------------------------------------------------------
;;; gtk_hpaned_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-hpaned-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @return{The new @class{gtk-hpaned} container.}
  @subheading{Warning}
    @sym{gtk-hpaned-new} has been deprecated since version 3.2 and should not be
    used in newly-written code. Use @fun{gtk-paned-new} with @code{:horizontal}
    instead.

  @short{Create a new @class{gtk-hpaned} container.}"
  (make-instance 'gtk-paned
                 :orientation :horizontal))

(export 'gtk-hpaned-new)

;;; ----------------------------------------------------------------------------
;;; GtkVPaned
;;;
;;; A container with two panes arranged vertically
;;;
;;; Synopsis
;;;
;;;     GtkVPaned
;;;
;;;     gtk_vpaned_new
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkVPaned
;;; ----------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkVPaned" 'gtk-vpaned))

(define-g-object-class "GtkVPaned" gtk-vpaned
  (:superclass gtk-paned
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_vpaned_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-vpaned 'type)
 "@version{2013-5-18}
  @begin{short}
    The @sym{gtk-vpaned} container is a container widget with two children
    arranged vertically. The division between the two panes is adjustable by the
    user by dragging a handle. See @class{gtk-paned} for details.
  @end{short}

  @sym{gtk-vpaned} has been deprecated, use @class{gtk-paned} instead.")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Child Properties
;;;
;;; ----------------------------------------------------------------------------

(define-child-property "GtkVPaned"
                       gtk-vpaned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkVPaned"
                       gtk-vpaned-child-shrink "shrink" "gboolean" t t t)

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vpaned-child-resize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vpaned-child-resize 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"resize\"} of the @class{gtk-vpaned}
  class.
  @see-class{gtk-paned}
  @see-class{gtk-vpaned}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-vpaned-child-shrink atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-vpaned-child-shrink 'function)
 "@version{2013-8-28}
  Accessor of the child property @code{\"shrink\"} of the @class{gtk-vpaned}
  class.
  @see-class{gtk-paned}
  @see-class{gtk-vpaned}")

;;; ----------------------------------------------------------------------------
;;; gtk_vpaned_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-vpaned-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-5-18}
  @return{The new @class{gtk-vpaned} container.}
  @subheading{Warning}
    @sym{gtk-vpaned-new} has been deprecated since version 3.2 and should not be
    used in newly-written code. Use the function @fun{gtk-paned-new} with
    @code{:vertival} instead.

  @short{Create a new @class{gtk-vpaned} container.}"
  (make-instance 'gtk-paned
                 :orientation :vertical))

(export 'gtk-vpaned-new)

;;; --- End of file gtk.paned.lisp ---------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk.stack.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2021 Dieter Kaiser
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
;;; GtkStack
;;;
;;;     A stacking container
;;;
;;; Types and Values
;;;
;;;     GtkStack
;;;     GtkStackTransitionType
;;;
;;; Functions
;;;
;;;     gtk_stack_new
;;;     gtk_stack_add_named
;;;     gtk_stack_add_titled
;;;     gtk_stack_get_child_by_name
;;;     gtk_stack_set_visible_child                        Accessor
;;;     gtk_stack_get_visible_child                        Accessor
;;;     gtk_stack_set_visible_child_name                   Accessor
;;;     gtk_stack_get_visible_child_name                   Accessor
;;;     gtk_stack_set_visible_child_full
;;;     gtk_stack_set_homogeneous                          Accessor
;;;     gtk_stack_get_homogeneous                          Accessor
;;;     gtk_stack_set_hhomogeneous                         Accessor
;;;     gtk_stack_get_hhomogeneous                         Accessor
;;;     gtk_stack_set_vhomogeneous                         Accessor
;;;     gtk_stack_get_vhomogeneous                         Accessor
;;;     gtk_stack_set_transition_duration                  Accessor
;;;     gtk_stack_get_transition_duration                  Accessor
;;;     gtk_stack_set_transition_type                      Accessor
;;;     gtk_stack_get_transition_type                      Accessor
;;;     gtk_stack_get_transition_running                   Accessor
;;;     gtk_stack_get_interpolate_size                     Accessor
;;;     gtk_stack_set_interpolate_size                     Accessor
;;;
;;; Properties
;;;
;;;                   gboolean  hhomogeneous         Read / Write
;;;                   gboolean  homogeneous          Read / Write
;;;                   gboolean  interpolate-size     Read / Write
;;;                      guint  transition-duration  Read / Write
;;;                   gboolean  transition-running   Read
;;;     GtkStackTransitionType  transition-type      Read / Write
;;;                   gboolean  vhomogeneous         Read / Write
;;;                  GtkWidget  visible-child        Read / Write
;;;                      gchar  visible-child-name   Read / Write
;;;
;;; Child Properties
;;;
;;;                      gchar  icon-name            Read / Write
;;;                      gchar  name                 Read / Write
;;;                   gboolean  needs-attention      Read / Write
;;;                       gint  position             Read / Write
;;;                      gchar  title                Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkStack
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStack implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkStackTransitionType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkStackTransitionType" gtk-stack-transition-type
  (:export t
   :type-initializer "gtk_stack_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5)
  (:slide-left-right 6)
  (:slide-up-down 7)
  (:over-up 8)
  (:over-down 9)
  (:over-left 10)
  (:over-right 11)
  (:under-up 12)
  (:under-down 13)
  (:under-left 14)
  (:under-right 15)
  (:over-up-down 16)
  (:over-down-up 17)
  (:over-left-right 18)
  (:over-right-left 19))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-transition-type atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'gtk-stack-transition-type atdoc:*external-symbols*)
 "@version{*2021-12-16}
  @begin{short}
    These enumeration values describe the possible transitions between pages in
    a @class{gtk-stack} widget.
  @end{short}
  New values may be added to this enumeration over time.
  @begin{pre}
(define-g-enum \"GtkStackTransitionType\" gtk-stack-transition-type
  (:export t
   :type-initializer \"gtk_stack_transition_type_get_type\")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5)
  (:slide-left-right 6)
  (:slide-up-down 7)
  (:over-up 8)
  (:over-down 9)
  (:over-left 10)
  (:over-right 11)
  (:under-up 12)
  (:under-down 13)
  (:under-left 14)
  (:under-right 15)
  (:over-up-down 16)
  (:over-down-up 17)
  (:over-left-right 18)
  (:over-right-left 19))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No transition}
    @entry[:crossfade]{A cross fade.}
    @entry[:slide-right]{Slide from left to right.}
    @entry[:slide-left]{Slide from right to left.}
    @entry[:slide-up]{Slide from bottom up.}
    @entry[:slide-down]{Slide from top down.}
    @entry[:slide-left-right]{Slide from left or right according to the children
      order.}
    @entry[:slide-up-down]{Slide from top down or bottom up according to the
      order.}
    @entry[:over-up]{Cover the old page by sliding up.}
    @entry[:over-down]{Cover the old page by sliding down.}
    @entry[:over-left]{Cover the old page by sliding to the left.}
    @entry[:over-right]{Cover the old page by sliding to the right.}
    @entry[:under-up]{Uncover the new page by sliding up.}
    @entry[:under-down]{Uncover the new page by sliding down.}
    @entry[:under-left]{Uncover the new page by sliding to the left.}
    @entry[:under-right]{Uncover the new page by sliding to the right.}
    @entry[:over-up-down]{Cover the old page sliding up or uncover the new page
      sliding down, according to order.}
    @entry[:over-down-up]{Cover the old page sliding down or uncover the new
      page sliding up, according to order.}
    @entry[:over-left-right]{Cover the old page sliding left or uncover the new
      page sliding right, according to order.}
    @entry[:over-right-left]{Cover the old page sliding right or uncover the
      new page sliding left, according to order.}
  @end{table}
  @see-class{gtk-stack}")

;;; ----------------------------------------------------------------------------
;;; struct GtkStack
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStack" gtk-stack
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_stack_get_type")
  ((hhomogeneous
    gtk-stack-hhomogeneous
    "hhomogeneous" "gboolean" t t)
   (homogeneous
    gtk-stack-homogeneous
    "homogeneous" "gboolean" t t)
   (interpolate-size
    gtk-stack-interpolate-size
    "interpolate-size" "gboolean" t t)
   (transition-duration
    gtk-stack-transition-duration
    "transition-duration" "guint" t t)
   (transition-running
    gtk-stack-transition-running
    "transition-running" "gboolean" t nil)
   (transition-type
    gtk-stack-transition-type
    "transition-type" "GtkStackTransitionType" t t )
   (vhomogeneous
    gtk-stack-vhomogeneous
    "vhomogeneous" "gboolean" t t)
   (visible-child
    gtk-stack-visible-child
    "visible-child" "GtkWidget" t t)
   (visible-child-name
    gtk-stack-visible-child-name
    "visible-child-name" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-stack 'type)
 "@version{*2021-12-16}
  @begin{short}
    The @sym{gtk-stack} widget is a container which only shows one of its
    children at a time.
  @end{short}
  In contrast to the @class{gtk-notebook} widget, the @sym{gtk-stack} widget
  does not provide a means for users to change the visible child. Instead, the
  @class{gtk-stack-switcher} widget can be used with the @sym{gtk-stack} widget
  to provide this functionality.

  @image[stack]{}

  Transitions between pages can be animated as slides or fades. This can be
  controlled with the @fun{gtk-stack-transition-type} function. These animations
  respect the @slot[gtk-settings]{gtk-enable-animations} setting.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk-stack} implementation has a single CSS node named @code{stack}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[icon-name]{entry}
        The @code{icon-name} child property of type @code{:string}
        (Read / Write) @br{}
        The icon name of the child page. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[name]{entry}
        The @code{name} child property of type @code{:string} (Read / Write)
        @br{}
        The name of the child page. @br{}
        Default value: @code{nil}
      @end{entry}
      @begin[needs-attention]{entry}
        The @code{needs-attention} child property of type @code{:boolean}
        (Read / Write) @br{}
        Sets a flag specifying whether the child page requires the user
        attention. This is used by the the @class{gtk-stack-switcher} widget to
        change the appearance of the corresponding button when a page needs
        attention and it is not the current one. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int}
        (Read / Write) @br{}
        The index of the child page in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
      @begin[title]{entry}
        The @code{title} child property of type @code{:string}
        (Read / Write)@br{}
        The title of the child page. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-stack-hhomogeneous}
  @see-slot{gtk-stack-homogeneous}
  @see-slot{gtk-stack-interpolate-size}
  @see-slot{gtk-stack-transition-duration}
  @see-slot{gtk-stack-transition-running}
  @see-slot{gtk-stack-transition-type}
  @see-slot{gtk-stack-vhomogeneous}
  @see-slot{gtk-stack-visible-child}
  @see-slot{gtk-stack-visisble-child-name}
  @see-class{gtk-stack-switcher}
  @see-class{gtk-notebook}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-stack-hhomogeneous -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hhomogeneous" 'gtk-stack) 't)
 "The @code{hhomogeneous} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the stack allocates the same width for all children. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-hhomogeneous atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-stack-hhomogeneous 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-hhomogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-stack-hhomogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[homogeneous]{@em{true} to make stack horizontally homogeneous}
  @begin{short}
    Accessor of the @slot[gtk-stack]{hhomogeneous} slot of the @class{gtk-stack}
    class.
  @end{short}

  The @sym{gtk-stack-hhomogeneous} slot access function gets whether the stack
  is horizontally homogeneous. The @sym{(setf gtk-stack-hhomogeneous)} slot
  access function sets the stack to be horizontally homogeneous or not.

  If the stack is homogeneous, the stack will request the same width for all its
  children. If it is not, the stack may change width when a different child
  becomes visible.
  @see-class{gtk-stack}
  @see-function{gtk-stack-homogeneous}
  @see-function{gtk-stack-vhomogeneous}")

;;; --- gtk-stack-homogeneous --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "homogeneous" 'gtk-stack) 't)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Homogeneous sizing. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-homogeneous atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-stack-homogeneous 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-homogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-stack-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[homogeneous]{@em{true} to make the stack homogeneous}
  @begin{short}
    Accessor of the @slot[gtk-stack]{homogeneous} slot of the @class{gtk-stack}
    class.
  @end{short}

  The @sym{gtk-stack-homogeneous} slot access function gets whether the stack
  is homogeneous. The @sym{(setf gtk-stack-homogeneous)} slot access function
  sets the stack to be homogeneous or not. If it is homogeneous, the stack will
  request the same size for all its children. If it is not, the stack may change
  size when a different child becomes visible.

  Since 3.16, homogeneity can be controlled separately for horizontal and
  vertical size, with the @slot[gtk-stack]{hhomogeneous} and
  @slot[gtk-stack]{vhomogeneous} properties.
  @see-class{gtk-stack}
  @see-function{gtk-stack-hhomogeneous}
  @see-function{gtk-stack-vhomogeneous}")

;;; --- gtk-stack-interpolate-size ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "interpolate-size" 'gtk-stack)
                     't)
 "The @code{interpolate-size} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether or not the size should smoothly change when changing between
  differently sized children. Since 3.18 @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-interpolate-size atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-interpolate-size 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-interpolate-size object) => interpolate}
  @syntax[]{(setf (gtk-stack-interpolate-size object) interpolate)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[interpolate]{@em{true} if child sizes are interpolated}
  @begin{short}
    Accessor of the @slot[gtk-stack]{interpolate-size} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-interpolate-size} slot access function returns whether the
  stack is set up to interpolate between the sizes of children on page switch.
  The @sym{(setf gtk-stack-interpolate-size)} slot access function sets whether
  or not stack will interpolate its size when changing the visible child.

  If the @slot[gtk-stack]{interpolate-size} property is set to @em{true}, the
  stack will interpolate its size between the current one and the one it will
  take after changing the visible child, according to the set transition
  duration.

  Since 3.18
  @see-class{gtk-stack}")

;;; --- gtk-stack-transition-duration ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-duration" 'gtk-stack)
                     't)
 "The @code{transition-duration} property of type @code{:uint} (Read / Write)
  @br{}
  The animation duration, in milliseconds. 10 @br{}
  Default value: 200")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-transition-duration atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-transition-duration 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-transition-duration object) => duration}
  @syntax[]{(setf (gtk-stack-transition-duration object) duration)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[duration]{an unsigned integer with the duration, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk-stack]{transition-duration} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-transition-duration} slot access function returns the
  amount of time in milliseconds that transitions between pages in the stack
  will take. The @sym{(setf gtk-stack-transition-duration)} slot access function
  sets the duration.
  @see-class{gtk-stack}")

;;; --- gtk-stack-transition-running -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-running" 'gtk-stack)
                     't)
 "The @code{transition-running} property of type @code{:boolean} (Read) @br{}
  Whether or not the transition is currently running. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-transition-running atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-transition-running 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-transition-running object) => running}
  @argument[object]{a @class{gtk-stack} widget}
  @begin{short}
    Accessor of the @slot[gtk-stack]{transition-running} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-transition-running} slot access function returns whether
  the stack is currently in a transition from one page to another.
  @see-class{gtk-stack}")

;;; --- gtk-stack-transition-type ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "transition-type" 'gtk-stack)
                     't)
 "The @code{transition-type} property of type @symbol{gtk-stack-transition-type}
  (Read / Write) @br{}
  The type of animation used to transition. @br{}
  Default value: @code{:none}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-transition-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-transition-type 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-transition-type object) => setting}
  @syntax[]{(setf (gtk-stack-transition-type object) setting)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[setting]{a value of the @symbol{gtk-stack-transition-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk-stack]{transition-type} slot of the
    @class{gtk-stack} class.
  @end{short}

  The slot access function @sym{gtk-stack-transition-type} gets the type of
  animation that will be used for transitions between pages in the stack. The
  slot access function @sym{(setf gtk-stack-transition-type)} sets the type of
  animation that will be used for transitions between pages in the stack.
  Available types include various kinds of fades and slides.

  The transition type can be changed without problems at runtime, so it is
  possible to change the animation based on the page that is about to become
  current.
  @see-class{gtk-stack}
  @see-symbol{gtk-stack-transition-type}")

;;; --- gtk-stack-vhomogeneous -------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vhomogeneous" 'gtk-stack)
                     't)
 "The @code{vhomogeneous} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the stack allocates the same height for all children. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-vhomogeneous atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-vhomogeneous 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-vhomogeneous object) => homogeneous}
  @syntax[]{(setf (gtk-stack-vhomogeneous object) homogeneous)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[homogeneous]{@em{true} to make the stack vertically homogeneous}
  @begin{short}
    Accessor of the @slot[gtk-stack]{vhomogeneous} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-vhomogeneous} slot access function gets whether the stack
  is vertically homogeneous. The @sym{(setf gtk-stack-vhomogeneous)} slot access
  function sets the stack to be vertically homogeneous or not.

  If the stack is homogeneous, the stack will request the same height for all
  its children. If it is not, the stack may change height when a different
  child becomes visible.
  @see-class{gtk-stack}
  @see-function{gtk-stack-homogeneous}
  @see-function{gtk-stack-hhomogeneous}")


;;; --- gtk-stack-visible-child ------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-child" 'gtk-stack)
                     't)
 "The @code{visible-child} property of type @class{gtk-widget} (Read / Write)
  @br{}
  The widget currently visible in the stack.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-visible-child atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-visible-child 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-visible-child object) => child}
  @syntax[]{(setf (gtk-stack-visible-child object) child)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget of the stack}
  @begin{short}
    Accessor of the @slot[gtk-stack]{visible-child} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-visible-child} slot access function gets the currently
  visible child widget of the stack, or @code{nil} if there are no visible
  children. The @sym{(setf gtk-stack-visible-child)} slot access function makes
  the child widget the visible child widget of the stack.

  If the child widget is different from the currently visible child widget,
  the transition between the two will be animated with the current transition
  type of stack.

  Note that the child widget has to be visible itself, see the
  @fun{gtk-widget-show} function, in order to become the visible child of the
  stack.
  @see-class{gtk-stack}
  @see-class{gtk-widget}
  @see-function{gtk-widget-show}")

;;; --- gtk-stack-visible-child-name -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visible-child-name"
                                               'gtk-stack) 't)
 "The @code{visible-child-name} property of type @code{:string} (Read / Write)
  @br{}
  The name of the widget currently visible in the stack. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-visible-child-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-visible-child-name 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-visible-child-name object) => name}
  @syntax[]{(setf (gtk-stack-visible-child-name object) name)}
  @argument[object]{a @class{gtk-stack} widget}
  @argument[name]{a string with the name of the visible child of the stack}
  @begin{short}
    Accessor of the @slot[gtk-stack]{visible-child-name} slot of the
    @class{gtk-stack} class.
  @end{short}

  The @sym{gtk-stack-visible-child-name} slot access function returns the name
  of the currently visible child of the stack, or @code{nil} if there is no
  visible child. The @sym{(setf gtk-stack-visible-child-name)} slot access
  function makes the child widget with the given name visible.

  If the child widget is different from the currently visible child, the
  transition between the two will be animated with the current transition type
  of stack.

  Note that the child widget has to be visible itself, see the
  @fun{gtk-widget-show} function, in order to become the visible child of the
  stack.
  @see-class{gtk-stack}
  @see-function{gtk-widget-show}")

;;; ----------------------------------------------------------------------------
;;; Child Accessor Implementation
;;; ----------------------------------------------------------------------------

;; We need to register "GtkStack" before we define the access functions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkStack" 'gtk-stack))

;;; --- gtk-stack-child-icon-name ----------------------------------------------

(define-child-property "GtkStack"
                       gtk-stack-child-icon-name
                       "icon-name" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-child-icon-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-child-icon-name 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-child-icon-name object) => name)}
  @syntax[]{(setf (gtk-stack-child-icon-name object) name)}
  @argument[container]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[name]{a string with the icon name of the child page}
  @begin{short}
    Accessor of the @code{icon-name} child property of the @class{gtk-stack}
    class.
  @end{short}

  The icon name of the child page.
  @see-class{gtk-stack}
  @see-class{gtk-widget}")

;;; --- gtk-stack-child-name ---------------------------------------------------

(define-child-property "GtkStack"
                       gtk-stack-child-name
                       "name" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-child-name atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-child-name 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-child-name object) => name)}
  @syntax[]{(setf (gtk-stack-child-name object) name)}
  @argument[container]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[name]{a string with the name of the child page}
  @begin{short}
    Accessor of the @code{name} child property of the @class{gtk-stack} class.
  @end{short}

  The name of the child page.
  @see-class{gtk-stack}
  @see-class{gtk-widget}")

;;; --- gtk-stack-child-needs-attention ----------------------------------------

(define-child-property "GtkStack"
                       gtk-stack-child-needs-attention
                       "needs-attention" "gboolean" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-child-needs-attention atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-child-needs-attention 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-child-needs-attention container child) => attention)}
  @syntax[]{(setf (gtk-stack-child-needs-attention container child) attention)}
  @argument[container]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[attention]{a boolean whether the child page requires attention}
  @begin{short}
    Accessor of the @code{needs-attention} child property of the
    @class{gtk-stack} class.
  @end{short}

  Sets a flag specifying whether the child page requires the user attention.
  This is used by the @class{gtk-stack-switcher} widget to change the appearance
  of the corresponding button when a page needs attention and it is not the
  current one.
  @see-class{gtk-stack}
  @see-class{gtk-stack-switcher}
  @see-class{gtk-widget}")

;;; --- gtk-stack-child-position -----------------------------------------------

(define-child-property "GtkStack"
                       gtk-stack-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-child-position 'function)
 "@version{2021-12-7}
  @syntax[]{(gtk-stack-child-position container child) => position)}
  @syntax[]{(setf (gtk-stack-child-position container child) position)}
  @argument[container]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[position]{an integer with the index of the child page in the parent}
  @begin{short}
    Accessor of the @code{position} child property of the @class{gtk-stack}
    class.
  @end{short}

  The index of the child page in the parent.
  @see-class{gtk-stack}
  @see-class{gtk-widget}")

;;; --- gtk-stack-child-title --------------------------------------------------

(define-child-property "GtkStack"
                       gtk-stack-child-title
                       "title" "gchararray" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-stack-child-title atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-stack-child-title 'function)
 "@version{*2021-12-16}
  @syntax[]{(gtk-stack-child-title container child) => title)}
  @syntax[]{(setf (gtk-stack-child-title container child) title)}
  @argument[container]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget}
  @argument[title]{a string with the title of the child page}
  @begin{short}
    Accessor of the @code{title} child property of the @class{gtk-stack} class.
  @end{short}

  The title of the child page.
  @see-class{gtk-stack}
  @see-class{gtk-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-stack-new))

(defun gtk-stack-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-12-7}
  @return{The new @class{gtk-stack} widget.}
  @short{Creates a new stack.}
  @see-class{gtk-stack}"
  (make-instance 'gtk-stack))

(export 'gtk-stack-new)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_add_named ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_stack_add_named" gtk-stack-add-named) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-12-16}
  @argument[stack]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child page to add}
  @argument[name]{a string with the name for the child page}
  @begin{short}
    Adds a child page to the stack.
  @end{short}
  The child page is identified by @arg{name}.
  @see-class{gtk-stack}
  @see-class{gtk-widget}"
  (stack (g-object gtk-stack))
  (child (g-object gtk-widget))
  (name :string))

(export 'gtk-stack-add-named)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_add_titled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_stack_add_titled" gtk-stack-add-titled) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-7}
  @argument[stack]{a @class{gtk-stack} widget}
  @argument[child]{a @class{gtk-widget} child widget to add}
  @argument[name]{a string with the name for the child page}
  @argument[title]{a string with a human readable title for the child page}
  @begin{short}
    Adds a child page to the stack.
  @end{short}
  The child page is identified by @arg{name}. The title will be used by the
  @class{gtk-stack-switcher} widget to represent the child page in a tab bar,
  so it should be short.
  @see-class{gtk-stack}
  @see-class{gtk-stack-switcher}
  @see-class{gtk-widget}"
  (stack (g-object gtk-stack))
  (child (g-object gtk-widget))
  (name :string)
  (title :string))

(export 'gtk-stack-add-titled)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_get_child_by_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_stack_get_child_by_name" gtk-stack-child-by-name)
         (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2021-12-7}
  @argument[stack]{a @class{gtk-stack} widget}
  @argument[name]{a string with the name for the child page to find}
  @return{The requested @class{gtk-widget} child page of the stack.}
  @begin{short}
    Finds the child page of the stack with the name given as the argument.
  @end{short}
  Returns @code{nil} if there is no child page with this name.
  @see-class{gtk-stack}
  @see-class{gtk-widget}"
  (stack (g-object gtk-stack))
  (name :string))

(export 'gtk-stack-child-by-name)

;;; ----------------------------------------------------------------------------
;;; gtk_stack_set_visible_child_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_stack_set_visible_child_full" gtk-stack-set-visible-child-full)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2021-12-7}
  @argument[stack]{a @class{gtk-stack} widget}
  @argument[name]{a string with the name of the child page to make visible}
  @argument[transition]{a value of the @symbol{gtk-stack-transition-type}
    enumeration to use}
  @begin{short}
    Makes the child page with the given name visible.
  @end{short}

  Note that the child page has to be visible itself, see the
  @fun{gtk-widget-show} function, in order to become the visible child page of
  the stack.
  @see-class{gtk-stack}
  @see-symbol{gtk-stack-transition-type}
  @see-function{gtk-widget-show}"
  (stack (g-object gtk-stack))
  (name :string)
  (transition gtk-stack-transition-type))

(export 'gtk-stack-set-visible-child-full)

;;; --- End of file gtk.stack.lisp ---------------------------------------------

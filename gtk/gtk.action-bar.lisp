;;; ----------------------------------------------------------------------------
;;; gtk.action-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2020 Dieter Kaiser
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
;;; GtkActionBar
;;;
;;;     A full width bar for presenting contextual actions
;;;
;;; Types and Values
;;;
;;;     GtkActionBar
;;;
;;; Functions
;;;
;;;     gtk_action_bar_new
;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget
;;;
;;; Child Properties
;;;
;;;     GtkPackType    pack-type    Read / Write
;;;            gint    position     Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkActionBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkActionBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkActionBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkActionBar" gtk-action-bar
  (:superclass gtk-bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_action_bar_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-action-bar 'type)
 "@version{2020-5-9}
  @begin{short}
    @sym{gtk-action-bar} is designed to present contextual actions.
  @end{short}
  It is expected to be displayed below the content and expand horizontally to
  fill the area.

  @image[action-bar]{}

  It allows placing children at the start or the end. In addition, it contains
  an internal centered box which is centered with respect to the full width of
  the box, even if the children at either side take up different amounts of
  space.

  Since 3.12
  @begin[CSS nodes]{dictionary}
    @sym{gtk-action-bar} has a single CSS node with name @code{actionbar}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk-pack-type}
        (Read / Write) @br{}
        A value of the @symbol{gtk-pack-type} enumeration indicating whether the
        child is packed with reference to the start or end of the parent. @br{}
        Default value: @code{:start}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-class{gtk-stack}
  @see-class{gtk-box}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-action-bar-child-pack-type -----------------------------------------

(define-child-property "GtkActionBar"
                       gtk-action-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-bar-child-pack-type atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-bar-child-pack-type 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-action-bar-child-pack-type container child) => pack-type)}
  @syntax[]{(setf (gtk-action-bar-child-pack-type container child) pack-type)}
  @argument[container]{a @class{gtk-action-bar} container}
  @argument[child]{the @class{gtk-widget} child}
  @argument[pack-type]{a value of the @symbol{gtk-pack-type} enumeration for
    the child}
  @begin{short}
    Accessor of the @code{pack-type} child property of the
    @class{gtk-action-bar} class.
  @end{short}

  A value of the @symbol{gtk-pack-type} enumeration indicating whether the
  child widget is packed with reference to the start or end of the parent.

  Since 3.12
  @see-class{gtk-action-bar}
  @see-symbol{gtk-pack-type}")

;;; --- gtk-action-bar-child-position ------------------------------------------

(define-child-property "GtkActionBar"
                       gtk-action-bar-child-position
                       "position" "gint" t t t)

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-action-bar-child-position atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-action-bar-child-position 'function)
 "@version{2020-5-9}
  @syntax[]{(gtk-action-bar-child-position object) => position)}
  @syntax[]{(setf (gtk-action-bar-child-position object) position)}
  @argument[container]{a @class{gtk-action-bar} container}
  @argument[child]{the @class{gtk-widget} child}
  @argument[position]{An integer with the index of the child in the parent}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk-action-bar} class.
  @end{short}

  The index of the child widget in the parent.

  Since 3.12
  @see-class{gtk-action-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-action-bar-new))

(defun gtk-action-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @return{The new @class{gtk-action-bar} container.}
  @short{Creates a new action bar.}

  Since 3.12
  @see-class{gtk-action-bar}"
  (make-instance 'gtk-action-bar))

(export 'gtk-action-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_bar_pack_start" gtk-action-bar-pack-start) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[action-bar]{a @class{gtk-action-bar} container}
  @argument[child]{the @class{gtk-widget} child to be added to @arg{action-bar}}
  @begin{short}
    Adds the child widget to the action bar, packed with reference to the start
    of the action bar.
  @end{short}

  Since 3.12
  @see-class{gtk-action-bar}"
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

(export 'gtk-action-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_bar_pack_end" gtk-action-bar-pack-end) :void
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @argument[action-bar]{a @class{gtk-action-bar} container}
  @argument[child]{the @class{gtk-widget} child to be added to @arg{action-bar}}
  @begin{short}
    Adds the child widget to @arg{action-bar}, packed with reference to the end
    of the action bar.
  @end{short}

  Since 3.12
  @see-class{gtk-action-bar}"
  (action-bar (g-object gtk-action-bar))
  (child (g-object gtk-widget)))

(export 'gtk-action-bar-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_get_center_widget ()
;;; gtk_action_bar_set_center_widget () -> gtk-action-bar-center-widget
;;; ----------------------------------------------------------------------------

(defun (setf gtk-action-bar-center-widget) (center-widget action-bar)
  (foreign-funcall "gtk_action_bar_set_center_widget"
                   (g-object gtk-action-bar) action-bar
                   (g-object gtk-widget) center-widget
                   :void)
  center-widget)

(defcfun ("gtk_action_bar_get_center_widget" gtk-action-bar-center-widget)
    (g-object gtk-widget)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-9}
  @syntax[]{(gtk-action-bar-center-widget action-bar) => center-widget}
  @syntax[]{(setf (gtk-action-bar-center-widget action-bar) center-widget)}
  @argument[action-bar]{a @class{gtk-action-bar} container}
  @argument[center-widget]{a @class{gtk-widget} object to use for the center}
  @begin{short}
    Accessor of the center widget of the action bar.
  @end{short}

  The function @sym{gtk-action-bar-center-widget} retrieves the center bar
  widget of the bar. The function @sym{(setf gtk-action-bar-center-widget)}
  sets the center widget for the action bar.

  Since 3.12
  @see-class{gtk-action-bar}"
  (action-bar (g-object gtk-action-bar)))

(export 'gtk-action-bar-center-widget)

;;; --- End of file gtk.action-bar.lisp ----------------------------------------

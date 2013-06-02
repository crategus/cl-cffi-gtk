;;; ----------------------------------------------------------------------------
;;; gtk.menu-bar.lisp
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
;;; GtkMenuBar
;;;
;;; A subclass of GtkMenuShell which holds GtkMenuItem widgets
;;;
;;; Synopsis
;;;
;;;     GtkMenuBar
;;;
;;;     gtk_menu_bar_new
;;;     gtk_menu_bar_new_from_model
;;;
;;;     GtkPackDirection
;;;
;;;     gtk_menu_bar_set_pack_direction
;;;     gtk_menu_bar_get_pack_direction
;;;     gtk_menu_bar_set_child_pack_direction
;;;     gtk_menu_bar_get_child_pack_direction
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuBar" gtk-menu-bar
  (:superclass gtk-menu-shell
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_menu_bar_get_type")
  ((child-pack-direction
    gtk-menu-bar-child-pack-direction
    "child-pack-direction" "GtkPackDirection" t t)
   (pack-direction
    gtk-menu-bar-pack-direction
    "pack-direction" "GtkPackDirection" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-menu-bar 'type)
 "@version{2013-6-1}
  @begin{short}
    The @sym{gtk-menu-bar} is a subclass of @class{gtk-menu-shell} which
    contains one or more items of type @class{gtk-menu-item}. The result is a
    standard menu bar which can hold many menu items.
  @end{short}
  @begin[Style Property Details]{dictionary}
    @subheading{The \"internal-padding\" style property}
      @code{\"internal-padding\"} of type @code{:int} (Read) @br{}
      Amount of border space between the menubar shadow and the menu items.
      @br{}
      Allowed values: >= 0 @br{}
      Default value: 1

    @subheading{The \"shadow-type\" style property}
      @code{\"shadow-type\"} of type @symbol{gtk-shadow-type} (Read) @br{}
      Style of bevel around the menubar. @br{}
      Default value: @code{:out}
  @end{dictionary}
  @see-slot{gtk-menu-bar-child-pack-direction}
  @see-slot{gtk-menu-bar-pack-direction}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "child-pack-direction"
                                               'gtk-menu-bar) 't)
 "The @code{\"child-pack-direction\"} property of type
  @symbol{gtk-pack-direction} (Read / Write) @br{}
  The child pack direction of the menubar. It determines how the widgets
  contained in child menuitems are arranged. @br{}
  Default value: @code{:ltr} @br{}
  Since 2.8")

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pack-direction"
                                               'gtk-menu-bar) 't)
 "The @code{\"pack-direction\"} property of type @symbol{gtk-pack-direction}
  (Read / Write) @br{}
  The pack direction of the menubar. It determines how menuitems are arranged
  in the menubar. @br{}
  Default value: @code{:ltr} @br{}
  Since 2.8")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-bar-child-pack-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-bar-child-pack-direction 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"child-pack-direction\"} of the
  @class{gtk-menu-bar} class.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-menu-bar-pack-direction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-menu-bar-pack-direction 'function)
 "@version{2013-3-16}
  Accessor of the slot @code{\"pack-direction\"} of the @class{gtk-menu-bar}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-new))

(defun gtk-menu-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @return{The new menu bar, as a @class{gtk-widget} widget.}
  Creates a new @class{gtk-menu-bar} widget."
  (make-instance 'gtk-menu-bar))

(export 'gtk-menu-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new_from_model ()
;;;
;;; GtkWidget * gtk_menu_bar_new_from_model (GMenuModel *model);
;;;
;;; Creates a new GtkMenuBar and populates it with menu items and submenus
;;; according to model.
;;;
;;; The created menu items are connected to actions found in the
;;; GtkApplicationWindow to which the menu bar belongs - typically by means of
;;; being contained within the GtkApplicationWindows widget hierarchy.
;;;
;;; model :
;;;     a GMenuModel
;;;
;;; Returns :
;;;     a new GtkMenuBar
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkPackDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPackDirection" gtk-pack-direction
  (:export t
   :type-initializer "gtk_pack_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-pack-direction atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-pack-direction atdoc:*external-symbols*)
 "@version{2013-6-1}
  @begin{short}
    Determines how widgets should be packed insided menubars and menuitems
    contained in menubars.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPackDirection\" gtk-pack-direction
  (:export t
   :type-initializer \"gtk_pack_direction_get_type\")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))
  @end{pre}
  @begin[code]{table}
    @entry[:ltr]{Widgets are packed left-to-right.}
    @entry[:rtl]{Widgets are packed right-to-left.}
    @entry[:ttb]{Widgets are packed top-to-bottom.}
    @entry[:btt]{Widgets are packed bottom-to-top.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_set_pack_direction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-set-pack-direction))

(defun gtk-menu-bar-set-pack-direction (menubar pack-dir)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menubar]{a @class{gtk-menu-bar} widget}
  @argument[pack-dir]{a new @symbol{gtk-pack-direction}}
  @short{Sets how items should be packed inside a @arg{menubar}.}

  Since 2.8"
  (setf (gtk-menu-bar-pack-direction menubar) pack-dir))

(export 'gtk-menu-bar-set-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_get_pack_direction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-get-pack-direction))

(defun gtk-menu-bar-get-pack-direction (menubar)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menubar]{a @class{gtk-menu-bar} widget}
  @return{The pack direction.}
  @begin{short}
    Retrieves the current pack direction of the menubar.
  @end{short}
  See the @fun{gtk-menu-bar-set-pack-direction} function.

  Since 2.8
  @see-function{gtk-menu-bar-set-pack-direction}"
  (gtk-menu-bar-pack-direction menubar))

(export 'gtk-menu-bar-get-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_set_child_pack_direction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-set-child-pack-direction))

(defun gtk-menu-bar-set-child-pack-direction (menubar child-pack-dir)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menubar]{a @class{gtk-menu-bar} widget}
  @argument[child-pack_dir]{a new @symbol{gtk-pack-direction}}
  @begin{short}
    Sets how widgets should be packed inside the children of a @arg{menubar}.
  @end{short}

  Since 2.8"
  (setf (gtk-menu-bar-child-pack-direction menubar) child-pack-dir))

(export 'gtk-menu-bar-set-child-pack-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_get_child_pack_direction ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-menu-bar-get-child-pack-direction))

(defun gtk-menu-bar-get-child-pack-direction (menubar)
 #+cl-cffi-gtk-documentation
 "@version{2013-6-1}
  @argument[menubar]{a @class{gtk-menu-bar} widget}
  @retrun{The child pack direction.}
  @begin{short}
    Retrieves the current child pack direction of the menubar.
  @end{short}
  See the @fun{gtk-menu-bar-set-child-pack-direction} function.

  Since 2.8
  @see-function{gtk-menu-bar-set-child-pack-direction}"
  (gtk-menu-bar-child-pack-direction menubar))

(export 'gtk-menu-bar-get-child-pack-direction)

;;; --- End of file gtk.menu-bar.lisp ------------------------------------------

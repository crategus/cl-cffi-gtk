;;; ----------------------------------------------------------------------------
;;; gtk.scrollable.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012, 2013 Dieter Kaiser
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
;;; GtkScrollable
;;;
;;;     An interface for scrollable widgets
;;;
;;; Types and Values
;;;
;;;     GtkScrollable
;;;     GtkScrollablePolicy
;;;
;;; Functions
;;;
;;;     gtk_scrollable_get_hadjustment                     Accessor
;;;     gtk_scrollable_set_hadjustment                     Accessor
;;;     gtk_scrollable_get_vadjustment                     Accessor
;;;     gtk_scrollable_set_vadjustment                     Accessor
;;;
;;;     gtk_scrollable_get_hscroll_policy                  Accessor
;;;     gtk_scrollable_set_hscroll_policy                  Accessor
;;;     gtk_scrollable_get_vscroll_policy                  Accessor
;;;     gtk_scrollable_set_vscroll_policy                  Accessor
;;;
;;;     gtk_scrollable_get_border
;;;
;;; Properties
;;;
;;;           GtkAdjustment*  hadjustment       Read / Write / Construct
;;;     GtkScrollablePolicy   hscroll-policy    Read / Write
;;;           GtkAdjustment*  vadjustment       Read / Write / Construct
;;;     GtkScrollablePolicy   vscroll-policy    Read / Write
;;;
;;;Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScrollable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkScrollable" gtk-scrollable
  (:export t
   :type-initializer "gtk_scrollable_get_type")
  (hadjustment
   gtk-scrollable-hadjustment
   "hadjustment" "GtkAdjustment" t t)
  (hscroll-policy
   gtk-scrollable-hscroll-policy
   "hscroll-policy" "GtkScrollablePolicy" t t)
  (vadjustment
   gtk-scrollable-vadjustment
   "vadjustment" "GtkAdjustment" t t)
  (vscroll-policy
   gtk-scrollable-policy
   "vscroll-policy" "GtkScrollablePolicy" t t))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable atdoc:*class-name-alias*) "Interface"
      (documentation 'gtk-scrollable 'type)
 "@version{2013-7-17}
  @begin{short}
    @sym{gtk-scrollable} is an interface that is implemented by widgets with
    native scrolling ability.
  @end{short}

  To implement this interface you should override the @code{hadjustment}
  and @code{vadjustment} properties.

  @subheading{Creating a scrollable widget}
  All scrollable widgets should do the following.
  @begin{itemize}
    @begin{item}
      When a parent widget sets the scrollable child widget’s adjustments,
      the widget should populate the adjustments’ @code{lower}, @code{upper},
      @code{step-increment}, @code{page-increment} and @code{page-size}
      properties and connect to the \"value-changed\" signal.
    @end{item}
    @begin{item}
      Because its preferred size is the size for a fully expanded widget, the
      scrollable widget must be able to cope with underallocations. This means
      that it must accept any value passed to its
      @code{GtkWidgetClass.size_allocate()} function.
    @end{item}
    @begin{item}
      When the parent allocates space to the scrollable child widget, the widget
      should update the adjustments’ properties with new values.
    @end{item}
    @begin{item}
      When any of the adjustments emits the \"value-changed\" signal, the
      scrollable widget should scroll its contents.
    @end{item}
  @end{itemize}
  @see-slot{gtk-scrollable-hadjustment}
  @see-slot{gtk-scrollable-hscroll-policy}
  @see-slot{gtk-scrollable-vadjustment}
  @see-slot{gtk-scrollable-vscroll-policy}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-scrollable-hadjustment ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hadjustment"
                                               'gtk-scrollable) 't)
 "The @code{hadjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  Horizontal @class{gtk-adjustment} of the scrollable widget. This adjustment
  is shared between the scrollable widget and its parent.")




#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable-hadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrollable-hadjustment 'function)
 "@version{2013-3-22}
  Accessor of the slot @slot[gtk-scrollable]{hadjustment} of the
  @class{gtk-scrollable} class.")

;;; --- gtk-scrollable-hscroll-policy ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "hscroll-policy"
                                               'gtk-scrollable) 't)
 "The @code{hscroll-policy} property of type @symbol{gtk-scrollable-policy}
  (Read / Write) @br{}
  Determines whether horizontal scrolling should start once the scrollable
  widget is allocated less than its minimum width or less than its natural
  width. @br{}
  Default value: @code{:minimum}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable-hscroll-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrollable-hscroll-policy 'function)
 "@version{2013-3-22}
  Accessor of the slot @slot[gtk-scrollable]{hscroll-policy} of the
  @class{gtk-scrollable} class.")

;;; --- gtk-scrollabe-vadjustment ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vadjustment"
                                               'gtk-scrollable) 't)
 "The @code{vadjustment} property of type @class{gtk-adjustment}
  (Read / Write / Construct) @br{}
  Verical @class{gtk-adjustment} of the scrollable widget. This adjustment is
  shared between the scrollable widget and its parent.")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable-vadjustment atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrollable-vadjustment 'function)
 "@version{2013-3-22}
  Accessor of the slot @slot[gtk-scrollable]{vadjustment} of the
  @class{gtk-scrollable} class.")

;;; --- gtk-scrollable-vscroll-policy ------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "vscroll-policy"
                                               'gtk-scrollable) 't)
 "The @code{vscroll-policy} property of type @symbol{gtk-scrollable-policy}
  (Read / Write) @br{}
  Determines whether vertical scrolling should start once the scrollable
  widget is allocated less than its minimum height or less than its natural
  height. @br{}
  Default value: @code{:minimum}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable-vscroll-policy atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-scrollable-vscroll-policy 'function)
 "@version{2013-3-22}
  Accessor of the slot @slot[gtk-scrollable]{vscroll-policy} of the
  @class{gtk-scrollable} class.")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_hadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-hadjustment))

(defun gtk-scrollable-get-hadjustment (scrollable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @return{Horizontal @class{gtk-adjustment} object.}
  @begin{short}
    Retrieves the @class{gtk-adjustment} object used for horizontal scrolling.
  @end{short}"
  (gtk-scrollable-hadjustment scrollable))

(export 'gtk-scrollable-get-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_hadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-hadjustment))

(defun gtk-scrollable-set-hadjustment (scrollable hadjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @argument[hadjustment]{a @class{gtk-adjustment} object}
  @return{Sets the horizontal adjustment of the @class{gtk-scrollable}.}"
  (setf (gtk-scrollable-hadjustment scrollable) hadjustment))

(export 'gtk-scrollable-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_vadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-vadjustment))

(defun gtk-scrollable-get-vadjustment (scrollable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @return{Vertical @class{gtk-adjustment} object.}
  @begin{short}
    Retrieves the @class{gtk-adjustment} object used for vertical scrolling.
  @end{short}"
  (gtk-scrollable-vadjustment scrollable))

(export 'gtk-scrollable-get-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_vadjustment ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-vadjustment))

(defun gtk-scrollable-set-vadjustment (scrollable vadjustment)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @argument[vadjustment]{a @class{gtk-adjustment} object}
  @short{Sets the vertical adjustment of the @class{gtk-scrollable}.}"
  (setf (gtk-scrollable-vadjustment scrollable) vadjustment))

(export 'gtk-scrollable-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; enum GtkScrollablePolicy
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkScrollablePolicy" gtk-scrollable-policy
  (:export t
   :type-initializer "gtk_scrollable_policy_get_type")
  (:minimum 0)
  (:natural 1))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-scrollable-policy atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-scrollable-policy atdoc:*external-symbols*)
 "@version{2013-5-22}
  @begin{short}
    Defines the policy to be used in a scrollable widget when updating the
    scrolled window adjustments in a given orientation.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkScrollablePolicy\" gtk-scrollable-policy
  (:export t
   :type-initializer \"gtk_scrollable_policy_get_type\")
  (:minimum 0)
  (:natural 1))
  @end{pre}
  @begin[code]{table}
    @entry[:minimum]{Scrollable adjustments are based on the minimum size.}
    @entry[:natural]{Scrollable adjustments are based on the natural size.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_hscroll_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-hscroll-policy))

(defun gtk-scrollable-get-hscroll-policy (scrollable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @return{The horizontal @symbol{gtk-scrollable-policy}.}
  @short{Gets the horizontal @symbol{gtk-scrollable-policy}.}"
  (gtk-scrollable-hscroll-policy scrollable))

(export 'gtk-scrollable-get-hscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_hscroll_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-hscroll-policy))

(defun gtk-scrollable-set-hscroll-policy (scrollable policy)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @argument[policy]{the horizontal @symbol{gtk-scrollable-policy}}
  @begin{short}
    Sets the @symbol{gtk-scrollable-policy} to determine whether horizontal
    scrolling should start below the minimum width or below the natural width.
  @end{short}"
  (setf (gtk-scrollable-hscroll-policy scrollable) policy))

(export 'gtk-scrollable-set-hscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_vscroll_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-vscroll-policy))

(defun gtk-scrollable-get-vscroll-policy (scrollable)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @return{The vertical @symbol{gtk-scrollable-policy}.}
  @short{Gets the vertical @symbol{gtk-scrollable-policy}.}"
  (gtk-scrollable-vscroll-policy scrollable))

(export 'gtk-scrollable-get-vscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_vscroll_policy ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-vscroll-policy))

(defun gtk-scrollable-set-vscroll-policy (scrollable policy)
 #+cl-cffi-gtk-documentation
 "@version{2013-5-22}
  @argument[scrollable]{a @class{gtk-scrollable} object}
  @argument[policy]{the vertical @symbol{gtk-scrollable-policy}}
  @begin{short}
    Sets the @symbol{gtk-scrollable-policy} to determine whether vertical
    scrolling should start below the minimum height or below the natural height.
  @end{short}"
  (setf (gtk-scrollable-vscroll-policy scrollable) policy))

(export 'gtk-scrollable-set-vscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_border ()
;;;
;;; gboolean
;;; gtk_scrollable_get_border (GtkScrollable *scrollable, GtkBorder *border);
;;;
;;; Returns the size of a non-scrolling border around the outside of the
;;; scrollable. An example for this would be treeview headers. GTK+ can use this
;;; information to display overlayed graphics, like the overshoot indication, at
;;; the right position.
;;;
;;; scrollable :
;;;     a GtkScrollable
;;;
;;; border :
;;;     return location for the results.
;;;
;;; Returns :
;;;     TRUE if border has been set
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.scrollable.lisp ----------------------------------------

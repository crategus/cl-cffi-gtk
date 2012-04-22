;;; ----------------------------------------------------------------------------
;;; gtk.scrollable.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.2.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2012 Dieter Kaiser
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
;;; An interface for scrollable widgets
;;;     
;;; Synopsis
;;; 
;;;     GtkScrollable
;;;
;;;     gtk_scrollable_get_hadjustment
;;;     gtk_scrollable_set_hadjustment
;;;     gtk_scrollable_get_vadjustment
;;;     gtk_scrollable_set_vadjustment
;;;
;;;     GtkScrollablePolicy
;;;
;;;     gtk_scrollable_get_hscroll_policy
;;;     gtk_scrollable_set_hscroll_policy
;;;     gtk_scrollable_get_vscroll_policy
;;;     gtk_scrollable_set_vscroll_policy
;;; 
;;; Object Hierarchy
;;; 
;;;   GInterface
;;;    +----GtkScrollable
;;; 
;;; Prerequisites
;;; 
;;; GtkScrollable requires GObject.
;;;
;;; Known Implementations
;;; 
;;; GtkScrollable is implemented by GtkIconView, GtkLayout, GtkTextView,
;;; GtkToolPalette, GtkTreeView and GtkViewport.
;;;
;;; Properties
;;; 
;;;   "hadjustment"             GtkAdjustment*        : Read / Write / Construct
;;;   "hscroll-policy"          GtkScrollablePolicy   : Read / Write
;;;   "vadjustment"             GtkAdjustment*        : Read / Write / Construct
;;;   "vscroll-policy"          GtkScrollablePolicy   : Read / Write
;;; 
;;; Description
;;; 
;;; GtkScrollable is an interface that is implemented by widgets with native
;;; scrolling ability.
;;; 
;;; To implement this interface you should override the "hadjustment" and
;;; "vadjustment" properties.
;;; 
;;; Creating a scrollable widget
;;; 
;;; All scrollable widgets should do the following.
;;; 
;;;     When a parent widget sets the scrollable child widget's adjustments,
;;;     the widget should populate the adjustments' "lower", "upper",
;;;     "step-increment", "page-increment" and "page-size" properties and
;;;     connect to the "value-changed" signal.
;;; 
;;;     Because its preferred size is the size for a fully expanded widget, the
;;;     scrollable widget must be able to cope with underallocations. This
;;;     means that it must accept any value passed to its
;;;     GtkWidgetClass.size_allocate() function.
;;; 
;;;     When the parent allocates space to the scrollable child widget, the
;;;     widget should update the adjustments' properties with new values.
;;; 
;;;     When any of the adjustments emits the "value-changed" signal, the
;;;     scrollable widget should scroll its contents.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "hadjustment" property
;;; 
;;;   "hadjustment"             GtkAdjustment*        : Read / Write / Construct
;;; 
;;; Horizontal GtkAdjustment of the scrollable widget. This adjustment is shared
;;; between the scrollable widget and its parent.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "hscroll-policy" property
;;; 
;;;   "hscroll-policy"           GtkScrollablePolicy   : Read / Write
;;; 
;;; Determines whether horizontal scrolling should start once the scrollable
;;; widget is allocated less than its minimum width or less than its natural
;;; width.
;;; 
;;; Default value: GTK_SCROLL_MINIMUM
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vadjustment" property
;;; 
;;;   "vadjustment"             GtkAdjustment*        : Read / Write / Construct
;;; 
;;; Verical GtkAdjustment of the scrollable widget. This adjustment is shared
;;; between the scrollable widget and its parent.
;;; 
;;; Since 3.0
;;;
;;; ----------------------------------------------------------------------------
;;; The "vscroll-policy" property
;;; 
;;;   "vscroll-policy"           GtkScrollablePolicy   : Read / Write
;;; 
;;; Determines whether vertical scrolling should start once the scrollable
;;; widget is allocated less than its minimum height or less than its natural
;;; height.
;;; 
;;; Default value: GTK_SCROLL_MINIMUM
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScrollable
;;; 
;;; typedef struct _GtkScrollable GtkScrollable;
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkScrollable" gtk-scrollable
  (:export t
   :type-initializer "gtk_scrollable_get_type")
  (hadjustment
   gtk-scrollable-hadjustment
   "hadjustment" "GtkAdjustment" t t)
  (hscroll-policy
   gtk-scrollable-hscroll-policy
   "hsrcoll-policy" "GtkScrollablePolicy" t t)
  (vadjustment
   gtk-scrollable-vadjustment
   "vadjustment" "GtkAdjustment" t t)
  (vscroll-policy
   gtk-scrollable-policy
   "vscroll-policy" "GtkScollablePolicy" t t))

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_hadjustment ()
;;; 
;;; GtkAdjustment * gtk_scrollable_get_hadjustment (GtkScrollable *scrollable);
;;; 
;;; Retrieves the GtkAdjustment used for horizontal scrolling.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; Returns :
;;;     horizontal GtkAdjustment
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-hadjustment))

(defun gtk-scrollable-get-hadjustment (scrollable)
  (gtk-scrollable-hadjustment scrollable))

(export 'gtk-scrollable-get-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_hadjustment ()
;;; 
;;; void gtk_scrollable_set_hadjustment (GtkScrollable *scrollable,
;;;                                      GtkAdjustment *hadjustment);
;;; 
;;; Sets the horizontal adjustment of the GtkScrollable.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; hadjustment :
;;;     a GtkAdjustment
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-hadjustment))

(defun gtk-scrollable-set-hadjustment (scrollable hadjustment)
  (setf (gtk-scrollable-hadjustment scrollable) hadjustment))

(export 'gtk-scrollable-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_vadjustment ()
;;; 
;;; GtkAdjustment * gtk_scrollable_get_vadjustment (GtkScrollable *scrollable);
;;; 
;;; Retrieves the GtkAdjustment used for vertical scrolling.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; Returns :
;;;     vertical GtkAdjustment
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-vadjustment))

(defun gtk-scrollable-get-vadjustment (scrollable)
  (gtk-scrollable-vadjustment scrollable))

(export 'gtk-scrollable-get-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_vadjustment ()
;;; 
;;; void gtk_scrollable_set_vadjustment (GtkScrollable *scrollable,
;;;                                      GtkAdjustment *vadjustment);
;;; 
;;; Sets the vertical adjustment of the GtkScrollable.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; vadjustment :
;;;     a GtkAdjustment
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-vadjustment))

(defun gtk-scrollable-set-vadjustment (scrollable vadjustment)
  (setf (gtk-scrollable-vadjustment scrollable) vadjustment))

(export 'gtk-scrollable-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; enum GtkScrollablePolicy
;;; 
;;; typedef enum {
;;;   GTK_SCROLL_MINIMUM = 0,
;;;   GTK_SCROLL_NATURAL
;;; } GtkScrollablePolicy;
;;; 
;;; Defines the policy to be used in a scrollable widget when updating the
;;; scrolled window adjustments in a given orientation.
;;; 
;;; GTK_SCROLL_MINIMUM
;;;     Scrollable adjustments are based on the minimum size
;;; 
;;; GTK_SCROLL_NATURAL
;;;     Scrollable adjustments are based on the natural size
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkScrollablePolicy" gtk-scrollable-policy
  (:export t
   :type-initializer "gtk_scrollable_policy_get_type")
  (:minimum 0)
  (:natural 1))

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_hscroll_policy ()
;;; 
;;; GtkScrollablePolicy gtk_scrollable_get_hscroll_policy
;;;                                                  (GtkScrollable *scrollable)
;;; 
;;; Gets the horizontal GtkScrollablePolicy.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; Returns :
;;;     The horizontal GtkScrollablePolicy
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-hscroll-policy))

(defun gtk-scrollable-get-hscroll-policy (scrollable)
  (gtk-scrollable-hscroll-policy scrollable))

(export 'gtk-scrollable-get-hscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_hscroll_policy ()
;;; 
;;; void gtk_scrollable_set_hscroll_policy (GtkScrollable *scrollable,
;;;                                         GtkScrollablePolicy policy);
;;; 
;;; Sets the GtkScrollablePolicy to determine whether horizontal scrolling
;;; should start below the minimum width or below the natural width.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; policy :
;;;     the horizontal GtkScrollablePolicy
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-hscroll-policy))

(defun gtk-scrollable-set-hscroll-policy (scrollable policy)
  (setf (gtk-scrollable-hscroll-policy scrollable) policy))

(export 'gtk-scrollable-set-hscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_get_vscroll_policy ()
;;; 
;;; GtkScrollablePolicy gtk_scrollable_get_vscroll_policy
;;;                                                  (GtkScrollable *scrollable)
;;; 
;;; Gets the vertical GtkScrollablePolicy.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; Returns :
;;;     The vertical GtkScrollablePolicy
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-get-vscroll-policy))

(defun gtk-scrollable-get-vscroll-policy (scrollable)
  (gtk-scrollable-vscroll-policy scrollable))

(export 'gtk-scrollable-get-vscroll-policy)

;;; ----------------------------------------------------------------------------
;;; gtk_scrollable_set_vscroll_policy ()
;;; 
;;; void gtk_scrollable_set_vscroll_policy (GtkScrollable *scrollable,
;;;                                         GtkScrollablePolicy policy);
;;; 
;;; Sets the GtkScrollablePolicy to determine whether vertical scrolling should
;;; start below the minimum height or below the natural height.
;;; 
;;; scrollable :
;;;     a GtkScrollable
;;; 
;;; policy :
;;;     the vertical GtkScrollablePolicy
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-scrollable-set-vscroll-policy))

(defun gtk-scrollable-set-vscroll-policy (scrollable policy)
  (setf (gtk-scrollable-vscroll-policy scrollable) policy))

(export 'gtk-scrollable-set-vscroll-policy)

;;; --- End of file gtk.scrollable.lisp ----------------------------------------

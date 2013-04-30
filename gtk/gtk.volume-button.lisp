;;; ----------------------------------------------------------------------------
;;; gtk.volume-button.lisp
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
;;; GtkVolumeButton
;;; 
;;; A button which pops up a volume control
;;;     
;;; Synopsis
;;; 
;;;     GtkVolumeButton
;;;     
;;;     gtk_volume_button_new
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkScaleButton
;;;                                        +----GtkVolumeButton
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkVolumeButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkVolumeButton" gtk-volume-button
  (:superclass gtk-scale-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkOrientable")
   :type-initializer "gtk_volume_button_get_type")
  ((use-symbolic
    gtk-volume-button-use-symbolic
    "use-symbolic" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-volume-button 'type)
 "@version{2013-4-27}
  @begin{short}
    @sym{gtk-volume-button} is a subclass of @class{gtk-scale-button} that has
    been tailored for use as a volume control widget with suitable icons,
    tooltips and accessible labels.
  @end{short}
  @see-slot{gtk-volume-button-use-symbolic}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "use-symbolic"
                                               'gtk-volume-button) 't)
 "The @code{\"use-symbolic\"} property of type @code{:boolean}
  (Read / Write)@br{}
  Whether to use symbolic icons as the icons. Note that if the symbolic icons
  are not available in your installed theme, then the normal (potentially
  colorful) icons will be used. @br{}
  Default value: @code{nil}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors of Properties
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-volume-button-use-symbolic atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-volume-button-use-symbolic 'function)
 "@version{2013-4-27}
  Accessor of the slot @code{\"use-symbolic\"} of the @class{gtk-volume-button}
  class.")

;;; ----------------------------------------------------------------------------
;;; gtk_volume_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-volume-button-new))

(defun gtk-volume-button-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-4-27}
  @return{A new @class{gtk-volume-button} widget.}
  @begin{short}
    Creates a @class{gtk-volume-button}, with a range between 0.0 and 1.0, with
    a stepping of 0.02. Volume values can be obtained and modified using the
    functions from @class{gtk-scale-button}.
  @end{short}

  Since 2.12"
  (make-instance 'gtk-volume-button))

(export 'gtk-volume-button-new)

;;; --- End of file gtk.volume-button.lisp -------------------------------------

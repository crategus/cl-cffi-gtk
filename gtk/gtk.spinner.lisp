;;; ----------------------------------------------------------------------------
;;; gtk.spinner.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
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
;;; GtkSpinner
;;; 
;;; Show a spinner animation
;;;     
;;; Synopsis
;;; 
;;;     GtkSpinner
;;;     
;;;     gtk_spinner_new
;;;     gtk_spinner_start
;;;     gtk_spinner_stop
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkSpinner
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkSpinner implements AtkImplementorIface and GtkBuildable.
;;;
;;; Properties
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Description
;;; 
;;; A GtkSpinner widget displays an icon-size spinning animation. It is often
;;; used as an alternative to a GtkProgressBar for displaying indefinite
;;; activity, instead of actual progress.
;;; 
;;; To start the animation, use gtk_spinner_start(), to stop it use
;;; gtk_spinner_stop().
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "active" property
;;; 
;;;   "active"                   gboolean              : Read / Write
;;; 
;;; Whether the spinner is active.
;;; 
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSpinner
;;; 
;;; struct GtkSpinner;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSpinner" gtk-spinner
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_spinner_get_type")
  ((active
    gtk-spinner-active
    "active" "gboolean" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_new ()
;;; 
;;; GtkWidget * gtk_spinner_new (void);
;;; 
;;; Returns a new spinner widget. Not yet started.
;;; 
;;; Returns :
;;;     a new GtkSpinner
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-spinner-new))

(defun gtk-spinner-new ()
  (make-instance 'gtk-spinner))

(export 'gtk-spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_start ()
;;; 
;;; void gtk_spinner_start (GtkSpinner *spinner);
;;; 
;;; Starts the animation of the spinner.
;;; 
;;; spinner :
;;;     a GtkSpinner
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_start" gtk-spinner-start) :void
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_stop ()
;;; 
;;; void gtk_spinner_stop (GtkSpinner *spinner);
;;; 
;;; Stops the animation of the spinner.
;;; 
;;; spinner :
;;;     a GtkSpinner
;;; 
;;; Since 2.20
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_stop" gtk-spinner-stop) :void
  (spinner (g-object gtk-spinner)))

(export 'gtk-spinner-stop)

;;; --- End of file gtk.spinner.lisp -------------------------------------------

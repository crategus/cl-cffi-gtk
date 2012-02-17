;;; ----------------------------------------------------------------------------
;;; gtk.check-button.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkCheckButton
;;; 
;;; Create widgets with a discrete toggle button
;;; 
;;; Synopsis
;;; 
;;;     GtkCheckButton
;;;     
;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkContainer
;;;                      +----GtkBin
;;;                            +----GtkButton
;;;                                  +----GtkToggleButton
;;;                                        +----GtkCheckButton
;;;                                              +----GtkRadioButton
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkCheckButton implements AtkImplementorIface, GtkBuildable and
;;; GtkActivatable.
;;;
;;; Style Properties
;;; 
;;;   "indicator-size"           gint                  : Read
;;;   "indicator-spacing"        gint                  : Read
;;; 
;;; Description
;;; 
;;; A GtkCheckButton places a discrete GtkToggleButton next to a widget,
;;; (usually a GtkLabel). See the section on GtkToggleButton widgets for more
;;; information about toggle/check buttons.
;;; 
;;; The important signal ( "toggled" ) is also inherited from GtkToggleButton.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "indicator-size" style property
;;; 
;;;   "indicator-size"           gint                  : Read
;;; 
;;; Size of check or radio indicator.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 16
;;;
;;; ----------------------------------------------------------------------------
;;; The "indicator-spacing" style property
;;; 
;;;   "indicator-spacing"        gint                  : Read
;;; 
;;; Spacing around check or radio indicator.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 2
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCheckButton
;;; 
;;; struct GtkCheckButton;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCheckButton" gtk-check-button
  (:superclass gtk-toggle-button
   :export t
   :interfaces ("AtkImplementorIface" "GtkActivatable" "GtkBuildable")
   :type-initializer "gtk_check_button_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new ()
;;; 
;;; GtkWidget * gtk_check_button_new (void);
;;; 
;;; Creates a new GtkCheckButton.
;;; 
;;; Returns :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

(defun gtk-check-button-new ()
  (make-instance 'gtk-check-button))

(export 'gkt-check-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_label ()
;;; 
;;; GtkWidget * gtk_check_button_new_with_label (const gchar *label);
;;; 
;;; Creates a new GtkCheckButton with a GtkLabel to the right of it.
;;; 
;;; label :
;;;     the text for the check button.
;;; 
;;; Returns :
;;;     a GtkWidget.
;;; ----------------------------------------------------------------------------

(defun gtk-check-button-new-with-label (label)
  (make-instance 'gtk-check-button :label label))

(export 'gtk-check-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_mnemonic ()
;;; 
;;; GtkWidget * gtk_check_button_new_with_mnemonic  (const gchar *label);
;;; 
;;; Creates a new GtkCheckButton containing a label. The label will be created
;;; using gtk_label_new_with_mnemonic(), so underscores in label indicate the
;;; mnemonic for the check button.
;;; 
;;; label :
;;;     the text of the button, with an underscore in front of the mnemonic
;;;     character
;;; 
;;; Returns :
;;;     a new GtkCheckButton
;;; ----------------------------------------------------------------------------

(defun gtk-check-button-new-with-mnemonic (label)
  (make-instance 'gtk-check-button
                 :label label
                 :use-underline t))

(export 'gtk-check-button-new-with-mnemonic)

;;; --- End of file gtk.check-button.lisp --------------------------------------

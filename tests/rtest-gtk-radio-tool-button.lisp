;;; ----------------------------------------------------------------------------
;;; rtest-gtk-radio-tool-button.lisp
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk-tests)

(define-test gtk-radio-tool-button
  (let ((button nil)
        (group nil))
    ;; Create a radio tool button with gtk-radio-tool-button-new
    (setq button (gtk-radio-tool-button-new nil))
    (assert-equal (gtype "GtkRadioToolButton") (g-type-from-instance button))
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 1 (length group))
    (assert-equal (gtype "GtkRadioButton")
                  (g-type-from-instance (first group)))
    ;; Add a second radio tool button with gtk-radio-tool-button-new
    (setq button (gtk-radio-tool-button-new group))
    (assert-equal (gtype "GtkRadioToolButton") (g-type-from-instance button))
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 2 (length group))
    ;; Add a radio tool button with gtk-radio-tool-button-new-from-stock
    (setq button (gtk-radio-tool-button-new-from-stock group "gtk-ok"))
    (assert-equal (gtype "GtkRadioToolButton") (g-type-from-instance button))
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 3 (length group))
    ;; Add a radio tool button with gtk-radio-tool-button-new-from-widget
    (setq button (gtk-radio-tool-button-new-from-widget button))
    (assert-equal (gtype "GtkRadioToolButton") (g-type-from-instance button))
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 4 (length group))
    ;; Add a radio tool button with gtk-radio-tool-button-new-with-stock-from-widget
    (setq button (gtk-radio-tool-button-new-with-stock-from-widget button "gtk-ok"))
    (assert-equal (gtype "GtkRadioToolButton") (g-type-from-instance button))
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 5 (length group))
    ;; Create a radio tool button and add it to the existing group
    (setq button (gtk-radio-tool-button-new nil))
    (gtk-radio-tool-button-set-group button group)
    (setq group (gtk-radio-tool-button-get-group button))
    (assert-eql 6 (length group))
  ))

;;; --- End of file rtest-gtk-radio-tool-button.lisp ---------------------------

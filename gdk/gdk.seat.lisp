;;; ----------------------------------------------------------------------------
;;; gdk.seat.lisp
;;;
;;; Copyright (C) 2018 Sébastien Villemot
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
;;; GdkSeat
;;;
;;; Object representing an user seat
;;;
;;; Types and Values
;;;
;;;     GdkSeat
;;;     GdkSeatCapabilities
;;;
;;; Functions
;;;
;;;     GdkSeatGrabPrepareFunc
;;;     gdk_seat_get_display                   -> Accessor
;;;     gdk_seat_grab
;;;     gdk_seat_ungrab
;;;     gdk_seat_get_capabilities
;;;     gdk_seat_get_pointer
;;;     gdk_seat_get_keyboard
;;;     gdk_seat_get_slaves

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkSeat
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(define-g-object-class "GdkSeat" gdk-seat
  (:superclass g-object
   :export t
   :interfaces nil
   :type-initializer "gdk_seat_get_type")
  ((display
    gdk-seat-display
    "display" "GdkDisplay" t t)))

;;; ----------------------------------------------------------------------------
;;; GdkSeatCapabilities
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(define-g-flags "GdkSeatCapabilities" gdk-seat-capabilities
  (:export t
   :type-initializer "gdk_seat_capabilities_get_type")
  (:none 0)
  (:pointer 1)
  (:touch 2)
  (:tablet-stylus 4)
  (:keyboard 8)
  (:all-pointing 7)
  (:all 15))

;;; ----------------------------------------------------------------------------
;;; GdkSeatGrabPrepareFunc
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcallback gdk-seat-grab-prepare-func-cb :void
    ((seat (g-object gdk-seat))
     (window (g-object gdk-window))
     (user-data :pointer))
  (let ((fn (glib::get-stable-pointer-value user-data)))
    (funcall fn seat window)))

;;; ----------------------------------------------------------------------------
;;; gdk_seat_grab ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun ("gdk_seat_grab" %gdk-seat-grab) gdk-grab-status
  (seat (g-object gdk-seat))
  (window (g-object gdk-window))
  (capabilities gdk-seat-capabilities)
  (owner-events :boolean)
  (cursor (g-object gdk-cursor))
  (event (g-boxed-foreign gdk-event))
  (prepare-func :pointer)
  (prepare-func-data :pointer))

#+gdk-3-20
(defun gdk-seat-grab (seat window capabilities owner-events &key cursor event prepare-func)
  (if prepare-func
      (with-stable-pointer (ptr prepare-func)
        (%gdk-seat-grab seat window capabilities owner-events cursor event
                        (callback gdk-seat-grab-prepare-func-cb) ptr))
      (%gdk-seat-grab seat window capabilities owner-events cursor event
                      (null-pointer) (null-pointer))))

#+gdk-3-20
(export 'gdk-seat-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_ungrab ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun gdk-seat-ungrab :void
  (seat (g-object gdk-seat)))

#+gdk-3-20
(export 'gdk-seat-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_capabilities ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun gdk-seat-get-capabilities gdk-seat-capabilities
  (seat (g-object gdk-seat)))

#+gdk-3-20
(export 'gdk-seat-get-capabilities)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_pointer ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun gdk-seat-get-pointer (g-object gdk-device)
  (seat (g-object gdk-seat)))

#+gdk-3-20
(export 'gdk-seat-get-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_keyboard ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun gdk-seat-get-keyboard (g-object gdk-device)
  (seat (g-object gdk-seat)))

#+gdk-3-20
(export 'gdk-seat-get-keyboard)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_slaves ()
;;; ----------------------------------------------------------------------------

#+gdk-3-20
(defcfun gdk-seat-get-slaves (g-list (g-object gdk-device))
  (seat (g-object gdk-seat))
  (capabilities gdk-seat-capabilities))

#+gdk-3-20
(export 'gdk-seat-get-slaves)

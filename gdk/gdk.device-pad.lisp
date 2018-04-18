;;; ----------------------------------------------------------------------------
;;; gdk.device-pad.lisp
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
;;; GdkDevicePad
;;;
;;; Pad device interface
;;;
;;; Types and Values
;;;
;;;     GdkDevicePad
;;;     GdkDevicePadFeature
;;;
;;; Functions
;;;
;;;     gdk_device_pad_get_n_groups
;;;     gdk_device_pad_get_group_n_modes
;;;     gdk_device_pad_get_n_features
;;;     gdk_device_pad_get_feature_group

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDevicePad
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(define-g-interface "GdkDevicePad" gdk-device-pad
  (:type-initializer "gdk_device_pad_get_type"))

;;; ----------------------------------------------------------------------------
;;; GdkDevicePadFeature
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(define-g-enum "GdkDevicePadFeature" gdk-device-pad-feature
  (:type-initializer "gdk_device_pad_feature_get_type")
  :button
  :ring
  :strip)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_groups ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun gdk-device-pad-get-n-groups :int
  (pad (g-object gdk-device-pad)))

#+gdk-3-22
(export 'gdk-device-pad-get-n-groups)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_group_n_modes ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun gdk-device-pad-get-group-n-modes :int
  (pad (g-object gdk-device-pad))
  (group-idx :int))

#+gdk-3-22
(export 'gdk-device-pad-get-group-n-modes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_n_features ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun gdk-device-pad-get-n-features :int
  (pad (g-object gdk-device-pad))
  (feature gdk-device-pad-feature))

#+gdk-3-22
(export 'gdk-device-pad-get-n-features)

;;; ----------------------------------------------------------------------------
;;; gdk_device_pad_get_feature_group ()
;;; ----------------------------------------------------------------------------

#+gdk-3-22
(defcfun gdk-device-pad-get-feature-group :int
  (pad (g-object gdk-device-pad))
  (feature gdk-device-pad-feature)
  (feature-idx :int))

#+gdk-3-22
(export 'gdk-device-pad-get-feature-group)

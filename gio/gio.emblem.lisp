;;; ----------------------------------------------------------------------------
;;; gio.emblem.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.68 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 - 2021 Dieter Kaiser
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
;;; GEmblem
;;;
;;;     An object for emblems
;;;
;;; Types and Values
;;;
;;;     GEmblem
;;;     GEmblemOrigin
;;;
;;; Functions
;;;
;;;     g_emblem_new
;;;     g_emblem_new_with_origin
;;;     g_emblem_get_icon
;;;     g_emblem_get_origin
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ╰── GEmblemOrigin
;;;
;;;     GObject
;;;     ╰── GEmblem
;;;
;;; Implemented Interfaces
;;;
;;;     GEmblem implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; enum GEmblemOrigin
;;; ----------------------------------------------------------------------------

(define-g-enum "GEmblemOrigin" g-emblem-origin
  (:export t
   :type-initializer "g_emblem_origin_get_type")
  :unknown
  :device
  :livemetadata
  :tag)

#+cl-cffi-gtk-documentation
(setf (gethash 'g-emblem-origin atdoc:*symbol-name-alias*)
      "GEnum"
      (gethash 'g-emblem-origin atdoc:*external-symbols*)
 "@version{2021-4-25}
  @begin{short}
    The @sym{g-emblem-origin} enumeration is used to add information about the
    origin of the emblem to a @class{g-emblem} object.
  @end{short}
  @begin{pre}
(define-g-enum \"GEmblemOrigin\" g-emblem-origin
  (:export t
   :type-initializer \"g_emblem_origin_get_type\")
  :unknown
  :device
  :livemetadata
  :tag)
  @end{pre}
  @begin[code]{table}
    @entry[:unkown]{Emblem of unknown origin.}
    @entry[:device]{Emblem adds device-specific information.}
    @entry[:livedata]{Emblem depicts live metadata, such as \"readonly\".}
    @entry[:tag]{Emblem comes from a user-defined tag, e.g. set by nautilus
      (in the future).}
  @end{table}
  @see-class{g-emblem}")

;;; ----------------------------------------------------------------------------
;;; GEmblem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GEmblem" g-emblem
  (:superclass g-object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblem_get_type")
  ((icon
    g-emblem-icon
    "icon" "GIcon" t t)
   (origin
    g-emblem-origin
    "orign" "GEmblemOrigin" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-emblem 'type)
 "@version{*2021-4-27}
  @begin{short}
    The @sym{g-emblem} class is an implementation of the @class{g-icon} class
    that supports having an emblem, which is an icon with additional properties.
  @end{short}
  It can than be added to a @class{g-emblemed-icon} object.

  Currently, only metainformation about the emblem's origin is supported. More
  may be added in the future.
  @see-slot{g-emblem-icon}
  @see-slot{g-emblem-origin}
  @see-class{g-icon}
  @see-class{g-emblemed-icon}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- g-emblem-icon ----------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "icon" 'g-emblem) 't)
 "The @code{icon} property of tpye @class{g-object}
  (Read / Write / Construct Only) @br{}
  The actual icon of the emblem.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-emblem-icon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-emblem-icon 'function)
 "@version{*2021-4-27}
  @argument[object]{a @class{g-emblem} object from which the icon should be
    extracted}
  @begin{short}
    Accessor of the @slot[g-emblem]{icon} slot of the @class{g-emblem} class.
  @end{short}

  The slot access function @sym{g-emblem-icon} gives back the icon from the
  emblem.
  @see-class{g-emblem}")

;;; --- g-emblem-origin --------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "origin" 'g-emblem) 't)
 "The @code{origin} property of type @symbol{g-emblem-origin}
  (Read / Write / Construct Only) @br{}
  Tells which origin the emblem is derived from. @br{}
  Default value: @code{:unkown}")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-emblem-origin atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-emblem-origin 'function)
 "@version{2021-4-25}
  @argument[object]{a @class{g-emblem} object}
  @begin{short}
    Accessor of the @slot[g-emblem]{origin} slot of the @class{g-emblem} class.
  @end{short}

  The slot access function @sym{g-emblem-origin} gets the origin of the emblem.
  @see-class{g-emblem}")

;;; ----------------------------------------------------------------------------
;;; g_emblem_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblem_new" g-emblem-new) (g-object g-emblem)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[icon]{a @class{g-icon} object containing the icon}
  @return{A new @class{g-emblem} object.}
  @begin{short}
    Creates a new emblem for icon.
  @end{short}
  @see-class{g-emblem}"
  (icon (g-object g-icon)))

(export 'g-emblem-new)

;;; ----------------------------------------------------------------------------
;;; g_emblem_new_with_origin ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblem_new_with_origin" g-emblem-new-with-origin)
    (g-object g-emblem)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[icon]{a @class{g-icon} object containing the icon}
  @argument[origin]{a @symbol{g-emblem-origin} value defining the emblem's
    origin}
  @return{A new @class{g-emblem} object.}
  @begin{short}
    Creates a new emblem for icon.
  @end{short}
  @see-class{g-emblem}"
  (icon (g-object g-icon))
  (origin g-emblem-origin))

(export 'g-emblem-new-with-origin)

;;; --- End of file gio.emblem.lisp --------------------------------------------

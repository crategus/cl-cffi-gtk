;;; ----------------------------------------------------------------------------
;;; gio.emblemed-icon.lisp
;;;
;;; The documentation of this file is taken from the GIO Reference Manual
;;; Version 2.40 and modified to document the Lisp binding to the GIO library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2014 Dieter Kaiser
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
;;; GEmblemedIcon
;;;
;;; Icon with emblems
;;;
;;; Synopsis
;;;
;;;     GEmblemedIcon
;;;
;;;     g_emblemed_icon_new
;;;     g_emblemed_icon_get_icon
;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems
;;; ----------------------------------------------------------------------------

(in-package :gio)

;;; ----------------------------------------------------------------------------
;;; struct GEmblemedIcon
;;; ----------------------------------------------------------------------------

(define-g-object-class "GEmblemedIcon" g-emblemed-icon
  (:superclass g-object
   :export t
   :interfaces ("GIcon")
   :type-initializer "g_emblemed_icon_get_type")
  ((gicon
    g-emblemed-icon-gicon
    "gicon" "GIcon" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'g-emblemed-icon 'type)
 "@version{2014-1-5}
  @begin{short}
    @sym{g-emblemed-icon} is an implementation of @class{g-icon} that supports
    adding an emblem to an icon. Adding multiple emblems to an icon is ensured
    via the function @fun{g-emblemed-icon-add-emblem}.
  @end{short}

  Note that @sym{g-emblemed-icon} allows no control over the position of the
  emblems. See also @class{g-emblem} for more information.
  @see-slot{g-emblemed-icon-gicon}
  @see-class{g-emblem}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'g-emblemed-icon) 't)
 "The @code{gicon} property of type @class{g-icon}
  (Read / Write / Construct Only) @br{}
  The @class{g-icon} to attach emblems to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-emblemed-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-emblemed-icon-gicon 'function)
 "@version{2014-9-22}
  Accessor of the @slot[g-emblemed-icon]{gicon} slot of the
  @class{g-emblemed-icon} class.
  @see-class{g-emblemed-icon}
  @see-function{g-emblemed-icon-get-icon}")

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_new ()
;;;
;;; GIcon * g_emblemed_icon_new (GIcon *icon, GEmblem *emblem);
;;;
;;; Creates a new emblemed icon for icon with the emblem emblem.
;;;
;;; icon :
;;;     a GIcon
;;;
;;; emblem :
;;;     a GEmblem, or NULL
;;;
;;; Returns :
;;;     a new GIcon
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_icon ()
;;; ----------------------------------------------------------------------------

(declaim (inline g-emblemed-icon-get-icon))

(defun g-emblemed-icon-get-icon (emblemed)
 #+cl-cffi-gtk-documentation
 "@version{2014-2-2}
  @argument[emblemed]{a @class{g-emblemed-icon}}
  @return{A @class{g-icon} that is owned by @arg{emblemed}.}
  @short{Gets the main icon for @arg{emblemed}.}

  Since 2.18
  @see-class{g-emblemed-icon}
  @see-class{g-icon}"
  (g-emblemed-icon-gicon emblemed))

(export 'g-emblemed-icon-get-icon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_emblems ()
;;;
;;; GList * g_emblemed_icon_get_emblems (GEmblemedIcon *emblemed);
;;;
;;; Gets the list of emblems for the icon.
;;;
;;; emblemed :
;;;     a GEmblemedIcon
;;;
;;; Returns :
;;;     a GList of GEmblem's that is owned by emblemed.
;;;
;;; Since 2.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_add_emblem ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_add_emblem" g-emblemed-icon-add-emblem) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-2-2}
  @argument[emblemed]{a @class{g-emblemed-icon}}
  @argument[emblem]{a @class{g-emblem}}
  @short{Adds @arg{emblem} to the list of @class{g-emblem}'s.}

  Since 2.18
  @see-class{g-emblemed-icon}
  @see-class{g-embleme}"
  (emblemed (g-object g-emblemed-icon))
  (emblem (g-object g-emblem)))

(export 'g-emblemed-icon-add-emblem)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_clear_emblems ()
;;;
;;; void g_emblemed_icon_clear_emblems (GEmblemedIcon *emblemed);
;;;
;;; Removes all the emblems from icon.
;;;
;;; emblemed :
;;;     a GEmblemedIcon
;;;
;;; Since 2.28
;;; ----------------------------------------------------------------------------

;;; --- End of file gio.emblemed-icon.lisp -------------------------------------

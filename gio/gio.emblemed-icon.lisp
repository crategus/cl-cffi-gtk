;;; ----------------------------------------------------------------------------
;;; gio.emblemed-icon.lisp
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
;;; GEmblemedIcon
;;;
;;;     Icon with emblems
;;;
;;; Types and Values
;;;
;;;     GEmblemedIcon
;;;
;;; Functions
;;;
;;;     g_emblemed_icon_new
;;;     g_emblemed_icon_get_icon
;;;     g_emblemed_icon_get_emblems
;;;     g_emblemed_icon_add_emblem
;;;     g_emblemed_icon_clear_emblems
;;;
;;; Properties
;;;
;;;     GIcon*   gicon    Read / Write / Construct Only
;;;
;;; Implemented Interfaces
;;;
;;;     GEmblemedIcon implements GIcon.
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
 "@version{*2021-4-27}
  @begin{short}
    The @sym{g-emblemed-icon} class is an implementation of the @class{g-icon}
    interface that supports adding an emblem to an icon.
  @end{short}
  Adding multiple emblems to an icon is ensured via the function
  @fun{g-emblemed-icon-add-emblem}.

  Note that the @sym{g-emblemed-icon} class allows no control over the position
  of the emblems. See also the @class{g-emblem} class for more information.
  @see-slot{g-emblemed-icon-gicon}
  @see-class{g-icon}
  @see-class{g-emblem}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "gicon" 'g-emblemed-icon) 't)
 "The @code{gicon} property of type @class{g-icon}
  (Read / Write / Construct Only) @br{}
  The icon to attach emblems to.")

#+cl-cffi-gtk-documentation
(setf (gethash 'g-emblemed-icon-gicon atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'g-emblemed-icon-gicon 'function)
 "@version{2021-4-25}
  @syntax[]{(g-emblemed-icon-gicon object) => gicon}
  @syntax[]{(setf (g-emblemend-icon-gicon object) gicon)}
  @argument[object]{a @class{g-emblemed-icon} object}
  @argument[gicon]{a @class{g-icon} object to attach emblems to}
  @begin{short}
    Accessor of the @slot[g-emblemed-icon]{gicon} slot of the
    @class{g-emblemed-icon} class.
  @end{short}
  @see-class{g-emblemed-icon}
  @see-function{g-emblemed-icon-icon}")

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_new" g-emblemed-icon-new) (g-object g-icon)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[icon]{a @class{g-icon} object}
  @argument[emblem]{a @class{g-emblem} object, or @code{nil}}
  @begin{short}
    Creates a new emblemed icon for @arg{icon} with the emblem @arg{emblem}.
  @end{short}
  @see-class{g-emblemed-icon}"
  (icon (g-object g-icon))
  (emblem (g-object g-emblem)))

(export 'g-emblemed-icon-new)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_icon () -> g-emblemed-icon-icon
;;; ----------------------------------------------------------------------------

(declaim (inline g-emblemed-icon-icon))

(defun g-emblemed-icon-icon (emblemed)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[emblemed]{a @class{g-emblemed-icon} object}
  @return{A @class{g-icon} object that is owned by @arg{emblemed}.}
  @short{Gets the main icon for @arg{emblemed}.}
  @see-class{g-emblemed-icon}
  @see-class{g-icon}"
  (g-emblemed-icon-gicon emblemed))

(export 'g-emblemed-icon-icon)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_get_emblems () -> g-emblemed-icon-emblems
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_get_emblems" g-emblemed-icon-emblems)
    (g-list (g-object g-emblem) :free-from-foreign nil)
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[emblemed]{a @class{g-emblemed-icon} object}
  @return{A list of @class{g-emblem} objects that is owned by @arg{emblemed}.}
  @begin{short}
    Gets the list of emblems for the icon.
  @end{short}
  @see-class{g-emblemed-icon}
  @see-class{g-emblem}"
  (emblemed (g-object g-emblemed-icon)))

(export 'g-emblemed-icon-emblems)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_add_emblem ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_add_emblem" g-emblemed-icon-add-emblem) :void
 #+cl-cffi-gtk-documentation
 "@version{*2021-4-27}
  @argument[emblemed]{a @class{g-emblemed-icon} object}
  @argument[emblem]{a @class{g-emblem} object}
  @short{Adds @arg{emblem} to the list of @class{g-emblem} objects.}
  @see-class{g-emblemed-icon}
  @see-class{g-embleme}"
  (emblemed (g-object g-emblemed-icon))
  (emblem (g-object g-emblem)))

(export 'g-emblemed-icon-add-emblem)

;;; ----------------------------------------------------------------------------
;;; g_emblemed_icon_clear_emblems ()
;;; ----------------------------------------------------------------------------

(defcfun ("g_emblemed_icon_clear_emblems" g-emblemed-icon-clear-emblems) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-4-25}
  @argument[emblemed]{a @class{g-emblemed-icon} object}
  @begin{short}
    Removes all the emblems from icon.
  @end{short}
  @see-class{g-emblemed-icon}"
  (emblemed (g-object g-emblemed-icon)))

(export 'g-emblemed-icon-clear-emblems)

;;; --- End of file gio.emblemed-icon.lisp -------------------------------------

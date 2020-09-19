;;; ----------------------------------------------------------------------------
;;; gtk.im-multicontext.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2020 Dieter Kaiser
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
;;; GtkIMMulticontext
;;;
;;;     An input method context supporting multiple, loadable input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMMulticontext
;;;
;;; Functions
;;;
;;;     gtk_im_multicontext_new
;;;     gtk_im_multicontext_append_menuitems
;;;     gtk_im_multicontext_get_context_id
;;;     gtk_im_multicontext_set_context_id
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIMMulticontext" gtk-im-multicontext
  (:superclass gtk-im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_multicontext_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-im-multicontext 'type)
 "@version{2020-9-15}
  @begin{short}
    An input method context supporting multiple, loadable input methods.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_new ()
;;;
;;; GtkIMContext * gtk_im_multicontext_new (void);
;;;
;;; Creates a new GtkIMMulticontext.
;;;
;;; Returns :
;;;     a new GtkIMMulticontext.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_append_menuitems ()
;;;
;;; void gtk_im_multicontext_append_menuitems (GtkIMMulticontext *context,
;;;                                            GtkMenuShell *menushell);
;;;
;;; Add menuitems for various available input methods to a menu; the menuitems,
;;; when selected, will switch the input method for the context and the global
;;; default input method.
;;;
;;; Warning
;;;
;;; gtk_im_multicontext_append_menuitems has been deprecated since version 3.10
;;; and should not be used in newly-written code.
;;;
;;; It is better to use the system-wide input method framework for changing
;;; input methods. Modern desktop shells offer on-screen displays for this that
;;; can triggered with a keyboard shortcut, e.g. Super-Space.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; menushell :
;;;     a GtkMenuShell
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_get_context_id ()
;;;
;;; const char * gtk_im_multicontext_get_context_id (GtkIMMulticontext *context)
;;;
;;; Gets the id of the currently active slave of the context.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; Returns :
;;;     the id of the currently active slave
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_multicontext_set_context_id ()
;;;
;;; void gtk_im_multicontext_set_context_id (GtkIMMulticontext *context,
;;;                                          const char *context_id);
;;;
;;; Sets the context id for context.
;;;
;;; This causes the currently active slave of context to be replaced by the
;;; slave corresponding to the new context id.
;;;
;;; context :
;;;     a GtkIMMulticontext
;;;
;;; context_id :
;;;     the id to use
;;;
;;; Since 2.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.im-multicontext.lisp -----------------------------------

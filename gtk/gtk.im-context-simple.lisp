;;; ----------------------------------------------------------------------------
;;; gtk.im-context-simple.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2011 - 2019 Dieter Kaiser
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
;;; GtkIMContextSimple
;;;
;;;     An input method context supporting table-based input methods
;;;
;;; Types and Values
;;;
;;;     GtkIMContextSimple
;;;
;;;     GTK_MAX_COMPOSE_LEN
;;;
;;;
;;; Functions
;;;
;;;     gtk_im_context_simple_new
;;;     gtk_im_context_simple_add_table
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ╰── GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_MAX_COMPOSE_LEN
;;;
;;; #define GTK_MAX_COMPOSE_LEN 7
;;;
;;; The maximum length of sequences in compose tables.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkIMContextSimple
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIMContextSimple" gtk-im-context-simple
  (:superclass gtk-im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_context_simple_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-im-context-simple 'type)
 "@version{2020-9-15}
  @begin{short}
    @sym{gtk-im-context-simple} is a simple input method context supporting
    table-based input methods.
  @end{short}
  It has a built-in table of compose sequences that is derived from the X11
  Compose files.

  @sym{gtk-im-context-simple} reads additional compose sequences from the first
  of the following files that is found: @code{~/.config/gtk-3.0/Compose},
  @code{~/.XCompose}, @code{/usr/share/X11/locale/$locale/Compose}, for locales
  that have a nontrivial Compose file. The syntax of these files is described
  in the Compose(5) manual page.

  @sym{gtk-im-context-simple} also supports numeric entry of Unicode characters
  by typing Ctrl-Shift-u, followed by a hexadecimal Unicode codepoint. For
  example, Ctrl-Shift-u 1 2 3 Enter yields U+0123 LATIN SMALL LETTER G WITH
  CEDILLA, i.e. ģ.
  @see-class{gtk-im-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_new ()
;;;
;;; GtkIMContext * gtk_im_context_simple_new (void);
;;;
;;; Creates a new GtkIMContextSimple.
;;;
;;; Returns :
;;;     a new GtkIMContextSimple.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_simple_add_table ()
;;;
;;; void gtk_im_context_simple_add_table (GtkIMContextSimple *context_simple,
;;;                                       guint16 *data,
;;;                                       gint max_seq_len,
;;;                                       gint n_seqs);
;;;
;;; Adds an additional table to search to the input context. Each row of the
;;; table consists of max_seq_len key symbols followed by two guint16
;;; interpreted as the high and low words of a gunicode value. Tables are
;;; searched starting from the last added.
;;;
;;; The table must be sorted in dictionary order on the numeric value of the key
;;; symbol fields. (Values beyond the length of the sequence should be zero.)
;;;
;;; context_simple :
;;;     A GtkIMContextSimple
;;;
;;; data :
;;;     the table
;;;
;;; max_seq_len :
;;;     Maximum length of a sequence in the table (cannot be greater than
;;;     GTK_MAX_COMPOSE_LEN)
;;;
;;; n_seqs :
;;;     number of sequences in the table
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.im-context-simple.lisp ---------------------------------

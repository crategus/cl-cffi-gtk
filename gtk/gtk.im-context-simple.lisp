;;; ----------------------------------------------------------------------------
;;; gtk.im-context-simple.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See http://www.gtk.org.
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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
;;; An input method context supporting table-based input methods
;;;
;;; Synopsis
;;;
;;;     GtkIMContextSimple
;;;
;;;     gtk_im_context_simple_new
;;;     gtk_im_context_simple_add_table
;;;
;;;     GTK_MAX_COMPOSE_LEN
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GtkIMContext
;;;          +----GtkIMContextSimple
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkIMContextSimple
;;;
;;; struct GtkIMContextSimple;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkIMContextSimple" gtk-im-context-simple
  (:superclass gtk-im-context
   :export t
   :interfaces nil
   :type-initializer "gtk_im_context_simple_get_type")
  nil)

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

;;; ----------------------------------------------------------------------------
;;; GTK_MAX_COMPOSE_LEN
;;;
;;; #define GTK_MAX_COMPOSE_LEN 7
;;;
;;; The maximum length of sequences in compose tables.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.im-context-simple.lisp ---------------------------------

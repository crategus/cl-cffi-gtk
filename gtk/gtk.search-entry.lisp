;;; ----------------------------------------------------------------------------
;;; gtk.seach-entry.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013, 2014, 2015 Dieter Kaiser
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
;;; GtkSearchEntry
;;;
;;;     An entry which shows a search icon
;;;
;;; Functions
;;;
;;;     gtk_search_entry_new
;;;     gtk_search_entry_handle_event
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkEntry
;;;                      +----GtkSearchEntry
;;;
;;; Implemented Interfaces
;;;
;;; GtkSearchEntry implements AtkImplementorIface, GtkBuildable, GtkEditable
;;; and GtkCellEditable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSearchEntry
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSearchEntry" gtk-search-entry
  (:superclass gtk-entry
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkEditable"
                "GtkCellEditable")
   :type-initializer "gtk_search_entry_get_type")
  nil)

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-search-entry 'type)
 "@version{2015-12-28}
  @begin{short}
    @sym{gtk-search-entry} is a subclass of @class{gtk-entry} that has been
    tailored for use as a search entry.
  @end{short}

  It will show an inactive symbolic \"find\" icon when the search entry is
  empty, and a symbolic \"clear\" icon when there is text. Clicking on the
  \"clear\" icon will empty the search entry.

  Note that the search/clear icon is shown using a secondary icon, and thus
  does not work if you are using the secondary icon position for some other
  purpose.

  To make filtering appear more reactive, it is a good idea to not react to
  every change in the entry text immediately, but only after a short delay.
  To support this, @sym{gtk-search-entry} emits the \"search-changed\" signal
  which can be used instead of the \"changed\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"search-changed\" signal}
      @begin{pre}
 lambda (entry)   : Run Last
      @end{pre}
      The \"search-changed\" signal is emitted with a short delay of 150
      milliseconds after the last change to the entry text.
      @begin[code]{table}
        @entry[entry]{The object on which the signal was emitted.}
      @end{table}
      Since 3.10
  @end{dictionary}
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-6
(declaim (inline gtk-search-entry-new))

#+gtk-3-6
(defun gtk-search-entry-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-8-30}
  @return{A new @class{gtk-search-entry} widget.}
  @begin{short}
    Creates a @class{gtk-search-entry}, with a find icon when the search field
    is empty, and a clear icon when it is not.
  @end{short}

  Since 3.6
  @see-class{gtk-search-entry}"
  (make-instance 'gtk-search-entry))

#+gtk-3-6
(export 'gtk-search-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_handle_event ()
;;; ----------------------------------------------------------------------------

#+gtk-3-16
(defcfun gtk-search-entry-handle-event :boolean
  (entry (g-object gtk-search-entry))
  (event (g-boxed-foreign gdk-event)))

#+gtk-3-16
(export 'gtk-search-entry-handle-event)

;;; --- End of file gtk.search-entry.lisp --------------------------------------

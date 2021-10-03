;;; ----------------------------------------------------------------------------
;;; gtk.seach-entry.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 - 2019 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkSearchEntry
;;;
;;; Functions
;;;
;;;     gtk-search-entry-new
;;;     gtk_search_entry_handle_event
;;;
;;; Signals
;;;
;;;     void    next-match        Action
;;;     void    previous-match    Action
;;;     void    search-changed    Run Last
;;;     void    stop-search       Action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEntry
;;;                 ╰── GtkSearchEntry
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSearchEntry implements AtkImplementorIface, GtkBuildable, GtkEditable
;;;     and GtkCellEditable.
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
 "@version{2020-6-1}
  @begin{short}
    @sym{gtk-search-entry} is a subclass of @class{gtk-entry} that has been
    tailored for use as a search entry.
  @end{short}

  @image[search-entry]{}

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
    @subheading{The \"next-match\" signal}
      @begin{pre}
 lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the next match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-g} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-search-entry} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"previous-match\" signal}
      @begin{pre}
 lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a move to the previous match for the current search string.
      Applications should connect to it, to implement moving between matches.
      The default bindings for this signal is the @kbd{Ctrl-Shift-g} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-search-entry} widget on which the signal was
          emitted.}
        @end{table}
    @subheading{The \"search-changed\" signal}
      @begin{pre}
 lambda (entry)    : Run Last
      @end{pre}
      The \"search-changed\" signal is emitted with a short delay of 150
      milliseconds after the last change to the entry text.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-search-entry} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"stop-search\" signal}
      @begin{pre}
 lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user stops
      a search via keyboard input. Applications should connect to it, to
      implement hiding the search entry in this case. The default bindings for
      this signal is the @kbd{Escape} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk-search-entry} widget on which the signal was
          emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gtk-entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-search-entry-new))

(defun gtk-search-entry-new ()
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @return{A new @class{gtk-search-entry} widget.}
  @begin{short}
    Creates a search entry, with a find icon when the search field is empty,
    and a clear icon when it is not.
  @end{short}
  @see-class{gtk-search-entry}"
  (make-instance 'gtk-search-entry))

(export 'gtk-search-entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_search_entry_handle_event ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_search_entry_handle_event" gtk-search-entry-handle-event)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2020-6-1}
  @argument[entry]{a @class{gtk-entry-search} widget}
  @argument[event]{a key event of type @class{gdk-event}}
  @return{@var{+gdk-event-stop+} if the key press event resulted in a search
    beginning or continuing, @var{+gdk-event-propagate+} otherwise.}
  @begin{short}
    This function should be called when the toplevel window which contains the
    search entry received a key event.
  @end{short}
  If the entry is part of a search bar, it is preferable to call the
  @fun{gtk-search-bar-handle-event} function instead, which will reveal the
  entry in addition to passing the event to this function.

  If the key event is handled by the search entry and starts or continues a
  search, @var{+gdk-event-stop+} will be returned. The caller should ensure that
  the entry is shown in this case, and not propagate the event further.
  @see-class{gtk-search-entry}"
  (entry (g-object gtk-search-entry))
  (event (g-boxed-foreign gdk-event)))

(export 'gtk-search-entry-handle-event)

;;; --- End of file gtk.search-entry.lisp --------------------------------------

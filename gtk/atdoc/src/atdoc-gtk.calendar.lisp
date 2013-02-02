;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.calendar.lisp
;;; 
;;; Documentation strings for the library GTK+.
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See http://www.gtk.org.
;;; 
;;; Copyright (C) 2013 Dieter Kaiser
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

(in-package :gtk)

;;; --- gtk-calendar -----------------------------------------------------------

(setf (documentation 'gtk-calendar 'type)
 "@version{2013-1-28}
  @begin{short}
    @sym{gtk-calendar} is a widget that displays a Gregorian calendar, one month
    at a time.
  @end{short}
  It can be created with @fun{gtk-calendar-new}.

  The month and year currently displayed can be altered with
  @fun{gtk-calendar-select-month}. The exact day can be selected from the
  displayed month using @fun{gtk-calendar-select-day}.

  To place a visual marker on a particular day, use @fun{gtk-calendar-mark-day}
  and to remove the marker, @fun{gtk-calendar-unmark-day}. Alternative, all
  marks can be cleared with @fun{gtk-calendar-clear-marks}.

  The way in which the calendar itself is displayed can be altered using
  @fun{gtk-calendar-set-display-options}.

  The selected date can be retrieved from a @sym{gtk-calendar} using
  @fun{gtk-calendar-get-date}.

  Users should be aware that, although the Gregorian calendar is the legal
  calendar in most countries, it was adopted progressively between 1582 and
  1929. Display before these dates is likely to be historically incorrect.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"horizontal-separation\" style property}
    @code{\"horizontal-separation\"} of type @code{gint} (Read)@br{}
    Separation between week headers and main area.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{4}

    @subheading{The \"inner-border\" style property}
    @code{\"inner-border\"} of type @code{gint} (Read)@br{}
    The spacing around the day/week headers and main area.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{4}

    @subheading{The \"vertical-separation\" style property}
    @code{\"vertical-separation\"} of type @code{gint} (Read)@br{}
    Space between day headers and main area.@br{}
    Allowed values: @code{>= 0}@br{}
    Default value: @code{4}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @b{The \"day-selected\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when the user selects a day.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"day-selected-double-click\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when the user double-clicks a day.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"month-changed\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when the user clicks a button to change the selected month on a
    calendar.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"next-month\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when the user switched to the next month.
    @begin[code]{table}
      @enty[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"next-year\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when user switched to the next year.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"prev-month\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when the user switched to the previous month.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}

    @b{The \"prev-year\" signal}
    @begin{pre}
 void user_function (GtkCalendar *calendar,
                     gpointer     user_data)      : Run First
    @end{pre}
    Emitted when user switched to the previous year.
    @begin[code]{table}
      @entry[calendar]{the object which received the signal.}
      @entry[user_data]{user data set when the signal handler was connected.}
    @end{table}
  @end{dictionary}
  @see-slot{gtk-calendar-day}
  @see-slot{gtk-calendar-detail-height-rows}
  @see-slot{gtk-calendar-detail-width-chars}
  @see-slot{gtk-calendar-month}
  @see-slot{gtk-calendar-no-month-change}
  @see-slot{gtk-calendar-show-day-names}
  @see-slot{gtk-calendar-show-details}
  @see-slot{gtk-calendar-show-heading}
  @see-slot{gtk-calendar-show-week-numbers}
  @see-slot{gtk-calendar-year}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "day" 'gtk-calendar) 't)
 "The @code{\"day\"} property of type @code{gint} (Read / Write)@br{}
  The selected day (as a number between 1 and 31, or 0 to unselect the
  currently selected day). This property gets initially set to the current
  day.@br{}
  Allowed values: @code{[0,31]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "detail-height-rows" 'gtk-calendar) 't)
 "The @code{\"detail-height-rows\"} property of type @code{gint}
  (Read / Write)@br{}
  Height of a detail cell, in rows. A value of 0 allows any width. See
  @fun{gtk-calendar-set-detail-func}.@br{}
  Allowed values: @code{[0,127]}@br{}
  Default value: @code{0}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "detail-width-chars" 'gtk-calendar) 't)
 "The @code{\"detail-width-chars\"} property of type @code{gint}
  (Read / Write)@br{}
  Width of a detail cell, in characters. A value of 0 allows any width. See
  @fun{gtk-calendar-set-detail-func}.@br{}
  Allowed values: @code{0,127]}@br{}
  Default value: @code{0}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "month" 'gtk-calendar) 't)
 "The @code{\"month\"} property of type @code{gint} (Read / Write)@br{}
  The selected month (as a number between 0 and 11). This property gets
  initially set to the current month.@br{}
  Allowed values: @code{[0,11]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "no-month-change" 'gtk-calendar) 't)
 "The @code{\"no-month-change\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether the selected month can be changed.@br{}
  Default value: @code{nil}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "show-day-names" 'gtk-calendar) 't)
 "The @code{\"show-day-names\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether day names are displayed.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "show-details" 'gtk-calendar) 't)
 "The @code{\"show-details\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether details are shown directly in the widget, or if they are
  available only as tooltip. When this property is set days with details are
  marked.@br{}
  Default value: @arg{true}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "show-heading" 'gtk-calendar) 't)
 "The @code{\"show-heading\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether a heading is displayed.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "show-week-numbers" 'gtk-calendar) 't)
 "The @code{\"show-week-numbers\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether week numbers are displayed.@br{}
  Default value: @code{nil}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "year" 'gtk-calendar) 't)
 "The @code{\"year\"} property of type @code{gint} (Read / Write)@br{}
  The selected year. This property gets initially set to the current year.@br{}
  Allowed values: @code{[0,4194303]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-calendar-day -------------------------------------------------------

(setf (gethash 'gtk-calendar-day atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-day 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"day\"} of the @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-detail-height-rows ----------------------------------------

(setf (gethash 'gtk-calendar-detail-height-rows atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-detail-height-rows 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"detail-height-rows\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-detail-width-chars ----------------------------------------

(setf (gethash 'gtk-calendar-detail-width-chars atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-detail-width-chars 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"detail-width-chars\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-month -----------------------------------------------------

(setf (gethash 'gtk-calendar-month atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-month 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"month\"} of the @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-no-month-chage --------------------------------------------

(setf (gethash 'gtk-calendar-no-month-change atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-no-month-change 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"no-month-change\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-day-names --------------------------------------------

(setf (gethash 'gtk-calendar-show-day-names atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-show-day-names 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-day-names\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-details ----------------------------------------------

(setf (gethash 'gtk-calendar-show-details atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-show-details 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-details\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-heading ----------------------------------------------

(setf (gethash 'gtk-calendar-show-heading atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-show-heading 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-heading\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-week-numbers -----------------------------------------

(setf (gethash 'gtk-calendar-show-week-numbers atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-show-week-numbers 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-week-numbers\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-year ------------------------------------------------------

(setf (gethash 'gtk-calendar-year atdoc:*function-name-alias*) "Accessor")
(setf (documentation 'gtk-calendar-year 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"year\"} of the @class{gtk-calendar} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; GtkCalendarDetailFunc ()
;;;
;;; gchar * (*GtkCalendarDetailFunc) (GtkCalendar *calendar,
;;;                                   guint year,
;;;                                   guint month,
;;;                                   guint day,
;;;                                   gpointer user_data);
;;;
;;; This kind of functions provide Pango markup with detail information for the
;;; specified day. Examples for such details are holidays or appointments. The
;;; function returns NULL when no information is available.
;;;
;;; calendar :
;;;     a GtkCalendar.
;;;
;;; year :
;;;     the year for which details are needed.
;;;
;;; month :
;;;     the month for which details are needed.
;;;
;;; day :
;;;     the day of month for which details are needed.
;;;
;;; user_data :
;;;     the data passed with gtk_calendar_set_detail_func().
;;;
;;; Returns :
;;;     Newly allocated string with Pango markup with details for the specified
;;;     day, or NULL.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

#|
(defcallback gtk-calendar-detail-func-cb (g-string :free-to-foreign nil
                                                   :free-from-foreign nil)
    ((calendar (g-object gtk-calendar))
     (year :uint)
     (month :uint)
     (day :uint)
     (data :pointer))
  (restart-case
    (or (funcall (glib::get-stable-pointer-value data) calendar year month day)
        (null-pointer))
    (return-null () (null-pointer))))
|#

;;; --- gtk-calendar-display-options -------------------------------------------

(setf (gethash 'gtk-calendar-display-options atdoc:*symbol-name-alias*) "Enum")
(setf (gethash 'gtk-calendar-display-options atdoc:*external-symbols*)
 "@version{2013-1-28}
  @begin{short}
    These options can be used to influence the display and behaviour of a
    @class{gtk-calendar}.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkCalendarDisplayOptions\" gtk-calendar-display-options
  (:export t
   :type-initializer \"gtk_calendar_display_options_get_type\")
  (:show-heading 1)
  (:show-day-names 2)
  (:no-month-change 4)
  (:show-week-numbers 8)
  (:show-details 32))
  @end{pre}
  @begin[code]{table}
    @entry[:show-heading]{Specifies that the month and year should be
      displayed.}
    @entry[:show-day-name]{Specifies that three letter day descriptions should
      be present.}
    @entry[:no-month-chage]{Prevents the user from switching months with the
      calendar.}
    @entry[:show-week-numbers]{Displays each week numbers of the current year,
      down the left side of the calendar.}
    @entry[:show-details]{Just show an indicator, not the full details text when
      details are provided. See @fun{gtk-calendar-set-detail-func}.}
  @end{table}
  @see-function{gtk-calendar-set-detail-func}")

;;; --- gtk-calendar-new -------------------------------------------------------

(setf (documentation 'gtk-calendar-new 'function)
 "@version{2013-1-28}
  @return{A newly @class{gtk-calendar} widget.}
  @begin{short}
    Creates a new calendar, with the current date being selected.
  @end{short}")

;;; --- gtk-calendar-select-month ----------------------------------------------

(setf (documentation 'gtk-calendar-select-month 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[month]{a month number between 0 and 11.}
  @argument[year]{the year the @arg{month} is in.}
  @begin{short}
    Shifts the calendar to a different month.
  @end{short}")

;;; --- gtk-calendar-select-day ------------------------------------------------

(setf (documentation 'gtk-calendar-select-day 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number between 1 and 31, or 0 to unselect the currently
    selected day.}
  @begin{short}
    Selects a day from the current month.
  @end{short}")

;;; --- gtk-calendar-mark-day --------------------------------------------------

(setf (documentation 'gtk-calendar-mark-day 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number to mark between 1 and 31.}
  @begin{short}
    Places a visual marker on a particular day.
  @end{short}")

;;; --- gtk-calendar-unmark-day ------------------------------------------------

(setf (documentation 'gtk-calendar-unmark-day 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[day]{the day number to unmark between 1 and 31.}
  @begin{short}
    Removes the visual marker from a particular day.
  @end{short}")

;;; --- gtk-calendar-get-day-is-marked -----------------------------------------

(setf (documentation 'gtk-calendar-get-day-is-marked 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number between 1 and 31.}
  @return{whether the day is marked.}
  @begin{short}
    Returns if the day of the calendar is already marked.
  @end{short}

  Since 3.0")

;;; --- gtk-calendar-clear-marks -----------------------------------------------

(setf (documentation 'gtk-calendar-clear-marks 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @begin{short}
    Remove all visual markers.
  @end{short}")

;;; --- gtk-calendar-get-display-options ---------------------------------------

(setf (documentation 'gtk-calendar-get-display-options 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @return{the display options.}
  @begin{short}
    Returns the current display options of calendar.
  @end{short}

  Since 2.4")

;;; --- gtk-calendar-set-display-options ---------------------------------------

(setf (documentation 'gtk-calendar-set-display-options 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[flags]{the display options to set}
  @begin{short}
    Sets display options (whether to display the heading and the month
    headings).
  @end{short}

  Since 2.4")

;;; --- gtk-calendar-get-date --------------------------------------------------

(setf (documentation 'gtk-calendar-get-date 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[year]{location to store the year as a decimal number (e.g. 2011),
    or @code{nil}}
  @argument[month]{location to store the month number (between 0 and 11),
    or @code{nil}}
  @argument[day]{location to store the day number (between 1 and 31), or
    @code{nil}}
  @begin{short}
    Obtains the selected date from a @class{gtk-calendar}.
  @end{short}")

;;; --- gtk-calendar-set-detail-func -------------------------------------------

(setf (documentation 'gtk-calendar-set-detail-func 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[func]{a function providing details for each day.}
  @argument[data]{data to pass to func invokations.}
  @argument[destroy]{a function for releasing data.}
  @begin{short}
    Installs a function which provides Pango markup with detail information for
    each day.
  @end{short}
  Examples for such details are holidays or appointments. That information is
  shown below each day when @code{\"show-details\"} is set. A tooltip
  containing with full detail information is provided, if the entire text
  should not fit into the details area, or if @code{\"show-details\"} is not
  set.

  The size of the details area can be restricted by setting the
  @code{\"detail-width-chars\"} and @code{\"detail-height-rows\"} properties.

  Since 2.14")

;;; --- gtk-calendar-get-detail-width-chars ------------------------------------

(setf (documentation 'gtk-calendar-get-detail-width-chars 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @return{The width of detail cells, in characters.}
  @begin{short}
    Queries the width of detail cells, in characters.
  @end{short}
  See the property @code{\"detail-width-chars\"}.

  Since 2.14")

;;; --- gtk-calendar-set-detail-width-chars ------------------------------------

(setf (documentation 'gtk-calendar-set-detail-width-chars 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[chars]{detail width in characters.}
  @begin{short}
    Updates the width of detail cells.
  @end{short}
  See @code{\"detail-width-chars\"}.

  Since 2.14")

;;; --- gtk-calendar-get-detail-height-rows ------------------------------------

(setf (documentation 'gtk-calendar-get-detail-height-rows 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @return{The height of detail cells, in rows.}
  @begin{short}
    Queries the height of detail cells, in rows.
  @end{short}
  See @code{\"detail-width-chars\"}.

  Since 2.14")

;;; --- gtk-calendar-set-detail-height-rows ------------------------------------

(setf (documentation 'gtk-calendar-set-detail-height-rows 'function)
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[rows]{detail height in rows.}
  @begin{short}
    Updates the height of detail cells.
  @end{short}
  See @code{\"detail-height-rows\".}

  Since 2.14")

;;; --- End of file atdoc-gtk.calendar.lisp ------------------------------------

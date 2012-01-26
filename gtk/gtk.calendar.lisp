;;; ----------------------------------------------------------------------------
;;; gtk.calendar.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;; 
;;; The documentation has been copied from the GTK 3.2.3 Reference Manual
;;; See http://www.gtk.org.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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
;;; GtkCalendar
;;; 
;;; Displays a calendar and allows the user to select a date
;;; 	
;;; Synopsis
;;; 
;;;     GtkCalendar
;;;     GtkCalendarDisplayOptions
;;;
;;;     gtk_calendar_new
;;;     gtk_calendar_select_month
;;;     gtk_calendar_select_day
;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked
;;;     gtk_calendar_clear_marks
;;;     gtk_calendar_get_display_options
;;;     gtk_calendar_set_display_options
;;;     gtk_calendar_get_date
;;;     gtk_calendar_set_detail_func
;;;     gtk_calendar_get_detail_width_chars
;;;     gtk_calendar_set_detail_width_chars
;;;     gtk_calendar_get_detail_height_rows
;;;     gtk_calendar_set_detail_height_rows
;;; 
;;; Object Hierarchy
;;; 
;;;   GObject
;;;    +----GInitiallyUnowned
;;;          +----GtkWidget
;;;                +----GtkCalendar
;;; 
;;; Implemented Interfaces
;;; 
;;; GtkCalendar implements AtkImplementorIface and GtkBuildable.
;;; Properties
;;; 
;;;   "day"                      gint                  : Read / Write
;;;   "detail-height-rows"       gint                  : Read / Write
;;;   "detail-width-chars"       gint                  : Read / Write
;;;   "month"                    gint                  : Read / Write
;;;   "no-month-change"          gboolean              : Read / Write
;;;   "show-day-names"           gboolean              : Read / Write
;;;   "show-details"             gboolean              : Read / Write
;;;   "show-heading"             gboolean              : Read / Write
;;;   "show-week-numbers"        gboolean              : Read / Write
;;;   "year"                     gint                  : Read / Write
;;; 
;;; Style Properties
;;; 
;;;   "horizontal-separation"    gint                  : Read
;;;   "inner-border"             gint                  : Read
;;;   "vertical-separation"      gint                  : Read
;;; 
;;; Signals
;;; 
;;;   "day-selected"                                   : Run First
;;;   "day-selected-double-click"                      : Run First
;;;   "month-changed"                                  : Run First
;;;   "next-month"                                     : Run First
;;;   "next-year"                                      : Run First
;;;   "prev-month"                                     : Run First
;;;   "prev-year"                                      : Run First
;;; 
;;; Description
;;; 
;;; GtkCalendar is a widget that displays a Gregorian calendar, one month at a
;;; time. It can be created with gtk_calendar_new().
;;; 
;;; The month and year currently displayed can be altered with
;;; gtk_calendar_select_month(). The exact day can be selected from the
;;; displayed month using gtk_calendar_select_day().
;;; 
;;; To place a visual marker on a particular day, use gtk_calendar_mark_day()
;;; and to remove the marker, gtk_calendar_unmark_day(). Alternative, all marks
;;; can be cleared with gtk_calendar_clear_marks().
;;; 
;;; The way in which the calendar itself is displayed can be altered using
;;; gtk_calendar_set_display_options().
;;; 
;;; The selected date can be retrieved from a GtkCalendar using
;;; gtk_calendar_get_date().
;;; 
;;; Users should be aware that, although the Gregorian calendar is the legal
;;; calendar in most countries, it was adopted progressively between 1582 and
;;; 1929. Display before these dates is likely to be historically incorrect.
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "day" property
;;; 
;;;   "day"                      gint                  : Read / Write
;;; 
;;; The selected day (as a number between 1 and 31, or 0 to unselect the
;;; currently selected day). This property gets initially set to the current
;;; day.
;;; 
;;; Allowed values: [0,31]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "detail-height-rows" property
;;; 
;;;   "detail-height-rows"       gint                  : Read / Write
;;; 
;;; Height of a detail cell, in rows. A value of 0 allows any width.
;;; See gtk_calendar_set_detail_func().
;;; 
;;; Allowed values: [0,127]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "detail-width-chars" property
;;; 
;;;   "detail-width-chars"       gint                  : Read / Write
;;; 
;;; Width of a detail cell, in characters. A value of 0 allows any width.
;;; See gtk_calendar_set_detail_func().
;;; 
;;; Allowed values: [0,127]
;;; 
;;; Default value: 0
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "month" property
;;; 
;;;   "month"                    gint                  : Read / Write
;;; 
;;; The selected month (as a number between 0 and 11). This property gets
;;; initially set to the current month.
;;; 
;;; Allowed values: [0,11]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;; The "no-month-change" property
;;; 
;;;   "no-month-change"          gboolean              : Read / Write
;;; 
;;; Determines whether the selected month can be changed.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-day-names" property
;;; 
;;;   "show-day-names"           gboolean              : Read / Write
;;; 
;;; Determines whether day names are displayed.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-details" property
;;; 
;;;   "show-details"             gboolean              : Read / Write
;;; 
;;; Determines whether details are shown directly in the widget, or if they are
;;; available only as tooltip. When this property is set days with details are
;;; marked.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-heading" property
;;; 
;;;   "show-heading"             gboolean              : Read / Write
;;; 
;;; Determines whether a heading is displayed.
;;; 
;;; Default value: TRUE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "show-week-numbers" property
;;; 
;;;   "show-week-numbers"        gboolean              : Read / Write
;;; 
;;; Determines whether week numbers are displayed.
;;; 
;;; Default value: FALSE
;;; 
;;; Since 2.4
;;;
;;; ----------------------------------------------------------------------------
;;; The "year" property
;;; 
;;;   "year"                     gint                  : Read / Write
;;; 
;;; The selected year. This property gets initially set to the current year.
;;; 
;;; Allowed values: [0,4194303]
;;; 
;;; Default value: 0
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "horizontal-separation" style property
;;; 
;;;   "horizontal-separation"    gint                  : Read
;;; 
;;; Separation between week headers and main area.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 4
;;;
;;; ----------------------------------------------------------------------------
;;; The "inner-border" style property
;;; 
;;;   "inner-border"             gint                  : Read
;;; 
;;; The spacing around the day/week headers and main area.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 4
;;;
;;; ----------------------------------------------------------------------------
;;; The "vertical-separation" style property
;;; 
;;;   "vertical-separation"      gint                  : Read
;;; 
;;; Space between day headers and main area.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 4
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Signal Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "day-selected" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when the user selects a day.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "day-selected-double-click" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when the user double-clicks a day.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "month-changed" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when the user clicks a button to change the selected month on a
;;; calendar.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "next-month" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when the user switched to the next month.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "next-year" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when user switched to the next year.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "prev-month" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when the user switched to the previous month.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;;
;;; ----------------------------------------------------------------------------
;;; The "prev-year" signal
;;; 
;;; void user_function (GtkCalendar *calendar,
;;;                     gpointer     user_data)      : Run First
;;; 
;;; Emitted when user switched to the previous year.
;;; 
;;; calendar :
;;; 	the object which received the signal.
;;; 
;;; user_data :
;;; 	user data set when the signal handler was connected.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCalendar
;;; 
;;; struct GtkCalendar;
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCalendar" gtk-calendar
  (:superclass gtk-widget
    :export t
    :interfaces ("AtkImplementorIface" "GtkBuildable")
    :type-initializer "gtk_calendar_get_type")
  ((day calendar-day
    "day" "gint" t t)
   (detail-height-rows gtk-calendar-detail-height-rows
    "detail-height-rows" "gint" t t)
   (detail-width-chars gtk-calendar-detail-width-chars
    "detail-width-chars" "gint" t t)
   (month gtk-calendar-month "month" "gint" t t)
   (no-month-change gtk-calendar-no-month-change
    "no-month-change" "gboolean" t t)
   (show-day-names gtk-calendar-show-day-names
    "show-day-names" "gboolean" t t)
   (show-details gtk-calendar-show-details
    "show-details" "gboolean" t t)
   (show-heading gtk-calendar-show-heading
    "show-heading" "gboolean" t t)
   (show-week-numbers gtk-calendar-show-week-numbers
    "show-week-numbers" "gboolean" t t)
   (year gtk-calendar-year
    "year" "gint" t t)
   (:cffi detail-function gtk-calendar-detail-function
          nil nil calendar-set-detail-function)
   (:cffi display-options gtk-calendar-display-options
          gtk-calendar-display-options
          "gtk_calendar_get_display_options"
          "gtk_calendar_set_display_options")))

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
;;; 	a GtkCalendar.
;;; 
;;; year :
;;; 	the year for which details are needed.
;;; 
;;; month :
;;; 	the month for which details are needed.
;;; 
;;; day :
;;; 	the day of month for which details are needed.
;;; 
;;; user_data :
;;; 	the data passed with gtk_calendar_set_detail_func().
;;; 
;;; Returns :
;;; 	Newly allocated string with Pango markup with details for the specified
;;;     day, or NULL.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCalendarDisplayOptions
;;; 
;;; typedef enum {
;;;   GTK_CALENDAR_SHOW_HEADING		= 1 << 0,
;;;   GTK_CALENDAR_SHOW_DAY_NAMES	= 1 << 1,
;;;   GTK_CALENDAR_NO_MONTH_CHANGE	= 1 << 2,
;;;   GTK_CALENDAR_SHOW_WEEK_NUMBERS    = 1 << 3,
;;;   GTK_CALENDAR_SHOW_DETAILS		= 1 << 5
;;; } GtkCalendarDisplayOptions;
;;; 
;;; These options can be used to influence the display and behaviour of a
;;; GtkCalendar.
;;; 
;;; GTK_CALENDAR_SHOW_HEADING
;;; 	Specifies that the month and year should be displayed.
;;; 
;;; GTK_CALENDAR_SHOW_DAY_NAMES
;;; 	Specifies that three letter day descriptions should be present.
;;; 
;;; GTK_CALENDAR_NO_MONTH_CHANGE
;;; 	Prevents the user from switching months with the calendar.
;;; 
;;; GTK_CALENDAR_SHOW_WEEK_NUMBERS
;;; 	Displays each week numbers of the current year, down the left side of
;;;     the calendar.
;;; 
;;; GTK_CALENDAR_SHOW_DETAILS
;;; 	Just show an indicator, not the full details text when details are
;;;     provided. See gtk_calendar_set_detail_func().
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkCalendarDisplayOptions" gtk-calendar-display-options
  (:export t
   :type-initializer "gtk_calendar_display_options_get_type")
  (:show-heading 1)
  (:show-day-names 2)
  (:no-month-change 4)
  (:show-week-numbers 8)
  (:week-start-monday 16)
  (:show-details 32))

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_new ()
;;; 
;;; GtkWidget * gtk_calendar_new (void);
;;; 
;;; Creates a new calendar, with the current date being selected.
;;; 
;;; Returns :
;;; 	a newly GtkCalendar widget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_month ()
;;; 
;;; void gtk_calendar_select_month (GtkCalendar *calendar,
;;;                                 guint month,
;;;                                 guint year);
;;; 
;;; Shifts the calendar to a different month.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; month :
;;; 	a month number between 0 and 11.
;;; 
;;; year :
;;; 	the year the month is in.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_day ()
;;; 
;;; void gtk_calendar_select_day (GtkCalendar *calendar, guint day);
;;; 
;;; Selects a day from the current month.
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; day :
;;; 	the day number between 1 and 31, or 0 to unselect the currently
;;;     selected day.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_mark_day ()
;;; 
;;; void gtk_calendar_mark_day (GtkCalendar *calendar, guint day);
;;; 
;;; Places a visual marker on a particular day.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; day :
;;; 	the day number to mark between 1 and 31.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_mark_day" gtk-calendar-mark-day) :boolean
  (calendar g-object)
  (day :uint))

(export 'gtk-calendar-mark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_unmark_day ()
;;; 
;;; void gtk_calendar_unmark_day (GtkCalendar *calendar, guint day);
;;; 
;;; Removes the visual marker from a particular day.
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; day :
;;; 	the day number to unmark between 1 and 31.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_unmark_day" gtk-calendar-unmark-day) :boolean
  (calendar g-object)
  (day :uint))

(export 'gtk-calendar-unmark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_day_is_marked ()
;;; 
;;; gboolean gtk_calendar_get_day_is_marked (GtkCalendar *calendar, guint day);
;;; 
;;; Returns if the day of the calendar is already marked.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; day :
;;; 	the day number between 1 and 31.
;;; 
;;; Returns :
;;; 	whether the day is marked.
;;; 
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_clear_marks ()
;;; 
;;; void gtk_calendar_clear_marks (GtkCalendar *calendar);
;;; 
;;; Remove all visual markers.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_clear_marks" gtk-calendar-clear-marks) :void
  (calendar g-object))

(export 'gtk-calendar-clear-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_display_options ()
;;; 
;;; GtkCalendarDisplayOptions gtk_calendar_get_display_options
;;;                                                      (GtkCalendar *calendar)
;;; 
;;; Returns the current display options of calendar.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; Returns :
;;; 	the display options.
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_display_options ()
;;; 
;;; void gtk_calendar_set_display_options (GtkCalendar *calendar,
;;;                                        GtkCalendarDisplayOptions flags);
;;; 
;;; Sets display options (whether to display the heading and the month
;;; headings).
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; flags :
;;; 	the display options to set
;;; 
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_date ()
;;; 
;;; void gtk_calendar_get_date (GtkCalendar *calendar,
;;;                             guint *year,
;;;                             guint *month,
;;;                             guint *day);
;;; 
;;; Obtains the selected date from a GtkCalendar.
;;; 
;;; calendar :
;;; 	a GtkCalendar
;;; 
;;; year :
;;; 	location to store the year as a decimal number (e.g. 2011), or NULL.
;;; 
;;; month :
;;; 	location to store the month number (between 0 and 11), or NULL.
;;; 
;;; day :
;;; 	location to store the day number (between 1 and 31), or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_func ()
;;; 
;;; void gtk_calendar_set_detail_func (GtkCalendar *calendar,
;;;                                    GtkCalendarDetailFunc func,
;;;                                    gpointer data,
;;;                                    GDestroyNotify destroy);
;;; 
;;; Installs a function which provides Pango markup with detail information for
;;; each day. Examples for such details are holidays or appointments. That
;;; information is shown below each day when "show-details" is set. A tooltip
;;; containing with full detail information is provided, if the entire text
;;; should not fit into the details area, or if "show-details" is not set.
;;; 
;;; The size of the details area can be restricted by setting the
;;; "detail-width-chars" and "detail-height-rows" properties.
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; func :
;;; 	a function providing details for each day.
;;; 
;;; data :
;;; 	data to pass to func invokations.
;;; 
;;; destroy :
;;; 	a function for releasing data.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

(defcallback gtk-calendar-detail-func-callback (g-string :free-to-foreign nil
                                                         :free-from-foreign nil)
    ((calendar g-object) (year :uint) (month :uint) (day :uint) (data :pointer))
  (restart-case
      (or (funcall (get-stable-pointer-value data)
                   calendar year month day)
          (null-pointer))
    (return-null () (null-pointer))))

(defcfun ("gtk_calendar_set_detail_func" %gtk-calendar-set-detail-func) :void
  (calendar g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-calendar-set-detail-function (calendar function)
  (%gtk-calendar-set-detail-func
                        calendar
                        (callback gtk-calendar-detail-func-callback)
                        (allocate-stable-pointer function)
                        (callback stable-pointer-free-destroy-notify-callback)))

(export 'gtk-calendar-set-detail-function)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_detail_width_chars ()
;;; 
;;; gint gtk_calendar_get_detail_width_chars (GtkCalendar *calendar);
;;; 
;;; Queries the width of detail cells, in characters. See "detail-width-chars".
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; Returns :
;;; 	The width of detail cells, in characters.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_width_chars ()
;;; 
;;; void gtk_calendar_set_detail_width_chars (GtkCalendar *calendar, gint chars)
;;; 
;;; Updates the width of detail cells. See "detail-width-chars".
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; chars :
;;; 	detail width in characters.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_detail_height_rows ()
;;; 
;;; gint gtk_calendar_get_detail_height_rows (GtkCalendar *calendar);
;;; 
;;; Queries the height of detail cells, in rows. See "detail-width-chars".
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; Returns :
;;; 	The height of detail cells, in rows.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_height_rows ()
;;; 
;;; void gtk_calendar_set_detail_height_rows (GtkCalendar *calendar, gint rows)
;;; 
;;; Updates the height of detail cells. See "detail-height-rows".
;;; 
;;; calendar :
;;; 	a GtkCalendar.
;;; 
;;; rows :
;;; 	detail height in rows.
;;; 
;;; Since 2.14
;;; ----------------------------------------------------------------------------


;;; --- End of file gtk.calendar.lisp ------------------------------------------

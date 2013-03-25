;;; ----------------------------------------------------------------------------
;;; gtk.calendar.lisp
;;; 
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;; 
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.3. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;; 
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2013 Dieter Kaiser
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
;;;
;;;     gtk_calendar_get_display_options
;;;     gtk_calendar_set_display_options
;;;     gtk_calendar_get_date
;;;
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
;;;
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
;;; Height of a detail cell, in rows. A value of 0 allows any width. See
;;; gtk_calendar_set_detail_func().
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
;;; Width of a detail cell, in characters. A value of 0 allows any width. See
;;; gtk_calendar_set_detail_func().
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCalendar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCalendar" gtk-calendar
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_calendar_get_type")
  ((day
    gtk-calendar-day
    "day" "gint" t t)
   (detail-height-rows
    gtk-calendar-detail-height-rows
    "detail-height-rows" "gint" t t)
   (detail-width-chars
    gtk-calendar-detail-width-chars
    "detail-width-chars" "gint" t t)
   (month
    gtk-calendar-month "month" "gint" t t)
   (no-month-change
    gtk-calendar-no-month-change
    "no-month-change" "gboolean" t t)
   (show-day-names
    gtk-calendar-show-day-names
    "show-day-names" "gboolean" t t)
   (show-details
    gtk-calendar-show-details
    "show-details" "gboolean" t t)
   (show-heading
    gtk-calendar-show-heading
    "show-heading" "gboolean" t t)
   (show-week-numbers
    gtk-calendar-show-week-numbers
    "show-week-numbers" "gboolean" t t)
   (year
    gtk-calendar-year
    "year" "gint" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
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
      Emitted when the user selects a day.
    @subheading{The \"day-selected\" signal}
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"day-selected-double-click\" signal}
      Emitted when the user double-clicks a day.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"month-changed\" signal}
      Emitted when the user clicks a button to change the selected month on a
      calendar.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"next-month\" signal}
      Emitted when the user switched to the next month.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @enty[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"next-year\" signal}
      Emitted when user switched to the next year.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"prev-month\" signal}
      Emitted when the user switched to the previous month.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
      @end{table}
    @subheading{The \"prev-year\" signal}
      Emitted when user switched to the previous year.
      @begin{pre}
 lambda (calendar)   : Run First
      @end{pre}
      @begin[code]{table}
        @entry[calendar]{the object which received the signal.}
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
  @see-slot{gtk-calendar-year}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "day" 'gtk-calendar) 't)
 "The @code{\"day\"} property of type @code{gint} (Read / Write)@br{}
  The selected day (as a number between 1 and 31, or 0 to unselect the
  currently selected day). This property gets initially set to the current
  day.@br{}
  Allowed values: @code{[0,31]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "detail-height-rows"
                                               'gtk-calendar) 't)
 "The @code{\"detail-height-rows\"} property of type @code{gint}
  (Read / Write)@br{}
  Height of a detail cell, in rows. A value of 0 allows any width. See
  @fun{gtk-calendar-set-detail-func}.@br{}
  Allowed values: @code{[0,127]}@br{}
  Default value: @code{0}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "detail-width-chars"
                                               'gtk-calendar) 't)
 "The @code{\"detail-width-chars\"} property of type @code{gint}
  (Read / Write)@br{}
  Width of a detail cell, in characters. A value of 0 allows any width. See
  @fun{gtk-calendar-set-detail-func}.@br{}
  Allowed values: @code{0,127]}@br{}
  Default value: @code{0}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "month" 'gtk-calendar) 't)
 "The @code{\"month\"} property of type @code{gint} (Read / Write)@br{}
  The selected month (as a number between 0 and 11). This property gets
  initially set to the current month.@br{}
  Allowed values: @code{[0,11]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "no-month-change"
                                               'gtk-calendar) 't)
 "The @code{\"no-month-change\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether the selected month can be changed.@br{}
  Default value: @code{nil}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-day-names"
                                               'gtk-calendar) 't)
 "The @code{\"show-day-names\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether day names are displayed.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-details"
                                               'gtk-calendar) 't)
 "The @code{\"show-details\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether details are shown directly in the widget, or if they are
  available only as tooltip. When this property is set days with details are
  marked.@br{}
  Default value: @arg{true}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-heading"
                                               'gtk-calendar) 't)
 "The @code{\"show-heading\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether a heading is displayed.@br{}
  Default value: @arg{true}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-week-numbers"
                                               'gtk-calendar) 't)
 "The @code{\"show-week-numbers\"} property of type @code{gboolean}
  (Read / Write)@br{}
  Determines whether week numbers are displayed.@br{}
  Default value: @code{nil}@br{}
  Since 2.4")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-day atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-day 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"day\"} of the @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-detail-height-rows ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-detail-height-rows atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-detail-height-rows 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"detail-height-rows\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-detail-width-chars ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-detail-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-detail-width-chars 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"detail-width-chars\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-month -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-month atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-month 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"month\"} of the @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-no-month-chage --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-no-month-change atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-no-month-change 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"no-month-change\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-day-names --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-day-names atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-day-names 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-day-names\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-details ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-details atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-details 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-details\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-heading ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-heading atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-heading 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-heading\"} of the @class{gtk-calendar}
    class.
  @end{short}")

;;; --- gtk-calendar-show-week-numbers -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-week-numbers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-week-numbers 'function)
 "@version{2013-1-28}
  @begin{short}
    Accessor of the slot @code{\"show-week-numbers\"} of the
    @class{gtk-calendar} class.
  @end{short}")

;;; --- gtk-calendar-year ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-year atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-year 'function)
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

;;; ----------------------------------------------------------------------------
;;; enum GtkCalendarDisplayOptions
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkCalendarDisplayOptions" gtk-calendar-display-options
  (:export t
   :type-initializer "gtk_calendar_display_options_get_type")
  (:show-heading 1)
  (:show-day-names 2)
  (:no-month-change 4)
  (:show-week-numbers 8)
  (:show-details 32))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-display-options atdoc:*symbol-name-alias*) "Enum"
      (gethash 'gtk-calendar-display-options atdoc:*external-symbols*)
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

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-new))

(defun gtk-calendar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @return{A newly @class{gtk-calendar} widget.}
  @begin{short}
    Creates a new calendar, with the current date being selected.
  @end{short}"
  (make-instance 'gtk-calendar))

(export 'gtk-calendar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_month ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-select-month))

(defun gtk-calendar-select-month (calendar month year)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[month]{a month number between 0 and 11.}
  @argument[year]{the year the @arg{month} is in.}
  @begin{short}
    Shifts the calendar to a different month.
  @end{short}"
  (setf (gtk-calendar-month calendar) month
        (gtk-calendar-year calendar) year))

(export 'gtk-calendar-select-month)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_day ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-select-day))

(defun gtk-calendar-select-day (calendar day)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number between 1 and 31, or 0 to unselect the currently
    selected day.}
  @begin{short}
    Selects a day from the current month.
  @end{short}"
  (setf (gtk-calendar-day calendar) day))

(export 'gtk-calendar-select-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_mark_day ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_mark_day" gtk-calendar-mark-day) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number to mark between 1 and 31.}
  @begin{short}
    Places a visual marker on a particular day.
  @end{short}"
  (calendar (g-object gtk-calendar))
  (day :uint))

(export 'gtk-calendar-mark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_unmark_day ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_unmark_day" gtk-calendar-unmark-day) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[day]{the day number to unmark between 1 and 31.}
  @begin{short}
    Removes the visual marker from a particular day.
  @end{short}"
  (calendar (g-object gtk-calendar))
  (day :uint))

(export 'gtk-calendar-unmark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_day_is_marked ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_get_day_is_marked" gtk-calendar-get-day-is-marked)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{the day number between 1 and 31.}
  @return{whether the day is marked.}
  @begin{short}
    Returns if the day of the calendar is already marked.
  @end{short}

  Since 3.0"
  (calendar (g-object gtk-calendar))
  (day :int))

(export 'gtk-calendar-get-day-is-marked)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_clear_marks ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_clear_marks" gtk-calendar-clear-marks) :void
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @begin{short}
    Remove all visual markers.
  @end{short}"
  (calendar (g-object gtk-calendar)))

(export 'gtk-calendar-clear-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_display_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_get_display_options" gtk-calendar-get-display-options)
    gtk-calendar-display-options
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @return{the display options.}
  @begin{short}
    Returns the current display options of calendar.
  @end{short}

  Since 2.4"
  (calendar (g-object gtk-calendar)))

(export 'gtk-calendar-get-display-options)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_display_options ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_set_display_options" gtk-calendar-set-display-options)
    :void
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[flags]{the display options to set}
  @begin{short}
    Sets display options (whether to display the heading and the month
    headings).
  @end{short}

  Since 2.4"
  (calendar (g-object gtk-calendar))
  (flags gtk-calendar-display-options))

(export 'gtk-calendar-set-display-options)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_date ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-get-date))

(defun gtk-calendar-get-date (calendar)
 #+cl-cffi-gtk-documentation
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
  @end{short}"
  (values (gtk-calendar-year calendar)
          (gtk-calendar-month calendar)
          (gtk-calendar-day calendar)))

(export 'gtk-calendar-get-date)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_set_detail_func" %gtk-calendar-set-detail-func) :void
  (calendar g-object)
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun gtk-calendar-set-detail-func (calendar func)
 #+cl-cffi-gtk-documentation
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

  Since 2.14"
  (%gtk-calendar-set-detail-func calendar
                                 (callback gtk-calendar-detail-func-cb)
                                 (glib::allocate-stable-pointer func)
                                 (callback glib::stable-pointer-destroy-notify-cb)))

(export 'gtk-calendar-set-detail-func)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_detail_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-get-detail-width-chars))

(defun gtk-calendar-get-detail-width-chars (calendar)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @return{The width of detail cells, in characters.}
  @begin{short}
    Queries the width of detail cells, in characters.
  @end{short}
  See the property @code{\"detail-width-chars\"}.

  Since 2.14"
  (gtk-calendar-detail-width-chars calendar))

(export 'gtk-calendar-get-detail-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_width_chars ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-set-detail-width-chars))

(defun gtk-calendar-set-detail-width-chars (calendar chars)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[chars]{detail width in characters.}
  @begin{short}
    Updates the width of detail cells.
  @end{short}
  See @code{\"detail-width-chars\"}.

  Since 2.14"
  (setf (gtk-calendar-detail-width-chars calendar) chars))

(export 'gtk-calendar-set-detail-width-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_detail_height_rows ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-get-detail-height-rows))

(defun gtk-calendar-get-detail-height-rows (calendar)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @return{The height of detail cells, in rows.}
  @begin{short}
    Queries the height of detail cells, in rows.
  @end{short}
  See @code{\"detail-width-chars\"}.

  Since 2.14"
  (gtk-calendar-detail-height-rows calendar))

(export 'gtk-calendar-get-detail-height-rows)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_set_detail_height_rows ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-set-detail-height-rows))

(defun gtk-calendar-set-detail-height-rows (calendar rows)
 #+cl-cffi-gtk-documentation
 "@version{2013-1-28}
  @argument[calendar]{a @class{gtk-calendar} widget.}
  @argument[rows]{detail height in rows.}
  @begin{short}
    Updates the height of detail cells.
  @end{short}
  See @code{\"detail-height-rows\".}

  Since 2.14"
  (setf (gtk-calendar-detail-height-rows calendar) rows))

(export 'gtk-calendar-set-detail-height-rows)

;;; --- End of file gtk.calendar.lisp ------------------------------------------

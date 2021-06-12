;;; ----------------------------------------------------------------------------
;;; gtk.calendar.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     Displays a calendar and allows the user to select a date
;;;
;;; Types and Values
;;;
;;;     GtkCalendar
;;;     GtkCalendarDisplayOptions
;;;
;;; Functions
;;;
;;;     GtkCalendarDetailFunc
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
;;; Properties
;;;
;;;         gint    day                          Read / Write
;;;         gint    detail-height-rows           Read / Write
;;;         gint    detail-width-chars           Read / Write
;;;         gint    month                        Read / Write
;;;     gboolean    no-month-change              Read / Write
;;;     gboolean    show-day-names               Read / Write
;;;     gboolean    show-details                 Read / Write
;;;     gboolean    show-heading                 Read / Write
;;;     gboolean    show-week-numbers            Read / Write
;;;         gint    year                         Read / Write
;;;
;;; Style Propertie
;;;
;;;         gint    horizontal-separation        Read
;;;         gint    inner-border                 Read
;;;         gint    vertical-separation          Read
;;;
;;; Signals
;;;
;;;         void    day-selected                 Run First
;;;         void    day-selected-double-click    Run First
;;;         void    month-changed                Run First
;;;         void    next-month                   Run First
;;;         void    next-year                    Run First
;;;         void    prev-month                   Run First
;;;         void    prev-year                    Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkCalendar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCalendar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

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

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-display-options atdoc:*symbol-name-alias*)
      "GFlags"
      (gethash 'gtk-calendar-display-options atdoc:*external-symbols*)
 "@version{2021-6-11}
  @begin{short}
    These options can be used to influence the display and behaviour of a
    @class{gtk-calendar} widget.
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
    @entry[:show-details]{Just show an indicator, not the full details text
      when details are provided. See the function
      @fun{gtk-calendar-set-detail-func}.}
  @end{table}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-set-detail-func}")

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

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-calendar 'type)
 "@version{2021-6-11}
  @begin{short}
    The @sym{gtk-calendar} widget displays a Gregorian calendar, one month at a
    time.
  @end{short}
  It can be created with the function @fun{gtk-calendar-new}.

  The month and year currently displayed can be altered with the function
  @fun{gtk-calendar-select-month}. The exact day can be selected from the
  displayed month using the function @fun{gtk-calendar-select-day}.

  To place a visual marker on a particular day, use the function
  @fun{gtk-calendar-mark-day} and to remove the marker, the function
  @fun{gtk-calendar-unmark-day}. Alternative, all marks can be cleared with
  the function @fun{gtk-calendar-clear-marks}.

  The way in which the calendar itself is displayed can be altered using the
  function @fun{gtk-calendar-display-options}.

  The selected date can be retrieved from a @sym{gtk-calendar} using the
  function @fun{gtk-calendar-date}.

  Users should be aware that, although the Gregorian calendar is the legal
  calendar in most countries, it was adopted progressively between 1582 and
  1929. Display before these dates is likely to be historically incorrect.
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[horizontal-separation]{entry}
        The @code{horizontal-separation} style property of type @code{:int}
        (Read) @br{}
        Separation between week headers and main area. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[inner-border]{entry}
        The @code{inner-border} style property of type @code{:int} (Read) @br{}
        The spacing around the day/week headers and main area. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[vertical-separation]{entry}
        The @code{vertical-separation} style property of type @code{:int}
        (Read) @br{}
        Space between day headers and main area. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"day-selected\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when the user selects a day.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"day-selected-double-click\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when the user double-clicks a day.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"month-changed\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when the user clicks a button to change the selected month on a
      calendar.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"next-month\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when the user switched to the next month.
      @begin[code]{table}
        @enty[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"next-year\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when user switched to the next year.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"prev-month\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when the user switched to the previous month.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"prev-year\" signal}
      @begin{pre}
 lambda (calendar)    :run-first
      @end{pre}
      Emitted when user switched to the previous year.
      @begin[code]{table}
        @entry[calendar]{The @sym{gtk-calendar} widget which received the
          signal.}
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
  @see-symbol{gtk-calendar-display-options}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk-calendar-day -------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "day" 'gtk-calendar) 't)
 "The @code{day} property of type @code{:int} (Read / Write) @br{}
  The selected day as a number between 1 and 31, or 0 to unselect the
  currently selected day. This property gets initially set to the current
  day. @br{}
  Allowed values: [0, 31] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-day atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-day 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calender-day object) => day}
  @syntax[]{(setf (gtk-calendar-day object) day)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[day]{an integer with the selected day}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{day} slot of the @class{gtk-calendar}
    class.
  @end{short}

  The selected day as a number between 1 and 31, or 0 to unselect the
  currently selected day. This property gets initially set to the current
  day.
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-date}
  @see-function{gtk-calendar-select-day}")

;;; --- gtk-calendar-detail-height-rows ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "detail-height-rows"
                                               'gtk-calendar) 't)
 "The @code{detail-height-rows} property of type @code{:int} (Read / Write)
  @br{}
  Height of a detail cell, in rows. A value of 0 allows any width. @br{}
  Allowed values: [0, 127] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-detail-height-rows atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-detail-height-rows 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-detail-height-rows object) => rows}
  @syntax[]{(setf (gtk-calendar-detail-height-rows object) rows)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[rows]{an integer with the detail height in rows}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{detail-height-rows} slot of the
    @class{gtk-calendar} class.
  @end{short}

  The slot access function @sym{gtk-calendar-detail-height-rows} queries the
  height of detail cells, in rows. The slot access function
  @sym{(setf gtk-calendar-detail-height-rows)} updates the height of detail
  cells.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-detail-width-chars ----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "detail-width-chars"
                                               'gtk-calendar) 't)
 "The @code{detail-width-chars} property of type @code{:int} (Read / Write)
  @br{}
  Width of a detail cell, in characters. A value of 0 allows any width. @br{}
  Allowed values: [0, 127] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-detail-width-chars atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-detail-width-chars 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-detail-width-chars object) => chars}
  @syntax[]{(setf (gtk-calendar-detail-width-chars object) chars)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[chars]{an integer with the detail width in characters}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{detail-width-chars} slot of the
    @class{gtk-calendar} class.
  @end{short}

  The slot access function @sym{gtk-calendar-detail-width-chars} queries the
  width of detail cells, in characters. The slot access function
  @sym{(setf gtk-calendar-detail-width-chars)} updates the width of detail
  cells.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-month -----------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "month" 'gtk-calendar) 't)
 "The @code{month} property of type @code{:int} (Read / Write) @br{}
  The selected month as a number between 0 and 11. This property gets
  initially set to the current month. @br{}
  Allowed values: [0, 11] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-month atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-month 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-month object) => month}
  @syntax[]{(setf (gtk-calendar-month object) month)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[month]{an integer with the selected month}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{month} slot of the @class{gtk-calendar}
    class.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-date}
  @see-function{gtk-calendar-select-month}")

;;; --- gtk-calendar-no-month-change -------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "no-month-change"
                                               'gtk-calendar) 't)
 "The @code{no-month-change} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the selected month can be changed. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-no-month-change atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-no-month-change 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-no-month-change object) => no-change}
  @syntax[]{(setf (gtk-calendar-no-month-change object) no-change)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[no-change]{a boolean whether the selected month can be changed}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{no-month-change} slot of the
    @class{gtk-calendar} class.
  @end{short}

  Determines whether the selected month can be changed.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-show-day-names --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-day-names"
                                               'gtk-calendar) 't)
 "The @code{show-day-names} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether day names are displayed. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-day-names atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-day-names 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-show-day-names object) => show-day-names}
  @syntax[]{(setf (gtk-calendar-show-day-names object) show-day-names)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[show-day-names]{a boolean whether day names are displayed}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{show-day-names} slot of the
    @class{gtk-calendar} class.
  @end{short}

  Determines whether day names are displayed.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-show-details ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-details"
                                               'gtk-calendar) 't)
 "The @code{show-details} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether details are shown directly in the widget, or if they are
  available only as tooltip. When this property is set days with details are
  marked. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-details atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-details 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-show-details object) => show-details}
  @syntax[]{(setf (gtk-calendar-show-details object) show-details)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[show-details]{a boolean whether details are shown}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{show-details} slot of the
    @class{gtk-calendar} class.
  @end{short}

  Determines whether details are shown directly in the widget, or if they are
  available only as tooltip. When this property is set days with details are
  marked.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-show-heading ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-heading"
                                               'gtk-calendar) 't)
 "The @code{show-heading} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether a heading is displayed. @br{}
  Default value: @em{true}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-heading atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-heading 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-show-heading object) => show-heading}
  @syntax[]{(setf (gtk-calendar-show-heading object) show-heading)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[show-heading]{a boolean whether a heading is displayed}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{show-heading} slot of the
    @class{gtk-calendar} class.
  @end{short}

  Determines whether a heading is displayed.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-show-week-numbers -----------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-week-numbers"
                                               'gtk-calendar) 't)
 "The @code{show-week-numbers} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether week numbers are displayed. @br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-show-week-numbers atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-show-week-numbers 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-show-week-numbers object) => show-week-numbers}
  @syntax[]{(setf (gtk-calendar-show-week-numbers object) show-week-numbers)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[show-week-numbers]{a boolean whether week numbers are displayed}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{show-week-numbers} slot of the
    @class{gtk-calendar} class.
  @end{short}

  Determines whether week numbers are displayed.
  @see-class{gtk-calendar}")

;;; --- gtk-calendar-year ------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "year" 'gtk-calendar) 't)
 "The @code{year} property of type @code{:int} (Read / Write) @br{}
  The selected year. This property gets initially set to the current year. @br{}
  Allowed values: [0, 4194303] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-year atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-calendar-year 'function)
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-year object) => year}
  @syntax[]{(setf (gtk-calendar-year object) year)}
  @argument[object]{a @class{gtk-calendar} widget}
  @argument[year]{an integer with the selected year}
  @begin{short}
    Accessor of the @slot[gtk-calendar]{year} slot of the @class{gtk-calendar}
    class.
  @end{short}

  The selected year. This property gets initially set to the current year.
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-date}
  @see-function{gtk-calendar-select-month}")

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-new))

(defun gtk-calendar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @return{A newly @class{gtk-calendar} widget.}
  @begin{short}
    Creates a new calendar, with the current date being selected.
  @end{short}
  @see-class{gtk-calendar}"
  (make-instance 'gtk-calendar))

(export 'gtk-calendar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_month ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-select-month))

(defun gtk-calendar-select-month (calendar month year)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[month]{an integer with the month between 0 and 11}
  @argument[year]{an integer with the year}
  @begin{short}
    Shifts the calendar to a different month.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-select-day}"
  (setf (gtk-calendar-month calendar) month
        (gtk-calendar-year calendar) year))

(export 'gtk-calendar-select-month)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_day ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-calendar-select-day))

(defun gtk-calendar-select-day (calendar day)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{an integer with the day between 1 and 31, or 0 to unselect the
    currently selected day}
  @begin{short}
    Selects a day from the current month.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-select-month}"
  (setf (gtk-calendar-day calendar) day))

(export 'gtk-calendar-select-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_mark_day ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_mark_day" gtk-calendar-mark-day) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{an unsigned integer with the day to mark between 1 and 31}
  @begin{short}
    Places a visual marker on a particular day.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-unmark-day}
  @see-function{gtk-calendar-day-is-marked}
  @see-function{gtk-calendar-clear-marks}"
  (calendar (g-object gtk-calendar))
  (day :uint))

(export 'gtk-calendar-mark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_unmark_day ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_unmark_day" gtk-calendar-unmark-day) :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{an unsigned integer with the day to unmark between 1 and 31}
  @begin{short}
    Removes the visual marker from a particular day.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-mark-day}
  @see-function{gtk-calendar-day-is-marked}
  @see-function{gtk-calendar-clear-marks}"
  (calendar (g-object gtk-calendar))
  (day :uint))

(export 'gtk-calendar-unmark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_day_is_marked () -> gtk-calendar-day-is-marked
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_get_day_is_marked" gtk-calendar-day-is-marked)
    :boolean
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[day]{an unsigned integer with the day between 1 and 31}
  @return{A boolean whether the day is marked.}
  @begin{short}
    Returns if the day of the calendar is already marked.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-mark-day}
  @see-function{gtk-calendar-unmark-day}"
  (calendar (g-object gtk-calendar))
  (day :int))

(export 'gtk-calendar-day-is-marked)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_clear_marks ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_calendar_clear_marks" gtk-calendar-clear-marks) :void
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @begin{short}
    Remove all visual markers.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-mark-day}
  @see-function{gtk-calendar-unmark-day}"
  (calendar (g-object gtk-calendar)))

(export 'gtk-calendar-clear-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_display_options ()
;;; gtk_claendar_set_display_options () -> gtk-calendar-display-options
;;; ----------------------------------------------------------------------------

(defun (setf gtk-calendar-display-options) (flags calendar)
  (foreign-funcall "gtk_calendar_set_display_options"
                   (g-object gtk-calendar) calendar
                   gtk-calendar-display-options flags
                   :void)
  flags)

(defcfun ("gtk_calendar_get_display_options" gtk-calendar-display-options)
    gtk-calendar-display-options
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @syntax[]{(gtk-calendar-display-options calendar) => flags}
  @syntax[]{(setf (gtk-calendar-display-options calendar) flags)}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[flags]{the @symbol{gtk-calendar-display-options} flags}
  @begin{short}
    Accessor of the display options of the calendar.
  @end{short}

  The function @sym{gtk-calendar-display-options} returns the current display
  options of the calendar. The function
  @sym{(setf gtk-calendar-display-options)} sets display options, e.g.
  whether to display the heading and the month headings.
  @see-class{gtk-calendar}
  @see-symbol{gtk-calendar-display-options}"
  (calendar (g-object gtk-calendar)))

(export 'gtk-calendar-display-options)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_date () -> gtk-calendar-date
;;; ----------------------------------------------------------------------------

(defun gtk-calendar-date (calendar)
 #+cl-cffi-gtk-documentation
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @begin{return}
    @code{year} -- the year as a decimal number, e.g. 2021 @br{}
    @code{month} -- the month number, between 0 and 11 @br{}
    @code{day} -- the day number, between 1 and 31
  @end{return}
  @begin{short}
    Obtains the selected date from the calendar.
  @end{short}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-select-day}
  @see-function{gtk-calendar-select-month}"
  (values (gtk-calendar-year calendar)
          (gtk-calendar-month calendar)
          (gtk-calendar-day calendar)))

(export 'gtk-calendar-date)

;;; ----------------------------------------------------------------------------
;;; GtkCalendarDetailFunc ()
;;; ----------------------------------------------------------------------------

(defcallback gtk-calendar-detail-func (g-string :free-to-foreign nil
                                                :free-from-foreign nil)
    ((calendar (g-object gtk-calendar))
     (year :uint)
     (month :uint)
     (day :uint)
     (data :pointer))
  (restart-case
    (or (funcall (get-stable-pointer-value data) calendar year month day)
        (null-pointer))
    (return-null () (null-pointer))))

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-calendar-detail-func atdoc:*symbol-name-alias*)
      "Callback"
      (gethash 'gtk-calendar-detail-func atdoc:*external-symbols*)
 "@version{2021-6-11}
  @begin{short}
    This kind of functions provide Pango markup with detail information for the
    specified day.
  @end{short}
  Examples for such details are holidays or appointments. The function returns
  @code{nil} when no information is available.
  @begin{pre}
 lambda (calendar year month day)
  @end{pre}
  @begin[code]{table}
    @entry[calendar]{A @class{gtk-calendar} widget.}
    @entry[year]{An unsigned integer with the year for which details are
      needed.}
    @entry[month]{An unsigned integer with the month for which details are
      needed.}
    @entry[day]{An unsigned integer with the day of month for which details are
      needed.}
    @entry[Return]{A string with Pango markup with details for the specified
      day, or @code{nil}.}
  @end{table}
  @see-class{gtk-calendar}
  @see-function{gtk-calendar-set-detail-func}")

(export 'gtk-calendar-detail-func)

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
 "@version{2021-6-11}
  @argument[calendar]{a @class{gtk-calendar} widget}
  @argument[func]{a @symbol{gtk-calendar-detail-func} callback function
    providing details for each day}
  @begin{short}
    Installs a function which provides Pango markup with detail information for
    each day.
  @end{short}
  Examples for such details are holidays or appointments. That information is
  shown below each day when the @slot[gtk-calendar]{show-details} property is
  set. A tooltip containing with full detail information is provided, if the
  entire text should not fit into the details area, or if the
  @slot[gtk-calendar]{show-details} property is not set.

  The size of the details area can be restricted by setting the
  @slot[gtk-calendar]{detail-width-chars} and
  @slot[gtk-calendar]{detail-height-rows} properties.
  @see-class{gtk-calendar}
  @see-symbol{gtk-calendar-detail-func}"
  (%gtk-calendar-set-detail-func calendar
                                 (callback gtk-calendar-detail-func)
                                 (allocate-stable-pointer func)
                                 (callback stable-pointer-destroy-notify-cb)))

(export 'gtk-calendar-set-detail-func)

;;; --- End of file gtk.calendar.lisp ------------------------------------------

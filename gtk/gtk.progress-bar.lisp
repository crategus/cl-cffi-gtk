;;; ----------------------------------------------------------------------------
;;; gtk.progress-bar.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.8.9 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2014 Dieter Kaiser
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
;;; GtkProgressBar
;;;
;;; A widget which indicates progress visually
;;;
;;; Synopsis
;;;
;;;     GtkProgressBar
;;;
;;;     gtk_progress_bar_new
;;;     gtk_progress_bar_pulse
;;;     gtk_progress_bar_set_fraction
;;;     gtk_progress_bar_get_fraction
;;;     gtk_progress_bar_set_inverted
;;;     gtk_progress_bar_get_inverted
;;;     gtk_progress_bar_set_show_text
;;;     gtk_progress_bar_get_show_text
;;;     gtk_progress_bar_set_text
;;;     gtk_progress_bar_get_text
;;;     gtk_progress_bar_set_ellipsize
;;;     gtk_progress_bar_get_ellipsize
;;;     gtk_progress_bar_set_pulse_step
;;;     gtk_progress_bar_get_pulse_step
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkProgressBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkProgressBar" gtk-progress-bar
  (:superclass gtk-widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_progress_bar_get_type")
  ((ellipsize
    gtk-progress-bar-ellipsize
    "ellipsize" "PangoEllipsize" t t)
   (fraction
    gtk-progress-bar-fraction
    "fraction" "gdouble" t t)
   (inverted
    gtk-progress-bar-inverted
    "inverted" "gboolean" t t)
   (pulse-step
    gtk-progress-bar-pulse-step
    "pulse-step" "gdouble" t t)
   (show-text
    gtk-progress-bar-show-text
    "show-text" "gboolean" t t)
   (text
    gtk-progress-bar-text
    "text" "gchararray" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-progress-bar 'type)
 "@version{2014-3-16}
  @begin{short}
    The @sym{gtk-progress-bar} is typically used to display the progress of a
    long running operation. It provides a visual clue that processing is
    underway. The @sym{gtk-progress-bar} can be used in two different modes:
    percentage mode and activity mode.
  @end{short}

  When an application can determine how much work needs to take place, e. g.
  read a fixed number of bytes from a file, and can monitor its progress, it
  can use the @sym{gtk-progress-bar} in percentage mode and the user sees a
  growing bar indicating the percentage of the work that has been completed.
  In this mode, the application is required to call the generic function
  @fun{gtk-progress-bar-fraction} periodically to update the progress bar.

  When an application has no accurate way of knowing the amount of work to do,
  it can use the @sym{gtk-progress-bar} in activity mode, which shows activity
  by a block moving back and forth within the progress area. In this mode, the
  application is required to call the function @fun{gtk-progress-bar-pulse}
  periodically to update the progress bar.

  There is quite a bit of flexibility provided to control the appearance of
  the @sym{gtk-progress-bar}. Functions are provided to control the orientation
  of the bar, optional text can be displayed along with the bar, and the step
  size used in activity mode can be set.
  @begin[Style Property Details]{dictionary}
    @subheading{The \"min-horizontal-bar-height\" style property}
      @code{\"min-horizontal-bar-height\"} of type @code{:int}
      (Read / Write)@br{}
      Minimum horizontal height of the progress bar. @br{}
      Allowed values: >= 1 @br{}
      Default value: 20 @br{}
      Since 2.14

    @subheading{The \"min-horizontal-bar-width\" style property}
      @code{\"min-horizontal-bar-width\"} of type @code{:int}
      (Read / Write)@br{}
      The minimum horizontal width of the progress bar. @br{}
      Allowed values: >= 1 @br{}
      Default value: 150 @br{}
      Since 2.14

    @subheading{The \"min-vertical-bar-height\" style property}
      @code{\"min-vertical-bar-height\"} of type @code{:int} (Read / Write)@br{}
      The minimum vertical height of the progress bar. @br{}
      Allowed values: >= 1 @br{}
      Default value: 80 @br{}
      Since 2.14

    @subheading{The \"min-vertical-bar-width\" style property}
      @code{\"min-vertical-bar-width\"} of type @code{:int} (Read / Write)@br{}
      The minimum vertical width of the progress bar. @br{}
      Allowed values: >= 1 @br{}
      Default value: 22 @br{}
      Since 2.14

    @subheading{The \"xspacing\" style property}
      @code{\"xspacing\"} of type @code{:int} (Read / Write)@br{}
      Extra spacing applied to the width of a progress bar. @br{}
      Allowed values: >= 0 @br{}
      Default value: 7

    @subheading{The \"yspacing\" style property}
      @code{\"yspacing\"} of type @code{:int} (Read / Write)@br{}
      Extra spacing applied to the height of a progress bar. @br{}
      Allowed values: >= 0 @br{}
      Default value: 7
  @end{dictionary}
  @see-slot{gtk-progress-bar-ellipsize}
  @see-slot{gtk-progress-bar-fraction}
  @see-slot{gtk-progress-bar-inverted}
  @see-slot{gtk-progress-bar-pulse-step}
  @see-slot{gtk-progress-bar-show-text}
  @see-slot{gtk-progress-bar-text}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property and Accessor Details
;;;
;;; ----------------------------------------------------------------------------

;;; --- gtk-progress-bar-ellipsize ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "ellipsize"
                                               'gtk-progress-bar) 't)
 "The @code{\"ellipsize\"} property of type @symbol{pango-ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the progress bar does not
  have enough room to display the entire string, specified as a
  @symbol{pango-ellipsize-mode}.
  Note that setting this property to a value other than @code{:none} has the
  side-effect that the progress bar requests only enough space to display the
  ellipsis (\"...\"). Another means to set a progress bar's width is the
  function @fun{gtk-widget-size-request}. @br{}
  Default value: @code{:none} @br{}
  Since 2.6")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-ellipsize atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-ellipsize 'function)
 "@version{2014-3-16}
  @argument[object]{a @sym{gtk-progress-bar} widget}
  @argument[mode]{a @symbol{pango-ellipsize-mode}}
  @syntax[]{(gtk-progress-bar-ellipsize object) => mode}
  @syntax[]{(setf (gtk-progress-bar-ellipsize object) mode)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{ellipsize} of the
    @class{gtk-progress-bar} class.
  @end{short}

  The generic function @sym{gtk-progress-bar-ellipsize} returns the ellipsizing
  position of type @symbol{pango-ellipsize-mode} of the progress bar.

  The generic function @sym{(setf gtk-progress-bar-ellipsize)} sets the mode
  used to ellipsize, add an ellipsis: \"...\", the text if there is not enough
  space to render the entire string.

  Since 2.6
  @see-class{gtk-progress-bar}")

;;; --- gtk-progress-bar-fraction ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "fraction" 'gtk-progress-bar) 't)
 "The @code{\"fraction\"} property of type @code{:double} (Read / Write) @br{}
  The fraction of total work that has been completed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-fraction atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-fraction 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-progress-bar} widget}
  @argument[fraction]{fraction of the task that is been completed}
  @syntax[]{(gtk-progress-bar-fraction object) => fraction}
  @syntax[]{(setf (gtk-progress-bar-fraction object) fraction)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{fraction} of the
    @class{gtk-progress-bar} class.
  @end{short}

  The generic function @sym{gtk-progress-bar-fraction} returns the current
  fraction from 0.0 to 1.0 of the task that is been completed.

  The generic function @sym{(setf gtk-progress-bar-fraction)} causes the
  progress bar to \"fill in\" the given fraction of the bar. The fraction should
  be between 0.0 and 1.0, inclusive.
  @see-class{gtk-progress-bar}")

;;; --- gtk-progress-bar-inverted ----------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "inverted" 'gtk-progress-bar) 't)
 "The @code{\"inverted\"} property of type @code{:boolean} (Read / Write) @br{}
  Invert the direction in which the progress bar grows. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-inverted atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-inverted 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-progress-bar} widget}
  @argument[inverted]{@em{true} to invert the progress bar}
  @syntax[]{(gtk-progress-bar-inverted object) => inverted}
  @syntax[]{(setf (gtk-progress-bar-inverted object) inverted)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{inverted} of the
    @class{gtk-progress-bar} class.
  @end{short}

  @return{@em{True} if the progress bar is inverted.}

  The generic function @sym{gtk-progress-bar-inverted} gets the value set by the
  generic function @sym{(setf gtk-progress-bar-inverted)}.

  Progress bars normally grow from top to bottom or left to right. Inverted
  progress bars grow in the opposite direction.
  @see-class{gtk-progress-bar}")

;;; --- gtk-progress-bar-pulse-step --------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "pulse-step"
                                               'gtk-progress-bar) 't)
 "The @code{\"pulse-step\"} property of type @code{:double} (Read / Write) @br{}
  The fraction of total progress to move the bouncing block when pulsed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.1")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-pulse-step atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-pulse-step 'function)
 "@version{2014-3-16}
  @argument[object]{a @sym{gtk-progress-bar} widget}
  @argument[fraction]{fraction between 0.0 and 1.0}
  @syntax[]{(gtk-progress-bar-pulse-step object) => fraction}
  @syntax[]{(setf (gtk-progress-bar-pulse-step object) fraction)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{pulse-step} of the
    @class{gtk-progress-bar} class.
  @end{short}

  The generic function @sym{gtk-progress-bar-pulse-step} retrieves the pulse
  step which is a fraction from 0.0 to 1.0.

  The generic function @sym{(setf gtk-progress-bar-pulse-step)} sets the
  fraction of total progress bar length to move the bouncing block for each call
  to the function @fun{gtk-progress-bar-pulse}.
  @see-class{gtk-progress-bar}
  @see-function{gtk-progress-bar-pulse}")

;;; --- gtk-progress-bar-show-text ---------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-text"
                                               'gtk-progress-bar) 't)
 "The @code{\"show-text\"} property of type @code{:boolean} (Read / Write) @br{}
  Sets whether the progress bar will show text superimposed over the bar.
  The shown text is either the value of the @code{\"text\"} property or, if that
  is @code{nil}, the @code{\"fraction\"} value, as a percentage.
  To make a progress bar that is styled and sized suitably for containing text,
  even if the actual text is blank, set @code{\"show-text\"} to @em{true} and
  @code{\"text\"} to the empty string (not @code{nil}). @br{}
  Default value: @code{nil} @br{}
  Since 3.0")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-show-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-show-text 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-progress-bar} widget}
  @argument[show-text]{whether to show superimposed text}
  @syntax[]{(gtk-progress-bar-show-text object) => show-text}
  @syntax[]{(setf (gtk-progress-bar-show-text object) show-text)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{show-text} of the
    @class{gtk-progress-bar} class.
  @end{short}

  The generic function @sym{gtk-progress-bar-show-text} gets the value of the
  @slot[gtk-progress-bar]{show-text} property.

  The generic function @sym{gtk-progress-bar-show-text} sets whether the
  progress bar will show text superimposed over the bar. The shown text is
  either the value of the @slot[gtk-progress-bar]{text} property or, if that
  is @code{nil}, the @slot[gtk-progress-bar]{fraction} value, as a percentage.

  To make a progress bar that is styled and sized suitably for containing text,
  even if the actual text is blank, set the @code{\"show-text\"} property to
  @em{true} and the @slot[gtk-progress-bar]{text} property to the empty string,
  not @code{nil}.

  Since 3.0
  @see-class{gtk-progress-bar}")

;;; --- gtk-progress-bar-text --------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-progress-bar) 't)
 "The @code{\"text\"} property of type @code{:string} (Read / Write) @br{}
  Text to be displayed in the progress bar. @br{}
  Default value: @code{nil}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-progress-bar-text atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-progress-bar-text 'function)
 "@version{2014-3-16}
  @argument[object]{a @class{gtk-progress-bar} widget}
  @argument[text]{a UTF-8 string, or @code{nil}}
  @syntax[]{(gtk-progress-bar-text object) => text}
  @syntax[]{(setf (gtk-progress-bar-text object) text)}
  @begin{short}
    Accessor of the slot @slot[gtk-progress-bar]{text} of the
    @class{gtk-progress-bar} class.
  @end{short}

  The generic function @sym{gtk-progress-bar-text} retrieves the text displayed
  superimposed on the progress bar, if any, otherwise @code{nil}. The return
  value is a reference to the text, not a copy of it, so will become invalid if
  you change the text in the progress bar.

  The generic function @sym{(setf gtk-progress-bar-text)} causes the given text
  to appear superimposed on the progress bar.

  If text is @code{nil} and the @slot[gtk-progress-bar]{show-text} property is
  @em{true}, the current value of the @slot[gtk-progress-bar]{fraction} property
  will be displayed as a percentage.

  If text is non-@code{nil} and the @slot[gtk-progress-bar]{show-text} property
  is @em{true}, the text will be displayed. In this case, it will not display
  the progress percentage. If text is the empty string, the progress bar will
  still be styled and sized suitably for containing text, as long as the
  @slot[gtk-progress-bar]{show-text} property is @em{true}.
  @see-class{gtk-progress-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-new))

(defun gtk-progress-bar-new ()
 #+cl-cffi-gtk-documentation
 "@version{2014-3-16}
  @return{A @class{gtk-progress-bar} widget.}
  Creates a new @class{gtk-progress-bar} widget.
  @see-class{gtk-progress-bar}"
  (make-instance 'gtk-progress-bar))

(export 'gtk-progress-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_pulse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_progress_bar_pulse" gtk-progress-bar-pulse) :void
 #+cl-cffi-gtk-documentation
 "@version{2014-3-16}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @begin{short}
    Indicates that some progress has been made, but you do not know how much.
    Causes the progress bar to enter \"activity mode\", where a block bounces
    back and forth.
  @end{short}

  Each call to @sym{gtk-progress-bar-pulse} causes the block to move by
  a little bit, the amount of movement per pulse is determined by the generic
  function @fun{gtk-progress-bar-pulse-step}.
  @see-class{gtk-progress-bar}
  @see-function{gtk-progress-bar-pulse-step}"
  (pbar (g-object gtk-progress-bar)))

(export 'gtk-progress-bar-pulse)

;;; --- End of file gtk.progress-bar.lisp --------------------------------------

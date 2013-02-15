;;; ----------------------------------------------------------------------------
;;; atdoc-gtk.progress-bar.lisp
;;;
;;; Documentation strings for the library GTK+.
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
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
;;; 
;;; Style Properties
;;; 
;;;   "min-horizontal-bar-height" gint                  : Read / Write
;;;   "min-horizontal-bar-width"  gint                  : Read / Write
;;;   "min-vertical-bar-height"   gint                  : Read / Write
;;;   "min-vertical-bar-width"    gint                  : Read / Write
;;;   "xspacing"                  gint                  : Read / Write
;;;   "yspacing"                  gint                  : Read / Write
;;; 
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Style Property Details
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-horizontal-bar-height" style property
;;; 
;;;   "min-horizontal-bar-height" gint                  : Read / Write
;;; 
;;; Minimum horizontal height of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 20
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-horizontal-bar-width" style property
;;; 
;;;   "min-horizontal-bar-width" gint                  : Read / Write
;;; 
;;; The minimum horizontal width of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 150
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-vertical-bar-height" style property
;;; 
;;;   "min-vertical-bar-height"  gint                  : Read / Write
;;; 
;;; The minimum vertical height of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 80
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "min-vertical-bar-width" style property
;;; 
;;;   "min-vertical-bar-width"   gint                  : Read / Write
;;; 
;;; The minimum vertical width of the progress bar.
;;; 
;;; Allowed values: >= 1
;;; 
;;; Default value: 22
;;; 
;;; Since 2.14
;;;
;;; ----------------------------------------------------------------------------
;;; The "xspacing" style property
;;; 
;;;   "xspacing"                 gint                  : Read / Write
;;; 
;;; Extra spacing applied to the width of a progress bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 7
;;;
;;; ----------------------------------------------------------------------------
;;; The "yspacing" style property
;;; 
;;;   "yspacing"                 gint                  : Read / Write
;;; 
;;; Extra spacing applied to the height of a progress bar.
;;; 
;;; Allowed values: >= 0
;;; 
;;; Default value: 7
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; --- gtk-progress-bar -------------------------------------------------------

(setf (documentation 'gtk-progress-bar 'type)
 "@version{2013-2-2}
  @begin{short}
    The GtkProgressBar is typically used to display the progress of a long
    running operation. It provides a visual clue that processing is underway.
    The GtkProgressBar can be used in two different modes: percentage mode and
    activity mode.
  @end{short}

  When an application can determine how much work needs to take place (e.g.
  read a fixed number of bytes from a file) and can monitor its progress, it
  can use the GtkProgressBar in percentage mode and the user sees a growing
  bar indicating the percentage of the work that has been completed. In this
  mode, the application is required to call gtk_progress_bar_set_fraction()
  periodically to update the progress bar.

  When an application has no accurate way of knowing the amount of work to do,
  it can use the GtkProgressBar in activity mode, which shows activity by a
  block moving back and forth within the progress area. In this mode, the
  application is required to call gtk_progress_bar_pulse() periodically to
  update the progress bar.

  There is quite a bit of flexibility provided to control the appearance of
  the GtkProgressBar. Functions are provided to control the orientation of the
  bar, optional text can be displayed along with the bar, and the step size
  used in activity mode can be set.
  @see-slot{gtk-progress-bar-ellipsize}
  @see-slot{gtk-progress-bar-fraction}
  @see-slot{gtk-progress-bar-inverted}
  @see-slot{gtk-progress-bar-pulse-step}
  @see-slot{gtk-progress-bar-show-text}
  @see-slot{gtk-progress-bar-text}
")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "ellipsize" 'gtk-progress-bar) 't)
 "The @code{\"ellipsize\"} property of type @symbol{pango-ellipsize-mode}
  (Read / Write)@br{}
  The preferred place to ellipsize the string, if the progress bar does not
  have enough room to display the entire string, specified as a
  @symbol{pango-ellipsize-mode}.@br{}
  Note that setting this property to a value other than @code{:none} has the
  side-effect that the progress bar requests only enough space to display the
  ellipsis (\"...\"). Another means to set a progress bar's width is
  @fun{gtk-widget-set-size-request}.@br{}
  Default value: @code{:none}@br{}
  Since 2.6")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "fraction" 'gtk-progress-bar) 't)
 "The @code{\"fraction\"} property of type @code{gdouble} (Read / Write)@br{}
  The fraction of total work that has been completed.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "inverted" 'gtk-progress-bar) 't)
 "The @code{\"inverted\"} property of type @code{gboolean} (Read / Write)@br{}
  Invert the direction in which the progress bar grows.@br{}
  Default value: @code{nil}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "pulse-step" 'gtk-progress-bar) 't)
 "The @code{\"pulse-step\"} property of type @code{gdouble} (Read / Write)@br{}
  The fraction of total progress to move the bouncing block when pulsed.@br{}
  Allowed values: @code{[0,1]}@br{}
  Default value: @code{0.1}")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "show-text" 'gtk-progress-bar) 't)
 "The @code{\"show-text\"} property of type @code{gboolean} (Read / Write)@br{}
  Sets whether the progress bar will show text superimposed over the bar.
  The shown text is either the value of the @code{\"text\"} property or, if that
  is @code{nil}, the @code{\"fraction\"} value, as a percentage.@br{}
  To make a progress bar that is styled and sized suitably for containing text
  (even if the actual text is blank), set @code{\"show-text\"} to @arg{true} and
  @code{\"text\"} to the empty string (not @code{nil}).@br{}
  Default value: @code{nil}@br{}
  Since 3.0")

;;; ----------------------------------------------------------------------------

(setf (documentation (atdoc:get-slot-from-name "text" 'gtk-progress-bar) 't)
 "The @code{\"text\"} property of type @code{gchar*} (Read / Write)@br{}
  Text to be displayed in the progress bar.@br{}
  Default value: @code{nil}")

;;; --- gtk-progress-bar-new ---------------------------------------------------

(setf (documentation 'gtk-progress-bar-new 'function)
 "@version{2013-2-2}
  @return{A @class{gtk-progress-bar} widget.}
  @begin{short}
    Creates a new GtkProgressBar.
  @end{short}")

;;; --- gtk-progress-bar-pulse -------------------------------------------------

(setf (documentation 'gtk-progress-bar-pulse 'function)
 "@version{2013-2-2}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @begin{short}
    Indicates that some progress has been made, but you don't know how much.
    Causes the progress bar to enter \"activity mode\", where a block bounces
    back and forth.
  @end{short}
  Each call to @sym{gtk-progress-bar-pulse} causes the block to move by
  a little bit (the amount of movement per pulse is determined by
  @fun{gtk-progress-bar-set-pulse-step}).
  @see-function{gtk-progress-bar-set-pulse-step}")

;;; --- gtk-progress-bar-set-fraction ------------------------------------------

(setf (documentation 'gtk-progress-bar-set-fraction 'function)
 "@version{2013-2-2}
  @argument[pbar]{a GtkProgressBar}
  @argument[fraction]{fraction of the task that's been completed}
  @begin{short}
    Causes the progress bar to \"fill in\" the given fraction of the bar.
  @end{short}
  The fraction should be between 0.0 and 1.0, inclusive.")

;;; --- gtk-progress-bar-get-fraction ------------------------------------------

(setf (documentation 'gtk-progress-bar-get-fraction 'function)
 "@version{2013-2-2}
  @argument[pbar]{a GtkProgressBar}
  @return{a fraction from 0.0 to 1.0}
  @begin{short}
    Returns the current fraction of the task that's been completed.
  @end{short}")

;;; --- gtk-progress-bar-set-inverted ------------------------------------------

(setf (documentation 'gtk-progress-bar-set-inverted 'function)
 "@version{2013-2-2}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @argument[inverted]{TRUE to invert the progress bar}
  @begin{short}
    Progress bars normally grow from top to bottom or left to right. Inverted
    progress bars grow in the opposite direction.
  @end{short}")

;;; -- gtk-progress-bar-get-inverted -------------------------------------------

(setf (documentation 'gtk-progress-bar-get-inverted 'function)
 "@version{2013-2-2}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @return{TRUE if the progress bar is inverted}
  @begin{short}
    Gets the value set by @fun{gtk-progress-bar-set-inverted}.
  @end{short}
  @see-function{gtk-progress-bar-set-inverted}")

;;; --- gtk-progress-bar-set-show-text -----------------------------------------

(setf (documentation 'gtk-progress-bar-set-show-text 'function)
 "@version{2013-2-2}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @argument[show-text]{whether to show superimposed text}
  @begin{short}
    Sets whether the progress bar will show text superimposed over the bar. The
    shown text is either the value of the @code{\"text\"} property or, if that
    is @code{nil}, the @code{\"fraction\"} value, as a percentage.
  @end{short}
  To make a progress bar that is styled and sized suitably for containing text
  (even if the actual text is blank), set @code{\"show-text\"} to @arg{true} and
  @code{\"text\"} to the empty string (not @code{nil}.

  Since 3.0")

;;; --- gtk-progress-bar-get-show-text -----------------------------------------

(setf (documentation 'gtk-progress-bar-get-show-text 'function)
 "@version{2013-2-2}
  @argument[pbar]{a @class{gtk-progress-bar} widget}
  @return{@arg{true} if text is shown in the progress bar}
  @begin{short}
    Gets the value of the @code{\"show-text\"} property.
  @end{short}
  See @fun{gtk-progress-bar-set-show-text}.

  Since 3.0
  @see-function{gtk-progress-bar-set-show-text}")

#|
;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_set_text ()
;;; 
;;; void gtk_progress_bar_set_text (GtkProgressBar *pbar, const gchar *text);
;;; 
;;; Causes the given text to appear superimposed on the progress bar.
;;; 
;;; If text is NULL and "show-text" is TRUE, the current value of "fraction"
;;; will be displayed as a percentage.
;;; 
;;; If text is non-NULL and "show-text" is TRUE, the text will be displayed. In
;;; this case, it will not display the progress percentage. If text is the empty
;;; string, the progress bar will still be styled and sized suitably for
;;; containing text, as long as "show-text" is TRUE.
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; text :
;;;     a UTF-8 string, or NULL
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-set-text))

(defun gtk-progress-bar-set-text (pbar text)
  (setf (gtk-progress-bar-text pbar) text))

(export 'gtk-progress-bar-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_get_text ()
;;; 
;;; const gchar * gtk_progress_bar_get_text (GtkProgressBar *pbar);
;;; 
;;; Retrieves the text displayed superimposed on the progress bar, if any,
;;; otherwise NULL. The return value is a reference to the text, not a copy of
;;; it, so will become invalid if you change the text in the progress bar.
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; Returns :
;;;     text, or NULL; this string is owned by the widget and should not be
;;;     modified or freed
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-get-text))

(defun gtk-progress-bar-get-text (pbar)
  (gtk-progress-bar-text pbar))

(export 'gtk-progress-bar-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_set_ellipsize ()
;;; 
;;; void gtk_progress_bar_set_ellipsize (GtkProgressBar *pbar,
;;;                                      PangoEllipsizeMode mode);
;;; 
;;; Sets the mode used to ellipsize (add an ellipsis: "...") the text if there
;;; is not enough space to render the entire string.
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; mode :
;;;     a PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-set-ellipsize))

(defun gtk-progress-bar-set-ellipsize (pbar mode)
  (setf (gtk-progress-bar-ellipsize pbar) mode))

(export 'gtk-progress-bar-set-ellipsize)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_get_ellipsize ()
;;; 
;;; PangoEllipsizeMode gtk_progress_bar_get_ellipsize (GtkProgressBar *pbar);
;;; 
;;; Returns the ellipsizing position of the progress bar.
;;; See gtk_progress_bar_set_ellipsize().
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; Returns :
;;;     PangoEllipsizeMode
;;; 
;;; Since 2.6
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-get-ellipsize))

(defun gtk-progress-bar-get-ellipsize (pbar)
  (gtk-progress-bar-ellipsize pbar))

(export 'gtk-progress-bar-get-ellipsize)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_set_pulse_step ()
;;; 
;;; void gtk_progress_bar_set_pulse_step (GtkProgressBar *pbar,
;;;                                       gdouble fraction);
;;; 
;;; Sets the fraction of total progress bar length to move the bouncing block
;;; for each call to gtk_progress_bar_pulse().
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; fraction :
;;;     fraction between 0.0 and 1.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-set-pulse-step))

(defun gtk-progress-bar-set-pulse-step (pbar fraction)
  (setf (gtk-progress-bar-pulse-step pbar) fraction))

(export 'gtk-progress-bar-set-pulse-step)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_get_pulse_step ()
;;; 
;;; gdouble gtk_progress_bar_get_pulse_step (GtkProgressBar *pbar);
;;; 
;;; Retrieves the pulse step set with gtk_progress_bar_set_pulse_step().
;;; 
;;; pbar :
;;;     a GtkProgressBar
;;; 
;;; Returns :
;;;     a fraction from 0.0 to 1.0
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-progress-bar-get-pulse-step))

(defun gtk-progress-bar-get-pulse-step (pbar)
  (gtk-progress-bar-pulse-step pbar))

(export 'gtk-progress-bar-get-pulse-step)

|#

;;; --- End of file atdoc-gtk.progress-bar.lisp --------------------------------

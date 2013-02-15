;;; ----------------------------------------------------------------------------
;;; gtk.link-button.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.4.1. See http://www.gtk.org.
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

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; Class gtk-link-button
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLinkButton" gtk-link-button
  (:superclass gtk-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActionable"
                 "GtkActivatable")
    :type-initializer "gtk_link_button_get_type")
  ((uri
    gtk-link-button-uri
    "uri" "gchararray" t t)
   (visited
    gtk-link-button-visited
    "visited" "gboolean" t t)))

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-link-button 'type)
 "@version{2013-2-4}
  @begin{short}
    A @sym{gtk-link-button} is a @class{gtk-button} with a hyperlink, similar to
    the one used by web browsers, which triggers an action when clicked. It is
    useful to show quick links to resources.
  @end{short}
  A link button is created by calling either @fun{gtk-link-button-new} or
  @fun{gtk-link-button-new-with-label}. If using the former, the URI you pass to
  the constructor is used as a label for the widget.

  The URI bound to a @sym{gtk-link-button} can be set specifically using
  @fun{gtk-link-button-set-uri}, and retrieved using
  @fun{gtk-link-button-get-uri}.

  By default, @sym{gtk-link-button} calls @fun{gtk-show-uri} when the button is
  clicked. This behaviour can be overridden by connecting to the
  \"activate-link\" signal and returning @arg{true} from the signal handler.
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      The \"activate-link\" signal is emitted each time the GtkLinkButton has
      been clicked.
      The default handler will call @fun{gtk-show-uri} with the URI stored
      inside the @code{\"uri\"} property.
      To override the default behavior, you can connect to the \"activate-link\"
      signal and stop the propagation of the signal by returning @arg{true} from
      your handler.
      @begin{pre}
 gboolean user_function (GtkLinkButton *button,
                         gpointer user_data)        : Run Last
      @end{pre}
      @begin[code]{table}
        @entry[button]{the @sym{gtk-link-button} that emitted the signal}
        @entry[user_data]{user data set when the signal handler was connected}
      @end{table}
  @end{dictionary}
  @see-slot{gtk-link-button-uri}
  @see-slot{gtk-link-button-visited}")

;;; ----------------------------------------------------------------------------
;;;
;;; Property Details
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "uri" 'gtk-link-button) 't)
 "The @code{\"uri\"} property of type @code{gchar*} (Read / Write)@br{}
  The URI bound to this button.@br{}
  Default value: @code{nil}@br{}
  Since 2.10")

;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "visited" 'gtk-link-button) 't)
 "The @code{\"visited\"} property of type @code{gboolean} (Read / Write)@br{}
  The \"visited\" state of this button. A visited link is drawn in a different
  color.@br{}
  Default value: @code{nil}@br{}
  Since 2.14")

;;; ----------------------------------------------------------------------------
;;;
;;; Accessors
;;;
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-link-button-uri atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-link-button-uri 'function)
 "@version{2013-2-4}
  @begin{short}
    Accessor of the slot \"uri\" of the @class{gtk-link-button} class.
  @end{short}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-link-button-visited atdoc:*function-name-alias*) "Accessor"
      (documentation 'gtk-link-button-visited 'function)
 "@version{2013-2-4}
  @begin{short}
    Accessor of the slot \"visited\" of the @class{gtk-link-button} class.
  @end{short}")

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-new
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-new))

(defun gtk-link-button-new (uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[uri]{a valid URI}
  @return{A new link button widget.}
  @begin{short}
    Creates a new @class{gtk-link-button} widget with the URI as its text.
  @end{short}@break{}
  Since 2.10"
  (make-instance 'gtk-link-button
                 :uri uri
                 :label uri))

(export 'gtk-link-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-new-with-label
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-new-with-label))

(defun gtk-link-button-new-with-label (uri label)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[uri]{a valid URI}
  @argument[label]{the text of the button}
  @return{A new link button widget.}
  @begin{short}
    Creates a new @class{gtk-link-button} widget containing a label.
  @end{short}@break{}
  Since 2.10"
  (make-instance 'gtk-link-button
                 :uri uri
                 :label label))

(export 'gtk-link-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-get-uri
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-get-uri))

(defun gtk-link-button-get-uri (link-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[link-button]{a @class{gtk-link-button} widget}
  @return{A valid URI.}
  @begin{short}
    Retrieves the URI set using @fun{gtk-link-button-set-uri}.
  @end{short}@break{}
  Since 2.10"
  (gtk-link-button-uri link-button))

(export 'gtk-link-button-get-uri)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-set-uri
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-set-uri))

(defun gtk-link-button-set-uri (link-button uri)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[link-button]{a @class{gtk-link-button} widget}
  @argument[uri]{A valid URI.}
  @begin{short}
    Sets uri as the URI where the @class{gtk-link-button} points.
  @end{short}
  As a side-effect this unsets the 'visited' state of the button.@break{}
  Since 2.10"
  (setf (gtk-link-button-uri link-button) uri))

(export 'gtk-link-button-set-uri)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-get-visited
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-get-visited))

(defun gtk-link-button-get-visited (link-button)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[link-button]{a @class{gtk-link-button} widget}
  @return{@arg{true} if the link has been visited, @code{nil} otherwise}
  @begin{short}
    Retrieves the \"visited\" state of the URI where the @class{gtk-link-button}
    points.
  @end{short}
  The button becomes visited when it is clicked. If the URI is changed on the
  button, the 'visited' state is unset again.@break{}
  The state may also be changed using gtk_link_button_set_visited().@break{}
  Since 2.14"
  (gtk-link-button-visited link-button))

(export 'gtk-link-button-get-visited)

;;; ----------------------------------------------------------------------------
;;; gtk-link-button-set-visited
;;; ----------------------------------------------------------------------------

(declaim (inline gtk-link-button-set-visited))

(defun gtk-link-button-set-visited (link-button visited)
 #+cl-cffi-gtk-documentation
 "@version{2013-2-4}
  @argument[link-button]{a @class{gtk-link-button} widget}
  @argument[visited]{the new \"visited\" state}
  @begin{short}
    Sets the \"visited\" state of the URI where the @class{gtk-link-button}
    points.
  @end{short}@break{}
  See @fun{gtk-link-button-get-visited} for more details.@break{}
  Since 2.14"
  (setf (gtk-link-button-visited link-button) visited))

(export 'gtk-link-button-set-visited)

;;; --- End of file gtk.link-button.lisp ---------------------------------------

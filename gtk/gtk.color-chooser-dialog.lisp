;;; ----------------------------------------------------------------------------
;;; gtk.color-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; GtkColorChooserDialog
;;;
;;;     A dialog for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_dialog_new
;;;
;;; Properties
;;;
;;;     gboolean   show-editor    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkColorChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorChooserDialog implements AtkImplementorIface, GtkBuildable and
;;;     GtkColorChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorChooserDialog" gtk-color-chooser-dialog
  (:superclass gtk-dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_dialog_get_type")
  ((show-editor
    gtk-color-chooser-dialog-show-editor
    "show-editor" "gboolean" t t)))

#+cl-cffi-gtk-documentation
(setf (documentation 'gtk-color-chooser-dialog 'type)
 "@version{*2021-2-4}
  @begin{short}
    The @sym{gtk-color-chooser-dialog} widget is a dialog for choosing a color.
  @end{short}
  It implements the @class{gtk-color-chooser} interface.

  @image[colorchooser]{}
  @begin[Example]{dictionary}
    Clicking on the drawing area opens a color chooser dialog to select a
    background color for the drawing area. The default palettes are replaced
    for this color chooser dialog.
    @begin{pre}
(let ((message \"Click to change the background color.\")
      (bg-color (gdk-rgba-parse \"White\"))
      ;; Color palette with 9 Red RGBA colors
      (palette1 (list (gdk-rgba-parse \"IndianRed\")
                      (gdk-rgba-parse \"LightCoral\")
                      (gdk-rgba-parse \"Salmon\")
                      (gdk-rgba-parse \"DarkSalmon\")
                      (gdk-rgba-parse \"LightSalmon\")
                      (gdk-rgba-parse \"Crimson\")
                      (gdk-rgba-parse \"Red\")
                      (gdk-rgba-parse \"FireBrick\")
                      (gdk-rgba-parse \"DarkRed\")))
      ;; Gray palette with 9 gray RGBA colors
      (palette2 (list (gdk-rgba-parse \"Gainsboro\")
                      (gdk-rgba-parse \"LightGray\")
                      (gdk-rgba-parse \"Silver\")
                      (gdk-rgba-parse \"DarkGray\")
                      (gdk-rgba-parse \"Gray\")
                      (gdk-rgba-parse \"DimGray\")
                      (gdk-rgba-parse \"LightSlateGray\")
                      (gdk-rgba-parse \"SlateGray\")
                      (gdk-rgba-parse \"DarkSlateGray\"))))

  (defun example-color-chooser-dialog ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title \"Example Color Chooser Dialog\"
                                   :default-width 400))
            (area (make-instance 'gtk-drawing-area)))
        (g-signal-connect window \"destroy\"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Draw the background color and a hint on the drawing area
        (g-signal-connect area \"draw\"
            (lambda (widget cr)
              (declare (ignore widget))
              (let ((cr (pointer cr))
                    (red (gdk-rgba-red bg-color))
                    (green (gdk-rgba-green bg-color))
                    (blue (gdk-rgba-blue bg-color)))
                    ;; Paint the current color on the drawing area
                    (cairo-set-source-rgb cr red green blue)
                    (cairo-paint cr)
                    ;; Print a hint on the drawing area
                    (cairo-set-source-rgb cr (- 1 red) (- 1 green) (- 1 blue))
                    (cairo-select-font-face cr \"Sans\")
                    (cairo-set-font-size cr 12)
                    (cairo-move-to cr 12 24)
                    (cairo-show-text cr message)
                    (cairo-destroy cr))))
        ;; Create and run a color chooser dialog to select a background color
        (g-signal-connect area \"event\"
            (lambda (widget event)
              (declare (ignore widget))
              (when (eq (gdk-event-type event) :button-press)
                (let ((dialog (make-instance 'gtk-color-chooser-dialog
                                             :use-alpha nil)))
                  ;; Add a custom palette to the dialog
                  (gtk-color-chooser-add-palette dialog :vertical 1 palette1)
                  ;; Add a second coustom palette to the dialog
                  (gtk-color-chooser-add-palette dialog :vertical 1 palette2)
                  ;; Set the actual background color for the color chooser
                  (setf (gtk-color-chooser-rgba dialog) bg-color)
                  ;; Run the color chooser dialog
                  (let ((response (gtk-dialog-run dialog)))
                    (when (eq response :ok)
                      ;; Change the background color for the drawing area
                      (setf bg-color (gtk-color-chooser-rgba dialog)))
                      ;; Destroy the color chooser dialog
                      (gtk-widget-destroy dialog))))))
        ;; Set the event mask for the drawing area
        (setf (gtk-widget-events area) :button-press-mask)
        ;; Add the drawing area to the window
        (gtk-container-add window area)
        (gtk-widget-show-all window)))))
    @end{pre}
  @end{dictionary}
  @see-slot{gtk-color-chooser-dialog-show-editor}
  @see-class{gtk-color-chooser}
  @see-class{gtk-color-button}
  @see-class{gtk-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

#+cl-cffi-gtk-documentation
(setf (documentation (atdoc:get-slot-from-name "show-editor"
                                               'gtk-color-chooser-dialog) 't)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} when the color chooser dialog is showing the single-color editor.
  It can be set to switch the color chooser into single-color editing mode.@br{}
  Default value: @em{false}")

#+cl-cffi-gtk-documentation
(setf (gethash 'gtk-color-chooser-dialog-show-editor atdoc:*function-name-alias*)
      "Accessor"
      (documentation 'gtk-color-chooser-dialog-show-editor 'function)
 "@version{2020-5-23}
  @syntax[]{(gtk-color-chooser-dialog-show-editor object) => show-editor}
  @syntax[]{(setf (gtk-color-chooser-dialog-show-editor object) show-editor)}
  @argument[object]{a @class{gtk-color-chooser-dialog} widget}
  @argument[show-editor]{a boolean wether to show the single-color editor}
  @begin{short}
    Accessor of the @slot[gtk-color-chooser-dialog]{show-editor} slot of the
    @class{gtk-color-chooser-dialog} class.
  @end{short}

  @em{True} when the color chooser dialog is showing the single-color editor.
  It can be set to switch the color chooser into single-color editing mode.@br{}
  @see-class{gtk-color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun gtk-color-chooser-dialog-new (title parent)
 #+cl-cffi-gtk-documentation
 "@version{2020-5-23}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk-window} transient parent of the dialog,
    or @code{nil}}
  @return{A new @class{gtk-color-chooser-dialog} widget.}
  @short{Creates a new color chooser dialog.}
  @see-class{gtk-window}
  @see-class{gtk-color-chooser-dialog}"
  (make-instance 'gtk-color-chooser-dialog
                 :title (if title title (null-pointer))
                 :parent parent))

(export 'gtk-color-chooser-dialog-new)

;;; --- End of file gtk.color-chooser-dialog.lisp ------------------------------

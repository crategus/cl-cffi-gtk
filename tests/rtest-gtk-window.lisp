;;; ----------------------------------------------------------------------------
;;; rtest-gtk-window.lisp
;;;
;;; Copyright (C) 2011 - 2012 Dieter Kaiser
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

(in-package :gtk-tests)

(define-test gtk-window
  (let* ((window (gtk-window-new :toplevel))
         (type (g-type-from-instance (pointer window))))
    (assert-equal "GtkWindow" (gtype-name type))
    (assert-eql 'gtk-window (registered-object-type-by-name "GtkWindow"))
    (assert-equal "GtkBin" (gtype-name (g-type-parent type)))
    (unordered-equal '("GtkDialog" "GtkPlug" "GtkAssistant")
                     (mapcar #'gtype-name (g-type-children type)))
    
    ;; Access all available slots and lookup the default values.
    (assert-true         (gtk-window-accept-focus window))
    ;; application is missing
    (assert-true         (gtk-window-decorated window))
    (assert-eql -1       (gtk-window-default-height window))
    (assert-eql -1       (gtk-window-default-width window))
    (assert-true         (gtk-window-deletable window))
    (assert-false        (gtk-window-destroy-with-parent window))
    (assert-true         (gtk-window-focus-on-map window))
    (assert-true         (gtk-window-focus-visible window))
    (assert-eq :north-west (gtk-window-gravity window))
    (assert-true         (gtk-window-has-resize-grip window))
    (assert-false        (gtk-window-has-toplevel-focus window))
    (assert-false        (gtk-window-icon window))
    (assert-false        (gtk-window-icon-name window))
    (assert-false        (gtk-window-is-active window))
    (assert-true         (gtk-window-mnemonics-visible window))
    (assert-false        (gtk-window-modal window))
    (assert-eql 1.0d0    (gtk-window-opacity window))
    (assert-true         (gtk-window-resizable window))
    (assert-true         (gtk-window-resize-grip-visible window))
    (assert-false        (gtk-window-role window))
    (assert-equal "GdkX11Screen"
                         (gtype-name
                           (g-type-from-instance
                             (pointer (gtk-window-screen window)))))
    (assert-false        (gtk-window-skip-pager-hint window))
    (assert-false        (gtk-window-skip-taskbar-hint window))
    ;; startup-id is only writable
    (assert-false        (gtk-window-title window))
    (assert-false        (gtk-window-transient-for window))
    (assert-eq :toplevel (gtk-window-type window))
    (assert-eq :normal   (gtk-window-type-hint window))
    (assert-false        (gtk-window-ubuntu-no-proxy window))
    (assert-false        (gtk-window-urgency-hint window))
    (assert-eq :none     (gtk-window-window-position window))
    
    ;; Check the defintion of the class gtk-window
    (assert-equal
      '(DEFINE-G-OBJECT-CLASS "GtkWindow" GTK-WINDOW
                               (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_window_get_type")
                               ((ACCEPT-FOCUS GTK-WINDOW-ACCEPT-FOCUS
                                 "accept-focus" "gboolean" T T)
                                (APPLICATION GTK-WINDOW-APPLICATION
                                 "application" "GtkApplication" T T)
                                (ATTACHED-TO GTK-WINDOW-ATTACHED-TO
                                 "attached-to" "GtkWidget" T T)
                                (DECORATED GTK-WINDOW-DECORATED "decorated"
                                 "gboolean" T T)
                                (DEFAULT-HEIGHT GTK-WINDOW-DEFAULT-HEIGHT
                                 "default-height" "gint" T T)
                                (DEFAULT-WIDTH GTK-WINDOW-DEFAULT-WIDTH
                                 "default-width" "gint" T T)
                                (DELETABLE GTK-WINDOW-DELETABLE "deletable"
                                 "gboolean" T T)
                                (DESTROY-WITH-PARENT
                                 GTK-WINDOW-DESTROY-WITH-PARENT
                                 "destroy-with-parent" "gboolean" T T)
                                (FOCUS-ON-MAP GTK-WINDOW-FOCUS-ON-MAP
                                 "focus-on-map" "gboolean" T T)
                                (FOCUS-VISIBLE GTK-WINDOW-FOCUS-VISIBLE
                                 "focus-visible" "gboolean" T T)
                                (GRAVITY GTK-WINDOW-GRAVITY "gravity"
                                 "GdkGravity" T T)
                                (HAS-RESIZE-GRIP GTK-WINDOW-HAS-RESIZE-GRIP
                                 "has-resize-grip" "gboolean" T T)
                                (HAS-TOPLEVEL-FOCUS
                                 GTK-WINDOW-HAS-TOPLEVEL-FOCUS
                                 "has-toplevel-focus" "gboolean" T NIL)
                                (HIDE-TITLEBAR-WHEN-MAXIMIZED
                                 GTK-WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED
                                 "hide-titlebar-when-maximized" "gboolean" T T)
                                (ICON GTK-WINDOW-ICON "icon" "GdkPixbuf" T T)
                                (ICON-NAME GTK-WINDOW-ICON-NAME "icon-name"
                                 "gchararray" T T)
                                (IS-ACTIVE GTK-WINDOW-IS-ACTIVE "is-active"
                                 "gboolean" T NIL)
                                (MNEMONICS-VISIBLE GTK-WINDOW-MNEMONICS-VISIBLE
                                 "mnemonics-visible" "gboolean" T T)
                                (MODAL GTK-WINDOW-MODAL "modal" "gboolean" T T)
                                (OPACITY GTK-WINDOW-OPACITY "opacity" "gdouble"
                                 T T)
                                (RESIZABLE GTK-WINDOW-RESIZABLE "resizable"
                                 "gboolean" T T)
                                (RESIZE-GRIP-VISIBLE
                                 GTK-WINDOW-RESIZE-GRIP-VISIBLE
                                 "resize-grip-visible" "gboolean" T NIL)
                                (ROLE GTK-WINDOW-ROLE "role" "gchararray" T T)
                                (SCREEN GTK-WINDOW-SCREEN "screen" "GdkScreen"
                                 T T)
                                (SKIP-PAGER-HINT GTK-WINDOW-SKIP-PAGER-HINT
                                 "skip-pager-hint" "gboolean" T T)
                                (SKIP-TASKBAR-HINT GTK-WINDOW-SKIP-TASKBAR-HINT
                                 "skip-taskbar-hint" "gboolean" T T)
                                (STARTUP-ID GTK-WINDOW-STARTUP-ID "startup-id"
                                 "gchararray" NIL T)
                                (TITLE GTK-WINDOW-TITLE "title" "gchararray" T
                                 T)
                                (TRANSIENT-FOR GTK-WINDOW-TRANSIENT-FOR
                                 "transient-for" "GtkWindow" T T)
                                (TYPE GTK-WINDOW-TYPE "type" "GtkWindowType" T
                                 NIL)
                                (TYPE-HINT GTK-WINDOW-TYPE-HINT "type-hint"
                                 "GdkWindowTypeHint" T T)
                                (UBUNTU-NO-PROXY GTK-WINDOW-UBUNTU-NO-PROXY
                                 "ubuntu-no-proxy" "gboolean" T T)
                                (URGENCY-HINT GTK-WINDOW-URGENCY-HINT
                                 "urgency-hint" "gboolean" T T)
                                (WINDOW-POSITION GTK-WINDOW-WINDOW-POSITION
                                 "window-position" "GtkWindowPosition" T T)))

     (get-g-class-definition type))
    
    ;; Check the expansion of the class definition
    (assert-equal
      '(PROGN
         (DEFCLASS GTK-WINDOW (GTK-BIN ATK-IMPLEMENTOR-IFACE GTK-BUILDABLE)
                   ((ACCEPT-FOCUS :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-ACCEPT-FOCUS :INITARG :ACCEPT-FOCUS
                     :G-PROPERTY-NAME "accept-focus")
                    (APPLICATION :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GtkApplication" :ACCESSOR GTK-WINDOW-APPLICATION :INITARG
                     :APPLICATION :G-PROPERTY-NAME "application")
                    (ATTACHED-TO :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GtkWidget" :ACCESSOR GTK::GTK-WINDOW-ATTACHED-TO :INITARG
                     :ATTACHED-TO :G-PROPERTY-NAME "attached-to")
                    (DECORATED :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-WINDOW-DECORATED :INITARG
                     :DECORATED :G-PROPERTY-NAME "decorated")
                    (DEFAULT-HEIGHT :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gint" :ACCESSOR
                     GTK-WINDOW-DEFAULT-HEIGHT :INITARG :DEFAULT-HEIGHT
                     :G-PROPERTY-NAME "default-height")
                    (DEFAULT-WIDTH :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gint" :ACCESSOR GTK-WINDOW-DEFAULT-WIDTH
                     :INITARG :DEFAULT-WIDTH :G-PROPERTY-NAME "default-width")
                    (DELETABLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-WINDOW-DELETABLE :INITARG
                     :DELETABLE :G-PROPERTY-NAME "deletable")
                    (DESTROY-WITH-PARENT :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-DESTROY-WITH-PARENT :INITARG
                     :DESTROY-WITH-PARENT :G-PROPERTY-NAME
                     "destroy-with-parent")
                    (FOCUS-ON-MAP :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-FOCUS-ON-MAP :INITARG :FOCUS-ON-MAP
                     :G-PROPERTY-NAME "focus-on-map")
                    (FOCUS-VISIBLE :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-FOCUS-VISIBLE :INITARG :FOCUS-VISIBLE
                     :G-PROPERTY-NAME "focus-visible")
                    (GRAVITY :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GdkGravity" :ACCESSOR GTK-WINDOW-GRAVITY :INITARG
                     :GRAVITY :G-PROPERTY-NAME "gravity")
                    (HAS-RESIZE-GRIP :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-HAS-RESIZE-GRIP :INITARG :HAS-RESIZE-GRIP
                     :G-PROPERTY-NAME "has-resize-grip")
                    (HAS-TOPLEVEL-FOCUS :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-HAS-TOPLEVEL-FOCUS :INITARG :HAS-TOPLEVEL-FOCUS
                     :G-PROPERTY-NAME "has-toplevel-focus")
                    (HIDE-TITLEBAR-WHEN-MAXIMIZED :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK::GTK-WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED :INITARG
                     :HIDE-TITLEBAR-WHEN-MAXIMIZED :G-PROPERTY-NAME
                     "hide-titlebar-when-maximized")
                    (ICON :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GdkPixbuf" :ACCESSOR GTK-WINDOW-ICON :INITARG :ICON
                     :G-PROPERTY-NAME "icon")
                    (ICON-NAME :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-WINDOW-ICON-NAME :INITARG
                     :ICON-NAME :G-PROPERTY-NAME "icon-name")
                    (IS-ACTIVE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-WINDOW-IS-ACTIVE :INITARG
                     :IS-ACTIVE :G-PROPERTY-NAME "is-active")
                    (MNEMONICS-VISIBLE :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-MNEMONICS-VISIBLE :INITARG :MNEMONICS-VISIBLE
                     :G-PROPERTY-NAME "mnemonics-visible")
                    (MODAL :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-WINDOW-MODAL :INITARG :MODAL
                     :G-PROPERTY-NAME "modal")
                    (OPACITY :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gdouble" :ACCESSOR GTK-WINDOW-OPACITY :INITARG :OPACITY
                     :G-PROPERTY-NAME "opacity")
                    (RESIZABLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gboolean" :ACCESSOR GTK-WINDOW-RESIZABLE :INITARG
                     :RESIZABLE :G-PROPERTY-NAME "resizable")
                    (RESIZE-GRIP-VISIBLE :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-RESIZE-GRIP-VISIBLE :INITARG
                     :RESIZE-GRIP-VISIBLE :G-PROPERTY-NAME
                     "resize-grip-visible")
                    (ROLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-WINDOW-ROLE :INITARG :ROLE
                     :G-PROPERTY-NAME "role")
                    (SCREEN :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GdkScreen" :ACCESSOR GTK-WINDOW-SCREEN :INITARG :SCREEN
                     :G-PROPERTY-NAME "screen")
                    (SKIP-PAGER-HINT :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-SKIP-PAGER-HINT :INITARG :SKIP-PAGER-HINT
                     :G-PROPERTY-NAME "skip-pager-hint")
                    (SKIP-TASKBAR-HINT :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-SKIP-TASKBAR-HINT :INITARG :SKIP-TASKBAR-HINT
                     :G-PROPERTY-NAME "skip-taskbar-hint")
                    (STARTUP-ID :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-WINDOW-STARTUP-ID :INITARG
                     :STARTUP-ID :G-PROPERTY-NAME "startup-id")
                    (TITLE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "gchararray" :ACCESSOR GTK-WINDOW-TITLE :INITARG :TITLE
                     :G-PROPERTY-NAME "title")
                    (TRANSIENT-FOR :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "GtkWindow" :ACCESSOR
                     GTK-WINDOW-TRANSIENT-FOR :INITARG :TRANSIENT-FOR
                     :G-PROPERTY-NAME "transient-for")
                    (TYPE :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GtkWindowType" :ACCESSOR GTK-WINDOW-TYPE :INITARG :TYPE
                     :G-PROPERTY-NAME "type")
                    (TYPE-HINT :ALLOCATION :GOBJECT-PROPERTY :G-PROPERTY-TYPE
                     "GdkWindowTypeHint" :ACCESSOR GTK-WINDOW-TYPE-HINT
                     :INITARG :TYPE-HINT :G-PROPERTY-NAME "type-hint")
                    (UBUNTU-NO-PROXY :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-UBUNTU-NO-PROXY :INITARG :UBUNTU-NO-PROXY
                     :G-PROPERTY-NAME "ubuntu-no-proxy")
                    (URGENCY-HINT :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "gboolean" :ACCESSOR
                     GTK-WINDOW-URGENCY-HINT :INITARG :URGENCY-HINT
                     :G-PROPERTY-NAME "urgency-hint")
                    (WINDOW-POSITION :ALLOCATION :GOBJECT-PROPERTY
                     :G-PROPERTY-TYPE "GtkWindowPosition" :ACCESSOR
                     GTK-WINDOW-WINDOW-POSITION :INITARG :WINDOW-POSITION
                     :G-PROPERTY-NAME "window-position"))
                   (:METACLASS GOBJECT-CLASS) (:G-TYPE-NAME . "GtkWindow")
                   (:G-TYPE-INITIALIZER . "gtk_window_get_type"))
         (EXPORT 'GTK-WINDOW (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-ACCEPT-FOCUS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-APPLICATION (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK::GTK-WINDOW-ATTACHED-TO (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-DECORATED (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-DEFAULT-HEIGHT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-DEFAULT-WIDTH (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-DELETABLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-DESTROY-WITH-PARENT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-FOCUS-ON-MAP (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-FOCUS-VISIBLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-GRAVITY (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-HAS-RESIZE-GRIP (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-HAS-TOPLEVEL-FOCUS (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK::GTK-WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED
                 (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-ICON (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-ICON-NAME (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-IS-ACTIVE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-MNEMONICS-VISIBLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-MODAL (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-OPACITY (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-RESIZABLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-RESIZE-GRIP-VISIBLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-ROLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-SCREEN (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-SKIP-PAGER-HINT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-SKIP-TASKBAR-HINT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-STARTUP-ID (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-TITLE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-TRANSIENT-FOR (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-TYPE (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-TYPE-HINT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-UBUNTU-NO-PROXY (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-URGENCY-HINT (FIND-PACKAGE "GTK"))
         (EXPORT 'GTK-WINDOW-WINDOW-POSITION (FIND-PACKAGE "GTK")))
     ;; macroexpand the class definition
     (macroexpand-1 (get-g-class-definition (gtype "GtkWindow"))))
      ))

;;; --- End of the file rtest-gtk-window.lisp ----------------------------------

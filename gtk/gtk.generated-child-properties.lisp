;;; ----------------------------------------------------------------------------
;;; gtk.generated-child-properties.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
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

(in-package :gtk)



(define-child-property "GtkMenu" menu-child-left-attach "left-attach" "gint" t
                       t t)

(define-child-property "GtkMenu" menu-child-right-attach "right-attach" "gint"
                       t t t)

(define-child-property "GtkMenu" menu-child-top-attach "top-attach" "gint" t t
                       t)

(define-child-property "GtkMenu" menu-child-bottom-attach "bottom-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-left-attach "left-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-right-attach "right-attach"
                       "gint" t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-top-attach "top-attach" "gint"
                       t t t)

(define-child-property "GtkRecentChooserMenu"
                       recent-chooser-menu-child-bottom-attach "bottom-attach"
                       "gint" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-label "tab-label"
                       "gchararray" t t t)

(define-child-property "GtkNotebook" notebook-child-menu-label "menu-label"
                       "gchararray" t t t)

(define-child-property "GtkNotebook" notebook-child-position "position" "gint"
                       t t t)

(define-child-property "GtkNotebook" notebook-child-tab-expand "tab-expand"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-fill "tab-fill"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-tab-pack "tab-pack"
                       "GtkPackType" t t t)

(define-child-property "GtkNotebook" notebook-child-reorderable "reorderable"
                       "gboolean" t t t)

(define-child-property "GtkNotebook" notebook-child-detachable "detachable"
                       "gboolean" t t t)



(define-child-property "GtkButtonBox" button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkButtonBox" button-box-child-fill "fill" "gboolean" t
                       t t)

(define-child-property "GtkButtonBox" button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkButtonBox" button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkButtonBox" button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkButtonBox" button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkHButtonBox" h-button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-position "position"
                       "gint" t t t)

(define-child-property "GtkVButtonBox" v-button-box-child-secondary "secondary"
                       "gboolean" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserWidget" file-chooser-widget-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserWidget"
                       file-chooser-widget-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkFileChooserWidget"
                       file-chooser-widget-child-position "position" "gint" t t
                       t)

(define-child-property "GtkFontSelection" font-selection-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkFontSelection" font-selection-child-fill "fill"
                       "gboolean" t t t)

(define-child-property "GtkFontSelection" font-selection-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFontSelection" font-selection-child-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkFontSelection" font-selection-child-position
                       "position" "gint" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-expand "expand"
                       "gboolean" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-fill "fill" "gboolean"
                       t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-padding "padding"
                       "guint" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkGammaCurve" gamma-curve-child-position "position"
                       "gint" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-expand "expand" "gboolean" t
                       t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-fill "fill" "gboolean" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-padding "padding" "guint" t
                       t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkRecentChooserWidget"
                       recent-chooser-widget-child-position "position" "gint" t
                       t t)



(define-child-property "GtkFileChooserButton" file-chooser-button-child-expand
                       "expand" "gboolean" t t t)

(define-child-property "GtkFileChooserButton" file-chooser-button-child-fill
                       "fill" "gboolean" t t t)

(define-child-property "GtkFileChooserButton" file-chooser-button-child-padding
                       "padding" "guint" t t t)

(define-child-property "GtkFileChooserButton"
                       file-chooser-button-child-pack-type "pack-type"
                       "GtkPackType" t t t)

(define-child-property "GtkFileChooserButton"
                       file-chooser-button-child-position "position" "gint" t t
                       t)



(define-child-property "GtkFixed" fixed-child-x "x" "gint" t t t)

(define-child-property "GtkFixed" fixed-child-y "y" "gint" t t t)

(define-child-property "GtkPaned" paned-child-resize "resize" "gboolean" t t t)

(define-child-property "GtkPaned" paned-child-shrink "shrink" "gboolean" t t t)

(define-child-property "GtkHPaned" h-paned-child-resize "resize" "gboolean" t t
                       t)

(define-child-property "GtkHPaned" h-paned-child-shrink "shrink" "gboolean" t t
                       t)

(define-child-property "GtkVPaned" v-paned-child-resize "resize" "gboolean" t t
                       t)

(define-child-property "GtkVPaned" v-paned-child-shrink "shrink" "gboolean" t t
                       t)

(define-child-property "GtkLayout" layout-child-x "x" "gint" t t t)

(define-child-property "GtkLayout" layout-child-y "y" "gint" t t t)





(define-child-property "GtkToolbar" toolbar-child-expand "expand" "gboolean" t
                       t t)

(define-child-property "GtkToolbar" toolbar-child-homogeneous "homogeneous"
                       "gboolean" t t t)


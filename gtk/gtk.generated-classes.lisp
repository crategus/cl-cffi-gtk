;;; ----------------------------------------------------------------------------
;;; gtk.generated-classes.lisp
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

(define-g-enum "GtkUnit"
    unit
    (:export t :type-initializer "gtk_unit_get_type")
  (:pixel 0)
  (:points 1)
  (:inch 2)
  (:mm 3))

(define-g-enum "GtkNotebookTab"
    notebook-tab
    (:export t :type-initializer "gtk_notebook_tab_get_type")
  (:first 0)
  (:last 1))

(define-g-enum "GtkAnchorType"
    anchor-type
    (:export t :type-initializer "gtk_anchor_type_get_type")
  (:center 0)
  (:north 1)
  (:north-west 2)
  (:north-east 3)
  (:south 4)
  (:south-west 5)
  (:south-east 6)
  (:west 7)
  (:east 8)
  (:n 1)
  (:nw 2)
  (:ne 3)
  (:s 4)
  (:sw 5)
  (:se 6)
  (:w 7)
  (:e 8))

(define-g-enum "GtkCellType"
    cell-type
    (:export t :type-initializer "gtk_cell_type_get_type")
  (:empty 0)
  (:text 1)
  (:pixmap 2)
  (:pixtext 3)
  (:widget 4))

(define-g-enum "GtkCListDragPos"
    c-list-drag-pos
    (:export t :type-initializer "gtk_clist_drag_pos_get_type")
  (:none 0)
  (:before 1)
  (:into 2)
  (:after 3))

(define-g-enum "GtkCTreeExpanderStyle"
    c-tree-expander-style
    (:export t :type-initializer "gtk_ctree_expander_style_get_type")
  (:none 0)
  (:square 1)
  (:triangle 2)
  (:circular 3))

(define-g-enum "GtkCTreeExpansionType"
    c-tree-expansion-type
    (:export t :type-initializer "gtk_ctree_expansion_type_get_type")
  (:expand 0)
  (:expand-recursive 1)
  (:collapse 2)
  (:collapse-recursive 3)
  (:toggle 4)
  (:toggle-recursive 5))

(define-g-enum "GtkCTreeLineStyle"
    c-tree-line-style
    (:export t :type-initializer "gtk_ctree_line_style_get_type")
  (:none 0)
  (:solid 1)
  (:dotted 2)
  (:tabbed 3))

(define-g-enum "GtkCTreePos"
    c-tree-pos
    (:export t :type-initializer "gtk_ctree_pos_get_type")
  (:before 0)
  (:as-child 1)
  (:after 2))

(define-g-enum "GtkMatchType"
    match-type
    (:export t :type-initializer "gtk_match_type_get_type")
  (:all 0)
  (:all-tail 1)
  (:head 2)
  (:tail 3)
  (:exact 4)
  (:last 5))

(define-g-enum "GtkPreviewType"
    preview-type
    (:export t :type-initializer "gtk_preview_type_get_type")
  (:color 0)
  (:grayscale 1))

(define-g-enum "GtkSideType"
    side-type
    (:export t :type-initializer "gtk_side_type_get_type")
  (:top 0)
  (:bottom 1)
  (:left 2)
  (:right 3))

(define-g-enum "GtkSubmenuDirection"
    submenu-direction
    (:export t :type-initializer "gtk_submenu_direction_get_type")
  (:left 0)
  (:right 1))

(define-g-enum "GtkSubmenuPlacement"
    submenu-placement
    (:export t :type-initializer "gtk_submenu_placement_get_type")
  (:top-bottom 0)
  (:left-right 1))

(define-g-enum "GtkToolbarChildType"
    toolbar-child-type
    (:export t :type-initializer "gtk_toolbar_child_type_get_type")
  (:space 0)
  (:button 1)
  (:togglebutton 2)
  (:radiobutton 3)
  (:widget 4))

(define-g-enum "GtkTreeViewMode"
    tree-view-mode
    (:export t :type-initializer "gtk_tree_view_mode_get_type")
  (:line 0)
  (:item 1))

(define-g-enum "GtkVisibility"
    visibility
    (:export t :type-initializer "gtk_visibility_get_type")
  (:none 0)
  (:partial 1)
  (:full 2))

(define-g-flags "GtkButtonAction"
    button-action
    (:export t :type-initializer "gtk_button_action_get_type")
  (:ignored 0)
  (:selects 1)
  (:drags 2)
  (:expands 4))

(define-g-flags "GtkDebugFlag"
    debug-flag
    (:export t :type-initializer "gtk_debug_flag_get_type")
  (:misc 1)
  (:plugsocket 2)
  (:text 4)
  (:tree 8)
  (:updates 16)
  (:keybindings 32)
  (:multihead 64)
  (:modules 128)
  (:geometry 256)
  (:icontheme 512)
  (:printing 1024)
  (:builder 2048))

(define-g-flags "GtkPrivateFlags"
    private-flags
    (:export t :type-initializer "gtk_private_flags_get_type")
  (:user-style 1)
  (:resize-pending 4)
  (:has-pointer 8)
  (:shadowed 16)
  (:has-shape-mask 32)
  (:in-reparent 64)
  (:direction-set 128)
  (:direction-ltr 256)
  (:anchored 512)
  (:child-visible 1024)
  (:redraw-on-alloc 2048)
  (:alloc-needed 4096)
  (:request-needed 8192))

(define-g-flags "GtkSignalRunType"
    signal-run-type
    (:export t :type-initializer "gtk_signal_run_type_get_type")
  (:first 1)
  (:last 2)
  (:both 3)
  (:no-recurse 8)
  (:action 32)
  (:no-hooks 64))

;;; ----------------------------------------------------------------------------

(def-suite gtk-notebook :in gtk-suite)
(in-suite gtk-notebook)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNotebook

(test gtk-notebook-class
  ;; Type check
  (is-true  (g-type-is-object "GtkNotebook"))
  ;; Check the registered name
  (is (eq 'gtk-notebook
          (registered-object-type-by-name "GtkNotebook")))
  ;; Check the type initializer
  (is (string= "GtkNotebook"
               (g-type-name (gtype (foreign-funcall "gtk_notebook_get_type" :int)))))
  ;; Check the parent
  (is (equal (gtype "GtkContainer") (g-type-parent "GtkNotebook")))
  ;; Check the children
  (is (equal '()
             (mapcar #'gtype-name (g-type-children "GtkNotebook"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkNotebook"))))
  ;; Check the class properties
  (is (equal '("app-paintable" "border-width" "can-default" "can-focus" "child"
               "composite-child" "double-buffered" "enable-popup" "events" "expand"
               "focus-on-click" "group-name" "halign" "has-default" "has-focus" "has-tooltip"
               "height-request" "hexpand" "hexpand-set" "is-focus" "margin" "margin-bottom"
               "margin-end" "margin-left" "margin-right" "margin-start" "margin-top" "name"
               "no-show-all" "opacity" "page" "parent" "receives-default" "resize-mode"
               "scale-factor" "scrollable" "sensitive" "show-border" "show-tabs" "style"
               "tab-pos" "tooltip-markup" "tooltip-text" "valign" "vexpand" "vexpand-set"
               "visible" "width-request" "window")
             (stable-sort (mapcar #'param-spec-name
                                  (g-object-class-list-properties "GtkNotebook"))
                          #'string-lessp)))
  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern" "focus-line-width"
               "focus-padding" "interior-focus" "link-color" "scroll-arrow-hlength"
               "scroll-arrow-vlength" "secondary-cursor-color" "separator-height"
               "separator-width" "text-handle-height" "text-handle-width"
               "visited-link-color" "wide-separators" "window-dragging" "arrow-spacing"
               "has-backward-stepper" "has-forward-stepper" "has-secondary-backward-stepper"
               "has-secondary-forward-stepper" "has-tab-gap" "initial-gap" "tab-curvature"
               "tab-overlap")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkNotebook"))))
  ;; Get the names of the child properties
  (is (equal '("tab-label" "menu-label" "position" "tab-expand" "tab-fill" "reorderable"
               "detachable")
             (mapcar #'param-spec-name
                     (gtk-container-class-list-child-properties "GtkNotebook"))))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkNotebook" GTK-NOTEBOOK
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_notebook_get_type")
                       ((ENABLE-POPUP GTK-NOTEBOOK-ENABLE-POPUP "enable-popup"
                         "gboolean" T T)
                        (GROUP-NAME GTK-NOTEBOOK-GROUP-NAME "group-name"
                         "gchararray" T T)
                        (PAGE GTK-NOTEBOOK-PAGE "page" "gint" T T)
                        (SCROLLABLE GTK-NOTEBOOK-SCROLLABLE "scrollable"
                         "gboolean" T T)
                        (SHOW-BORDER GTK-NOTEBOOK-SHOW-BORDER "show-border"
                         "gboolean" T T)
                        (SHOW-TABS GTK-NOTEBOOK-SHOW-TABS "show-tabs"
                         "gboolean" T T)
                        (TAB-POS GTK-NOTEBOOK-TAB-POS "tab-pos"
                         "GtkPositionType" T T)))
             (get-g-type-definition "GtkNotebook"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-notebook-properties
  (let ((notebook (make-instance 'gtk-notebook)))
    (is-false (gtk-notebook-enable-popup notebook))
    (is-false (gtk-notebook-group-name notebook))
    (is (= -1 (gtk-notebook-page notebook)))
    (is-false (gtk-notebook-scrollable notebook))
    (is-true (gtk-notebook-show-border notebook))
    (is-true (gtk-notebook-show-tabs notebook))
    (is (eq :top (gtk-notebook-tab-pos notebook)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-notebook-child-properties
  (let ((notebook (make-instance 'gtk-notebook))
        (child (make-instance 'gtk-frame))
        (label (make-instance 'gtk-label :label "label")))

    (is (= 0 (gtk-notebook-append-page notebook child label)))

    (is-false (gtk-notebook-child-detachable notebook child))
    (is-false (gtk-notebook-child-menu-label notebook child))
    (is (= 0 (gtk-notebook-child-position notebook child)))
    (is-false (gtk-notebook-child-reorderable notebook child))
    (is-false (gtk-notebook-child-tab-expand notebook child))
    (is-true (gtk-notebook-child-tab-fill notebook child))
    (is (string= "label" (gtk-notebook-child-tab-label notebook child)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-notebook-style-properties
  (let ((notebook (make-instance 'gtk-notebook)))
    (is (= 0 (gtk-widget-style-get-property notebook "arrow-spacing")))
    (is-true (gtk-widget-style-get-property notebook "has-backward-stepper"))
    (is-true (gtk-widget-style-get-property notebook "has-forward-stepper"))
    (is-false (gtk-widget-style-get-property notebook "has-secondary-backward-stepper"))
    (is-false (gtk-widget-style-get-property notebook "has-secondary-forward-stepper"))
    (is-true (gtk-widget-style-get-property notebook "has-tab-gap"))
    (is (= 0 (gtk-widget-style-get-property notebook "initial-gap")))
    (is (= 1 (gtk-widget-style-get-property notebook "tab-curvature")))
    (is (= 2 (gtk-widget-style-get-property notebook "tab-overlap")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_notebook_new

(test gtk-notebook-new
  (is (eq 'gtk-notebook (type-of (gtk-notebook-new)))))

;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page

(test gtk-notebook-add-page.1
  (let ((notebook (make-instance 'gtk-notebook))
        (page1 (make-instance 'gtk-frame))
        (page2 (make-instance 'gtk-frame))
        (page3 (make-instance 'gtk-frame))
        (page4 (make-instance 'gtk-frame))
        (page5 (make-instance 'gtk-frame))
        (page6 (make-instance 'gtk-frame))
        (label1 (make-instance 'gtk-label :label "label1"))
        (label2 (make-instance 'gtk-label :label "label2"))
        (label3 (make-instance 'gtk-label :label "label3")))

    (is (= 0 (gtk-notebook-append-page notebook page1 nil)))
    (is (= 1 (gtk-notebook-append-page notebook page2 label1)))

    (is (= 0 (gtk-notebook-prepend-page notebook page3 nil)))
    (is (= 0 (gtk-notebook-prepend-page notebook page4 label2)))

    (is (= 3 (gtk-notebook-insert-page notebook page5 nil 3)))
    (is (= 3 (gtk-notebook-insert-page notebook page6 label3 3)))

    (is (= 6 (length (gtk-container-children notebook))))
    (is-false (gtk-notebook-remove-page notebook 0))
    (is (= 5 (length (gtk-container-children notebook))))
    (is-false (gtk-notebook-remove-page notebook page6))
    (is (= 4 (length (gtk-container-children notebook))))
))

(test gtk-notebook-add-page.2
  (let ((notebook (make-instance 'gtk-notebook))
        (page1 (make-instance 'gtk-frame))
        (page2 (make-instance 'gtk-frame))
        (page3 (make-instance 'gtk-frame))
        (label1 (make-instance 'gtk-label :label "label1"))
        (label2 (make-instance 'gtk-label :label "label2"))
        (label3 (make-instance 'gtk-label :label "label3"))
        (menu-label1 (make-instance 'gtk-label :label "menu-label1"))
        (menu-label2 (make-instance 'gtk-label :label "menu-label2"))
        (menu-label3 (make-instance 'gtk-label :label "menu-label3")))

    (is (= 0 (gtk-notebook-append-page-menu notebook page1 label1 menu-label1)))
    (is (= 0 (gtk-notebook-prepend-page-menu notebook page2 label2 menu-label2)))
    (is (= 1 (gtk-notebook-insert-page-menu notebook page3 label3 menu-label3 1)))
))

(test gtk-notebook-add-page.3
  (let ((notebook (make-instance 'gtk-notebook))
        (page1 (make-instance 'gtk-frame))
        (page2 (make-instance 'gtk-frame))
        (page3 (make-instance 'gtk-frame))
        (page4 (make-instance 'gtk-frame))
        (page5 (make-instance 'gtk-frame))
        (page6 (make-instance 'gtk-frame))
        (label1 (make-instance 'gtk-label :label "label1"))
        (label2 (make-instance 'gtk-label :label "label2"))
        (label3 (make-instance 'gtk-label :label "label3"))
        (label4 (make-instance 'gtk-label :label "label4"))
        (label5 (make-instance 'gtk-label :label "label5"))
        (label6 (make-instance 'gtk-label :label "label6"))
        (menu-label1 (make-instance 'gtk-label :label "menu-label1"))
        (menu-label2 (make-instance 'gtk-label :label "menu-label2"))
        (menu-label3 (make-instance 'gtk-label :label "menu-label3")))

    (is (= 0 (gtk-notebook-add-page notebook page1 label1)))
    (is (= 0 (gtk-notebook-add-page notebook page2 label2 :position :start)))
    (is (= 1 (gtk-notebook-add-page notebook page3 label3 :position 1)))

    (is (= 3 (gtk-notebook-add-page notebook page4 label4 :menu menu-label1)))
    (is (= 0 (gtk-notebook-add-page notebook page5 label5 :position :start :menu menu-label2)))
    (is (= 1 (gtk-notebook-add-page notebook page6 label6 :position 1 :menu menu-label3)))
))

;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child

;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text

;;;     gtk_notebook_get_tab_label_text

;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_get_tab_hborder                       deprecated
;;;     gtk_notebook_get_tab_vborder                       deprecated
;;;     gtk_notebook_set_current_page

;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget


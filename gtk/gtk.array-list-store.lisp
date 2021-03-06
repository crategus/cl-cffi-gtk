;;; ----------------------------------------------------------------------------
;;; struct GtkTreeModelIface
;;;
;;; struct GtkTreeModelIface {
;;;   GTypeInterface g_iface;
;;;
;;;   /* Signals */
;;;   void         (* row_changed)           (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_inserted)          (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_has_child_toggled) (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter);
;;;   void         (* row_deleted)           (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path);
;;;   void         (* rows_reordered)        (GtkTreeModel *tree_model,
;;;                                           GtkTreePath  *path,
;;;                                           GtkTreeIter  *iter,
;;;                                           gint         *new_order);
;;;
;;;   /* Virtual Table */
;;;   GtkTreeModelFlags (* get_flags)  (GtkTreeModel *tree_model);
;;;
;;;   gint         (* get_n_columns)   (GtkTreeModel *tree_model);
;;;   GType        (* get_column_type) (GtkTreeModel *tree_model,
;;;                                     gint          index_);
;;;   gboolean     (* get_iter)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreePath  *path);
;;;   GtkTreePath *(* get_path)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   void         (* get_value)       (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     gint          column,
;;;                                     GValue       *value);
;;;   gboolean     (* iter_next)       (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_previous)   (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_children)   (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *parent);
;;;   gboolean     (* iter_has_child)  (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gint         (* iter_n_children) (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   gboolean     (* iter_nth_child)  (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *parent,
;;;                                     gint          n);
;;;   gboolean     (* iter_parent)     (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter,
;;;                                     GtkTreeIter  *child);
;;;   void         (* ref_node)        (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;;   void         (* unref_node)      (GtkTreeModel *tree_model,
;;;                                     GtkTreeIter  *iter);
;;; };
;;; ----------------------------------------------------------------------------

(define-vtable ("GtkTreeModel" gtk-tree-model)
  (:skip parent-instance (:pointer (:struct g-type-interface)))
  ;; some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;; methods
  (get-flags (gtk-tree-model-flags (tree-model g-object)))
  (get-n-columns (:int (tree-model g-object)))
  (get-column-type (g-type (tree-model g-object) (index :int)))
  (get-iter (:boolean
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))
             (path (g-boxed-foreign gtk-tree-path))))
  (get-path ((g-boxed-foreign gtk-tree-path :return)
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))))
  (get-value (:void
              (tree-model g-object)
              (iter (g-boxed-foreign gtk-tree-iter))
              (n :int)
              (value (:pointer (:struct g-value))))
             :impl-call
             ((tree-model iter n)
              (multiple-value-bind (v type)
                  (gtk-tree-model-get-value-impl tree-model iter n)
                (set-g-value value v type))))
  (iter-next (:boolean
              (tree-model g-object)
              (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-previous (:boolean
                  (tree-model g-object)
                  (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-children (:boolean
                  (tree-model g-object)
                  (iter (g-boxed-foreign gtk-tree-iter))
                  (parent (g-boxed-foreign gtk-tree-iter))))
  (iter-has-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-n-children (:int
                    (tree-model g-object)
                    (iter (g-boxed-foreign gtk-tree-iter))))
  (iter-nth-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign gtk-tree-iter))
                   (parent (g-boxed-foreign gtk-tree-iter))
                   (n :int)))
  (iter-parent (:boolean
                (tree-model g-object)
                (iter (g-boxed-foreign gtk-tree-iter))
                (child (g-boxed-foreign gtk-tree-iter))))
  (ref-node (:void
             (tree-model g-object)
             (iter (g-boxed-foreign gtk-tree-iter))))
  (unref-node (:void
               (tree-model g-object)
                (iter (g-boxed-foreign gtk-tree-iter)))))

;;; ----------------------------------------------------------------------------

;; Implementation of array-list-store

(defclass array-list-store (gtk-tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t)
          :reader store-items)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t)
                    :reader store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t)
                  :reader store-types))
  (:metaclass gobject-class)
  (:g-type-name . "LispArrayListStore"))

(export 'array-list-store)

(register-object-type-implementation "LispArrayListStore"
                                     array-list-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

(defun store-items-count (store)
  (length (store-items store)))

(export 'store-items-count)

(defun store-item (store index)
  (aref (store-items store) index))

(export 'store-item)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (let* ((path (make-instance 'gtk-tree-path))
         (iter (make-gtk-tree-iter)))
    (setf (gtk-tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (gtk-tree-iter-stamp iter)
          0
          (gtk-tree-iter-user-data iter)
          (1- (length (store-items store))))
    (g-signal-emit store "row-inserted" path iter)))

(export 'store-add-item)

(defun store-remove-item (store item &key (test 'eq))
  (with-slots (items) store
    (let ((index (position item items :test test)))
      (unless index (error "No such item~%~A~%in list-store~%~A" item store))
      (setf items (delete item items :test test))
      (let ((path (make-instance 'gtk-tree-path)))
        (setf (gtk-tree-path-indices path) (list index))
        (g-signal-emit store "row-deleted" path)))))

(export 'store-remove-item)

(defun store-add-column (store type getter)
  (vector-push-extend type (store-types store))
  (vector-push-extend getter (store-getters store))
  (1- (length (store-types store))))

(export 'store-add-column)

(defmethod gtk-tree-model-get-flags-impl ((model array-list-store))
  '(:list-only))

(defmethod gtk-tree-model-get-n-columns-impl ((model array-list-store))
  (length (store-types model)))

(defmethod gtk-tree-model-get-column-type-impl
    ((tree-model array-list-store) index)
  (aref (store-types tree-model) index))

(defmethod gtk-tree-model-get-iter-impl ((model array-list-store) iter path)
  (let ((indices (gtk-tree-path-indices path)))
    (when (and (= 1 (length indices))
               (< (first indices) (length (store-items model))))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (first indices))
      t)))

(defmethod gtk-tree-model-ref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))

(defmethod gtk-tree-model-unref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))

(defmethod gtk-tree-model-iter-next-impl ((model array-list-store) iter)
  (let ((n (gtk-tree-iter-user-data iter)))
    (when (< n (1- (length (store-items model))))
      (setf (gtk-tree-iter-user-data iter) (1+ n))
      t)))

(defmethod gtk-tree-model-iter-nth-child-impl
    ((model array-list-store) iter parent n)
  (declare (ignorable parent))
  (setf (gtk-tree-iter-stamp iter) 0
        (gtk-tree-iter-user-data iter) n)
  t)

(defmethod gtk-tree-model-iter-children-impl
    ((model array-list-store) iter parent)
  (declare (ignore iter parent))
  nil)

(defmethod gtk-tree-model-iter-n-children-impl ((model array-list-store) iter)
  (if (null iter)
      (length (store-items model))
      0))

(defmethod gtk-tree-model-get-path-impl ((model array-list-store) iter)
  (let ((path (make-instance 'gtk-tree-path)))
    (setf (gtk-tree-path-indices path)
          (list (gtk-tree-iter-user-data iter)))
    path))

(defmethod gtk-tree-model-iter-has-child-impl ((model array-list-store) iter)
  (declare (ignorable iter))
  nil)

(defgeneric gtk-tree-model-item (model iter-or-path))

(defmethod gtk-tree-model-item ((model array-list-store) (iter gtk-tree-iter))
  (let ((n-row (gtk-tree-iter-user-data iter)))
    (aref (store-items model) n-row)))

(defmethod gtk-tree-model-item ((model array-list-store) (path gtk-tree-path))
  (let ((n-row (first (gtk-tree-path-indices path))))
    (aref (store-items model) n-row)))

(export 'gtk-tree-model-item)

(defmethod gtk-tree-model-get-value-impl ((model array-list-store) iter n)
  (let ((n-row (gtk-tree-iter-user-data iter)))
    (values (funcall (aref (store-getters model) n)
                     (aref (store-items model) n-row))
            (aref (store-types model) n))))

;;; ----------------------------------------------------------------------------

(defun array-insert-at (array element index)
  (assert (adjustable-array-p array))
  (adjust-array array (1+ (length array)) :fill-pointer t)
  (iter (for i from (1- (length array)) above index)
        (setf (aref array i)
              (aref array (1- i))))
  (setf (aref array index) element)
  array)

(defun array-remove-at (array index)
  (assert (adjustable-array-p array))
  (iter (for i from index below (1- (length array)))
        (setf (aref array i)
              (aref array (1+ i))))
  (adjust-array array (1- (length array)) :fill-pointer t)
  array)

(defstruct tree-node
  (tree nil)
  (parent nil)
  (id nil)
  (item nil)
  (children (make-array 0 :element-type 'tree-node
                          :adjustable t
                          :fill-pointer t)))

(defclass tree-lisp-store (gtk-tree-model)
  ((columns-getters :initform (make-array 0 :adjustable t :fill-pointer t)
                    :reader tree-lisp-store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t)
                  :reader tree-lisp-store-types)
   (root :initform (make-tree-node)
         :reader tree-lisp-store-root)
   (id-map :initform (make-hash-table)
           :reader tree-lisp-store-id-map)
   (next-id-value :initform 0
                  :accessor tree-lisp-store-next-id-value))
  (:metaclass gobject-class)
  (:g-type-name . "LispTreeStore"))

(defmethod initialize-instance :after ((object tree-lisp-store) &key
                                       &allow-other-keys)
  (setf (tree-node-tree (tree-lisp-store-root object)) object))

(register-object-type-implementation "LispTreeStore"
                                     tree-lisp-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

(defun map-subtree (node fn)
  (funcall fn node)
  (iter (for child in-vector (tree-node-children node))
        (map-subtree child fn)))

(defun clear-id (node)
  (map-subtree node
               (lambda (n)
                 (when (and (tree-node-id n)
                            (tree-node-tree n))
                   (remhash (tree-node-id n)
                            (tree-lisp-store-id-map (tree-node-tree n))))
                 (setf (tree-node-id n) nil))))

(defun set-node-tree (node tree)
  (map-subtree node
               (lambda (n)
                 (setf (tree-node-tree n) tree))))

(defun tree-node-insert-at (node child index)
  (assert (null (tree-node-parent child)))
  (clear-id child)
  (setf (tree-node-parent child) node)
  (set-node-tree child (tree-node-tree node))
  (array-insert-at (tree-node-children node) child index)
  (notice-tree-node-insertion (tree-node-tree node) node child index)
  node)

(defun tree-node-child-at (node index)
  (aref (tree-node-children node) index))

(defun tree-node-remove-at (node index)
  (assert (<= 0 index (1- (length (tree-node-children node)))))
  (let ((child (tree-node-child-at node index)))
    (clear-id child)
    (setf (tree-node-parent child) nil)
    (set-node-tree child nil)
    (array-remove-at (tree-node-children node) index)
    (notice-tree-node-removal (tree-node-tree node) node child index)))

(defun tree-lisp-store-add-column (store column-type column-getter)
  (vector-push-extend column-getter (tree-lisp-store-getters store))
  (vector-push-extend column-type (tree-lisp-store-types store)))

(defmethod gtk-tree-model-get-flags-impl ((store tree-lisp-store))
  nil)

(defmethod gtk-tree-model-get-n-columns-impl ((store tree-lisp-store))
  (length (tree-lisp-store-getters store)))

(defmethod gtk-tree-model-get-column-type-impl ((store tree-lisp-store) index)
  (aref (tree-lisp-store-types store) index))

(defun get-node-by-indices (root indices)
  (if indices
      (get-node-by-indices (tree-node-child-at root (first indices))
                           (rest indices))
      root))

(defun get-node-by-path (tree path)
  (let ((indices (gtk-tree-path-indices path)))
    (get-node-by-indices (tree-lisp-store-root tree) indices)))

(defun get-node-path (node)
  (iter (with z = nil)
        (for parent = (tree-node-parent node))
        (while parent)
        (for index = (position node (tree-node-children parent)))
        (push index z)
        (setf node parent)
        (finally (return z))))

(defun tree-lisp-store-get-next-id (tree)
  (incf (tree-lisp-store-next-id-value tree)))

(defun tree-lisp-store-add-id-map (tree id node)
  (setf (gethash id (tree-lisp-store-id-map tree)) node))

(defun get-assigned-id (tree node)
  (or (tree-node-id node)
      (let ((id (tree-lisp-store-get-next-id tree)))
        (tree-lisp-store-add-id-map tree id node)
        (setf (tree-node-id node) id)
        id)))

(defun get-node-by-id (tree id)
  (gethash id (tree-lisp-store-id-map tree)))

(defmethod gtk-tree-model-get-iter-impl ((store tree-lisp-store) iter path)
  (let* ((node (get-node-by-path store path))
         (node-idx (get-assigned-id store node)))
    (setf (gtk-tree-iter-stamp iter) 0
          (gtk-tree-iter-user-data iter) node-idx)))

(defun get-node-by-iter (tree iter)
  (get-node-by-id tree (gtk-tree-iter-user-data iter)))

(defmethod gtk-tree-model-get-path-impl ((store tree-lisp-store) iter)
  (let* ((path (make-instance 'gtk-tree-path))
         (node (get-node-by-iter store iter))
         (indices (get-node-path node)))
    (setf (gtk-tree-path-indices path) indices)
    path))

(defmethod gtk-tree-model-get-value-impl ((store tree-lisp-store) iter n)
  (let* ((node (get-node-by-iter store iter))
         (getter (aref (tree-lisp-store-getters store) n))
         (type (aref (tree-lisp-store-types store) n)))
    (values (funcall getter (tree-node-item node))
            type)))

(defmethod gtk-tree-model-iter-next-impl ((store tree-lisp-store) iter)
  (let* ((node (get-node-by-iter store iter))
         (parent (tree-node-parent node))
         (index (position node (tree-node-children parent))))
    (when (< (1+ index) (length (tree-node-children parent)))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at parent (1+ index))))
      t)))

(defmethod gtk-tree-model-iter-children-impl ((store tree-lisp-store) iter parent)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store))))
    (when (plusp (length (tree-node-children node)))
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at node 0)))
      t)))

(defmethod gtk-tree-model-iter-has-child-impl ((store tree-lisp-store) iter)
  (let ((node (get-node-by-iter store iter)))
    (plusp (length (tree-node-children node)))))

(defmethod gtk-tree-model-iter-n-children-impl ((store tree-lisp-store) iter)
  (let* ((node (if iter
                   (get-node-by-iter store iter)
                   (tree-lisp-store-root store))))
    (length (tree-node-children node))))

(defmethod gtk-tree-model-iter-nth-child-impl
    ((store tree-lisp-store) iter parent n)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store)))
         (requested-node (tree-node-child-at node n)))
    (setf (gtk-tree-iter-stamp iter)
          0
          (gtk-tree-iter-user-data iter)
          (get-assigned-id store requested-node))
    t))

(defmethod gtk-tree-model-iter-parent-impl ((store tree-lisp-store) iter child)
  (let ((node (get-node-by-iter store child)))
    (when (tree-node-parent node)
      (setf (gtk-tree-iter-stamp iter)
            0
            (gtk-tree-iter-user-data iter)
            (get-assigned-id store (tree-node-parent node))))))

(defmethod gtk-tree-model-ref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

(defmethod gtk-tree-model-unref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

;;; ----------------------------------------------------------------------------

(defun notice-tree-node-insertion (tree node child index)
  (declare (ignore node index))
  (when tree
    (let* ((path (make-instance 'gtk-tree-path))
           (iter (make-gtk-tree-iter)))
      (setf (gtk-tree-path-indices path) (get-node-path child)
            (gtk-tree-iter-stamp iter) 0
            (gtk-tree-iter-user-data iter) (get-assigned-id tree child))
      (g-signal-emit tree "row-inserted" path iter)
      (when (plusp (length (tree-node-children child)))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

(defun notice-tree-node-removal (tree node child index)
  (declare (ignore child))
  (when tree
    (let ((path (make-instance 'gtk-tree-path)))
      (setf (gtk-tree-path-indices path)
            (nconc (get-node-path node) (list index)))
      (g-signal-emit tree "row-deleted" path))
    (when (zerop (length (tree-node-children node)))
      (let* ((path (make-instance 'gtk-tree-path))
             (iter (make-gtk-tree-iter)))
        (setf (gtk-tree-path-indices path)
              (get-node-path node)
              (gtk-tree-iter-stamp iter)
              0
              (gtk-tree-iter-user-data iter)
              (get-assigned-id tree node))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

;;; --- End of file gtk.array-list-store.lisp ----------------------------------

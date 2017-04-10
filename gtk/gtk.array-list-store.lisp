(in-package :gtk)

;;; ----------------------------------------------------------------------------

;; Implementation of array-list-store

(register-object-type-implementation "LispArrayListStore"
                                     array-list-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

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

(defun store-items-count (store)
  (length (store-items store)))

(export 'store-items-count)

(defun store-item (store index)
  (aref (store-items store) index))

(export 'store-item)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (let* ((path (apply #'gtk-tree-path-new-from-indices (list (1- (length (store-items store))))))
         (iter (make-gtk-tree-iter)))
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
      (g-signal-emit store "row-deleted" (gtk-tree-path-new-from-indices index)))))

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
  (let ((indices (gtk-tree-path-get-indices path)))
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
  (gtk-tree-path-new-from-indices (gtk-tree-iter-user-data iter)))

(defmethod gtk-tree-model-iter-has-child-impl ((model array-list-store) iter)
  (declare (ignorable iter))
  nil)

(defgeneric gtk-tree-model-item (model iter-or-path))

(defmethod gtk-tree-model-item ((model array-list-store) (iter gtk-tree-iter))
  (let ((n-row (gtk-tree-iter-user-data iter)))
    (aref (store-items model) n-row)))

(defmethod gtk-tree-model-item ((model array-list-store) (path gtk-tree-path))
  (let ((n-row (first (gtk-tree-path-get-indices path))))
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

(export 'make-tree-node)

(register-object-type-implementation "LispTreeStore"
                                     tree-lisp-store
                                     "GObject"
                                     ("GtkTreeModel")
                                     nil)

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

(export 'tree-lisp-store)
(export 'tree-lisp-store-root)

(defmethod initialize-instance :after ((object tree-lisp-store) &key
                                       &allow-other-keys)
  (setf (tree-node-tree (tree-lisp-store-root object)) object))

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

(export 'tree-node-insert-at)

(defun tree-node-child-at (node index)
  (let* ((children (tree-node-children node))
         (in-bounds-p (< index (length children))))
    (values (and in-bounds-p (aref (tree-node-children node) index))
            in-bounds-p)))

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

(export 'tree-lisp-store-add-column)

(defmethod gtk-tree-model-get-flags-impl ((store tree-lisp-store))
  nil)

(defmethod gtk-tree-model-get-n-columns-impl ((store tree-lisp-store))
  (length (tree-lisp-store-getters store)))

(defmethod gtk-tree-model-get-column-type-impl ((store tree-lisp-store) index)
  (aref (tree-lisp-store-types store) index))

(defun get-node-by-indices (root indices)
  (if indices
      (multiple-value-bind (child in-bounds-p)
          (tree-node-child-at root (first indices))
        (values (and in-bounds-p (get-node-by-indices child (rest indices)))
                in-bounds-p))
      (values root t)))

(defun get-node-by-path (tree path)
  (let ((indices (gtk-tree-path-get-indices path)))
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
  (multiple-value-bind (node in-bounds-p)
      (get-node-by-path store path)
    (when in-bounds-p
      (let ((node-idx (get-assigned-id store node)))
        (setf (gtk-tree-iter-stamp iter) 0
              (gtk-tree-iter-user-data iter) node-idx)
        t))))

(defun get-node-by-iter (tree iter)
  (get-node-by-id tree (gtk-tree-iter-user-data iter)))

(defmethod gtk-tree-model-get-path-impl ((store tree-lisp-store) iter)
  (let* ((node (get-node-by-iter store iter))
         (indices (get-node-path node)))
    (apply #'gtk-tree-path-new-from-indices indices)))

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
    (let* ((path (apply #'gtk-tree-path-new-from-indices (get-node-path child)))
           (iter (make-gtk-tree-iter)))
      (setf (gtk-tree-iter-stamp iter) 0
            (gtk-tree-iter-user-data iter) (get-assigned-id tree child))
      (g-signal-emit tree "row-inserted" path iter)
      (when (plusp (length (tree-node-children child)))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

(defun notice-tree-node-removal (tree node child index)
  (declare (ignore child))
  (when tree
    (let ((path (apply #'gtk-tree-path-new-from-indices (nconc (get-node-path node) (list index)))))
      (g-signal-emit tree "row-deleted" path))
    (when (zerop (length (tree-node-children node)))
      (let* ((path (apply #'gtk-tree-path-new-from-indices (get-node-path node)))
             (iter (make-gtk-tree-iter)))
        (setf (gtk-tree-iter-stamp iter)
              0
              (gtk-tree-iter-user-data iter)
              (get-assigned-id tree node))
        (g-signal-emit tree "row-has-child-toggled" path iter)))))

;;; --- End of file gtk.array-list-store.lisp ----------------------------------

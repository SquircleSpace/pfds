;; Copyright 2019 Ada Avery
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defpackage :pfds.shcl.io/red-black-tree-set
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:define-adt #:compare #:define-structure
   #:to-list)
  (:import-from :pfds.shcl.io/set
   #:is-empty #:empty #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/eql-set
   #:make-eql-set #:eql-set-with #:eql-set-count
   #:eql-set-representative #:eql-set-contains-p #:do-eql-set
   #:eql-set-without)
  (:export
   #:is-empty
   #:empty
   #:with-member
   #:without-member
   #:is-member
   #:make-red-black-tree-set*
   #:make-red-black-tree-set))
(in-package :pfds.shcl.io/red-black-tree-set)

(deftype color ()
  '(member :red :black))

(defvar *tree-nil*)

(defun tree-nil ()
  *tree-nil*)

(define-adt rb-tree
    ()
  ((tree-nil (:constructor %make-tree-nil)))
  ((tree-node (:constructor %make-tree-node))
   (color :black :type color)
   (left (tree-nil) :type rb-tree)
   (right (tree-nil) :type rb-tree)
   value))

(defmethod print-object ((tree tree-node) stream)
  (when *print-readably*
    (return-from print-object
      (call-next-method)))

  (let (rep)
    (push (tree-node-left tree) rep)
    (push (tree-node-color tree) rep)
    (push (tree-node-value tree) rep)
    (push (tree-node-right tree) rep)
    (print-object (nreverse rep) stream)))

(defmethod print-object ((tree tree-nil) stream)
  (when *print-readably*
    (return-from print-object
      (call-next-method)))

  (print-object nil stream))

(defun make-tree-node (&key (color :black) (left (tree-nil)) (right (tree-nil)) value)
  (check-type color color)
  (check-type left rb-tree)
  (check-type right rb-tree)
  (%make-tree-node :color color :left left :right right :value value))

(define-structure (uneql-tree-node (:include tree-node) (:constructor %make-uneql-tree-node)))

(defun make-uneql-tree-node (&key (color :black) (left (tree-nil)) (right (tree-nil)) value)
  (check-type color color)
  (check-type left rb-tree)
  (check-type right rb-tree)
  (%make-uneql-tree-node :color color :left left :right right :value value))

(defvar *tree-nil*
  (%make-tree-nil))

(defun black-depth (tree)
  (etypecase tree
    (tree-nil
     1)
    (tree-node
     (if (eq :black (tree-node-color tree))
         (1+ (black-depth (tree-node-left tree)))
         (black-depth (tree-node-left tree))))))

(defun validate-black-depth (tree expected)
  (etypecase tree
    (tree-nil
     (unless (equal expected 1)
       (cerror "ignore" "Black depth is incorrect")))
    (tree-node
     (let ((new-expected (if (eq :black (tree-node-color tree))
                             (1- expected)
                             expected)))
       (validate-black-depth (tree-node-left tree) new-expected)
       (validate-black-depth (tree-node-right tree) new-expected)))))

(defun red-p (tree)
  (and (tree-node-p tree)
       (eq :red (tree-node-color tree))))

(defun black-p (tree)
  (and (tree-node-p tree)
       (eq :black (tree-node-color tree))))

(defun validate-red-invariant (tree)
  (etypecase tree
    (tree-nil)
    (tree-node
     (when (and (red-p tree)
                (or (red-p (tree-node-left tree))
                    (red-p (tree-node-right tree))))
       (cerror "ignore" "red-red violation"))
     (validate-red-invariant (tree-node-left tree))
     (validate-red-invariant (tree-node-right tree)))))

(defun validate-ordering (tree comparator)
  (etypecase tree
    (tree-nil)
    (tree-node
     (let ((value (tree-node-representative-value tree))
           (left (tree-node-left tree))
           (right (tree-node-right tree)))
       (when (and (tree-node-p left)
                  (not (eq :less (funcall comparator (tree-node-representative-value left) value))))
         (cerror "ignore" "ordering violation"))
       (when (and (tree-node-p right)
                  (not (eq :greater (funcall comparator (tree-node-representative-value right) value))))
         (cerror "ignore" "ordering violation"))))))

(defun validate-tree (tree comparator)
  (validate-red-invariant tree)
  (validate-black-depth tree (black-depth tree))
  (validate-ordering tree comparator))

(defun tree-node-with-changes (tree &key
                                      (color (tree-node-color tree))
                                      (left (tree-node-left tree))
                                      (right (tree-node-right tree))
                                      (value (tree-node-value tree)))
  (when (and (eq color (tree-node-color tree))
             (eq left (tree-node-left tree))
             (eq right (tree-node-right tree))
             (eq value (tree-node-value tree)))
    (return-from tree-node-with-changes tree))

  (etypecase tree
    (uneql-tree-node
     (make-uneql-tree-node
      :color color
      :left left
      :right right
      :value value))
    (tree-node
     (make-tree-node
      :color color
      :left left
      :right right
      :value value))))

(defun tree-node-with-uneql-member (tree item)
  (etypecase tree
    (uneql-tree-node
     (tree-node-with-changes tree :value (eql-set-with (tree-node-value tree) item)))
    (tree-node
     (assert (not (eq item (tree-node-value tree))))
     (make-uneql-tree-node
      :color (tree-node-color tree)
      :left (tree-node-left tree)
      :right (tree-node-right tree)
      :value (make-eql-set (tree-node-value tree) item)))))

(defun tree-node-without-uneql-member (tree item)
  (etypecase tree
    (uneql-tree-node
     (let ((new-set (eql-set-without (tree-node-value tree) item)))
       (assert (plusp (eql-set-count new-set)))
       (if (> (eql-set-count new-set) 1)
           (tree-node-with-changes tree :value new-set)
           (make-tree-node
            :color (tree-node-color tree)
            :left (tree-node-left tree)
            :right (tree-node-right tree)
            :value (eql-set-representative new-set)))))
    (tree-node
     (assert (not (eq item (tree-node-value tree))))
     tree)))

(defmacro %path (l-or-r &rest rest)
  (if rest
      (ecase l-or-r
        (l `(tree-node-left (%path ,@rest)))
        (r `(tree-node-right (%path ,@rest))))
      (ecase l-or-r
        (l 'left)
        (r 'right))))

(defmacro path (&rest rest)
  (if rest
      `(%path ,@(reverse rest))
      'tree))

(defun balance (tree &key (left (tree-node-left tree)) (right (tree-node-right tree)))
  (when (and (eq left (tree-node-left tree))
             (eq right (tree-node-right tree))
             (eq :black (tree-node-color tree)))
    (return-from balance tree))

  (labels
      ((result (a x b y c z d)
         (return-from balance
           (tree-node-with-changes
            y
            :left (tree-node-with-changes
                   x
                   :left a
                   :right b
                   :color :black)
            :right (tree-node-with-changes
                    z
                    :left c
                    :right d
                    :color :black)
            :color :red))))

    (when (and (red-p (path l))
               (red-p (path r)))
      (result (path l l)
              (path l) ; left
              (path l r)
              (path) ; top
              (path r l)
              (path r) ; right
              (path r r)))

    (when (red-p (path l))
      (cond
        ((red-p (path l l))
         (result (path l l l)
                 (path l l) ; left
                 (path l l r)
                 (path l) ; top
                 (path l r)
                 (path) ; right
                 (path r)))

        ((red-p (path l r))
         (result (path l l)
                 (path l) ; left
                 (path l r l)
                 (path l r) ; top
                 (path l r r)
                 (path) ; right
                 (path r)))))

    (when (red-p (path r))
      (cond
        ((red-p (path r l))
         (result (path l)
                 (path) ; left
                 (path r l l)
                 (path r l) ; top
                 (path r l r)
                 (path r) ; right
                 (path r r)))

        ((red-p (path r r))
         (result (path l)
                 (path) ; left
                 (path r l)
                 (path r) ; top
                 (path r r l)
                 (path r r) ; right
                 (path r r r)))))

    (tree-node-with-changes tree :left left :right right :color :black)))

(defun tree-node-representative-value (tree)
  (etypecase tree
    (uneql-tree-node
     (eql-set-representative (tree-node-value tree)))
    (tree-node
     (tree-node-value tree))))

(defun tree-member (comparator tree item)
  (when (tree-nil-p tree)
    (return-from tree-member nil))

  (let ((comparison (funcall comparator (tree-node-representative-value tree) item)))
    (ecase comparison
      (:greater
       (tree-member comparator (tree-node-left tree) item))
      (:less
       (tree-member comparator (tree-node-right tree) item))
      (:equal
       t)
      (:unequal
       (etypecase tree
         (uneql-tree-node
          (eql-set-contains-p (tree-node-value tree) item))
         (tree-node
          nil))))))

(defun tree-append (left right)
  (cond
    ((tree-nil-p left)
     right)
    ((tree-nil-p right)
     left)
    ((and (red-p left) (red-p right))
     (let ((merged (tree-append (tree-node-right left) (tree-node-left right))))
       (if (red-p merged)
           (tree-node-with-changes
            merged
            :color :red
            :left (tree-node-with-changes
                   left
                   :color :red
                   :left (tree-node-left left)
                   :right (tree-node-left merged))
            :right (tree-node-with-changes
                    right
                    :color :red
                    :left (tree-node-right merged)
                    :right (tree-node-right right)))
           (tree-node-with-changes
            left
            :color :red
            :left (tree-node-left left)
            :right (tree-node-with-changes
                    right
                    :color :red
                    :left merged
                    :right (tree-node-right right))))))
    ((and (black-p left) (black-p right))
     (let ((merged (tree-append (tree-node-right left) (tree-node-left right))))
       (if (red-p merged)
           (tree-node-with-changes
            merged
            :color :red
            :left (tree-node-with-changes
                   left
                   :color :black
                   :left (tree-node-left left)
                   :right (tree-node-left merged))
            :right (tree-node-with-changes
                    right
                    :color :black
                    :left (tree-node-right merged)
                    :right (tree-node-right right)))
           (balance-left left :right
                         (tree-node-with-changes
                          right
                          :color :black
                          :left merged
                          :right (tree-node-right right))))))
    ((red-p right)
     (tree-node-with-changes
      right
      :left (tree-append left (tree-node-left right))))
    ((red-p left)
     (tree-node-with-changes
      left
      :right (tree-append (tree-node-right left) right)))
    (t
     (error "This should be impossible"))))

(defun balance-left (tree &key (left (tree-node-left tree)) (right (tree-node-right tree)))
  (when (and (eq left (tree-node-left tree))
             (eq right (tree-node-right tree)))
    (return-from balance-left tree))

  (cond
    ((red-p left)
     (tree-node-with-changes
      tree
      :left (tree-node-with-changes
             left
             :color :black)
      :right right
      :color :red))

    ((black-p right)
     (balance tree :right (tree-node-with-changes
                           right
                           :color :red)
                   :left left))

    ((and (red-p (path r))
          (black-p (path r l)))
     (tree-node-with-changes
      (path r l)
      :left (tree-node-with-changes
             (path)
             :left (path l)
             :right (path r l l)
             :color :black)
      :right (balance
              (path r)
              :left (path r l r)
              :right (tree-node-with-changes (path r r) :color :red))
      :color :red))

    (t
     (assert nil nil "This should be impossible"))))

(defun balance-right (tree &key (left (tree-node-left tree)) (right (tree-node-right tree)))
  (when (and (eq left (tree-node-left tree))
             (eq right (tree-node-right tree)))
    (return-from balance-right tree))

  (cond
    ((red-p right)
     (tree-node-with-changes
      tree
      :left left
      :right (tree-node-with-changes
              right
              :color :black)
      :color :red))

    ((black-p left)
     (balance tree :left (tree-node-with-changes
                          left
                          :color :red)
              :right right))

    ((and (red-p (path l))
          (black-p (path l r)))
     (tree-node-with-changes
      (path l r)
      :left (balance
             (path l)
             :left (tree-node-with-changes (path l l) :color :red)
             :right (path l r l))
      :right (tree-node-with-changes
              (path)
              :left (path l r r)
              :right (path r)
              :color :black)
      :color :red))

    (t
     (assert nil nil "This should be impossible"))))

(defun tree-remove (comparator original-tree item)
  (labels
      ((%remove (comparator tree item)
         (when (tree-nil-p tree)
           (return-from tree-remove original-tree))

         (let* ((representative-value (tree-node-representative-value tree))
                (comparison (funcall comparator item representative-value)))
           (ecase comparison
             (:less
              (%remove-from-left comparator tree item))
             (:greater
              (%remove-from-right comparator tree item))
             (:unequal
              (etypecase tree
                (uneql-tree-node
                 (let ((replacement-tree (tree-node-without-uneql-member tree item)))
                   (when (eq replacement-tree tree)
                     (return-from tree-remove original-tree))
                   (values replacement-tree t)))
                (tree-node
                 (return-from tree-remove original-tree))))
             (:equal
              (etypecase tree
                (uneql-tree-node
                 (let ((replacement-tree (tree-node-without-uneql-member tree item)))
                   (when (eq replacement-tree tree)
                     (return-from tree-remove original-tree))
                   (values replacement-tree t)))
                (tree-node
                 (tree-append (tree-node-left tree) (tree-node-right tree))))))))

       (%remove-from-left (comparator tree item)
         (multiple-value-bind (updated-left tree-already-balanced) (%remove comparator (tree-node-left tree) item)
           (cond
             (tree-already-balanced
              (values (tree-node-with-changes tree :left updated-left)
                      t))
             ((black-p (tree-node-left tree))
              (values (balance-left tree :left updated-left)
                      nil))
             (t
              (values (tree-node-with-changes tree :left updated-left :color :red)
                      nil)))))

       (%remove-from-right (comparator tree item)
         (multiple-value-bind (updated-right tree-already-balanced) (%remove comparator (tree-node-right tree) item)
           (cond
             (tree-already-balanced
              (values (tree-node-with-changes tree :right updated-right)
                      t))
             ((black-p (tree-node-right tree))
              (balance-right tree :right updated-right))
             (t
              (tree-node-with-changes tree :right updated-right :color :red))))))

    (let* ((removed (%remove comparator original-tree item))
           (result (if (tree-node-p removed)
                       (tree-node-with-changes removed :color :black)
                       removed)))
      (validate-tree result comparator)
      result)))

(defun tree-insert (comparator tree item)
  (when (tree-nil-p tree)
    (return-from tree-insert (make-tree-node :color :black :value item)))

  (labels
      ((insert (tree)
         (when (tree-nil-p tree)
           (return-from insert
             (make-tree-node :color :red :value item)))

         (let* ((representative-value (tree-node-representative-value tree))
                (comparison (funcall comparator item representative-value))
                (color (tree-node-color tree)))
           (ecase comparison
             (:less
              (if (eq :red color)
                  (tree-node-with-changes tree :left (insert (tree-node-left tree)))
                  (balance tree :left (insert (tree-node-left tree)))))
             (:greater
              (if (eq :red color)
                  (tree-node-with-changes tree :right (insert (tree-node-right tree)))
                  (balance tree :right (insert (tree-node-right tree)))))
             (:equal
              tree)
             (:unequal
              (tree-node-with-uneql-member tree item))))))

    ;; I see a red black tree and I want it painted blaaaack
    (let ((result (tree-node-with-changes (insert tree) :color :black)))
      (validate-tree result comparator)
      result)))

(defun show-structure (tree &optional (stream *standard-output*))
  (let ((id 0))
    (labels
        ((visit (tree)
           (let ((our-id (incf id)))
             (when (tree-nil-p tree)
               (format stream "ID~A [label=\"nil\" shape=box]~%" our-id)
               (return-from visit our-id))

             (format stream "ID~A [label=\"~A\" color=~A]~%"
                     our-id (tree-node-value tree)
                     (if (eq :black (tree-node-color tree))
                         "black"
                         "red"))
             (let ((child-id (visit (tree-node-left tree))))
               (format stream "ID~A -> ID~A~%" our-id child-id))
             (let ((child-id (visit (tree-node-right tree))))
               (format stream "ID~A -> ID~A~%" our-id child-id))
             our-id)))
      (format stream "digraph {~%")
      (visit tree)
      (format stream "}~%"))))

(define-structure (red-black-tree-set (:constructor %make-red-black-tree-set))
  (tree (tree-nil) :type rb-tree)
  (count 0 :type (integer 0))
  (comparator 'compare))

(defun red-black-tree-set-to-list (tree)
  (let (items)
    (labels
        ((visit (tree)
           (when (tree-nil-p tree)
             (return-from visit))

           (visit (tree-node-right tree))
           (etypecase tree
             (uneql-tree-node
              (do-eql-set (value (tree-node-value tree))
                (push value items)))
             (tree-node
              (push (tree-node-value tree) items)))
           (visit (tree-node-left tree))))
      (visit (red-black-tree-set-tree tree))
      items)))

(defmethod to-list ((tree red-black-tree-set))
  (red-black-tree-set-to-list tree))

(defmethod print-object ((tree red-black-tree-set) stream)
  (write
   `(make-red-black-tree-set* (quote ,(red-black-tree-set-comparator tree))
                          :items (quote ,(red-black-tree-set-to-list tree)))
   :stream stream))

(defmethod is-empty ((tree red-black-tree-set))
  (zerop (red-black-tree-set-count tree)))

(defmethod empty ((tree red-black-tree-set))
  (%make-red-black-tree-set :comparator (red-black-tree-set-comparator tree)))

(defmethod with-member ((tree red-black-tree-set) item)
  (let ((new-tree (tree-insert (red-black-tree-set-comparator tree)
                               (red-black-tree-set-tree tree)
                               item)))
    (if (eq new-tree (red-black-tree-set-tree tree))
        tree
        (%make-red-black-tree-set
         :tree new-tree
         :count (1+ (red-black-tree-set-count tree))
         :comparator (red-black-tree-set-comparator tree)))))

(defmethod without-member ((tree red-black-tree-set) item)
  (let ((new-tree (tree-remove (red-black-tree-set-comparator tree)
                               (red-black-tree-set-tree tree)
                               item)))
    (if (eq new-tree (red-black-tree-set-tree tree))
        tree
        (%make-red-black-tree-set
         :tree new-tree
         :count (1- (red-black-tree-set-count tree))
         :comparator (red-black-tree-set-comparator tree)))))

(defmethod is-member ((tree red-black-tree-set) item)
  (tree-member (red-black-tree-set-comparator tree)
               (red-black-tree-set-tree tree)
               item))

(defun make-red-black-tree-set* (comparator &key items)
  (let ((tree (tree-nil))
        (count 0))
    (dolist (item items)
      (let ((new-tree (tree-insert comparator tree item)))
        (unless (eq new-tree tree)
          (incf count)
          (setf tree new-tree))))
    (%make-red-black-tree-set
     :tree tree
     :comparator comparator
     :count count)))

(defun make-red-black-tree-set (comparator &rest items)
  (make-red-black-tree-set* comparator :items items))

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

(defpackage :pfds.shcl.io/red-black-tree
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:define-adt #:compare #:define-immutable-structure
   #:to-list)
  (:import-from :pfds.shcl.io/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/map
   #:with-entry #:without-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/list-utility
   #:list-map-with #:list-map-without #:list-map-lookup)
  (:export
   #:is-empty
   #:empty
   #:with-member
   #:without-member
   #:is-member
   #:make-red-black-tree-set*
   #:make-red-black-tree-set))
(in-package :pfds.shcl.io/red-black-tree)

(deftype color ()
  '(member :red :black))

(defvar *tree-nil*)

(defun tree-nil ()
  *tree-nil*)

(define-adt rb-tree
    ()
  ((tree-nil (:constructor %make-tree-nil)))
  ((tree-node (:constructor nil) (:copier nil))
   (color :black :type color)
   (left (tree-nil) :type rb-tree)
   (right (tree-nil) :type rb-tree)))

(define-immutable-structure (tree-node-1 (:include tree-node) (:constructor %make-tree-node-1))
  (key (error "required"))
  value)

(define-immutable-structure (tree-node-n (:include tree-node) (:constructor %make-tree-node-n))
  (values nil :type list))

(defmethod print-object ((tree tree-node-1) stream)
  (when *print-readably*
    (return-from print-object
      (call-next-method)))

  (let (rep)
    (push (tree-node-left tree) rep)
    (push (tree-node-color tree) rep)
    (push (tree-node-1-key tree) rep)
    (push (tree-node-1-value tree) rep)
    (push (tree-node-right tree) rep)
    (write (nreverse rep) :stream stream)))

(defmethod print-object ((tree tree-node-n) stream)
  (when *print-readably*
    (return-from print-object
      (call-next-method)))

  (let (rep)
    (push (tree-node-left tree) rep)
    (push (tree-node-color tree) rep)
    (push (tree-node-n-values tree) rep)
    (push (tree-node-right tree) rep)
    (write (nreverse rep) :stream stream)))

(defmethod print-object ((tree tree-nil) stream)
  (when *print-readably*
    (return-from print-object
      (call-next-method)))

  (write nil :stream stream))

(defun make-tree-node-1 (&key (color :black) (left (tree-nil)) (right (tree-nil)) (key (error "required")) value)
  (check-type color color)
  (check-type left rb-tree)
  (check-type right rb-tree)
  (%make-tree-node-1 :color color :left left :right right :value value :key key))

(defun make-tree-node-n (&key (color :black) (left (tree-nil)) (right (tree-nil)) (values nil))
  (check-type color color)
  (check-type left rb-tree)
  (check-type right rb-tree)
  (check-type values list)
  (unless (cdr values)
    (error "tree-node-n must have multiple values"))
  (%make-tree-node-n :color color :left left :right right :values values))

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

(defun tree-node-representative (tree)
  (etypecase tree
    (tree-node-n
     (let ((first-pair (car (tree-node-n-values tree))))
       (assert first-pair)
       (values (car first-pair) (cdr first-pair))))
    (tree-node-1
     (values (tree-node-1-key tree) (tree-node-1-value tree)))))

(defun validate-ordering (tree comparator)
  (etypecase tree
    (tree-nil)
    (tree-node
     (let ((key (tree-node-representative tree))
           (left (tree-node-left tree))
           (right (tree-node-right tree)))
       (when (and (tree-node-p left)
                  (not (eq :less (funcall comparator (tree-node-representative left) key))))
         (cerror "ignore" "ordering violation"))
       (when (and (tree-node-p right)
                  (not (eq :greater (funcall comparator (tree-node-representative right) key))))
         (cerror "ignore" "ordering violation"))))))

(defun validate-tree (tree comparator)
  (validate-red-invariant tree)
  (validate-black-depth tree (black-depth tree))
  (validate-ordering tree comparator))

(defun tree-node-with-changes (tree &key
                                      (color (tree-node-color tree))
                                      (left (tree-node-left tree))
                                      (right (tree-node-right tree)))
  (etypecase tree
    (tree-node-n
     (copy-tree-node-n tree :color color :left left :right right))
    (tree-node-1
     (copy-tree-node-1 tree :color color :left left :right right))))

(defun tree-node-with-unequal-member (comparator tree key value)
  (etypecase tree
    (tree-node-n
     (copy-tree-node-n tree :values (list-map-with comparator (tree-node-n-values tree) key value)))
    (tree-node-1
     (assert (not (eql key (tree-node-1-key tree))))
     (make-tree-node-n
      :color (tree-node-color tree)
      :left (tree-node-left tree)
      :right (tree-node-right tree)
      :values (list (cons key value)
                    (cons (tree-node-1-key tree) (tree-node-1-value tree)))))))

(defun tree-node-without-unequal-member (comparator tree key)
  (etypecase tree
    (tree-node-n
     (let ((new-values (list-map-without comparator (tree-node-n-values tree) key)))
       (assert new-values)
       (when (cdr new-values)
         (return-from tree-node-without-unequal-member
           (copy-tree-node-n tree :values new-values)))
       (make-tree-node-1
        :color (tree-node-color tree)
        :left (tree-node-left tree)
        :right (tree-node-right tree)
        :key (car (car new-values))
        :value (cdr (car new-values)))))
    (tree-node-1
     (assert (not (eq key (tree-node-1-key tree))))
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

(defun tree-member (comparator tree key)
  (when (tree-nil-p tree)
    (return-from tree-member
      (values nil nil)))

  (multiple-value-bind (representative-key representative-value) (tree-node-representative tree)
    (let ((comparison (funcall comparator representative-key key)))
      (ecase comparison
        (:greater
         (tree-member comparator (tree-node-left tree) key))
        (:less
         (tree-member comparator (tree-node-right tree) key))
        (:equal
         (values representative-value t))
        (:unequal
         (etypecase tree
           (tree-node-n
            (list-map-lookup comparator (tree-node-n-values tree) key))
           (tree-node-1
            (values nil nil))))))))

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

(defun tree-remove (comparator original-tree key)
  (labels
      ((%remove (comparator tree key)
         (when (tree-nil-p tree)
           (return-from tree-remove original-tree))

         (let* ((representative-key (tree-node-representative tree))
                (comparison (funcall comparator key representative-key)))
           (ecase comparison
             (:less
              (%remove-from-left comparator tree key))
             (:greater
              (%remove-from-right comparator tree key))
             (:unequal
              (etypecase tree
                (tree-node-n
                 (let ((replacement-tree (tree-node-without-unequal-member comparator tree key)))
                   (when (eq replacement-tree tree)
                     (return-from tree-remove original-tree))
                   (values replacement-tree t)))
                (tree-node-1
                 (return-from tree-remove original-tree))))
             (:equal
              (etypecase tree
                (tree-node-n
                 (let ((replacement-tree (tree-node-without-unequal-member comparator tree key)))
                   (when (eq replacement-tree tree)
                     (return-from tree-remove original-tree))
                   (values replacement-tree t)))
                (tree-node
                 (tree-append (tree-node-left tree) (tree-node-right tree))))))))

       (%remove-from-left (comparator tree key)
         (multiple-value-bind (updated-left tree-already-balanced) (%remove comparator (tree-node-left tree) key)
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

       (%remove-from-right (comparator tree key)
         (multiple-value-bind (updated-right tree-already-balanced) (%remove comparator (tree-node-right tree) key)
           (cond
             (tree-already-balanced
              (values (tree-node-with-changes tree :right updated-right)
                      t))
             ((black-p (tree-node-right tree))
              (balance-right tree :right updated-right))
             (t
              (tree-node-with-changes tree :right updated-right :color :red))))))

    (let* ((removed (%remove comparator original-tree key))
           (result (if (tree-node-p removed)
                       (tree-node-with-changes removed :color :black)
                       removed)))
      (validate-tree result comparator)
      result)))

(defun tree-insert (comparator tree key value)
  (when (tree-nil-p tree)
    (return-from tree-insert (make-tree-node-1 :color :black :key key :value value)))

  (labels
      ((insert (tree)
         (when (tree-nil-p tree)
           (return-from insert
             (make-tree-node-1 :color :red :key key :value value)))

         (let* ((representative-key (tree-node-representative tree))
                (comparison (funcall comparator key representative-key))
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
              (etypecase tree
                (tree-node-n
                 (copy-tree-node-n
                  tree
                  :values (list-map-with comparator (tree-node-n-values tree) key value)))
                (tree-node-1
                 (copy-tree-node-1
                  tree
                  :value value))))
             (:unequal
              (tree-node-with-unequal-member comparator tree key value))))))

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
                     our-id
                     (etypecase tree
                       (tree-node-1
                        (format nil "~A=~A" (tree-node-1-key tree) (tree-node-1-value tree)))
                       (tree-node-n
                        (tree-node-n-values tree)))
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

(defun do-tree-f (tree fn reverse-p)
  (when (tree-nil-p tree)
    (return-from do-tree-f))
  (labels
      ((emit ()
         (etypecase tree
           (tree-node-1
            (funcall fn (tree-node-1-key tree) (tree-node-1-value tree)))
           (tree-node-n
            (dolist (pair (tree-node-n-values tree))
              (funcall fn (car pair) (cdr pair)))))))
    (declare (dynamic-extent #'emit))

    (let ((first-child (tree-node-left tree))
          (second-child (tree-node-right tree)))
      (when reverse-p
        (rotatef first-child second-child))
      (do-tree-f first-child fn reverse-p)
      (emit)
      (do-tree-f second-child fn reverse-p))))

(defmacro do-tree ((key value tree &key result reverse-p) &body body)
  `(block nil
     (do-tree-f ,tree (lambda (,key ,value) ,@body) ,reverse-p)
     ,result))

(define-immutable-structure (red-black-tree (:constructor #:make-red-black-tree))
  (tree (tree-nil) :type rb-tree)
  (count 0 :type (integer 0))
  (comparator 'compare))

(define-immutable-structure (red-black-tree-set (:include red-black-tree)
                                                (:constructor %make-red-black-tree-set)))

(defun red-black-tree-set-to-list (set)
  (let (items)
    (do-tree (key value (red-black-tree-tree set) :reverse-p t)
      (declare (ignore value))
      (push key items))
    items))

(defmethod to-list ((tree red-black-tree-set))
  (red-black-tree-set-to-list tree))

(defmethod print-object ((tree red-black-tree-set) stream)
  (write
   `(make-red-black-tree-set* (quote ,(red-black-tree-comparator tree))
                              :items (quote ,(red-black-tree-set-to-list tree)))
   :stream stream))

(defmethod pfds.shcl.io/set:is-empty ((tree red-black-tree-set))
  (zerop (red-black-tree-count tree)))

(defmethod pfds.shcl.io/set:empty ((tree red-black-tree-set))
  (%make-red-black-tree-set :comparator (red-black-tree-comparator tree)))

(defmethod with-member ((tree red-black-tree-set) item)
  (let ((new-tree (tree-insert (red-black-tree-comparator tree)
                               (red-black-tree-tree tree)
                               item
                               nil)))
    (if (eq new-tree (red-black-tree-tree tree))
        tree
        (%make-red-black-tree-set
         :tree new-tree
         :count (1+ (red-black-tree-count tree))
         :comparator (red-black-tree-comparator tree)))))

(defmethod without-member ((tree red-black-tree-set) item)
  (let ((new-tree (tree-remove (red-black-tree-comparator tree)
                               (red-black-tree-tree tree)
                               item)))
    (if (eq new-tree (red-black-tree-tree tree))
        tree
        (%make-red-black-tree-set
         :tree new-tree
         :count (1- (red-black-tree-count tree))
         :comparator (red-black-tree-comparator tree)))))

(defmethod is-member ((tree red-black-tree-set) item)
  (nth-value 1 (tree-member (red-black-tree-comparator tree)
                            (red-black-tree-tree tree)
                            item)))

(defun make-red-black-tree-set* (comparator &key items)
  (let ((tree (tree-nil))
        (count 0))
    (dolist (item items)
      (let ((new-tree (tree-insert comparator tree item nil)))
        (unless (eq new-tree tree)
          (incf count)
          (setf tree new-tree))))
    (%make-red-black-tree-set
     :tree tree
     :comparator comparator
     :count count)))

(defun make-red-black-tree-set (comparator &rest items)
  (make-red-black-tree-set* comparator :items items))

(define-immutable-structure (red-black-tree-map (:include red-black-tree)
                                                (:constructor %make-red-black-tree-map)))

(defun red-black-tree-map-to-list (map)
  (let (items)
    (do-tree (key value (red-black-tree-tree map) :reverse-p t)
      (push (cons key value) items))
    items))

(defmethod to-list ((tree red-black-tree-map))
  (red-black-tree-map-to-list tree))

(defmethod print-object ((tree red-black-tree-map) stream)
  (write
   `(make-red-black-tree-map* (quote ,(red-black-tree-comparator tree))
                              :alist (quote ,(red-black-tree-map-to-list tree)))
   :stream stream))

(defmethod pfds.shcl.io/map:is-empty ((tree red-black-tree-map))
  (zerop (red-black-tree-count tree)))

(defmethod pfds.shcl.io/map:empty ((tree red-black-tree-map))
  (%make-red-black-tree-map :comparator (red-black-tree-comparator tree)))

(defmethod with-entry ((tree red-black-tree-map) key value)
  (let ((new-tree (tree-insert (red-black-tree-comparator tree)
                               (red-black-tree-tree tree)
                               key
                               value)))
    (if (eq new-tree (red-black-tree-tree tree))
        tree
        (%make-red-black-tree-map
         :tree new-tree
         :count (1+ (red-black-tree-count tree))
         :comparator (red-black-tree-comparator tree)))))

(defmethod without-entry ((tree red-black-tree-map) key)
  (let ((new-tree (tree-remove (red-black-tree-comparator tree)
                               (red-black-tree-tree tree)
                               key)))
    (if (eq new-tree (red-black-tree-tree tree))
        tree
        (%make-red-black-tree-map
         :tree new-tree
         :count (1- (red-black-tree-count tree))
         :comparator (red-black-tree-comparator tree)))))

(defmethod lookup-entry ((tree red-black-tree-map) key)
  (tree-member (red-black-tree-comparator tree)
               (red-black-tree-tree tree)
               key))

(defun make-red-black-tree-map* (comparator &key alist plist)
  (let ((tree (tree-nil))
        (count 0))
    (loop :while plist
          :for key = (pop plist)
          :for value = (if plist (pop plist) (error "Odd number of items in plist"))
          :do (setf tree (tree-insert comparator tree key value)))
    (dolist (pair alist)
      (setf tree (tree-insert comparator tree (car pair) (cdr pair))))
    (do-tree (key value tree)
      (declare (ignore key value))
      (incf count))
    (%make-red-black-tree-map
     :tree tree
     :count count
     :comparator comparator)))

(defun make-red-black-tree-map (comparator &rest plist)
  (make-red-black-tree-map* comparator :plist plist))

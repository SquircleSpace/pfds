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
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:is-empty #:empty #:check-invariants
   #:print-graphviz)
  (:import-from :pfds.shcl.io/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/interface/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/interface/map
   #:with-entry #:without-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/tree
   #:define-tree #:print-tree-node-properties
   #:node-left #:node-right #:nil-tree-p)
  (:export
   #:is-empty
   #:empty
   #:with-member
   #:without-member
   #:is-member
   #:with-entry
   #:without-entry
   #:lookup-entry
   #:red-black-set
   #:make-red-black-set*
   #:make-red-black-set
   #:red-black-map
   #:make-red-black-map*
   #:make-red-black-map))
(in-package :pfds.shcl.io/red-black-tree)

;; See "Purely Functional Data Structures" by Chris Okasaki, and
;; Stefan Kahrs' red black tree untyped haskell implementation at
;; https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs

(deftype color ()
  '(member :red :black))

(defmacro with-tree-path (base-name &body body)
  (let ((l-or-r (gensym "L-OR-R"))
        (rest (gensym "REST"))
        (%path (gensym "PATH")))
    `(macrolet
         ((,%path (,l-or-r &rest ,rest)
            (if ,rest
                (ecase ,l-or-r
                  (l `(,',(intern-conc (symbol-package base-name) base-name "-NODE-LEFT")
                       (,',%path ,@,rest)))
                  (r `(,',(intern-conc (symbol-package base-name) base-name "-NODE-RIGHT")
                       (,',%path ,@,rest))))
                (ecase ,l-or-r
                  (l 'left)
                  (r 'right))))
          (path (&rest ,rest)
            (if ,rest
                `(,',%path ,@(reverse ,rest))
                'tree)))
       ,@body)))

(defmacro define-balancers (base-name)
  ;; This is only intended for use within this package, so we're going
  ;; to be lazy about gensyms.
  (let ((red-p (intern-conc *package* base-name "-RED-P"))
        (black-p (intern-conc *package* base-name "-BLACK-P"))
        (copy-node (intern-conc *package* "COPY-" base-name "-NODE"))
        (node-color (intern-conc *package* base-name "-NODE-COLOR"))
        (node-left (intern-conc *package* base-name "-NODE-LEFT"))
        (node-right (intern-conc *package* base-name "-NODE-RIGHT"))
        (node-p (intern-conc *package* base-name "-NODE-P"))
        (insert-balance-right (intern-conc *package* base-name "-INSERT-BALANCE-RIGHT"))
        (insert-balance-left (intern-conc *package* base-name "-INSERT-BALANCE-LEFT"))
        (remove-balance-right (intern-conc *package* base-name "-REMOVE-BALANCE-RIGHT"))
        (remove-balance-left (intern-conc *package* base-name "-REMOVE-BALANCE-LEFT")))
    `(progn

       (declaim (inline ,red-p))
       (defun ,red-p (tree)
         (and (,node-p tree) (eq (,node-color tree) :red)))

       (declaim (inline ,black-p))
       (defun ,black-p (tree)
         (and (,node-p tree) (eq (,node-color tree) :black)))

       (defun ,insert-balance-right (tree &key (left (,node-left tree)) (right (,node-right tree)))
         (assert (not (and (eq left (,node-left tree))
                           (eq right (,node-right tree))))
                 (left right tree)
                 "Something must have changed!")

         (when (eq :red (,node-color tree))
           (return-from ,insert-balance-right
             (,copy-node tree :left left :right right)))

         (with-tree-path ,base-name
           (labels
               ((result (a x b y c z d)
                  (return-from ,insert-balance-right
                    (,copy-node
                     y
                     :left (,copy-node
                            x
                            :left a
                            :right b
                            :color :black)
                     :right (,copy-node
                             z
                             :left c
                             :right d
                             :color :black)
                     :color :red))))

             (when (,red-p (path r))
               (cond
                 ((,red-p (path l))
                  (result (path l l)
                          (path l) ; left
                          (path l r)
                          (path) ; top
                          (path r l)
                          (path r) ; right
                          (path r r)))

                 ((,red-p (path r l))
                  (result (path l)
                          (path) ; left
                          (path r l l)
                          (path r l) ; top
                          (path r l r)
                          (path r) ; right
                          (path r r)))

                 ((,red-p (path r r))
                  (result (path l)
                          (path) ; left
                          (path r l)
                          (path r) ; top
                          (path r r l)
                          (path r r) ; right
                          (path r r r))))))

           (,copy-node tree :left left :right right :color :black)))

       (defun ,insert-balance-left (tree &key (left (,node-left tree)) (right (,node-right tree)))
         (assert (not (and (eq left (,node-left tree))
                           (eq right (,node-right tree))))
                 (left right tree)
                 "Something must have changed!")

         (when (eq :red (,node-color tree))
           (return-from ,insert-balance-left
             (,copy-node tree :left left :right right)))

         (with-tree-path ,base-name
           (labels
               ((result (a x b y c z d)
                  (return-from ,insert-balance-left
                    (,copy-node
                     y
                     :left (,copy-node
                            x
                            :left a
                            :right b
                            :color :black)
                     :right (,copy-node
                             z
                             :left c
                             :right d
                             :color :black)
                     :color :red))))

             (when (,red-p (path l))
               (cond
                 ((,red-p (path r))
                  (result (path l l)
                          (path l) ; left
                          (path l r)
                          (path) ; top
                          (path r l)
                          (path r) ; right
                          (path r r)))

                 ((,red-p (path l l))
                  (result (path l l l)
                          (path l l) ; left
                          (path l l r)
                          (path l) ; top
                          (path l r)
                          (path) ; right
                          (path r)))

                 ((,red-p (path l r))
                  (result (path l l)
                          (path l) ; left
                          (path l r l)
                          (path l r) ; top
                          (path l r r)
                          (path) ; right
                          (path r))))))

           (,copy-node tree :left left :right right :color :black)))

       (defun ,remove-balance-left (tree &key (left (,node-left tree)) (right (,node-right tree)))
         (assert (not (and (eq left (,node-left tree))
                           (eq right (,node-right tree))))
                 (left right tree)
                 "Something must have changed!")

         (unless (,black-p (,node-left tree)) ;; Yes, we're looking at the old left!
           (return-from ,remove-balance-left
             (,copy-node tree :left left :right right :color :red)))

         (with-tree-path ,base-name
           (cond
             ((,red-p (path l))
              (,copy-node tree
                          :left (,copy-node (path l) :color :black)
                          :right (path r)
                          :color :red))

             ((,black-p (path r))
              (,insert-balance-right (,copy-node tree :color :black)
                                     :left (path l)
                                     :right (,copy-node (path r) :color :red)))

             ((and (,red-p (path r))
                   (,black-p (path r l)))
              (,copy-node (path r l)
                          :left (,copy-node (path)
                                            :left (path l)
                                            :right (path r l l)
                                            :color :black)
                          :right (,insert-balance-right (,copy-node (path r) :color :black)
                                                        :left (path r l r)
                                                        :right (,copy-node (path r r) :color :red))
                          :color :red))

             (t
              (assert nil nil "This should be impossible")))))

       (defun ,remove-balance-right (tree &key (left (,node-left tree)) (right (,node-right tree)))
         (assert (not (and (eq left (,node-left tree))
                           (eq right (,node-right tree))))
                 (left right tree)
                 "Something must have changed!")

         (unless (,black-p (,node-right tree)) ;; Yes, we're looking at the old right!
           (return-from ,remove-balance-right
             (,copy-node tree :left left :right right :color :red)))

         (with-tree-path ,base-name
           (cond
             ((,red-p (path r))
              (,copy-node tree
                          :left (path l)
                          :right (,copy-node (path r) :color :black)
                          :color :red))

             ((,black-p (path l))
              (,insert-balance-left (,copy-node tree :color :black)
                                    :left (,copy-node (path l) :color :red)
                                    :right (path r)))

             ((and (,red-p (path l))
                   (,black-p (path l r)))
              (,copy-node (path l r)
                          :left (,insert-balance-left (,copy-node (path l) :color :black)
                                                      :left (,copy-node (path l l) :color :red)
                                                      :right (path l r l))
                          :right (,copy-node (path)
                                             :left (path l r r)
                                             :right (path r)
                                             :color :black)
                          :color :red))

             (t
              (assert nil nil "This should be impossible"))))))))

(define-tree rb-set (:map-p nil
                     :define-maker-p nil
                     :insert-left-balancer rb-set-insert-balance-left
                     :insert-right-balancer rb-set-insert-balance-right
                     :remove-left-balancer rb-set-remove-balance-left
                     :remove-right-balancer rb-set-remove-balance-right)
  (color :red :type color))

(define-balancers rb-set)

(defun rb-set-insert-from-root (comparator tree key)
  (copy-rb-set-node (rb-set-insert comparator tree key) :color :black))

(defun rb-set-remove-from-root (comparator tree key)
  (let ((tree (rb-set-remove comparator tree key)))
    (if (rb-set-node-p tree)
        (copy-rb-set-node tree :color :black)
        tree)))

(defun make-rb-set (comparator &key items)
  (let ((tree (rb-set-nil)))
    (dolist (item items)
      (setf tree (rb-set-insert-from-root comparator tree item)))
    tree))

(define-tree rb-map (:map-p t
                     :define-maker-p nil
                     :insert-left-balancer rb-map-insert-balance-left
                     :insert-right-balancer rb-map-insert-balance-right
                     :remove-left-balancer rb-map-remove-balance-left
                     :remove-right-balancer rb-map-remove-balance-right)
  (color :red :type color))

(define-balancers rb-map)

(defun rb-map-insert-from-root (comparator tree key value)
  (copy-rb-map-node (rb-map-insert comparator tree key value) :color :black))

(defun rb-map-remove-from-root (comparator tree key)
  (let ((tree (rb-map-remove comparator tree key)))
    (if (rb-map-node-p tree)
        (copy-rb-map-node tree :color :black)
        tree)))

(defun make-rb-map (comparator &key alist plist)
  (let ((tree (rb-map-nil)))
    (loop :while plist
          :for key = (pop plist)
          :for value = (if plist (pop plist) (error "Odd number of items in plist"))
          :do (setf tree (rb-map-insert-from-root comparator tree key value)))
    (dolist (pair alist)
      (setf tree (rb-map-insert-from-root comparator tree (car pair) (cdr pair))))
    tree))

(defgeneric node-color (tree))

(defmethod node-color ((tree rb-map-node))
  (rb-map-node-color tree))

(defmethod node-color ((tree rb-set-node))
  (rb-set-node-color tree))

(defmethod print-tree-node-properties ((tree rb-set-node) stream)
  (call-next-method)
  (format stream " color=~A" (if (eq :black (rb-set-node-color tree)) "black" "red")))

(defmethod print-tree-node-properties ((tree rb-map-node) stream)
  (call-next-method)
  (format stream " color=~A" (if (eq :black (rb-map-node-color tree)) "black" "red")))

(defun black-depth (tree)
  (if (nil-tree-p tree)
      1
      (if (eq :black (node-color tree))
          (1+ (black-depth (node-left tree)))
          (black-depth (node-left tree)))))

(defun check-black-depth (tree expected)
  (if (nil-tree-p tree)
      (cassert (equal expected 1) nil "Black depth violation")
      (let ((new-expected (if (eq :black (node-color tree))
                              (1- expected)
                              expected)))
        (check-black-depth (node-left tree) new-expected)
        (check-black-depth (node-right tree) new-expected))))

(defun node-red-p (tree)
  (and (not (nil-tree-p tree))
       (eq :red (node-color tree))))

(defun check-red-invariant (tree)
  (unless (nil-tree-p tree)
    (cassert (or (not (node-red-p tree))
                 (and (not (node-red-p (node-left tree)))
                      (not (node-red-p (node-right tree)))))
             nil "red-red violation")

    (check-red-invariant (node-left tree))
    (check-red-invariant (node-right tree))))

(defun check-balance (tree)
  (check-red-invariant tree)
  (check-black-depth tree (black-depth tree)))

(define-immutable-structure (red-black-set (:constructor %make-red-black-set))
  (tree (rb-set-nil) :type rb-set)
  (comparator (error "required")))

(defmethod check-invariants ((set red-black-set))
  (check-rb-set (red-black-set-tree set) (red-black-set-comparator set))
  (check-balance (red-black-set-tree set)))

(defmethod to-list ((set red-black-set))
  (rb-set-to-list (red-black-set-tree set)))

(defmethod print-object ((set red-black-set) stream)
  (write
   `(make-red-black-set* (quote ,(red-black-set-comparator set))
                         :items (quote ,(rb-set-to-list (red-black-set-tree set))))
   :stream stream))

(defmethod print-graphviz ((tree red-black-set) stream id-vendor)
  (print-graphviz (red-black-set-tree tree) stream id-vendor))

(defmethod is-empty ((set red-black-set))
  (rb-set-nil-p (red-black-set-tree set)))

(defmethod empty ((set red-black-set))
  (%make-red-black-set :comparator (red-black-set-comparator set)))

(defmethod with-member ((set red-black-set) item)
  (let ((new-tree (rb-set-insert-from-root
                   (red-black-set-comparator set)
                   (red-black-set-tree set)
                   item)))
    (if (eq new-tree (red-black-set-tree set))
        set
        (%make-red-black-set
         :tree new-tree
         :comparator (red-black-set-comparator set)))))

(defmethod without-member ((set red-black-set) item)
  (let ((new-tree (rb-set-remove-from-root
                   (red-black-set-comparator set)
                   (red-black-set-tree set)
                   item)))
    (if (eq new-tree (red-black-set-tree set))
        set
        (%make-red-black-set
         :tree new-tree
         :comparator (red-black-set-comparator set)))))

(defmethod is-member ((set red-black-set) item)
  (nth-value 1 (rb-set-lookup (red-black-set-comparator set)
                              (red-black-set-tree set)
                              item)))

(defun make-red-black-set* (comparator &key items)
  (let ((tree (make-rb-set comparator :items items)))
    (%make-red-black-set
     :tree tree
     :comparator comparator)))

(defun make-red-black-set (comparator &rest items)
  (make-red-black-set* comparator :items items))

(define-immutable-structure (red-black-map (:constructor %make-red-black-map))
  (tree (rb-map-nil) :type rb-map)
  (comparator (error "required")))

(defmethod check-invariants ((map red-black-map))
  (check-rb-map (red-black-map-tree map) (red-black-map-comparator map))
  (check-balance (red-black-map-tree map)))

(defmethod to-list ((map red-black-map))
  (rb-map-to-list (red-black-map-tree map)))

(defmethod print-object ((map red-black-map) stream)
  (write
   `(make-red-black-map* (quote ,(red-black-map-comparator map))
                         :alist (quote ,(rb-map-to-list (red-black-map-tree map))))
   :stream stream))

(defmethod print-graphviz ((tree red-black-map) stream id-vendor)
  (print-graphviz (red-black-map-tree tree) stream id-vendor))

(defmethod is-empty ((map red-black-map))
  (rb-map-nil-p (red-black-map-tree map)))

(defmethod empty ((map red-black-map))
  (%make-red-black-map :comparator (red-black-map-comparator map)))

(defmethod with-entry ((map red-black-map) key value)
  (let ((new-tree (rb-map-insert-from-root
                   (red-black-map-comparator map)
                   (red-black-map-tree map)
                   key
                   value)))
    (if (eq new-tree (red-black-map-tree map))
        map
        (%make-red-black-map
         :tree new-tree
         :comparator (red-black-map-comparator map)))))

(defmethod without-entry ((map red-black-map) key)
  (let ((new-tree (rb-map-remove-from-root
                   (red-black-map-comparator map)
                   (red-black-map-tree map)
                   key)))
    (if (eq new-tree (red-black-map-tree map))
        map
        (%make-red-black-map
         :tree new-tree
         :comparator (red-black-map-comparator map)))))

(defmethod lookup-entry ((tree red-black-map) key)
  (rb-map-lookup (red-black-map-comparator tree)
                 (red-black-map-tree tree)
                 key))

(defun make-red-black-map* (comparator &key alist plist)
  (let ((tree (make-rb-map comparator :alist alist :plist plist)))
    (%make-red-black-map
     :tree tree
     :comparator comparator)))

(defun make-red-black-map (comparator &rest plist)
  (make-red-black-map* comparator :plist plist))

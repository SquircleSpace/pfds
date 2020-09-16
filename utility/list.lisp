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

(defpackage :pfds.shcl.io/utility/list
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add #:impure-list-builder-extract)
  (:export
   #:list-map-with
   #:list-map-without
   #:list-map-mutate
   #:list-map-lookup
   #:list-map-map-entries
   #:list-map
   #:list-set-with
   #:list-set-without
   #:list-set-mutate
   #:list-set-is-member
   #:list-set
   #:impure-destructive-mapcar))
(in-package :pfds.shcl.io/utility/list)

(defun list-remove-index (list index)
  (let ((builder (make-impure-list-builder)))
    (dotimes (i index)
      (unless list
        (error "index is out of range: ~A" index))
      (impure-list-builder-add builder (pop list)))
    (unless list
      (error "index is out of range: ~A" index))
    (pop list)
    (impure-list-builder-extract builder list)))

(declaim (inline compare-equal-p))
(defun compare-equal-p (comparator left right)
  (eq :equal (funcall comparator left right)))

(defun list-map-with (comparator alist key value)
  (loop :for pair :in alist
        :for index :from 0
        :do
           (when (compare-equal-p comparator key (car pair))
             (if (eql (cdr pair) value)
                 (return-from list-map-with
                   (values alist nil))
                 (return
                   (setf alist (list-remove-index alist index))))))
  (values (cons (cons key value) alist) t))

(defun list-map-without (comparator alist key)
  (loop :for pair :in alist
        :for index :from 0
        :do
           (when (compare-equal-p comparator key (car pair))
             (return-from list-map-without
               (values (list-remove-index alist index) t))))
  (values alist nil))

(defun list-map-mutate (comparator alist key action-function)
  (loop :for pair :in alist
        :for index :from 0
        :do
           (when (compare-equal-p comparator key (car pair))
             (multiple-value-bind (action value) (funcall action-function (car pair) (cdr pair))
               (ecase action
                 (:insert
                  (when (eql (cdr pair) value)
                    (return-from list-map-mutate
                      (values alist 0 nil)))

                  (setf alist (list-remove-index alist index))
                  (push (cons key value) alist)
                  (return-from list-map-mutate
                    (values alist 0 nil)))
                 (:remove
                  (setf alist (list-remove-index alist index))
                  (return-from list-map-mutate
                    (values alist -1 nil)))
                 ((nil)
                  (return-from list-map-mutate
                    (values alist 0 nil)))))))

  (multiple-value-bind (action value) (funcall action-function)
    (ecase action
      (:insert
       (push (cons key value) alist)
       (return-from list-map-mutate
         (values alist 1 nil)))
      ((:remove nil)
       (return-from list-map-mutate
         (values alist 0 nil))))))

(defun list-map-lookup (comparator alist key)
  (loop :for pair :in alist
        :do
        (when (compare-equal-p comparator key (car pair))
          (return-from list-map-lookup
            (values (cdr pair) t))))
  (values nil nil))

(defun list-map-map-entries (list function)
  ;; list maps are really inefficient.  If a list map is long enough
  ;; that this recursive solution becomes a problem then we have much
  ;; more serious problems.
  (when list
    (let* ((head (car list))
           (key (car head))
           (old-value (cdr head))
           (new-value (funcall function key old-value))
           (old-tail (cdr list))
           (new-tail (list-map-map-entries (cdr list) function)))
      (if (and (eql new-value old-value)
               (eql new-tail old-tail))
          list
          (cons (cons key new-value) new-tail)))))

(defun list-map (comparator &rest plist)
  (let (map)
    (loop :while plist
          :for key = (pop plist)
          :for value = (if plist (pop plist) (error "odd number of elements in plist"))
          :do (setf map (list-map-with comparator map key value)))
    map))

(defun list-set-with (comparator list key)
  (loop :for item :in list
        :do
           (when (compare-equal-p comparator key item)
             (return-from list-set-with
               (values list nil))))
  (values (cons key list) t))

(defun list-set-without (comparator list key)
  (loop :for item :in list
        :for index :from 0
        :do
        (when (compare-equal-p comparator key item)
          (return-from list-set-without
            (values (list-remove-index list index) t))))
  (values list nil))

(defun list-set-mutate (comparator list key action-function)
  (loop :for item :in list
        :for index :from 0
        :do
           (when (compare-equal-p comparator key item)
             (let ((action (funcall action-function item)))
               (ecase action
                 (:remove
                  (setf list (list-remove-index list index))
                  (return-from list-set-mutate
                    (values list -1 nil)))
                 ((nil)
                  (return-from list-set-mutate
                    (values list 0 nil)))))))

  (let ((action (funcall action-function)))
    (ecase action
      (:insert
       (push key list)
       (return-from list-set-mutate
         (values list 1 nil)))
      ((:remove nil)
       (return-from list-set-mutate
         (values list 0 nil))))))

(defun list-set-is-member (comparator list key)
  (loop :for item :in list
        :do
        (when (compare-equal-p comparator key item)
          (return-from list-set-is-member t)))
  nil)

(defun list-set (comparator &rest items)
  (let (set)
    (dolist (item items)
      (setf set (list-set-with comparator set item)))
    set))

(defun impure-destructive-mapcar (function list)
  (maplist (lambda (sublist) (setf (car sublist) (funcall function (car sublist)))) list))

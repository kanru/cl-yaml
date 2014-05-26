;;;; queue.lisp --- Queue

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@isil.kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(defpackage #:yaml-queue
  (:use #:cl)
  (:export #:queue
           #:make-queue
           #:enqueue
           #:dequeue
           #:inqueue
           #:queue-length
           #:queue-list))
(in-package #:yaml-queue)

(defclass queue ()
  ((%front :type cons
           :initarg :front
           :accessor front)
   (%end :type cons
         :initarg :end
         :accessor end)))

(defun make-queue ()
  (let ((cell (cons nil nil)))
    (make-instance 'queue :front cell :end cell)))

(defun enqueue (item queue)
  (let ((room (cons nil nil)))
    (setf (cdr (end queue)) room
          (car (end queue)) item
          (end queue) (cdr (end queue))))
  queue)

(defun dequeue (queue)
  (pop (front queue)))

(defun inqueue (item n queue)
  (if (zerop n)
      (setf (front queue) (cons item (front queue)))
      (let ((cell (nthcdr (1- n) (front queue))))
        (setf (cdr cell) (cons item (cdr cell))))))

(defun queue-length (queue)
  (1- (length (front queue))))

(defun queue-list (queue)
  (butlast (front queue)))

;;; queue.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

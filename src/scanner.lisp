;;;; scanner.lisp --- YAML Scanner

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

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

(defpackage :yaml-scanner
  (:use :cl))
(in-package :yaml-scanner)

(defclass scanner ()
  ((%indent :type fixnum
            :initform -1
            :accessor indent)
   (%simple-key-allowed-p :type boolean
                          :initform t
                          :accessor simple-key-allowed-p)
   (%stream-start-produced-p :type boolean
                             :initform nil
                             :accessor stream-start-produced-p)
   (%simple-keys :type (list simple-key)
                 :initform ()
                 :accessor simple-keys)
   (%tokens :type (list token)
            :initform ()
            :accessor tokens)))

(defun make-scanner ()
  (make-instance 'scanner))

(defclass mark ()
  ((%index  :type fixnum
            :initform 0
            :reader mark-index)
   (%line   :type fixnum
            :initform 0
            :reader mark-line)
   (%column :type fixnum
            :initform 0
            :reader mark-column)))

(defun make-mark ()
  (make-instance 'mark))

(defclass simple-key ()
  ((%possible :type boolean
              :initform nil
              :reader simple-key-possible-p)
   (%required :type boolean
              :initform nil
              :reader simple-key-required-p)
   (%ntoken   :type fixnum
              :initform 0
              :reader simple-key-token-number)
   (%mark     :type mark
              :initform (make-mark)
              :reader simple-key-mark)))

(defun make-simple-key ()
  (make-instance 'simple-key))

(defclass token ()
  ((%start-mark :type mark
                :reader token-start)
   (%end-mark   :type mark
                :reader token-end)))

(deftype encoding ()
  '(member :any :utf-8 :utf-16-le :utf-16-be))

(defclass stream-start-token (token)
  ((%encoding :type encoding
              :initform :any
              :reader stream-start-token-encoding)))

(defun make-stream-start-token ()
  (make-instance 'stream-start-token))

(defmethod print-object ((token stream-start-token) stream)
  (print-unreadable-object (token stream :type t)
    (princ (stream-start-token-encoding token) stream)))

(defun fetch-stream-start (scanner)
  (setf (indent scanner) -1
        (simple-key-allowed-p scanner) t
        (stream-start-produced-p scanner) t)
  (push (make-simple-key) (simple-keys scanner))
  (push (make-stream-start-token) (tokens scanner)))

;;; scanner.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

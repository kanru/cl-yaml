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
   (%indents :type (list fixnum)
             :initform ()
             :accessor indents)
   (%simple-key-allowed-p :type boolean
                          :initform t
                          :accessor simple-key-allowed-p)
   (%stream-start-produced-p :type boolean
                             :initform nil
                             :accessor stream-start-produced-p)
   (%flow-level-p :type boolean
                  :initform nil
                  :accessor flow-level-p)
   (%simple-keys :type (list simple-key)
                 :initform ()
                 :accessor simple-keys)
   (%tokens :type (list token)
            :initform ()
            :accessor tokens)
   (%mark   :type mark
            :initform (make-mark)
            :accessor position-mark)))

(defun current-column (scanner)
  (mark-column (position-mark scanner)))

(defsetf current-column (scanner) (column)
  `(setf (mark-column (position-mark ,scanner)) ,column))

(defun current-line (scanner)
  (mark-line (position-mark scanner)))

(defsetf current-line (scanner) (line)
  `(setf (mark-line (position-mark ,scanner)) ,line))

(defun make-scanner ()
  (make-instance 'scanner))

(defclass mark ()
  ((%index  :type fixnum
            :initform 0
            :accessor mark-index)
   (%line   :type fixnum
            :initform 0
            :accessor mark-line)
   (%column :type fixnum
            :initform 0
            :accessor mark-column)))

(defmethod print-object ((mark mark) stream)
  (print-unreadable-object (mark stream :type t)
    (format stream "index: ~a line: ~a column: ~a"
            (mark-index mark)
            (mark-line mark)
            (mark-column mark))))

(defun make-mark ()
  (make-instance 'mark))

(defclass simple-key ()
  ((%possible :type boolean
              :initform nil
              :accessor simple-key-possible-p)
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
                :initarg :start
                :reader token-start)
   (%end-mark   :type mark
                :initarg :end
                :reader token-end)))

(defgeneric print-token-data (token stream)
  (:documentation "Print token's data")
  (:method (token stream)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (print-token-data token stream)))

(defun make-token (token-type start-mark end-mark)
  (make-instance token-type :start start-mark :end end-mark))

(defclass no (token) ())

(deftype encoding ()
  '(member :any :utf-8 :utf-16-le :utf-16-be))

(defclass stream-start (token)
  ((%encoding :type encoding
              :initform :any
              :initarg :encoding
              :reader stream-start-encoding)))

(defun make-stream-start (encoding start-mark end-mark)
  (make-instance 'stream-start :encoding encoding
                               :start start-mark
                               :end end-mark))

(defmethod print-token-data ((token stream-start) stream)
  (princ (stream-start-encoding token) stream))

(defclass stream-end (token) ())

(defun make-stream-end (start-mark end-mark)
  (make-token 'stream-end start-mark end-mark))

(defclass version-directive (token)
  ((%major :type fixnum
           :reader version-major)
   (%minor :type fixnum
           :reader version-minor)))
(defclass tag-directive (token)
  ((%handle :type string
            :reader tag-handle)
   (%suffix :type string
            :reader tag-suffix)))
(defclass document-start (token) ())
(defclass document-end (token) ())
(defclass block-sequence-start (token) ())
(defclass block-mapping-start (token) ())
(defclass block-end (token) ())
(defclass flow-sequence-start (token) ())
(defclass flow-sequence-end (token) ())
(defclass flow-mapping-start (token) ())
(defclass flow-mapping-end (token) ())
(defclass block-entry (token) ())
(defclass flow-entry (token) ())
(defclass key (token) ())
(defclass value (token) ())
(defclass alias (token)
  ((%value :type string
           :reader token-value)))
(defclass anchor (token)
  ((%value :type string
           :reader token-value)))
(defclass tag (token)
  ((%handle :type string
            :reader tag-handle)
   (%suffix :type string
            :reader tag-suffix)))
(defclass scalar (token)
  ((%value :type string
           :reader token-value)
   (%length :type fixnum
            :reader scalar-length)
   (%style :type scalar-style
           :reader scalar-style)))

(defun unroll-indent (scanner column)
  "Pop indentation levels from the indents stack until the current
level becomes less or equal to the column.  For each indentation
level, append the BLOCK-END token."
  ;; Do nothing in the flow context
  (unless (flow-level-p scanner)
    ;; Loop through the indentation levels in the stack 
    (loop :while (> (indent scanner) column)
          ;; Create a token and append it to the queue
          :do (push (make-token 'block-end
                                (position-mark scanner)
                                (position-mark scanner))
                    (tokens scanner))
              ;; Pop the indentation level
              (setf (indent scanner) (pop (indents scanner))))))

(defun remove-simple-key (scanner)
  "Remove a potential simple key at the current flow level."
  (let ((simple-key (first (simple-keys scanner))))
    ;; If the key is required, it is an error.
    (when (and (simple-key-possible-p simple-key)
               (simple-key-required-p simple-key))
      (error "~a While scanning a simple key, could not find expected ':'"
             (position-mark scanner)))
    ;; Remove the key from the stack.
    (setf (simple-key-possible-p simple-key) nil)))

(defun fetch-stream-start (scanner)
  "Initialize the scanner and produce the STREAM-START token."
  ;; Set the initial indentation
  (setf (indent scanner) -1
        ;; A simple-key is allowed at the beginning of the stream.
        (simple-key-allowed-p scanner) t
        ;; We have started
        (stream-start-produced-p scanner) t)
  ;; Initialize the simple key stack
  (push (make-simple-key) (simple-keys scanner))
  ;; Create the STREAM-START token and append it to the queue.
  (push (make-stream-start :any
                           (position-mark scanner)
                           (position-mark scanner))
        (tokens scanner))
  (values))

(defun fetch-stream-end (scanner)
  "Produce the STREAM-END token and shut down the scanner."
  ;; Force new line
  (when (not (zerop (current-column scanner)))
    (setf (current-column scanner) 0)
    (incf (current-line scanner)))
  ;; Reset the indentation level
  (unroll-indent scanner -1)
  ;; Reset simple keys
  (remove-simple-key scanner)
  (setf (simple-key-allowed-p scanner) nil)
  ;; Create the STREAM-END token and append it to the queue
  (push (make-stream-end (position-mark scanner)
                         (position-mark scanner))
        (tokens scanner))
  (values))

;;; scanner.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

;;;; ast.lisp --- Abstract Syntax Tree of YAML

;;; Copyright (C) 2012  Kan-Ru Chen

;;; Author(s): Kan-Ru Chen <kanru@kanru.info>

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

(in-package :yaml)

(defclass document ()
  ((major-version :reader major-version
                  :initform nil
                  :initarg :major-version)
   (minor-version :reader minor-version
                  :initform nil
                  :initarg :minor-version)
   (content       :reader content
                  :initform nil
                  :initarg :content)))

(defclass node ()
  ((start-pos :reader start-pos
              :initform nil
              :initarg  :start-pos)
   (tag       :accessor tag
              :initform nil
              :initarg  :tag)
   (data      :accessor data
              :initform nil
              :initarg  :data)))

(defclass basic-node (node)
  ())

(defclass empty (basic-node)
  ())

(defclass key-value-pair ()
  ((key   :accessor key
          :initform nil
          :initarg :key)
   (value :accessor value
          :initform nil
          :initarg :value)))

(defclass mapping (basic-node)
  ((pairs :accessor pairs
          :initform nil
          :initarg :pairs
          :type string)))

(defun mapping-get (mapping key)
  (let ((pair-key (string key)))
    (loop for pair in (pairs mapping)
          when (and pair-key
                    (string= pair-key (key mapping)))
            do (return (values (value mapping) t)))
    (values nil nil)))

(defun mapping->hashtable (mapping)
  (let ((table (make-hash-table :test #'string=)))
    (loop for pair in (pairs mapping)
          do (setf (gethash (key pair) table)
                   (value pair)))
    table))

(defclass sequence (basic-node)
  ((nodes :accessor nodes
          :initform nil
          :initarg  :nodes)))

(defmethod len ((sequence sequence))
  (length (nodes sequence)))

(defun sequence->vector (sequence)
  (let ((s (make-array (len sequence))))
    (loop for i from 0
          for elm in (nodes sequence)
          do (setf (elt s i) elm))
    s))

(defclass scalar (basic-node)
  ((value :accessor value
          :initform nil
          :initarg :value)))

;;; ast.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

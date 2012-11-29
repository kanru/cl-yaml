;;;; reader.lisp --- YAML source reader

;;; Copyright (C) 2012  Kan-Ru Chen (陳侃如)

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

;;; TODO:
;;;  - Count line & columns
;;;  - Recycle buffer for stream-reader impl

;;;; Code:

(in-package #:yaml)

(defclass reader ()
  ())

(defclass standard-reader (reader)
  ((source
    :initarg :source
    :reader source)
   (pointer
    :initform 0
    :accessor pointer)
   (line
    :initform 0
    :accessor line
    :documentation
    "The line of the current character.")
   (column
    :initform 0
    :accessor column
    :documentation
    "The column of the current character.")))

(defclass string-reader (standard-reader)
  ())

(defclass stream-reader (standard-reader)
  ((buffer :initform (make-array 0 :element-type 'character
                                   :adjustable t
                                   :fill-pointer t)
           :accessor buffer)))

(defgeneric peek (reader &optional n)
  (:documentation
   "Return the Nth character from current tip or nil when out of bounds."))
(defgeneric prefix (reader &optional length)
  (:documentation
   "Return a string containing the first LENGTH characters from current tip.
The returned string might be shorter if we are near the end of the source."))
(defgeneric forward (reader &optional length)
  (:documentation
   "Skip LENGTH characters."))
(defgeneric mark (reader)
  (:documentation
   "Return the MARK object that represents the current character in tip."))

(defun make-reader (source)
  "Make a buffered reader object from SOURCE.
SOURCE is either a stream or a string."
  (typecase source
    (string
     (make-instance 'string-reader :source source))
    (stream
     (make-instance 'stream-reader :source source))))

(defmethod peek ((reader string-reader) &optional (n 0))
  (let ((offset (+ n (pointer reader))))
    (when (and (not (minusp n))
               (< offset (length (source reader))))
      (char (source reader) offset))))

(defmethod prefix ((reader string-reader) &optional (length 1))
  (when (plusp length)
    (let ((offset (+ length (pointer reader))))
      (if (< offset (length (source reader)))
          (subseq (source reader) (pointer reader) offset)
          (subseq (source reader) (pointer reader))))))

(defmethod forward ((reader string-reader) &optional (length 1))
  (when (plusp length)
    (let ((offset (+ length (pointer reader))))
      (if (< offset (length (source reader)))
          (setf (pointer reader) offset)
          (setf (pointer reader) (length (source reader))))
      (values))))

(defmethod peek ((reader stream-reader) &optional (n 0))
  (let ((offset (+ n (pointer reader))))
    (when (not (minusp n))
      (ensure-buffer-length reader (1+ offset))
      (when (< offset (fill-pointer (buffer reader)))
        (char (buffer reader) offset)))))

(defmethod prefix ((reader stream-reader) &optional (length 1))
  (when (plusp length)
    (let ((offset (+ length (pointer reader))))
      (ensure-buffer-length reader offset)
      (subseq (buffer reader) (pointer reader)
              (min (length (buffer reader)) offset)))))

(defmethod forward ((reader stream-reader) &optional (length 1))
  (when (plusp length)
    (let ((offset (+ length (pointer reader))))
      (ensure-buffer-length reader offset)
      (setf (pointer reader) offset)
      (values))))

(defmethod ensure-buffer-length ((reader stream-reader) &optional length)
  (assert length)
  (with-accessors ((buffer buffer) (source source)) reader
    (let ((orig (fill-pointer buffer)))
      (when (< (array-total-size buffer) length)
        (adjust-array buffer length :initial-element #\Nul
                                    :fill-pointer t)
        (symbol-macrolet ((fp (fill-pointer buffer)))
          (setf fp (read-sequence buffer source
                                  :start orig :end length)))))))

;;; reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

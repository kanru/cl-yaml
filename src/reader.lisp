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
    :initform 1
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
           :accessor buffer)
   (cache-pointer :initform 0
                  :accessor cache-pointer)))

(defclass mark ()
  ((line :accessor line
         :initarg :line)
   (column :accessor column
           :initarg :column))
  (:default-initargs :line 0 :column 0))

(defgeneric peek (reader &optional n)
  (:documentation
   "Return the Nth character from current tip or #\Nul when out of bounds."))
(defgeneric prefix (reader &optional length)
  (:documentation
   "Return a string containing the first LENGTH characters from current tip.
The returned string might be shorter if we are near the end of the source."))
(defgeneric forward (reader &optional length)
  (:documentation
   "Skip LENGTH characters."))
(defgeneric mark (reader)
  (:documentation
   "Return the MARK object that represents the current character in tip.")
  (:method (reader)
    (make-instance 'mark :line (line reader) :column (column reader))))

(defun make-reader (source)
  "Make a buffered reader object from SOURCE.
SOURCE is either a stream or a string."
  (typecase source
    (string
     (make-instance 'string-reader :source source))
    (stream
     (make-instance 'stream-reader :source source))))

(defun count-newline (buffer &key start end)
  (count #\Linefeed buffer :start start :end end))

(defmethod peek ((reader string-reader) &optional (n 0))
  (assert (>= n 0))
  (let ((offset (+ n (pointer reader))))
    (if (< offset (length (source reader)))
        (char (source reader) offset)
        #\Nul)))

(defmethod prefix ((reader string-reader) &optional (length 1))
  (assert (>= length 0))
  (let ((offset (+ length (pointer reader))))
    (if (< offset (length (source reader)))
        (subseq (source reader) (pointer reader) offset)
        (subseq (source reader) (pointer reader)))))

(defmethod forward ((reader string-reader) &optional (length 1))
  (assert (>= length 0))
  (let* ((offset (+ length (pointer reader)))
         (target (min offset (length (source reader)))))
    (incf (line reader)
          (count-newline (source reader) :start (pointer reader) :end target))
    (setf (pointer reader) target)
    (setf (column reader)
          (loop :for i :downfrom (pointer reader) :to 0 :by 1
                :until (and (< i (length (source reader)))
                            (char= #\Linefeed (char (source reader) i)))
                :count (not (= i (pointer reader)))))
    (values)))

(defun ensure-buffer-length (buffer length)
  (let ((size (array-total-size buffer)))
    (when (< size length)
      (adjust-array buffer length :initial-element #\Nul
                                  :fill-pointer t))))

(defun consume-stream (stream length)
  (let ((?line 0)
        (?column 0))
    (dotimes (n length)
      (case (read-char stream nil)
        (#\Linefeed
         (incf ?line)
         (setf ?column 0))
        (nil)
        (t (incf ?column))))
    (values ?line ?column)))

;;; Part of the stream is in the buffer if we have peeked it. First we
;;; check it if we need more data. If so we make sure the buffer is
;;; large enough then read the stream into it. Then return the target
;;; character.
(defmethod peek ((reader stream-reader) &optional (n 0))
  (assert (>= n 0))
  (with-accessors ((cache cache-pointer)
                   (pointer pointer)
                   (stream source)
                   (buffer buffer)) reader
    (let* ((target (+ n pointer))
           (pass (- target cache)))
      (when (>= pass 0)
        (ensure-buffer-length buffer (1+ target))
        (setf cache
              (read-sequence buffer stream :start cache :end (1+ target))))
      (if (< target cache)
          (char buffer target)
          #\Nul))))

(defmethod prefix ((reader stream-reader) &optional (length 1))
  (assert (>= length 0))
  (with-accessors ((buffer buffer)
                   (pointer pointer)
                   (cache cache-pointer)) reader
    (peek reader length)
    (subseq buffer pointer (min cache length))))

;;; Part of the stream is in the buffer if we have peeked it. First we
;;; check it if forward will pass the `cache-pointer', if so we set
;;; `cache-pointer' and `pointer' to 0, otherwise we set `pointer' to
;;; the target offest and finish. If the cache has been flushed, we
;;; consume remain data from stream.
(defmethod forward ((reader stream-reader) &optional (length 1))
  (assert (>= length 0))
  (with-accessors ((cache cache-pointer)
                   (pointer pointer)
                   (stream source)
                   (line line)
                   (column column)
                   (buffer buffer)) reader
    (let* ((target (+ pointer length))
           (pass (- target cache)))
      (if (>= pass 0)
          (progn
            (incf line (count-newline buffer :start pointer :end cache))
            (setf pointer 0)
            (setf cache 0)
            (multiple-value-bind (?line ?column)
                (consume-stream stream pass)
              (incf line ?line)
              (setf column ?column)))
          (setf pointer target
                column target)))))

;;; reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

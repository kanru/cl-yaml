;;;; reader.lisp --- YAML source reader

;;; Copyright (C) 2012, 2014  Kan-Ru Chen (陳侃如)

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

(defpackage #:yaml-reader
  (:use #:cl)
  (:export #:make-mark
           #:mark-index
           #:mark-line
           #:mark-column
           #:string-reader
           #:stream-reader
           #:mark
           #:copy-mark
           #:determine-encoding
           #:ensure-buffer-length
           #:current-column
           #:peek
           #:yread
           #:yread-line
           #:check
           #:alphap
           #:hexp
           #:digitp
           #:spacep
           #:tabp
           #:nulp
           #:breakp
           #:break-or-nul-p
           #:blankp
           #:blank-or-break-or-nul-p
           #:looking-at
           #:skip
           #:skip-line))
(in-package #:yaml-reader)

(defclass mark ()
  ((%index  :type fixnum
            :initform 0
            :accessor mark-index)
   (%line   :type fixnum
            :initform 1
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

(defun make-mark (&optional mark)
  (let ((new-mark (make-instance 'mark)))
    (when mark
      (with-accessors ((di mark-index)
                       (dl mark-line)
                       (dc mark-column)) new-mark
        (with-accessors ((si mark-index)
                         (sl mark-line)
                         (sc mark-column)) mark
          (setf di si
                dl sl
                dc sc))))
    new-mark))

(defclass reader ()
  ((%source :initarg :source
            :reader source
            :documentation
            "The input source, either a STRING or a STREAM")
   (%mark :type mark
          :initform (make-mark)
          :reader mark)))

(defgeneric determine-encoding (reader)
  (:documentation
   "Return the input stream encoding."))
(defgeneric ensure-buffer-length (reader n)
  (:documentation
   "Ensure the reader buffer has at least N characters."))
(defgeneric peek (reader n)
  (:documentation
   "Return the Nth character from current tip or #\Nul when out of bounds."))
(defgeneric yread (reader)
  (:documentation
   "Return and remove one character from unread buffer."))

(defun copy-mark (reader)
  (declare (inline))
  (make-mark (mark reader)))

(defun current-column (reader)
  (declare (inline))
  (mark-column (mark reader)))

(defun check (reader char &optional (n 0))
  (declare (inline))
  (char= (peek reader n) char))

(defun looking-at (reader str)
  (loop :for char :across str
        :for idx :from 0
        :always (check reader char idx)))

(define-compiler-macro looking-at (&whole form reader str)
  (if (and (typep str 'string)
           (= 1 (length str)))
      `(check ,reader ,(char str 0))
      form))

(defun digitp (reader &optional (n 0))
  (digit-char-p (peek reader n) 10))

(defun alphap (reader &optional (n 0))
  (member (peek reader n)
          '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
            #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
            #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
            #\u #\v #\w #\x #\y #\z
            #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
            #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
            #\U #\V #\W #\X #\Y #\Z
            #\_ #\-)))

(defun hexp (reader &optional (n 0))
  (digit-char-p (peek reader n) 16))

(DEFUN nulp (reader &optional (n 0))
  (declare (inline))
  (check reader #\Nul n))

(defun tabp (reader &optional (n 0))
  (declare (inline))
  (check reader #\Tab n))

(defun spacep (reader &optional (n 0))
  (declare (inline))
  (check reader #\Space n))

(defun blankp (reader &optional (n 0))
  (declare (inline))
  (or (spacep reader n)
      (tabp reader n)))

(defun crlfp (reader &optional (n 0))
  (declare (inline))
  (and (check reader #\Return n)
       (check reader #\Newline (1+ n))))

(defun breakp (reader &optional (n 0))
  (declare (inline))
  (or (check reader #\Return n)
      (check reader #\Newline n)
      #+unicode
      (or
       ;; #\Next-Line
       (check reader (code-char #x85) n)
       ;; #\LINE_SEPARATOR
       (check reader (code-char #x2028) n)
       ;; #\PARAGRAPH_SEPARATOR
       (check reader (code-char #x2029) n))))

(defun break-or-nul-p (reader &optional (n 0))
  (declare (inline))
  (or (breakp reader n)
      (nulp reader n)))

(defun blank-or-break-or-nul-p (reader &optional (n 0))
  (declare (inline))
  (or (blankp reader n)
      (breakp reader n)
      (nulp reader n)))

(defun skip (reader &optional (n 1))
  (declare (inline))
  (dotimes (i n)
    (yread reader)))

(defun skip-line (reader)
  (cond
    ((crlfp reader)
     (skip reader 2)
     (setf (mark-column (mark reader)) 0)
     (incf (mark-line (mark reader))))
    ((breakp reader)
     (skip reader)
     (setf (mark-column (mark reader)) 0)
     (incf (mark-line (mark reader))))))

(defun yread-line (reader)
  ;; FIXME
  (skip-line reader)
  #\Newline)

(defclass string-reader (reader) ())

(defmethod determine-encoding ((sr string-reader))
  (declare (ignore sr))
  nil)

(defmethod ensure-buffer-length ((sr string-reader) n)
  (declare (ignore sr n))
  t)

(defmethod peek ((sr string-reader) n)
  (let ((pos (+ n (mark-index (mark sr)))))
    (if (>= pos (length (source sr)))
        #\Nul
        (char (source sr) pos))))

(defmethod yread ((sr string-reader))
  (prog1
      (char (source sr) (mark-index (mark sr)))
    (incf (mark-index (mark sr)))
    (incf (mark-column (mark sr)))))

(defclass stream-reader (reader)
  ((%buffer :type (vector character)
            :initform (make-array 0 :element-type 'character
                                    :adjustable t
                                    :fill-pointer t)
            :accessor buffer
            :documentation
            "Caches part of the input stream")
   (%pointer :type fixnum
             :initform 0
             :accessor pointer
             :documentation
             "The position of first unread character")
   (%unread :type fixnum
            :initform 0
            :accessor unread
            :documentation
            "Number of unread characters in the buffer")
   (%eofp   :type boolean
            :initform nil
            :accessor eofp)))

(defmethod determine-encoding ((sr stream-reader))
  (stream-external-format (source sr)))

(defmethod ensure-buffer-length ((sr stream-reader) n)
  (cond
    ((or (eofp sr)
         (>= (unread sr) n))
     (unread sr))
    (t
     ;; Move the unread characters to the beginning of the buffer
     (replace (buffer sr) (buffer sr) :start2 (pointer sr) :end2 (unread sr))
     (setf (fill-pointer (buffer sr)) (unread sr))
     ;; Fill the buffer until it has enough characters
     (dotimes (i n)
       (vector-push-extend (read-char (source sr) nil #\Nul) (buffer sr))
       (incf (unread sr)))
     (when (null (peek-char nil (source sr) nil))
       (setf (eofp sr) t)))))

(defmethod peek ((sr stream-reader) n)
  (assert (< n (unread sr)))
  (char (buffer sr) (+ n (pointer sr))))

(defmethod yread ((sr stream-reader))
  (prog1
      (char (buffer sr) (pointer sr))
    (incf (pointer sr))
    (incf (mark-index (mark sr)))
    (incf (mark-column (mark sr)))
    (decf (unread sr))))

;;; reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

;;;; chars.lisp --- Chars

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

(defconstant +bom-rune+ #xfeff)

(defconstant +nb-json-min+ #x000020)
(defconstant +nb-json-max+ #x10ffff)

(defparameter +decimal-digits+ "0123456789")
(defparameter +hex-digits+ (concatenate 'string
                                        +decimal-digits+
                                        "abcdefABCDEF"))
(defparameter +ascii-letters+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(defparameter +break-chars+ (concatenate 'string
                                         '(#\Linefeed #\Return)))
(defparameter +whitespace+ (concatenate 'string
                                        '(#\Space #\Tab)))
(defparameter +word-chars+ (concatenate 'string
                                        "-"
                                        +ascii-letters+
                                        +decimal-digits+))
(defparameter +tag-chars+ (concatenate 'string
                                       +word-chars+
                                       "%#;/?:@&=+$.~*'()_"))
(defparameter +uni-chars+ (concatenate 'string
                                       +tag-chars+
                                       "!,[]"))
(defparameter +indicator-chars+ "-?:,[]{}#&*!|>'\"%@`")
(defparameter +flow-indicator-chars+ ",[]{}")

(defun ascii-p (character)
  (declare (type character character))
  (zerop (logand #x80 (char-code character))))

(defun json-char-p (character)
  (declare (type character character))
  (or (char= character #\Tab)
      (and (>= (char-code character) +nb-json-min+)
           (<= (char-code character) +nb-json-max+))))

(defun contains-char (string character)
  (declare (type string string)
           (type character character))
  (find character string))

(declaim (inline contains-rune))
(defun contains-rune (string character)
  (contains-char string character))

(defun join-char (chars-list seperator)
  (let ((len (length chars-list)))
    (case len
      (0 (list))
      (1 (first chars-list))
      (t (reduce (lambda (list1 list2)
                   (append list1 seperator list2))
                 chars-list)))))

(declaim (inline join-rune))
(defun join-rune (chars-list seperator)
  (join-char chars-list seperator))

;;; chars.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

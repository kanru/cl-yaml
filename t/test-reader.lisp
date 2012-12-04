;;;; test-reader.lisp --- YAML reader test

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

;;; 

;;;; Code:

(in-package #:yaml-test)

(in-suite yaml)

(defvar *input-string* "abcdefghijklmnopqrstuvwxyz")

(def-fixture string-fixture (&optional input-string)
  (let ((reader (yaml::make-reader (or input-string *input-string*))))
    (&body)))

(def-fixture stream-fixture (&optional input-string)
  (with-input-from-string (input (or input-string *input-string*))
    (let ((reader (yaml::make-reader input)))
      (&body))))

(defmacro def-reader-test (test input-string &body body)
  `(progn
     ,@(mapcar (lambda (type)
                 `(test (,(intern (format nil "~A-~A/~A" type 'reader test)
                                  (find-package :yaml-test))
                         :fixture ,(list (intern (format nil "~A-~A" type 'fixture)
                                                 (find-package :yaml-test))
                                         (eval input-string)))
                    ,@body))
               '(string stream))))

(def-reader-test peek ()
  (is (char= #\a (yaml::peek reader)))
  (is (char= #\b (yaml::peek reader 1)))
  (is (char= #\z (yaml::peek reader 25)))
  (is (char= #\Nul (yaml::peek reader 26))))

(def-reader-test prefix ()
  (is (string= "a" (yaml::prefix reader)))
  (is (string= "ab" (yaml::prefix reader 2)))
  (is (string= "abcdefghijklmnopqrstuvwxyz"
               (yaml::prefix reader 26)))
  (is (string= "abcdefghijklmnopqrstuvwxyz"
               (yaml::prefix reader 27))))

(def-reader-test forward ()
  (is (char= #\b (progn (yaml::forward reader)
                        (yaml::peek reader))))
  (is (char= #\d (progn (yaml::forward reader 2)
                        (yaml::peek reader))))
  (is (char= #\Nul (progn (yaml::forward reader 27)
                          (yaml::peek reader)))))

(def-reader-test mark1 ()
  (let ((mark (yaml::mark reader)))
    (is (= 1 (yaml::line mark)))
    (is (= 0 (yaml::column mark)))))

(def-reader-test mark2 ()
  (yaml::forward reader 1)
  (let ((mark (yaml::mark reader)))
    (is (= 1 (yaml::line mark)))
    (is (= 1 (yaml::column mark)))))

(def-reader-test mark3 (format nil "a~%ab~%abc")
  (yaml::forward reader 2)
  (let ((mark (yaml::mark reader)))
    (is (= 2 (yaml::line mark)))
    (is (= 0 (yaml::column mark))))
  (yaml::forward reader 3)
  (let ((mark (yaml::mark reader)))
    (is (= 3 (yaml::line mark)))
    (is (= 0 (yaml::column mark)))))

;;; test-reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

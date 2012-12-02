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

(def-fixture string-fixture ()
  (let ((reader (yaml::make-reader "abcdefghijklmnopqrstuvwxyz")))
    (&body)))

(def-fixture stream-fixture ()
  (with-input-from-string (input "abcdefghijklmnopqrstuvwxyz")
    (let ((reader (yaml::make-reader input)))
      (&body))))

(defmacro def-reader-test (test &body body)
  `(progn
     ,@(mapcar (lambda (type)
                 `(test (,(intern (format nil "~A-~A/~A" type 'reader test)
                                  (find-package :yaml-test))
                         :fixture ,(intern (format nil "~A-~A" type 'fixture)
                                           (find-package :yaml-test)))
                    ,@body))
               '(string stream))))

(def-reader-test peek
  (is (char= #\a (yaml::peek reader)))
  (is (char= #\b (yaml::peek reader 1)))
  (is (char= #\z (yaml::peek reader 25)))
  (is (char= #\Nul (yaml::peek reader 26))))

(def-reader-test prefix
  (is (string= "a" (yaml::prefix reader)))
  (is (string= "ab" (yaml::prefix reader 2)))
  (is (string= "abcdefghijklmnopqrstuvwxyz"
               (yaml::prefix reader 26)))
  (is (string= "abcdefghijklmnopqrstuvwxyz"
               (yaml::prefix reader 27))))

(def-reader-test forward
  (is (char= #\b (progn (yaml::forward reader)
                        (yaml::peek reader))))
  (is (char= #\d (progn (yaml::forward reader 2)
                        (yaml::peek reader))))
  (is (char= #\Nul (progn (yaml::forward reader 27)
                          (yaml::peek reader)))))

;;; test-reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

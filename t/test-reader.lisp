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

(deftest reader
  (plan 22)

  (diag "string-reader")
  (let ((reader (yaml::make-reader "abcdefghijklmnopqrstuvwxyz")))
    (is (yaml::peek reader) #\a "Peek first character")
    (is (yaml::peek reader 1) #\b "Peek second character")
    (is (yaml::peek reader 25) #\z "Peek last character")
    (is (yaml::peek reader 26) #\Nul "Peek too much index")
    (is (yaml::prefix reader) "a" "Prefix")
    (is (yaml::prefix reader 2) "ab" "Prefix(2)")
    (is (yaml::prefix reader 26) "abcdefghijklmnopqrstuvwxyz" "Prefix(end)")
    (is (yaml::prefix reader 27) "abcdefghijklmnopqrstuvwxyz" "Prefix(end+1)")
    (yaml::forward reader)
    (is (yaml::peek reader) #\b "Forward")
    (yaml::forward reader 2)
    (is (yaml::peek reader) #\d "Forward(2)")
    (yaml::forward reader 27)
    (is (yaml::peek reader) #\Nul "Forward(end+1)"))

  (diag "stream-reader")
  (with-input-from-string (input "abcdefghijklmnopqrstuvwxyz")
    (let ((reader (yaml::make-reader input)))
      (is (yaml::peek reader) #\a "Peek first character")
      (is (yaml::peek reader 1) #\b "Peek second character")
      (is (yaml::peek reader 25) #\z "Peek last character")
      (is (yaml::peek reader 26) #\Nul "Peek too much index")
      (is (yaml::prefix reader) "a" "Prefix")
      (is (yaml::prefix reader 2) "ab" "Prefix(2)")
      (is (yaml::prefix reader 26) "abcdefghijklmnopqrstuvwxyz" "Prefix(end)")
      (is (yaml::prefix reader 27) "abcdefghijklmnopqrstuvwxyz" "Prefix(end+1)")
      (yaml::forward reader)
      (is (yaml::peek reader) #\b "Forward")
      (yaml::forward reader 2)
      (is (yaml::peek reader) #\d "Forward(2)")
      (yaml::forward reader 27)
      (is (yaml::peek reader) #\Nul "Forward(end+1)"))))

;;; test-reader.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

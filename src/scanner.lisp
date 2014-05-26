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

(defpackage #:yaml-scanner
  (:use #:cl
        #:yaml-reader
        #:yaml-queue))
(in-package #:yaml-scanner)

(defclass base-scanner ()
  ((%indent :type fixnum
            :initform -1
            :accessor indent)
   (%indents :type (or null (cons fixnum cons))
             :initform ()
             :accessor indents)
   (%simple-key-allowed-p :type boolean
                          :initform t
                          :accessor simple-key-allowed-p)
   (%stream-start-produced-p :type boolean
                             :initform nil
                             :accessor stream-start-produced-p)
   (%stream-end-produced-p :type boolean
                           :initform nil
                           :accessor stream-end-produced-p)
   (%flow-level :type fixnum
                :initform 0
                :accessor flow-level)
   (%simple-keys :type (or null (cons simple-key cons))
                 :initform ()
                 :accessor simple-keys)
   (%token-available-p :type boolean
                       :initform nil
                       :accessor token-available-p)
   (%tokens :type queue
            :initform (make-queue)
            :accessor tokens)
   (%tokens-parsed :type fixnum
                   :initform 0
                   :accessor tokens-parsed)))

(defclass string-scanner (base-scanner string-reader) ())
(defclass stream-scanner (base-scanner stream-reader) ())

(define-condition scanner-error (error)
  ((%context :type string
             :initarg :context
             :reader error-context)
   (%context-mark :type mark
                  :initarg :context-mark
                  :reader error-context-mark)
   (%problem :type string
             :initarg :problem
             :reader error-problem)
   (%problem-mark :type mark
                  :initarg :problem-mark
                  :reader error-problem-mark))
  (:report (lambda (error stream)
             (with-accessors ((problem-mark error-problem-mark)
                              (problem error-problem)
                              (context-mark error-context-mark)
                              (context error-context)) error
               (format stream "Line ~A Column ~A: Error, ~A~%"
                       (mark-line context-mark)
                       (mark-column context-mark)
                       context)
               (format stream "Line ~A Column ~A: ~A"
                       (mark-line problem-mark)
                       (mark-column problem-mark)
                       problem)))))

(defgeneric make-scanner (source)
  (:documentation
   "Return a YAML scanner for SOURCE."))

(defmethod make-scanner ((source string))
  (make-instance 'string-scanner :source source))

(defmethod make-scanner ((source stream))
  (make-instance 'stream-scanner :source source))

(defclass simple-key ()
  ((%possible :type boolean
              :initform nil
              :accessor simple-key-possible-p)
   (%required :type boolean
              :initform nil
              :accessor simple-key-required-p)
   (%ntoken   :type fixnum
              :initform 0
              :accessor simple-key-token-number)
   (%mark     :type mark
              :initform (make-mark)
              :accessor simple-key-mark)))

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
  (:method (token stream)
    (declare (ignore token stream))))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (print-token-data token stream)))

(defun make-token (token-type start-mark end-mark)
  (make-instance token-type :start start-mark :end end-mark))

(defclass no (token) ())

(deftype encoding ()
  '(member :any :utf-8 :utf-16le :utf-16be))

(defclass stream-start (token)
  ((%encoding :type encoding
              :initform :any
              :initarg :encoding
              :reader stream-start-encoding)))

(defun make-stream-start (encoding start-mark end-mark)
  (make-instance 'stream-start :encoding (or encoding :any)
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

(defun roll-indent (scanner column number type mark)
  "Push the current indentation level to the stack and set the new
level the current column is greater than the indentation level. In
this case, append or insert the specified token into the token queue."
  ;; In the flow context do nothing
  (unless (plusp (flow-level scanner))
    (when (< (indent scanner) column)
      ;; Push the current indentation level to the stack and set the
      ;; new indentation level
      (push (indent scanner) (indents scanner))
      (setf (indent scanner) column)
      ;; Create a token and insert it into the queue
      (let ((token (make-token type mark mark)))
        (if (= -1 number)
            (enqueue token (tokens scanner))
            ;; FIXME insert to correct position
            (let ((n (- number (tokens-parsed scanner))))
              (inqueue token n (tokens scanner))))))))

(defun unroll-indent (scanner column)
  "Pop indentation levels from the indents stack until the current
level becomes less or equal to the column.  For each indentation
level, append the BLOCK-END token."
  ;; Do nothing in the flow context
  (unless (plusp (flow-level scanner))
    ;; Loop through the indentation levels in the stack 
    (loop :for indents-poped :from 0
          :while (> (indent scanner) column)
           ;; Create a token and append it to the queue
          :do (enqueue (make-token 'block-end
                                   (copy-mark scanner)
                                   (copy-mark scanner))
                       (tokens scanner))
              ;; Pop the indentation level
              (setf (indent scanner) (pop (indents scanner)))
          :finally (return indents-poped))))

(defun save-simple-key (scanner)
  "Check if a simple key may start at the current position and add it if
needed."
  ;; A simple key is required at the current position if the scanner
  ;; is in the block context and the current column coincides with the
  ;; indentation level.
  (let ((required (and (zerop (flow-level scanner))
                       (= (indent scanner) (current-column scanner)))))
    ;; A simple key is required only when it is the first token in the
    ;; current line. Therefore it is always allowed. But we add a
    ;; check anyway.
    (assert (or (simple-key-allowed-p scanner) (not required)))
    ;; If the current position may start a simple key, save it
    (when (simple-key-allowed-p scanner)
      (let ((simple-key (make-simple-key)))
        (setf (simple-key-possible-p simple-key) t
              (simple-key-required-p simple-key) required
              (simple-key-token-number simple-key) (+ (tokens-parsed scanner)
                                                      (queue-length
                                                       (tokens scanner)))
              (simple-key-mark simple-key) (copy-mark scanner))
        (remove-simple-key scanner)
        (push simple-key (simple-keys scanner))))))

(defun remove-simple-key (scanner)
  "Remove a potential simple key at the current flow level."
  (let ((simple-key (first (simple-keys scanner))))
    ;; If the key is required, it is an error.
    (when (and (simple-key-possible-p simple-key)
               (simple-key-required-p simple-key))
      (error 'scanner-error
             :context "while scanning a simple key"
             :context-mark (simple-key-mark simple-key)
             :problem "could not find expected ':'"
             :problem-mark (copy-mark scanner)))
    ;; Remove the key from the stack.
    (setf (simple-key-possible-p simple-key) nil))
  t)

(defun scan (scanner)
  "Get the next token.
Return multiple values TOKEN STREAM-END-P"
  (break)
  (cond
    ;; No tokens after STREAM-END or error
    ((stream-end-produced-p scanner)
     (values nil t))
    (t
     ;; Ensure that the tokens queue contains enough tokens
     (unless (token-available-p scanner)
       (fetch-more-tokens scanner))
     ;; Fetch the next token from the queue
     (let ((token (dequeue (tokens scanner))))
       (setf (token-available-p scanner) nil)
       (incf (tokens-parsed scanner))
       (values token (stream-end-produced-p scanner))))))

(defun stale-simple-keys (scanner)
  "Check the list of potential simple keys and remove the positions
that cannot contain simple keys anymore."
  ;; Check for a potential simple key for each flow level
  (loop :for simple-key :in (simple-keys scanner)
        :for position-mark := (copy-mark scanner)
        :and simple-key-mark := (simple-key-mark simple-key)
        ;; The specification requires that a simple key
        ;;
        ;;  - is limited to a single line,
        ;;  - is shorter than 1024 characters.
        :when (and (simple-key-possible-p simple-key)
                   (or (< (mark-line simple-key-mark)
                          (mark-line position-mark))
                       (< (+ (mark-index simple-key-mark) 1024)
                          (mark-index position-mark))))
          :do (if (simple-key-required-p simple-key)
                  (error 'scanner-error
                         :context "while scanning a simple key"
                         :context-mark simple-key-mark
                         :problem "could not find expected ':'"
                         :problem-mark position-mark)
                  (setf (simple-key-possible-p simple-key) nil))
        :finally (return t)))

(defun maybe-want-simple-key-p (scanner)
  (loop :for simple-key :in (simple-keys scanner)
        :thereis (and (simple-key-possible-p simple-key)
                      (= (simple-key-token-number simple-key)
                         (tokens-parsed scanner)))))

(defun fetch-more-tokens (scanner)
  "Ensure that the tokens queue contains at least one token which can
be returned to the parser."
  ;; While we need more tokens to fetch, do it
  (loop :while (or (zerop (queue-length (tokens scanner)))
                   (and (stale-simple-keys scanner)
                        (maybe-want-simple-key-p scanner)))
        :do (fetch-next-token scanner)
        :finally (return (setf (token-available-p scanner) t))))

(defun fetch-next-token (scanner)
  "The dispatcher for token fetchers."
  (block nil
    ;; Ensure that the buffer is initialized
    (ensure-buffer-length scanner 1)
    ;; Check if we just started scanning. Fetch STREAM-START then
    (when (not (stream-start-produced-p scanner))
      (return (fetch-stream-start scanner)))
    ;; Eat whitespaces and comments until we reach the next token
    (scan-to-next-token scanner)
    ;; Remove obsolete potential simple keys
    (stale-simple-keys scanner)
    ;; Check the indentation level against the current column
    (unroll-indent scanner (current-column scanner))
    ;; Ensure that the buffer contains at least 4 characters. 4 is the
    ;; length of the longest indicators ('--- ' and '... ').
    (ensure-buffer-length scanner 4)
    ;; Is it the end of the stream?
    (when (nulp scanner)
      (return (fetch-stream-end scanner)))
    ;; Is it a directive?
    #+todo
    (when (and (zerop (current-column scanner))
               (looking-at scanner "%"))
      (return (fetch-directive scanner)))
    ;; Is it the document start indicator?
    (when (and (zerop (current-column scanner))
               (looking-at scanner "---")
               (blank-or-break-or-nul-p scanner 3))
      (return (fetch-document-indicator scanner 'document-start)))
    ;; Is it the document end indicator?
    (when (and (zerop (current-column scanner))
               (looking-at scanner "...")
               (blank-or-break-or-nul-p scanner 3))
      (return (fetch-document-indicator scanner 'document-end)))
    ;; Is it the flow sequence start indicator?
    (when (looking-at scanner "[")
      (return (fetch-flow-collection-start scanner 'flow-sequence-start)))
    ;; Is it the flow mapping start indicator?
    (when (looking-at scanner "{")
      (return (fetch-flow-collection-start scanner 'flow-mapping-start)))
    ;; Is it the flow sequence end indicator?
    (when (looking-at scanner "]")
      (return (fetch-flow-collection-end scanner 'flow-sequence-end)))
    ;; Is it the flow mapping end indicator?
    (when (looking-at scanner "}")
      (return (fetch-flow-collection-end scanner 'flow-mapping-end)))
    ;; Is it the flow entry indicator?
    (when (looking-at scanner ",")
      (return (fetch-flow-entry scanner)))
    ;; Is it the block entry indicator?
    (when (and (looking-at scanner "-")
               (blank-or-break-or-nul-p scanner 1))
      (return (fetch-block-entry scanner)))
    ;; Is it the key indicator?
    (when (and (looking-at scanner "?")
               (blank-or-break-or-nul-p scanner 1))
      (return (fetch-key scanner)))
    ;; Is it the value indicator?
    #+todo
    (when (and (looking-at scanner ":")
               (blank-or-null-p scanner 1))
      (return (fetch-value scanner)))
    ;; Is it an alias?
    #+todo
    (when (looking-at scanner "*")
      (return (fetch-anchor scanner 'alias)))
    ;; Is it an anchor?
    #+todo
    (when (looking-at scanner "&")
      (return (fetch-anchor scanner 'anchor)))
    ;; Is it a tag?
    #+todo
    (when (looking-at scanner "!")
      (return (fetch-tag scanner)))
    ;; Is it a literal scalar?
    #+todo
    (when (and (looking-at scanner "|")
               (zerop (flow-level scanner)))
      (return (fetch-block-scalar scanner :literal)))
    ;; Is it a folded scalar
    #+todo
    (when (and (looking-at scanner ">")
               (zerop (flow-level scanner)))
      (return (fetch-block-scalar scanner :folded)))
    ;; Is it a single-quoted scalar?
    #+todo
    (when (looking-at scanner "'")
      (return (fetch-flow-scalar scanner :single-quoted)))
    ;; Is it a double-quoted scalar?
    #+todo
    (when (looking-at scanner "\"")
      (return (fetch-flow-scalar scanner :double-quoted)))
    ;; Is it a plain scalar?
    ;;
    ;; A plain scalar may start with any non-blank characters except
    ;;
    ;;     '-', '?', ':', ',', '[', ']', '{', '}',
    ;;     '#', '&', '*', '!', '|', '>', '\'', '"',
    ;;     '%', '@', '`'.
    ;; In the block context (and, for the '-' indicator, in the flow context
    ;; too), it may also start with the characters
    ;;
    ;;     '-', '?', ':'
    ;; if it is followed by a non-space character.
    ;;
    ;; XXX The last rule is more restrictive than the specification requires.
    #+todo
    (when (or (not (or (blank-or-null-p scanner)
                       (some (lambda (str)
                               (looking-at scanner str))
                             '("-" "?" ":" "," "[" "]" "{" "}" "#" "&" "*"
                               "!" "|" ">" "'" "\"" "%" "@" "`"))))
              (and (looking-at scanner "-")
                   (not (blank-p scanner 1)))
              (and (zerop (flow-level scanner))
                   (or (looking-at scanner "?")
                       (looking-at scanner ":"))
                   (not (blank-or-null-p scanner 1))))
      (return (fetch-plain-scalar scanner)))
    ;; If we don't determine the token type so far, it is an error
    (error 'scanner-error
           :context "while scanning for the next token"
           :context-mark (copy-mark scanner)
           :problem "found character that cannot start any token"
           :problem-mark (copy-mark scanner))))

(defun scan-to-next-token (scanner)
  (loop :do (ensure-buffer-length scanner 1)
        ;; Allow the BOM mark to start a line
        ;; :when (and (zerop (current-column scanner))
        ;;            (bomp (scanner)))
        ;;   :do (skip scanner)
        ;; Eat whitespaces
        ;;
        ;; Tabs are allowed:
        ;;
        ;; - in the flow context;
        ;; - in the block context, but not at the beginning of the line or
        ;; after '-', '?', or ':' (complex value)
        :do (ensure-buffer-length scanner 1)
            (loop :while (or (spacep scanner)
                             (and (or (plusp (flow-level scanner))
                                      (simple-key-allowed-p scanner))
                                  (tabp scanner)))
                  :do (skip scanner)
                      (ensure-buffer-length scanner 1))
        ;; Eat a comment until a line break
        :when (check scanner #\#)
          :do (loop :until (break-or-nul-p scanner)
                    :do (skip scanner)
                        (ensure-buffer-length scanner 1))
        :while (breakp scanner)
        :do (ensure-buffer-length scanner 2)
            (skip-line scanner)
        :unless (plusp (flow-level scanner))
          :do (setf (simple-key-allowed-p scanner) t)))

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
  (let ((token (make-stream-start (determine-encoding scanner)
                                  (copy-mark scanner)
                                  (copy-mark scanner))))
    (enqueue token (tokens scanner))
    token))

(defun fetch-stream-end (scanner)
  "Produce the STREAM-END token and shut down the scanner."
  ;; Force new line
  (when (not (zerop (current-column scanner)))
    #+todo (%force-new-line scanner)
    ;; (setf (current-column scanner) 0)
    ;; (incf (current-line scanner))
    )
  ;; Reset the indentation level
  (unroll-indent scanner -1)
  ;; Reset simple keys
  (remove-simple-key scanner)
  (setf (simple-key-allowed-p scanner) nil
        (stream-end-produced-p scanner) t)
  ;; Create the STREAM-END token and append it to the queue
  (let ((token (make-stream-end (copy-mark scanner) (copy-mark scanner))))
    (enqueue token (tokens scanner))
    token))

(defun fetch-document-indicator (scanner type)
  "Produce the DOCUMENT-START or DOCUMENT-END token."
  (check-type type (or (eql document-start)
                       (eql document-end)))
  ;; Reset the indentation level
  (unroll-indent scanner -1)
  ;; Reset simple keys
  (remove-simple-key scanner)
  (setf (simple-key-allowed-p scanner) nil)
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner 3)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token type start-mark end-mark) (tokens scanner)))))

(defun increase-flow-level (scanner)
  "Increase the flow level and resize the simple key list if needed."
  ;; Reset the simple key on the next level
  (push (make-simple-key) (simple-keys scanner))
  ;; Increase the flow level
  (incf (flow-level scanner)))

(defun decrease-flow-level (scanner)
  "Decrease the flow level"
  (when (plusp (flow-level scanner))
    (decf (flow-level scanner))
    (pop (simple-keys scanner))))

(defun fetch-flow-collection-start (scanner type)
  "Produce the FLOW-SEQUENCE-START or FLOW-MAPPING-START token."
  (check-type type (or (eql flow-sequence-start)
                       (eql flow-mapping-start)))
  ;; The indicators '[' and '{' may start a simple key
  (save-simple-key scanner)
  ;; Increase the flow level
  (increase-flow-level scanner)
  ;; A simple key may follow the indicators '[' and '{'
  (setf (simple-key-allowed-p scanner) t)
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token type start-mark end-mark) (tokens scanner)))))

(defun fetch-flow-collection-end (scanner type)
  "Produce the FLOW-SEQUENCE-END or FLOW-MAPPING-END token."
  (check-type type (or (eql flow-sequence-end)
                       (eql flow-mapping-end)))
  ;; Reset any potential simple key on the current flow level
  (remove-simple-key scanner)
  ;; Decrease the flow level
  (decrease-flow-level scanner)
  ;; No simple keys after the indicators ']' and '}'
  (setf (simple-key-allowed-p scanner) nil)
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token type start-mark end-mark) (tokens scanner)))))

(defun fetch-flow-entry (scanner)
  "Produce the FLOW-ENTRY token."
  ;; Reset any potential simple keys on the current flow level
  (remove-simple-key scanner)
  ;; Simple keys are allowed after ','
  (setf (simple-key-allowed-p scanner) t)
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token 'flow-entry start-mark end-mark) (tokens scanner)))))

(defun fetch-block-entry (scanner)
  "Produce the BLOCK-ENTRY token."
  ;; Check if the scanner is in the block context
  (cond
    ((zerop (flow-level scanner))
     ;; Check if we are allowed to start a new entry
     (when (not (simple-key-allowed-p scanner))
       (error 'scanner-error
              :context-mark (copy-mark scanner)
              :problem "block sequence entries are not allowed in this context"
              :problem-mark (copy-mark scanner)))
     ;; Add the BLOCK-SEQUENCE-START token if needed
     (roll-indent scanner (current-column scanner) -1
                  'block-sequence-start (mark scanner)))
    (t
     ;; It is an error for the '-' indicator to occur in the flow
     ;; context, but we let the Parser detect and report about it
     ;; because the Parser is able to point to the context.
     ))
  ;; Reset any potential simple keys on the current flow level
  (remove-simple-key scanner)
  ;; Simple keys are allowed after '-'
  (setf (simple-key-allowed-p scanner) t)
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token 'block-entry start-mark end-mark)
               (tokens scanner)))))

(defun fetch-key (scanner)
  "Produce the KEY token."
  ;; In the block context, additional checks are required
  (when (zerop (flow-level scanner))
    ;; Check if we are allowed to start a new key (not nessesary
    ;; simple)
    (when (not (simple-key-allowed-p scanner))
      (error 'scanner-error
             :context-mark (copy-mark scanner)
             :problem "mapping keys are not allowed in this context"
             :problem-mark (copy-mark scanner)))
    ;; Add the BLOCK-MAPPING-START token if needed
    (roll-indent scanner (current-column scanner) -1
                 'block-mapping-start (copy-mark scanner)))
  ;; Reset any potential simple keys on the current flow level
  (remove-simple-key scanner)
  ;; Simple keys are allowed after '?' in the block context
  (setf (simple-key-allowed-p scanner) (zerop (flow-level scanner)))
  ;; Consume the token
  (let ((start-mark (copy-mark scanner)))
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (enqueue (make-token 'key start-mark end-mark)
               (tokens scanner)))))

;;; scanner.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

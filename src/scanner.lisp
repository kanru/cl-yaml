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
           :initarg :major
           :reader version-major)
   (%minor :type fixnum
           :initarg :minor
           :reader version-minor)))

(defun make-version-directive (major minor start-mark end-mark)
  (make-instance 'version-directive :major major :minor minor
                                    :start start-mark :end end-mark))

(defmethod print-token-data ((token version-directive) stream)
  (format stream "(~A,~A)" (version-major token) (version-minor token)))

(defclass tag-directive (token)
  ((%handle :type string
            :initarg :handle
            :reader tag-handle)
   (%suffix :type string
            :initarg :suffix
            :reader tag-suffix)))

(defun make-tag-directive (handle suffix start-mark end-mark)
  (make-instance 'tag-directive :handle handle :suffix suffix
                                :start start-mark :end end-mark))

(defmethod print-token-data ((token tag-directive) stream)
  (format stream "(~A,~A)" (tag-handle token) (tag-suffix token)))

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
           :initarg :value
           :reader token-value)))
(defclass anchor (token)
  ((%value :type string
           :initarg :value
           :reader token-value)))

(defun make-anchor-or-alias (type value start-mark end-mark)
  (check-type type (or (eql alias) (eql anchor)))
  (make-instance type :value value :start start-mark :end end-mark))

(defmethod print-token-data ((token anchor) stream)
  (prin1 (token-value token) stream))

(defmethod print-token-data ((token alias) stream)
  (prin1 (token-value token) stream))

(defclass tag (token)
  ((%handle :type string
            :initarg :handle
            :reader tag-handle)
   (%suffix :type string
            :initarg :suffix
            :reader tag-suffix)))

(defun make-tag (handle suffix start-mark end-mark)
  (make-instance 'tag :handle handle
                      :suffix suffix
                      :start start-mark
                      :end end-mark))

(defmethod print-token-data ((token tag) stream)
  (format stream "~S ~S" (tag-handle token) (tag-suffix token)))

(defclass scalar (token)
  ((%value :type string
           :initarg :value
           :reader token-value)
   (%length :type fixnum
            :initarg :length
            :reader scalar-length)
   (%style :type scalar-style
           :initarg :style
           :reader scalar-style)))

(defun make-scalar (value length style start-mark end-mark)
  (check-type style (or (eql :plain)
                        (eql :single-quoted)
                        (eql :double-quoted)
                        (eql :literal)
                        (eql :folded)))
  (make-instance 'scalar :value value :length length :style style
                         :start start-mark :end end-mark))

(defmethod print-token-data ((token scalar) stream)
  (format stream "~A ~S" (scalar-style token) (token-value token)))

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
  "Get the next token."
  ;; Ensure that the tokens queue contains enough tokens
  (unless (or (token-available-p scanner)
              (stream-end-produced-p scanner))
    (fetch-more-tokens scanner))
  ;; Fetch the next token from the queue
  (let ((token (dequeue (tokens scanner))))
    (when token
      (setf (token-available-p scanner) nil)
      (incf (tokens-parsed scanner))
      token)))

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
        :until (stream-end-produced-p scanner)
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
    (when (and (looking-at scanner ":")
               (blank-or-break-or-nul-p scanner 1))
      (return (fetch-value scanner)))
    ;; Is it an alias?
    (when (looking-at scanner "*")
      (return (fetch-anchor scanner 'alias)))
    ;; Is it an anchor?
    (when (looking-at scanner "&")
      (return (fetch-anchor scanner 'anchor)))
    ;; Is it a tag?
    (when (looking-at scanner "!")
      (return (fetch-tag scanner)))
    ;; Is it a literal scalar?
    (when (and (looking-at scanner "|")
               (zerop (flow-level scanner)))
      (return (fetch-block-scalar scanner :literal)))
    ;; Is it a folded scalar
    (when (and (looking-at scanner ">")
               (zerop (flow-level scanner)))
      (return (fetch-block-scalar scanner :folded)))
    ;; Is it a single-quoted scalar?
    (when (looking-at scanner "'")
      (return (fetch-flow-scalar scanner :single-quoted)))
    ;; Is it a double-quoted scalar?
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
    (when (or (not (or (blank-or-break-or-nul-p scanner)
                       (member (peek scanner 0)
                               '(#\- #\? #\: #\, #\[ #\] #\{ #\} #\# #\& #\*
                                 #\! #\| #\> #\' #\\ #\% #\@ #\`))))
              (and (looking-at scanner "-")
                   (not (blankp scanner 1)))
              (and (zerop (flow-level scanner))
                   (or (looking-at scanner "?")
                       (looking-at scanner ":"))
                   (not (blank-or-break-or-nul-p scanner 1))))
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
                                      (not (simple-key-allowed-p scanner)))
                                  (tabp scanner)))
                  :do (skip scanner)
                      (ensure-buffer-length scanner 1))
        ;; Eat a comment until a line break
        :when (check scanner #\#)
          :do (loop :until (break-or-nul-p scanner)
                    :do (skip scanner)
                        (ensure-buffer-length scanner 1))
        :until (not (breakp scanner))
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
              :context "while scanning a block entry"
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

(defun fetch-value (scanner)
  "Produce the VALUE token."
  (let ((simple-key (first (simple-keys scanner))))
    (cond
      ((simple-key-possible-p simple-key)
       (inqueue (make-token 'key
                            (simple-key-mark simple-key)
                            (simple-key-mark simple-key))
                (- (simple-key-token-number simple-key)
                   (tokens-parsed scanner))
                (tokens scanner))
       ;; In the block context, we may need to add the
       ;; BLOCK-MAPPING-START token
       (roll-indent scanner (current-column scanner)
                    (simple-key-token-number simple-key)
                    'block-mapping-start (simple-key-mark simple-key))
       ;; Remove the simple key
       (setf (simple-key-possible-p simple-key) nil
             (simple-key-allowed-p scanner) nil))
      (t
       ;; The ':' indicator follows a complex key
       ;; In the block context, extra checks are required
       (when (zerop (flow-level scanner))
         ;; Check if we are allowed to start a complex value
         (when (not (simple-key-allowed-p scanner))
           (error 'scanner-error
                  :context "while scanning a value token"
                  :context-mark (copy-mark scanner)
                  :problem "mapping values are not allowed in this context"
                  :problem-mark (copy-mark scanner)))
         ;; Add the BLOCK-MAPPING-START token if needed
         (roll-indent scanner (current-column scanner) -1
                      'block-mapping-start (copy-mark scanner)))
       ;; Simple keys after ':' are allowed in the block context
       (setf (simple-key-allowed-p scanner) (zerop (flow-level scanner)))))
    ;; Consume the token
    (let ((start-mark (copy-mark scanner)))
      (skip scanner)
      (let ((end-mark (copy-mark scanner)))
        (enqueue (make-token 'value start-mark end-mark)
                 (tokens scanner))))))

(defun scan-anchor (scanner type)
  "Create the ALIAS or ANCHOR token."
  (check-type type (or (eql anchor)
                       (eql alias)))
  (let ((start-mark (copy-mark scanner)))
    ;; Eat the indicator character
    (skip scanner)
    ;; Consume the value
    (ensure-buffer-length scanner 1)
    (let* ((length 0)
           (string (with-output-to-string (string)
                     (loop :while (alphap scanner)
                           :do (write-char (yread scanner) string)
                               (ensure-buffer-length scanner 1)
                               (incf length))))
           (end-mark (copy-mark scanner)))
      ;; Check if length of the anchor is greater than 0 and it is
      ;; followed by a whitespaces character or one of the indicators:
      ;;
      ;;     '?', ':', ',', ']', '}', '%', '@', '`'
      (when (or (zerop length)
                (not (or (blank-or-break-or-nul-p scanner)
                         (some (lambda (char)
                                 (check scanner char))
                               '(#\? #\: #\, #\] #\} #\% #\@ #\`)))))
        (error 'scanner-error
               :context (if (eql type 'anchor)
                            "while scanning an anchor"
                            "while scanning an alias")
               :context-mark start-mark
               :problem "did not find expected alphabetic or numeric character"
               :problem-mark (copy-mark scanner)))
      (make-anchor-or-alias type string start-mark end-mark))))

(defun fetch-anchor (scanner type)
  "Produce the ALIAS or ANCHOR token."
  (check-type type (or (eql anchor)
                       (eql alias)))
  ;; An anchor or an alias could be a simple key
  (save-simple-key scanner)
  ;; A simple key cannot follow an anchor or an alias
  (setf (simple-key-allowed-p scanner) nil)
  ;; Create the alias or anchor token and append it to the queue
  (enqueue (scan-anchor scanner type) (tokens scanner)))

(defun join-string (string1 string2)
  "Destructively join two strings. STRING1 must be adjustable."
  (let* ((len1 (length string1))
         (len2 (length string2))
         (size (+ len1 len2)))
    (adjust-array string1 (list size) :fill-pointer size)
    (replace string1 string2 :start1 len1)))

(defun scan-plain-scalar (scanner)
  "Scan a plain scalar."
  (let ((start-mark (copy-mark scanner))
        (end-mark (copy-mark scanner))
        (string (make-array 0 :element-type 'character
                              :adjustable t
                              :fill-pointer t))
        (leading-break (make-array 0 :element-type 'character
                                     :adjustable t
                                     :fill-pointer t))
        (trailing-breaks (make-array 0 :element-type 'character
                                       :adjustable t
                                       :fill-pointer t))
        (whitespaces (make-array 0 :element-type 'character
                                   :adjustable t
                                   :fill-pointer t))
        (leading-blanks-p nil)
        (indent (1+ (indent scanner))))
    ;; Consume the content of the plain scalar
    (loop :do (ensure-buffer-length scanner 4)
              ;; Check for a document indicator
          :until (and (zerop (current-column scanner))
                      (or (looking-at scanner "---")
                          (looking-at scanner "..."))
                      (blank-or-break-or-nul-p scanner 3))
          ;; Check for a comment
          :until (looking-at scanner "#")
          ;; Consume non-blank characters
          :do (loop :until (blank-or-break-or-nul-p scanner)
                    ;; Check for 'x:x' in the flow context. TODO: Fix the test "spec-08-13".
                    :when (and (plusp (flow-level scanner))
                               (looking-at scanner ":")
                               (not (blank-or-break-or-nul-p scanner 1)))
                      :do (error 'scanner-error
                                 :context "while scanning a plain scalar"
                                 :context-mark start-mark
                                 :problem "found unexpected ':'"
                                 :problem-mark (copy-mark scanner))
                    :when (or (and (check scanner #\:)
                                   (blank-or-break-or-nul-p scanner 1))
                              (member (peek scanner 0)
                                      '(#\, #\: #\? #\[ #\] #\{ #\})))
                      :do (return)
                    ;; Check if we need to join whitespaces and breaks
                    :when (or leading-blanks-p
                              (plusp (length whitespaces)))
                      :do (cond
                            (leading-blanks-p
                             ;; Do we need to fold the line breaks?
                             (cond
                               ((char= (char leading-break 0) #\Newline)
                                (if (zerop (length trailing-breaks))
                                    (vector-push-extend #\Space string)
                                    (setf string (join-string string trailing-breaks)
                                          (fill-pointer trailing-breaks) 0))
                                (setf (fill-pointer leading-break) 0))
                               (t
                                (setf string (join-string leading-break trailing-breaks)
                                      (fill-pointer leading-break) 0
                                      (fill-pointer trailing-breaks) 0)))
                             (setf leading-blanks-p nil))
                            (t
                             (setf string (join-string string whitespaces)
                                   (fill-pointer whitespaces) 0)))
                    :do
                       ;; Copy the character
                       (vector-push-extend (yread scanner) string)
                       (setf end-mark (copy-mark scanner))
                       (ensure-buffer-length scanner 2))
              ;; Is it the end?
          :until (not (or (blankp scanner)
                          (breakp scanner)))
          ;; Consume blank character
          :do (ensure-buffer-length scanner 1)
              (loop :while (or (blankp scanner)
                               (breakp scanner))
                    :do (cond
                          ((blankp scanner)
                           ;; Check for tab character that abuse indentation
                           (when (and leading-blanks-p
                                      (< (current-column scanner) indent)
                                      (tabp scanner))
                             (error 'scanner-error
                                    :context "while scanning a plain scalar"
                                    :context-mark start-mark
                                    :problem "found a tab character that violate indentation"
                                    :problem-mark (copy-mark scanner)))
                           ;; Consume a space or a tab character
                           (if leading-blanks-p
                               (skip scanner)
                               (vector-push-extend (yread scanner) whitespaces)))
                          (t
                           (ensure-buffer-length scanner 2)
                           ;; Check if it is a first line break
                           (cond
                             ((not leading-blanks-p)
                              (setf (fill-pointer whitespaces) 0
                                    leading-blanks-p t)
                              (vector-push-extend (yread-line scanner) leading-break))
                             (t
                              (vector-push-extend (yread-line scanner) trailing-breaks)))))
                        (ensure-buffer-length scanner 1))
          :until (and (zerop (flow-level scanner))
                      (< (current-column scanner) indent))
          ;; Note that we change the 'simple-key-allowed-p' flag
          :finally (when leading-blanks-p
                     (setf (simple-key-allowed-p scanner) t)))
    (make-scalar string (length string) :plain start-mark end-mark)))

(defun fetch-plain-scalar (scanner)
  "Produce the SCALAR(...,plain) token."
  ;; A plain scalar could be a simple key
  (save-simple-key scanner)
  ;; A simple key cannot follow a flow scalar
  (setf (simple-key-allowed-p scanner) nil)
  (enqueue (scan-plain-scalar scanner) (tokens scanner)))

(defun scan-flow-scalar (scanner singlep)
  "Scan a quoted scalar."
  (let ((start-mark (copy-mark scanner))
        (leading-blanks-p nil)
        (string (make-array 0 :element-type 'character
                              :adjustable t
                              :fill-pointer t))
        (leading-break (make-array 0 :element-type 'character
                                     :adjustable t
                                     :fill-pointer t))
        (trailing-breaks (make-array 0 :element-type 'character
                                       :adjustable t
                                       :fill-pointer t))
        (whitespaces (make-array 0 :element-type 'character
                                   :adjustable t
                                   :fill-pointer t)))
    ;; Eat the left quote.
    (skip scanner)
    ;; Consume the content of the quoted scalar
    (loop :do (ensure-buffer-length scanner 4)
          :when (and (zerop (current-column scanner))
                     (looking-at scanner "---")
                     (looking-at scanner "...")
                     (blank-or-break-or-nul-p scanner 3))
            :do (error 'scanner-error
                       :context "while scanning a quoted scalar"
                       :context-mark start-mark
                       :problem "found unexpected document indicator"
                       :problem-mark (copy-mark scanner))
          ;; Check for EOF
          :when (nulp scanner)
            :do (error 'scanner-error
                       :context "while scanning a quoted scalar"
                       :context-mark start-mark
                       :problem "found unexpected end of stream"
                       :problem-mark (copy-mark scanner))
          :do
             ;; Consume non-blank characters
             (ensure-buffer-length scanner 2)
             (loop :until (blank-or-break-or-nul-p scanner)
                   :do
                      (cond
                        ;; Check for an escaped single quote
                        ((and singlep
                              (looking-at scanner "''"))
                         (vector-push-extend #\' string)
                         (skip scanner 2))
                        ;; Check for the right quote
                        ((check scanner (if singlep #\' #\"))
                         (return))
                        ;; Check for an escaped line break
                        ((and (not singlep)
                              (check scanner #\\)
                              (breakp scanner 1))
                         (ensure-buffer-length scanner 3)
                         (skip scanner)
                         (skip-line scanner)
                         (setf leading-blanks-p t)
                         (return))
                        ((and (not singlep)
                              (check scanner #\\))
                         (let ((code-length 0))
                           (case (peek scanner 1)
                             (#\0
                              (vector-push-extend #\Nul string))
                             (#\a
                              (vector-push-extend (code-char #x07) string))
                             (#\b
                              (vector-push-extend (code-char #x08) string))
                             ((#\t #\Tab)
                              (vector-push-extend (code-char #x09) string))
                             (#\n
                              (vector-push-extend (code-char #x0A) string))
                             (#\v
                              (vector-push-extend (code-char #x0B) string))
                             (#\f
                              (vector-push-extend (code-char #x0C) string))
                             (#\r
                              (vector-push-extend (code-char #x0D) string))
                             (#\e
                              (vector-push-extend (code-char #x1B) string))
                             (#\Space
                              (vector-push-extend (code-char #x20) string))
                             (#\"
                              (vector-push-extend #\" string))
                             (#\'
                              (vector-push-extend #\" string))
                             (#\N
                              (vector-push-extend (code-char #x85) string))
                             (#\_
                              (vector-push-extend (code-char #xA0) string))
                             (#\L
                              (vector-push-extend (code-char #x2028) string))
                             (#\P
                              (vector-push-extend (code-char #x2029) string))
                             (#\x
                              (setf code-length 2))
                             (#\u
                              (setf code-length 4))
                             (#\U
                              (setf code-length 8))
                             (otherwise
                              (error 'scanner-error
                                     :context "while parsing a quoted scalar"
                                     :context-mark start-mark
                                     :problem "found unknown escape character"
                                     :problem-mark (copy-mark scanner))))
                           (skip scanner 2)
                           ;; Consume an arbitrary escape code
                           (when (plusp code-length)
                             (let ((value 0))
                               (ensure-buffer-length scanner code-length)
                               (dotimes (k code-length)
                                 (when (not (hexp scanner k))
                                   (error 'scanner-error
                                          :context "while parsing a quoted scalar"
                                          :context-mark start-mark
                                          :problem "did not find expected hexdecimal number"
                                          :problem-mark (copy-mark scanner)))
                                 (setf value
                                       (+ (ash value 4) (hexp scanner k))))
                               ;; Check the value and write the character
                               (when (or (and (>= value #xD800)
                                              (<= value #xDFFF))
                                         (> value #x10FFFF))
                                 (error 'scanner-error
                                        :context "while parsing a quoted scalar"
                                        :context-mark start-mark
                                        :problem "found invalid Unicode character escape code"
                                        :problem-mark (copy-mark scanner)))
                               (vector-push-extend (code-char value) string)
                               (skip scanner code-length)))))
                        (t
                         (vector-push-extend (yread scanner) string)))
                      (ensure-buffer-length scanner 2))
          :until (check scanner (if singlep #\' #\"))
          :do
             (ensure-buffer-length scanner 1)
             (loop :while (or (blankp scanner)
                              (breakp scanner))
                   :do
                      (cond
                        ((blankp scanner)
                         ;; Consume a space or a tab character
                         (if leading-blanks-p
                             (skip scanner)
                             (vector-push-extend (yread scanner) whitespaces)))
                        (t
                         (ensure-buffer-length scanner 2)
                         ;; Check if it is a first line break
                         (if leading-blanks-p
                             (vector-push-extend (yread-line scanner)
                                                 trailing-breaks)
                             (progn (setf (fill-pointer whitespaces) 0
                                          leading-blanks-p t)
                                    (vector-push-extend (yread-line scanner)
                                                        leading-break)))))
                      (ensure-buffer-length scanner 1))
             ;; Join the whitespaces or fold line breaks
             (cond
               (leading-blanks-p
                ;; Do we need to fold line breaks?
                (cond
                  ((char= (char leading-break 0) #\Newline)
                   (if (zerop (length trailing-breaks))
                       (vector-push-extend #\Space string)
                       (setf string (join-string string trailing-breaks)
                             (fill-pointer trailing-breaks) 0)))
                  (t
                   (setf string (join-string string leading-break)
                         string (join-string string trailing-breaks)
                         (fill-pointer leading-break) 0
                         (fill-pointer trailing-breaks) 0))))
               (t
                (setf string (join-string string whitespaces)
                      (fill-pointer whitespaces) 0))))
    ;; Eat the right quote
    (skip scanner)
    (let ((end-mark (copy-mark scanner)))
      (make-scalar string (length string)
                   (if singlep :single-quoted :double-quoted)
                   start-mark end-mark))))

(defun fetch-flow-scalar (scanner type)
  "Produce the SCALAR(...,single-quoted) or SCALAR(...,double-quoted) tokens."
  (check-type type (or (eql :single-quoted)
                       (eql :double-quoted)))
  ;; A plain scalar could be a simple key
  (save-simple-key scanner)
  ;; A simple key cannot follow a flow scalar.
  (setf (simple-key-allowed-p scanner) nil)
  (enqueue (scan-flow-scalar scanner (eql type :single-quoted))
           (tokens scanner)))

(defun utf8-width (octet)
  (cond
    ((= (logand octet #b10000000) #b00000000) 1)
    ((= (logand octet #b11100000) #b11000000) 2)
    ((= (logand octet #b11110000) #b11100000) 3)
    ((= (logand octet #b11111000) #b11110000) 4)
    (t 0)))

(defun utf8-octet (octet)
  (case (utf8-width octet)
    (1 (logand octet #b01111111))
    (2 (logand octet #b00011111))
    (3 (logand octet #b00001111))
    (4 (logand octet #b00000111))
    (otherwise
     (assert (= (logand octet #b11000000) #b10000000))
     (logand octet #b00111111))))

(defun scan-uri-escapes (scanner directivep start-mark)
  "Decode an URI-escape sequence corresponding to a single UTF-8 character."
  (let ((width 0)
        (code 0))
    (loop :do (ensure-buffer-length scanner 3)
              (when (not (and (check scanner #\%)
                              (hexp scanner 1)
                              (hexp scanner 2)))
                (error 'scanner-error
                       :context (if directivep
                                    "while parsing a %TAG directive"
                                    "while parsing a tag")
                       :context-mark start-mark
                       :problem "did not find URI escaped octet"
                       :problem-mark (copy-mark scanner)))
              ;; Get the octet
              (let ((octet (+ (ash (hexp scanner 1) 4)
                              (hexp scanner 2))))
                (cond
                  ((zerop width)
                   (setf width (utf8-width octet))
                   (when (zerop width)
                     (error 'scanner-error
                            :context (if directivep
                                         "while parsing a %TAG directive"
                                         "while parsing a tag")
                            :context-mark start-mark
                            :problem "found an incorrect leading UTF-8 otect"
                            :problem-mark (copy-mark scanner)))
                   (setf code (utf8-octet octet)))
                  (t
                   (when (/= (logand octet #b11000000) #b10000000)
                     (error 'scanner-error
                            :context (if directivep
                                         "while parsing a %TAG directive"
                                         "while parsing a tag")
                            :context-mark start-mark
                            :problem "found an incorrect trailing UTF-8 otect"
                            :problem-mark (copy-mark scanner)))
                   (setf code (+ (ash code 6) (utf8-octet octet))))))
              (decf width)
          :while (plusp width))
    code))

(defun scan-tag-uri (scanner directivep handle start-mark)
  "Scan a TAG."
  (let* ((length (length handle))
         (string (make-array length :element-type 'character
                                    :adjustable t
                                    :fill-pointer t)))
    ;; Copy the head if needed
    ;; Note that we don't copy the leading '!' character.
    (when (plusp length)
      (replace string handle :start2 1)
      (setf (fill-pointer string) (1- length)))
    ;; The set of characters that may appear in URI is as follows:
    ;; 
    ;;      '0'-'9', 'A'-'Z', 'a'-'z', '_', '-', ';', '/', '?', ':', '@', '&',
    ;;      '=', '+', '$', ',', '.', '!', '~', '*', '\'', '(', ')', '[', ']',
    ;;      '%'.
    (loop :while (or (alphap scanner)
                     (member (peek scanner 0)
                             '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$
                               #\, #\. #\! #\~ #\* #\\ #\( #\) #\[
                               #\] #\%)))
          ;; Check if it is a URI-escape sequence
          :do
             (if (check scanner #\%)
                 (let ((uri-escapes
                         (scan-uri-escapes scanner directivep start-mark)))
                   (setf string (join-string string uri-escapes)))
                 (vector-push-extend (yread scanner) string))
             (incf length)
             (ensure-buffer-length scanner 1))
    ;; Check if the tag is non-empty
    (when (zerop length)
      (error 'scanner-error
             :context (if directivep
                          "while parsing a %TAG directive"
                          "while parsing a tag")
             :context-mark start-mark
             :problem "did not find expected tag URI"
             :problem-mark (copy-mark scanner)))
    string))

(defun scan-tag-handle (scanner directivep start-mark)
  (let ((handle (make-array 0 :element-type 'character
                              :adjustable t
                              :fill-pointer t)))
    ;; Check the inital '!' character
    (ensure-buffer-length scanner 1)
    (when (not (check scanner #\!))
      (error 'scanner-error
             :context (if directivep
                          "while scanning a tag directive"
                          "while scanning a tag")
             :context-mark start-mark
             :problem "did not find expected '!'"
             :problem-mark (copy-mark scanner)))
    (vector-push-extend (yread scanner) handle)
    ;; Copy all subsequent alphabetical and numerical characters
    (ensure-buffer-length scanner 1)
    (loop :while (alphap scanner)
          :do (vector-push-extend (yread scanner) handle)
              (ensure-buffer-length scanner 1))
    ;; Check if the trailing character is '!' and copy it
    (cond
      ((check scanner #\!)
       (vector-push-extend (yread scanner) handle))
      (t
       ;; It's either the '!' tag or not really a tag handle.  If it's
       ;; a %TAG directive, it's an error.  If it's a tag token, it
       ;; must be a part of URI.
       (when (and directivep
                  (not (string= handle "!")))
         (error 'scanner-error
                :context "while parsing a tag directive"
                :context-mark start-mark
                :problem "did not find expected '!'"
                :problem-mark (copy-mark scanner)))))
    handle))

(defun scan-tag (scanner)
  "Scan a TAG token."
  (let ((start-mark (copy-mark scanner))
        (handle nil)
        (suffix nil))
    ;; Check if the tag is in the canonical form
    (ensure-buffer-length scanner 2)
    (cond
      ((check scanner #\< 1)
       ;; Eat '!<'
       (skip scanner 2)
       ;; Consume the tag value
       (setf suffix (scan-tag-uri scanner nil nil start-mark))
       ;; Check '>' and eat it
       (when (not (check scanner #\>))
         (error 'scanner-error
                :context "while scanning a tag"
                :context-mark start-mark
                :problem "did not find the expected '>'"
                :problem-mark (copy-mark scanner)))
       (skip scanner))
      (t
       ;; The tag has either the '!suffix or the '!handle!suffix' form
       ;; First, try to scan a handle
       (setf handle (scan-tag-handle scanner nil start-mark))
       ;; Check if it is, indeed, a handle
       (cond
         ((and (char= (char handle 0) #\!)
               (> (length handle) 1)
               (char= (char handle (1- (length handle))) #\!))
          ;; Scan the suffix now
          (setf suffix (scan-tag-uri scanner nil nil start-mark)))
         (t
          ;; It wasn't a handle after all. Scan the rest of the tag
          (setf suffix (scan-tag-uri scanner nil handle start-mark))
          ;; Set the handle to '!'
          (setf handle "!")
          ;; A special case: the '!' tag. Set the handle to '' and the
          ;; suffix to '!'
          (when (zerop (length suffix))
            (rotatef suffix handle))))))
    ;; Check the character which ends the tag
    (ensure-buffer-length scanner 1)
    (when (not (blank-or-break-or-nul-p scanner))
      (error 'scanner-error
             :context "while scanning a tag"
             :context-mark start-mark
             :problem "did not find expected whitespace or line break"
             :problem-mark (copy-mark scanner)))
    (let ((end-mark (copy-mark scanner)))
      (make-tag handle suffix start-mark end-mark))))

(defun fetch-tag (scanner)
  "Produce the TAG token."
  ;; A tag could be a simple key
  (save-simple-key scanner)
  ;; A simple key cannot follow a tag
  (setf (simple-key-allowed-p scanner) nil)
  (enqueue (scan-tag scanner) (tokens scanner)))

(defun scan-directive-name (scanner start-mark)
  "Scan the directive name.

Scope:
     %YAML   1.1     # a comment \n
      ^^^^
     %TAG    !yaml!  tag:yaml.org,2002:  \n
      ^^^"
  (let ((name (make-array 0 :element-type 'character
                            :adjustable t
                            :fill-pointer t)))
    ;; Consume the directive name
    (ensure-buffer-length scanner 1)
    (loop :while (alphap scanner)
          :do (vector-push-extend (yread scanner) name)
              (ensure-buffer-length scanner 1))
    ;; Check if the name is empty
    (when (zerop (length name))
      (error 'scanner-error
             :context "while scanning a directive"
             :context-mark start-mark
             :problem "could not find expected directive name"
             :problem-mark (copy-mark scanner)))
    ;; Check for an blank character after the name
    (when (not (blank-or-break-or-nul-p scanner))
      (error 'scanner-error
             :context "while scanning a directive"
             :context-mark start-mark
             :problem "found unexpected non-alphabetical character"
             :problem-mark (copy-mark scanner)))
    name))

(defparameter *max-number-length* 9)

(defun scan-version-directive-number (scanner start-mark)
  "Scan the version number of VERSION-DIRECTIVE

Scope:
     %YAML    1.1    # a comment \n
              ^
     %YAML    1.1    # a comment \n
                ^"
  (let ((value 0)
        (length 0))
    (ensure-buffer-length scanner 1)
    ;; Repeat while the next character is digit
    (loop :while (digitp scanner)
          :do (incf length)
              (when (> length *max-number-length*)
                (error 'scanner-error
                       :context "while scanning a %YAML directive"
                       :context-mark start-mark
                       :problem "found extremely long version number"
                       :problem-mark (copy-mark scanner)))
              (setf value (+ (* value 10) (digit-char-p (yread scanner) 10)))
              (ensure-buffer-length scanner 1))
    (when (zerop length)
      (error 'scanner-error
             :context "while scanning a %YAML directive"
             :context-mark start-mark
             :problem "did not find expected version number"
             :problem-mark (copy-mark scanner)))
    value))

(defun scan-version-directive-value (scanner start-mark)
  "Scan the value of VERSION-DIRECTIVE

Scope:
     %YAML    1.1    # a comment \n
          ^^^^^^^"
  ;; Eat whitespaces
  (ensure-buffer-length scanner 1)
  (loop :while (blankp scanner)
        :do (skip scanner)
            (ensure-buffer-length scanner 1))
  ;; Consume the major version number
  (let ((major (scan-version-directive-number scanner start-mark)))
    ;; Eat '.'
    (when (not (check scanner #\.))
      (error 'scanner-error
             :context "while scanning a %YAML directive"
             :context-mark start-mark
             :problem "did not find expected digit or '.' character"
             :problem-mark (copy-mark scanner)))
    (skip scanner)
    ;; Consume the minor version number
    (let ((minor (scan-version-directive-number scanner start-mark)))
      (values major minor))))

(defun scan-tag-directive-value (scanner start-mark)
  "Scan the value of a TAG-DIRECTIVE token

Scope:
     %TAG     !yaml!  tag:yaml.org,2002:  \n
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  ;; Eat whitespaces
  (ensure-buffer-length scanner 1)
  (loop :while (blankp scanner)
        :do (skip scanner)
            (ensure-buffer-length scanner 1))
  ;; Scan a handle
  (let ((handle (scan-tag-handle scanner t start-mark)))
    ;; Expect a whitespace
    (ensure-buffer-length scanner 1)
    (when (not (blankp scanner))
      (error 'scanner-error
             :context "while scanning a %TAG directive"
             :context-mark start-mark
             :problem "did not find expected whitespace"
             :problem-mark (copy-mark scanner)))
    ;; Eat whitespaces
    (loop :while (blankp scanner)
          :do (skip scanner)
              (ensure-buffer-length scanner 1))
    ;; Scan a prefix
    (let ((prefix (scan-tag-uri scanner t nil start-mark)))
      ;; Expect a whitespace or line break
      (ensure-buffer-length scanner 1)
      (when (not (blank-or-break-or-nul-p scanner))
        (error 'scanner-error
               :context "while scanning a %TAG directive"
               :context-mark start-mark
               :problem "did not find expected whitespace or line break"
               :problem-mark (copy-mark scanner)))
      (values handle prefix))))

(defun scan-directive (scanner)
  "Scan a YAML-DIRECTIVE or TAG-DIRECTIVE token.

Scope:
    %YAML    1.1    # a comment \n
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    %TAG    !yaml!  tag:yaml.org,2002:  \n
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  (let ((start-mark (copy-mark scanner)))
    ;; Eat '%'
    (skip scanner)
    ;; Scan the directive name
    (prog1
        (let ((name (scan-directive-name scanner start-mark)))
          (cond
            ((string= name "YAML")
             ;; Scan the VERSION directive value
             (multiple-value-bind (major minor)
                 (scan-version-directive-value scanner start-mark)
               (make-version-directive major minor
                                       start-mark (copy-mark scanner))))
            ((string= name "TAG")
             ;; Scan the TAG directive value
             (multiple-value-bind (handle prefix)
                 (scan-tag-directive-value scanner start-mark)
               (make-tag-directive handle prefix
                                   start-mark (copy-mark scanner))))
            (t
             (error 'scanner-error
                    :context "while scanning a directive"
                    :context-mark start-mark
                    :problem "found unknown directive name"
                    :problem-mark (copy-mark scanner)))))
      ;; Eat the rest of the line including any comments
      (loop :while (blankp scanner)
            :do (skip scanner)
                (ensure-buffer-length scanner 1))
      (when (check scanner #\#)
        (loop :while (not (or (breakp scanner)
                              (nulp scanner)))
              :do (skip scanner)
                  (ensure-buffer-length scanner 1)))
      ;; Check if we are at the end of the line
      (when (not (or (breakp scanner)
                     (nulp scanner)))
        (error 'scanner-error
               :context "while scanning a directive"
               :context-mark start-mark
               :problem "did not find expected comment or line break"
               :problem-mark (copy-mark scanner)))
      ;; Eat a line break
      (when (breakp scanner)
        (ensure-buffer-length scanner 2)
        (skip-line scanner)))))

(defun fetch-directive (scanner)
  "Produce a VERSION-DIRECTIVE or TAG-DIRECTIVE token."
  ;; Reset the indentation level
  (unroll-indent scanner -1)
  ;; Reset simple keys
  (remove-simple-key scanner)
  (setf (simple-key-allowed-p scanner) nil)
  (enqueue (scan-directive scanner) (tokens scanner)))

(defun scan-block-scalar-breaks (scanner start-mark indent breaks)
  "Scan indentation spaces and line breaks for a block
scalar. Determine the indentation level if needed."
  (let ((max-indent 0)
        (end-mark (copy-mark scanner)))
    ;; Eat the indentation spaces and line breaks
    (loop :do (ensure-buffer-length scanner 1)
              (loop :while (and (or (zerop indent)
                                    (< (current-column scanner) indent))
                                (spacep scanner))
                    :do (skip scanner)
                        (ensure-buffer-length scanner 1))
              (setf max-indent (max max-indent (current-column scanner)))
              ;; Check for a tab character messing the indentation
          :when (and (or (zerop indent)
                         (< (current-column scanner) indent))
                     (tabp scanner))
            :do (error 'scanner-error
                       :context "while scanning a block scalar"
                       :context-mark start-mark
                       :problem "found a tab character where an indentation space is expected")
          :until (not (breakp scanner))
          ;; Consume the line break
          :do (ensure-buffer-length scanner 2)
              (vector-push-extend (yread-line scanner) breaks))
    ;; Determine the indentation level if needed
    (when (zerop indent)
      (cond
        ((< max-indent (1+ (indent scanner)))
         (setf indent (1+ (indent scanner))))
        ((< max-indent 1)
         (setf indent 1))
        (t
         (setf indent max-indent))))
    (values indent end-mark)))

(defun scan-block-scalar (scanner literalp)
  "Scan a block scalar."
  (let ((start-mark (copy-mark scanner))
        (string (make-array 0 :element-type 'character
                              :adjustable t
                              :fill-pointer t))
        (leading-break (make-array 0 :element-type 'character
                                     :adjustable t
                                     :fill-pointer t))
        (trailing-breaks (make-array 0 :element-type 'character
                                       :adjustable t
                                       :fill-pointer t))
        (chomping 0)
        (indent 0)
        (increment 0)
        (leading-blank-p nil))
    ;; Eat the indicator '|' or '>'
    (skip scanner)
    ;; Scan the additional block scalar indicators
    (ensure-buffer-length scanner 1)
    ;; Check for a chomping indicator
    (cond
      ((or (check scanner #\+)
           (check scanner #\-))
       ;; Set the chomping method an eat the indicator
       (setf chomping (if (check scanner #\+) 1 -1))
       (skip scanner)
       ;; Check for an indentation indicator
       (ensure-buffer-length scanner 1)
       (when (digitp scanner)
         ;; Check that the indentation is greater than 0
         (when (check scanner #\0)
           (error 'scanner-error
                  :context "while scanning a block scalar"
                  :context-mark start-mark
                  :problem "found an indentation indicator equal to 0"
                  :problem-mark (copy-mark scanner)))
         ;; Get the indentation level and eat the indicator
         (setf increment (digitp scanner))
         (skip scanner)))
      ;; Do the same as above, but in the opposite order
      ((digitp scanner)
       (when (check scanner #\0)
         (error 'scanner-error
                :context "while scanning a block scalar"
                :context-mark start-mark
                :problem "found an indentation indicator equal to 0"
                :problem-mark (copy-mark scanner)))
       (setf increment (digitp scanner))
       (skip scanner)
       (ensure-buffer-length scanner 1)
       (when (or (check scanner #\+)
                 (check scanner #\-))
         (setf chomping (if (check scanner #\+) 1 -1))
         (skip scanner))))
    ;; Eat whitespaces and comments to the end of the line
    (ensure-buffer-length scanner 1)
    (loop :while (blankp scanner)
          :do (skip scanner)
              (ensure-buffer-length scanner 1))
    (when (check scanner #\#)
      (loop :while (not (blank-or-break-or-nul-p scanner))
            :do (skip scanner)
                (ensure-buffer-length scanner 1)))
    ;; Check if we are at the end of the line
    (when (not (blank-or-break-or-nul-p scanner))
      (error 'scanner-error
             :context "while scanning a block scalar"
             :context-mark start-mark
             :problem "did not find expected comment or line break"
             :problem-mark (copy-mark scanner)))
    ;; Eat a line break
    (when (breakp scanner)
      (ensure-buffer-length scanner 2)
      (skip-line scanner))
    (let ((end-mark (copy-mark scanner)))
      (when (plusp increment)
        (setf indent (if (>= (indent scanner) 0)
                         (+ (indent scanner) increment)
                         increment)))
      ;; Scan the leading line breaks and determine the indentation
      ;; level if needed
      (multiple-value-setq (indent end-mark)
        (scan-block-scalar-breaks scanner start-mark indent trailing-breaks))
      ;; Scan the block scalar content
      (ensure-buffer-length scanner 1)
      (loop :while (and (= (current-column scanner)
                           indent)
                        (not (nulp scanner)))
            ;; We are at the beginning of a non-empty line.
            ;; Is it a trailing whitespace?
            :for trailing-blank-p := (blankp scanner)
            ;; Check if we need to fold the leading line break
            :when (and (not literalp)
                       (plusp (length leading-break))
                       (char= (char leading-break 0) #\Newline)
                       (not leading-blank-p)
                       (not trailing-blank-p))
              :do (when (zerop (length trailing-breaks))
                    (vector-push-extend #\Space string))
                  (setf (fill-pointer leading-break) 0)
            :else
              :do (setf string (join-string string leading-break)
                        (fill-pointer leading-break) 0)
            :do (setf string (join-string string trailing-breaks)
                      (fill-pointer trailing-breaks) 0
                      leading-blank-p (blankp scanner))
                (loop :while (not (break-or-nul-p scanner))
                      :do (vector-push-extend (yread scanner) string)
                          (ensure-buffer-length scanner 1))
                (ensure-buffer-length scanner 2)
                (vector-push-extend (yread-line scanner) leading-break)
                ;; Eat the following indentation spaces and line breaks
                (multiple-value-setq (indent end-mark)
                  (scan-block-scalar-breaks scanner start-mark
                                            indent trailing-breaks)))
      ;; Chomp the tail
      (when (/= chomping -1)
        (setf string (join-string string leading-break)))
      (when (= chomping 1)
        (setf string (join-string string trailing-breaks)))
      (make-scalar string (length string)
                   (if literalp :literal :folded)
                   start-mark end-mark))))

(defun fetch-block-scalar (scanner literal-or-folded)
  "Produce the SCALAR(...,literal) or SCALAR(...,folded) tokens."
  (check-type literal-or-folded (or (eql :literal)
                                    (eql :folded)))
  ;; Remove any potential simple keys
  (remove-simple-key scanner)
  ;; A simple key may follow a block scalar
  (setf (simple-key-allowed-p scanner) t)
  (enqueue (scan-block-scalar scanner (eql literal-or-folded :literal))
           (tokens scanner)))

;;; scanner.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:

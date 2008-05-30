;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/test/test.lisp,v 1.39 2008/05/30 09:10:55 edi Exp $

;;; Copyright (c) 2006-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :flexi-streams-test)

(defmacro with-test ((test-description) &body body)
  "Defines a test.  Two utilities are available inside of the body of
the maco: The function FAIL, and the macro CHECK.  FAIL, the lowest
level utility, marks the test defined by WITH-TEST as failed.  CHECK
checks whether its argument is true, otherwise it calls FAIL. If
during evaluation of the specified expression any condition is
signalled, this is also considered a failure.

WITH-TEST prints reports while the tests run.  It also increments
*TEST-SUCCESS-COUNT* if a test completes successfully."
  (flex::with-unique-names (successp)
    `(let ((,successp t))
       (flet ((fail (format-str &rest format-args)
                (setf ,successp nil)
                (apply #'format *error-output* format-str format-args)))
         (macrolet ((check (expression)
                      `(handler-case
                           (unless ,expression
                             (fail "Expression ~S failed.~%" ',expression))
                         (error (c)
                           (fail "Expression ~S failed signalling error of type ~A: ~A.~%" 
                                 ',expression (type-of c) c))))
                    (with-expected-error ((condition-type) &body body)
                      `(handler-case (progn ,@body)
                         (,condition-type () t)
                         (:no-error (&rest args)
                           (declare (ignore args))                           
                           (fail "Expected condition ~S not signalled~%"
                                 ',condition-type)))))
           (format *error-output* "Test ~S~%" ,test-description)
           ,@body
           (if ,successp
             (incf *test-success-counter*)
             (format *error-output* "    Test failed!!!~%"))
           (terpri *error-output*)
           (terpri *error-output*))
         ,successp))))

;; LW can't indent this correctly because it's in a MACROLET
#+:lispworks
(editor:setup-indent "with-expected-error" 1 2 4)

(defconstant +buffer-size+ 8192
  "Size of buffers for COPY-STREAM* below.")

(defvar *copy-function* nil
  "Which function to use when copying from one stream to the other -
see for example COPY-FILE below.")

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The pathname of the file \(`test.lisp') where this variable was
defined.")

#+:lispworks
(defun get-env-variable-as-directory (name)
  (lw:when-let (string (lw:environment-variable name))
    (when (plusp (length string))
      (cond ((find (char string (1- (length string))) "\\/" :test #'char=) string)
            (t (lw:string-append string "/"))))))

(defvar *tmp-dir*
  (load-time-value
    (merge-pathnames "odd-streams-test/"
                     #+:allegro (system:temporary-directory)
                     #+:lispworks (pathname (or (get-env-variable-as-directory "TEMP")
                                                (get-env-variable-as-directory "TMP")
                                                #+:win32 "C:/"
                                                #-:win32 "/tmp/"))
                     #-(or :allegro :lispworks) #p"/tmp/"))
  "The pathname of a temporary directory used for testing.")

(defvar *test-files*
  '(("kafka" (:utf8 :latin1 :cp1252))
    ("tilton" (:utf8 :ascii))
    ("hebrew" (:utf8 :latin8))
    ("russian" (:utf8 :koi8r))
    ("unicode_demo" (:utf8 :ucs2 :ucs4)))
  "A list of test files where each entry consists of the name
prefix and a list of encodings.")

(defvar *test-success-counter* 0
  "Counts the number of successful tests.")

(defun create-file-variants (file-name symbol)
  "For a name suffix FILE-NAME and a symbol SYMBOL denoting an
encoding returns a list of pairs where the car is a full file
name and the cdr is the corresponding external format.  This list
contains all possible variants w.r.t. to line-end conversion and
endianness."
  (let ((args (ecase symbol
                (:ascii '(:ascii))
                (:latin1 '(:latin-1))
                (:latin8 '(:hebrew))
                (:cp1252 '(:code-page :id 1252))
                (:koi8r '(:koi8-r))
                (:utf8 '(:utf-8))
                (:ucs2 '(:utf-16))
                (:ucs4 '(:utf-32))))
        (endianp (member symbol '(:ucs2 :ucs4))))
    (loop for little-endian in (if endianp '(t nil) '(t))
          for endian-suffix in (if endianp '("_le" "_be") '(""))
          nconc (loop for eol-style in '(:lf :cr :crlf)
                      collect (cons (format nil "~A_~(~A~)_~(~A~)~A.txt"
                                            file-name symbol eol-style endian-suffix)
                                    (apply #'make-external-format
                                           (append args `(:eol-style ,eol-style
                                                          :little-endian ,little-endian))))))))

(defun create-test-combinations (file-name symbols &optional simplep)
  "For a name suffix FILE-NAME and a list of symbols SYMBOLS denoting
different encodings of the corresponding file returns a list of lists
which can be used as arglists for COMPARE-FILES.  If SIMPLEP is true,
a list which can be used for the string tests below is returned."
  (let ((file-variants (loop for symbol in symbols
                             nconc (create-file-variants file-name symbol))))
    (loop for (name-in . external-format-in) in file-variants
          when simplep
          collect (list name-in external-format-in)
          else
          nconc (loop for (name-out . external-format-out) in file-variants
                      collect (list name-in external-format-in name-out external-format-out)))))
                      
(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same
contents \(viewed as binary files)."
  (with-open-file (stream1 file1 :element-type 'octet)
    (with-open-file (stream2 file2 :element-type 'octet)
      (and (= (file-length stream1) (file-length stream2))
           (loop for byte1 = (read-byte stream1 nil nil)
                 for byte2 = (read-byte stream2 nil nil)
                 while (and byte1 byte2)
                 always (= byte1 byte2))))))

(defun copy-stream (stream-in external-format-in stream-out external-format-out)
  "Copies the contents of the binary stream STREAM-IN to the
binary stream STREAM-OUT using flexi streams - STREAM-IN is read
with the external format EXTERNAL-FORMAT-IN and STREAM-OUT is
written with EXTERNAL-FORMAT-OUT."
  (let ((in (make-flexi-stream stream-in :external-format external-format-in))
        (out (make-flexi-stream stream-out :external-format external-format-out)))
    (loop for line = (read-line in nil nil)
          while line
          do (write-line line out))))

(defun copy-stream* (stream-in external-format-in stream-out external-format-out)
  "Like COPY-STREAM, but uses READ-SEQUENCE and WRITE-SEQUENCE instead
of READ-LINE and WRITE-LINE."
  (let ((in (make-flexi-stream stream-in :external-format external-format-in))
        (out (make-flexi-stream stream-out :external-format external-format-out))
        (buffer (make-array +buffer-size+ :element-type 'flex::char*)))
    (loop
     (let ((position (read-sequence buffer in)))
       (when (zerop position) (return))
       (write-sequence buffer out :end position)))))

(defun copy-file (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Copies the contents of the file denoted by the pathname
PATH-IN to the file denoted by the pathname PATH-OUT using flexi
streams - STREAM-IN is read with the external format
EXTERNAL-FORMAT-IN and STREAM-OUT is written with
EXTERNAL-FORMAT-OUT.  The input file is opened with
the :DIRECTION keyword argument DIRECTION-IN, the output file is
opened with the :DIRECTION keyword argument DIRECTION-OUT."
  (with-open-file (in path-in
                      :element-type 'octet
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :element-type 'octet
                         :direction direction-out
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (funcall *copy-function* in external-format-in out external-format-out))))

#+:lispworks
(defun copy-file-lw (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Same as COPY-FILE, but uses character streams instead of
binary streams.  Only used to test LispWorks-specific behaviour."
  (with-open-file (in path-in
                      :external-format '(:latin-1 :eol-style :lf)
                      :element-type 'base-char
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :external-format '(:latin-1 :eol-style :lf)
                         :element-type 'base-char
                         :direction direction-out
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (funcall *copy-function* in external-format-in out external-format-out))))

(defun compare-files (path-in external-format-in path-out external-format-out)
  "Copies the contents of the file (in the `test') denoted by the
relative pathname PATH-IN to the file (in a temporary directory)
denoted by the relative pathname PATH-OUT using flexi streams -
STREAM-IN is read with the external format EXTERNAL-FORMAT-IN and
STREAM-OUT is written with EXTERNAL-FORMAT-OUT.  The resulting
file is compared with an existing file in the `test' directory to
check if the outcome is as expected.  Uses various variants of
the :DIRECTION keyword when opening the files."
  (let ((full-path-in (merge-pathnames path-in *this-file*))
        (full-path-out (ensure-directories-exist
                        (merge-pathnames path-out *tmp-dir*)))
        (full-path-orig (merge-pathnames path-out *this-file*)))
    (dolist (direction-out '(:output :io))
      (dolist (direction-in '(:input :io))
        (format *error-output* "Test \(using ~A) ~S ~S [~A]~% --> ~S [~A].~%"
                *copy-function* path-in
                (flex::normalize-external-format external-format-in) direction-in
                (flex::normalize-external-format external-format-out) direction-out)
        (copy-file full-path-in external-format-in
                   full-path-out external-format-out
                   direction-out direction-in)
        (cond ((file-equal full-path-out full-path-orig)
               (incf *test-success-counter*))
              (t (format *error-output* " Test failed!!!~%")))
        (terpri *error-output*)
        #+:lispworks
        (format *error-output* "LW-Test \(using ~A) ~S ~S [~A]~%    --> ~S [~A].~%"
                *copy-function* path-in
                (flex::normalize-external-format external-format-in) direction-in
                (flex::normalize-external-format external-format-out) direction-out)
        #+:lispworks
        (copy-file-lw full-path-in external-format-in
                      full-path-out external-format-out
                      direction-out direction-in)
        #+:lispworks
        (cond ((file-equal full-path-out full-path-orig)
               (incf *test-success-counter*))
              (t (format *error-output* "    Test failed!!!~%")))
        #+:lispworks
        (terpri *error-output*)))))

(defun file-as-octet-vector (pathspec)
  "Returns the contents of the file denoted by PATHSPEC as a vector of
octets."
  (with-open-file (in pathspec :element-type 'octet)
    (let ((vector (make-array (file-length in) :element-type 'octet)))
      (read-sequence vector in)
      vector)))

(defun file-as-string (pathspec external-format)
  "Reads the contents of the file denoted by PATHSPEC using the
external format EXTERNAL-FORMAT and returns the result as a string."
  (with-open-file (in pathspec :element-type 'octet)
    (let* ((number-of-octets (file-length in))
           (in (make-flexi-stream in :external-format external-format))
           (string (make-array number-of-octets
                               :element-type #+:lispworks 'lw:simple-char
                                             #-:lispworks 'character
                               :fill-pointer t)))
      (setf (fill-pointer string) (read-sequence string in))
      string)))

(defun old-string-to-octets (string &key
                                    (external-format (make-external-format :latin1))
                                    (start 0) end)
  "The old version of STRING-TO-OCTETS.  We can use it to test
in-memory streams."
  (declare (optimize speed))
  (with-output-to-sequence (out)
    (let ((flexi (make-flexi-stream out :external-format external-format)))
      (write-string string flexi :start start :end end))))

(defun old-octets-to-string (vector &key
                                    (external-format (make-external-format :latin1))
                                    (start 0) (end (length vector)))
  "The old version of OCTETS-TO-STRING.  We can use it to test
in-memory streams."
  (declare (optimize speed))
  (with-input-from-sequence (in vector :start start :end end)
    (let ((flexi (make-flexi-stream in :external-format external-format))
          (result (make-array (- end start)
                              :element-type #+:lispworks 'lw:simple-char
                                            #-:lispworks 'character
                              :fill-pointer t)))
      (setf (fill-pointer result)
            (read-sequence result flexi))
      result)))

(defun string-test (pathspec external-format)
  "Tests whether conversion from strings to octets and vice versa
using the external format EXTERNAL-FORMAT works as expected, using the
contents of the file denoted by PATHSPEC as test data and assuming
that the stream conversion functions work.

Also tests with the old versions of the conversion functions in order
to test in-memory streams."
  (let* ((full-path (merge-pathnames pathspec *this-file*))
         (octets-vector (file-as-octet-vector full-path))
         (octets-list (coerce octets-vector 'list))
         (string (file-as-string full-path external-format)))
    (with-test ((format nil "String tests with format ~S."
                        (flex::normalize-external-format external-format)))
      (check (string= (octets-to-string octets-vector :external-format external-format) string))
      (check (string= (octets-to-string octets-list :external-format external-format) string))
      (check (equalp (string-to-octets string :external-format external-format) octets-vector))
      (check (string= (old-octets-to-string octets-vector :external-format external-format) string))
      (check (string= (old-octets-to-string octets-list :external-format external-format) string))
      (check (equalp (old-string-to-octets string :external-format external-format) octets-vector)))))

(defun sequence-equal (seq1 seq2)
  "Whether the two sequences have the same elements."
  (and (= (length seq1) (length seq2))
       (loop for i below (length seq1)
             always (eql (elt seq1 i) (elt seq2 i)))))

(defun sequence-test (pathspec external-format)
  "Several tests to confirm that READ-SEQUENCE and WRITE-SEQUENCE
behave as expected."
  (with-test ((format nil "Sequence tests with format ~S and file ~A."
                      (flex::normalize-external-format external-format) pathspec))
    (let* ((full-path (merge-pathnames pathspec *this-file*))
           (file-string (file-as-string full-path external-format))
           (string-length (length file-string))
           (octets (file-as-octet-vector full-path))
           (octet-length (length octets)))
      (when (external-format-equal external-format (make-external-format :utf8))
        #-:openmcl
        ;; FLEXI-STREAMS puts integers into the list, but OpenMCL
        ;; thinks they are characters...
        (with-open-file (in full-path :element-type 'octet)
          (let* ((in (make-flexi-stream in :external-format external-format))
                 (list (make-list octet-length)))
            (setf (flexi-stream-element-type in) 'octet)
            #-:clisp
            (read-sequence list in)
            #+:clisp
            (ext:read-byte-sequence list in)
            (check (sequence-equal list octets))))
        (with-open-file (in full-path :element-type 'octet)
          (let* ((in (make-flexi-stream in :external-format external-format))
                 (third (floor octet-length 3))
                 (half (floor octet-length 2))
                 (vector (make-array half :element-type 'octet)))
            (check (sequence-equal (loop repeat third
                                         collect (read-byte in))
                                   (subseq octets 0 third)))
            (read-sequence vector in)
            (check (sequence-equal vector (subseq octets third (+ third half)))))))
      (with-open-file (in full-path :element-type 'octet)
        (let* ((in (make-flexi-stream in :external-format external-format))
               (string (make-string (- string-length 10) :element-type 'flex::char*)))
          (setf (flexi-stream-element-type in) 'octet)
          (check (sequence-equal (loop repeat 10
                                       collect (read-char in))
                                 (subseq file-string 0 10)))
          (read-sequence string in)
          (check (sequence-equal string (subseq file-string 10)))))
      (with-open-file (in full-path :element-type 'octet)
        (let* ((in (make-flexi-stream in :external-format external-format))
               (list (make-list (- string-length 100))))
          (check (sequence-equal (loop repeat 50
                                       collect (read-char in))
                                 (subseq file-string 0 50)))
          #-:clisp
          (read-sequence list in)
          #+:clisp
          (ext:read-char-sequence list in)
          (check (sequence-equal list (subseq file-string 50 (- string-length 50))))
          (check (sequence-equal (loop repeat 50
                                       collect (read-char in))
                                 (subseq file-string (- string-length 50))))))
      (with-open-file (in full-path :element-type 'octet)
        (let* ((in (make-flexi-stream in :external-format external-format))
               (array (make-array (- string-length 50))))
          (check (sequence-equal (loop repeat 25
                                       collect (read-char in))
                                 (subseq file-string 0 25)))
          #-:clisp
          (read-sequence array in)
          #+:clisp
          (ext:read-char-sequence array in)
          (check (sequence-equal array (subseq file-string 25 (- string-length 25))))
          (check (sequence-equal (loop repeat 25
                                       collect (read-char in))
                                 (subseq file-string (- string-length 25))))))
      (let ((path-out (ensure-directories-exist (merge-pathnames pathspec *tmp-dir*))))
        (with-open-file (out path-out
                             :direction :output
                             :if-exists :supersede
                             :element-type 'octet)
          (let ((out (make-flexi-stream out :external-format external-format)))
            (write-sequence octets out)))
        (check (file-equal full-path path-out))
        (with-open-file (out path-out
                             :direction :output
                             :if-exists :supersede
                             :element-type 'octet)
          (let ((out (make-flexi-stream out :external-format external-format)))
            (write-sequence file-string out)))
        (check (file-equal full-path path-out))
        (with-open-file (out path-out
                             :direction :output
                             :if-exists :supersede
                             :element-type 'octet)
          (let ((out (make-flexi-stream out :external-format external-format)))
            (write-sequence file-string out :end 100)
            (write-sequence octets out
                            :start (length (string-to-octets file-string
                                                             :external-format external-format
                                                             :end 100)))))
        (check (file-equal full-path path-out))))))

(defmacro using-values ((&rest values) &body body)
  "Executes BODY and feeds an element from VALUES to the USE-VALUE
restart each time a EXTERNAL-FORMAT-ENCODING-ERROR is signalled.
Signals an error when there are more or less
EXTERNAL-FORMAT-ENCODING-ERRORs than there are elements in VALUES."
  (flex::with-unique-names (value-stack condition-counter)
    `(let ((,value-stack ',values)
	   (,condition-counter 0))
       (handler-bind ((external-format-encoding-error
                       #'(lambda (c)
                           (declare (ignore c)) 
                           (unless ,value-stack
                             (error "Too many encoding errors signalled, expected only ~A."
                                    ,(length values)))
                           (incf ,condition-counter)
                           (use-value (pop ,value-stack)))))
         (prog1 (progn ,@body)
           (when ,value-stack
             (error "~A encoding errors signalled, but ~A were expected."
                    ,condition-counter ,(length values))))))))

(defun accept-overlong (octets code-point)
  "Converts the `overlong' UTF-8 sequence OCTETS to using
OCTETS-TO-STRINGS, accepts the expected error with the corresponding
restart and checks that the result is CODE-POINT."
  (handler-bind ((external-format-encoding-error
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'accept-overlong-sequence))))
    (string= (octets-to-string octets :external-format :utf-8)
             (string (code-char code-point)))))

(defun read-flexi-line (sequence external-format)
  "Creates and returns a string from the octet sequence SEQUENCE using
the external format EXTERNAL-FORMAT."
  (with-input-from-sequence (in sequence)
    (setq in (make-flexi-stream in :external-format external-format))
    (read-line in)))

(defun read-flexi-line* (sequence external-format)
  "Like READ-FLEXI-LINE but uses OCTETS-TO-STRING internally."
  (octets-to-string sequence :external-format external-format))

(defun error-handling-test ()
  "Tests several possible errors and how they are handled."
  (with-test ("Illegal values.")
    (macrolet ((want-encoding-error (input format)
                 `(with-expected-error (external-format-encoding-error)
                    (read-flexi-line* ,input ,format))))
      ;; "overlong"
      (want-encoding-error #(#b11000000 #b10000000) :utf-8)
      (want-encoding-error #(#b11000001 #b10000000) :utf-8)
      (want-encoding-error #(#b11100000 #b10011111 #b10000000) :utf-8)
      (want-encoding-error #(#b11110000 #b10001111 #b10000000 #b10000000) :utf-8)
      (check (accept-overlong #(#b11000000 #b10000000) #b00000000))
      (check (accept-overlong #(#b11000001 #b10000000) #b01000000))
      (check (accept-overlong #(#b11100000 #b10011111 #b10000000) #b011111000000))
      (check (accept-overlong #(#b11110000 #b10001111 #b10000000 #b10000000)
                              #b1111000000000000))
      ;; examples of invalid lead octets
      (want-encoding-error #(#b11111000) :utf-8)
      (want-encoding-error #(#b11111001) :utf-8)
      (want-encoding-error #(#b11111100) :utf-8)
      (want-encoding-error #(#b11111101) :utf-8)
      (want-encoding-error #(#b11111110) :utf-8)
      (want-encoding-error #(#b11111111) :utf-8)
      ;; illegal code points
      (want-encoding-error #(#x00 #x00 #x11 #x00) :utf-32le)
      (want-encoding-error #(#x00 #xd8) :utf-16le)
      (want-encoding-error #(#xff #xdf) :utf-16le)))
  (with-test ("Illegal lengths.")
    (macrolet ((want-encoding-error (input format)
                 `(with-expected-error (external-format-encoding-error)
                    (read-flexi-line* ,input ,format))))                 
      ;; UTF-8 sequences which are too short
      (want-encoding-error #(#xe4 #xf6 #xfc) :utf8)
      (want-encoding-error #(#xc0) :utf8)
      (want-encoding-error #(#xe0 #xff) :utf8)
      (want-encoding-error #(#xf0 #xff #xff) :utf8)
      ;; UTF-16 wants an even number of octets
      (want-encoding-error #(#x01) :utf-16le)
      (want-encoding-error #(#x01 #x01 #x01) :utf-16le)
      (want-encoding-error #(#x01) :utf-16be)
      (want-encoding-error #(#x01 #x01 #x01) :utf-16be)
      ;; another word should follow but it doesn't
      (want-encoding-error #(#x01 #xd8) :utf-16le)
      (want-encoding-error #(#xd8 #x01) :utf-16be)
      ;; UTF-32 always wants four octets
      (want-encoding-error #(#x01) :utf-32le)
      (want-encoding-error #(#x01 #x01) :utf-32le)
      (want-encoding-error #(#x01 #x01 #x01) :utf-32le)
      (want-encoding-error #(#x01 #x01 #x01 #x01 #x01) :utf-32le)
      (want-encoding-error #(#x01) :utf-32be)
      (want-encoding-error #(#x01 #x01) :utf-32be)
      (want-encoding-error #(#x01 #x01 #x01) :utf-32be)
      (want-encoding-error #(#x01 #x01 #x01 #x01 #x01) :utf-32be)))
  (with-test ("Errors while decoding and substitution of characters.")
    ;; handling of EOF in the middle of CRLF
    (check (string= #.(string #\Return)
                    (read-flexi-line `(,(char-code #\Return)) '(:ascii :eol-style :crlf))))
    (let ((*substitution-char* #\?))
      ;; :ASCII doesn't have characters with char codes > 127
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 128 200) :ascii)))
      (check (string= "a??" (read-flexi-line* `#(,(char-code #\a) 128 200) :ascii)))
      ;; :WINDOWS-1253 doesn't have a characters with codes 170 and 210
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253)))
      (check (string= "a??" (read-flexi-line* `#(,(char-code #\a) 170 210) :windows-1253)))
      ;; not a valid UTF-8 sequence
      (check (string= "??" (read-flexi-line '(#xe4 #xf6 #xfc) :utf8))))
    (let ((*substitution-char* nil))
      ;; :ASCII doesn't have characters with char codes > 127
      (check (string= "abc" (using-values (#\b #\c)
                              (read-flexi-line `(,(char-code #\a) 128 200) :ascii))))
      (check (string= "abc" (using-values (#\b #\c)
                              (read-flexi-line* `#(,(char-code #\a) 128 200) :ascii))))
      ;; :WINDOWS-1253 encoding doesn't have a characters with codes 170 and 210
      (check (string= "axy" (using-values (#\x #\y)
                              (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253))))
      (check (string= "axy" (using-values (#\x #\y)
                              (read-flexi-line* `#(,(char-code #\a) 170 210) :windows-1253))))
      ;; not a valid UTF-8 sequence
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line '(#xe4 #xf6 #xfc) :utf8))))
      ;; UTF-8 can't start neither with #b11111110 nor with #b11111111
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line '(#b11111110 #b11111111) :utf8))))
      ;; only one byte
      (check (string= "E" (using-values (#\E) (read-flexi-line '(#x01) :utf-16le))))
      ;; two bytes, but value of resulting word suggests that another word follows
      (check (string= "R" (using-values (#\R) (read-flexi-line '(#x01 #xd8) :utf-16le))))
      ;; the second word must fit into the [#xdc00; #xdfff] interval, but it is #xdbff
      (check (string= "T" (using-values (#\T) (read-flexi-line '(#x01 #xd8 #xff #xdb) :utf-16le))))
      (check (string= "T" (using-values (#\T) (read-flexi-line* #(#x01 #xd8 #xff #xdb) :utf-16le))))
      ;; the same as for little endian above, but using inverse order of bytes in words
      (check (string= "E" (using-values (#\E) (read-flexi-line '(#x01) :utf-16be))))
      (check (string= "R" (using-values (#\R) (read-flexi-line '(#xd8 #x01) :utf-16be))))
      (check (string= "T" (using-values (#\T) (read-flexi-line '(#xd8 #x01 #xdb #xff) :utf-16be))))
      (check (string= "T" (using-values (#\T) (read-flexi-line* #(#xd8 #x01 #xdb #xff) :utf-16be))))
      ;; the only case when errors are signalled for UTF-32 is at end
      ;; of file in the middle of 4-byte sequence, both for big and
      ;; little endian
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01 #x01) :utf-32le))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(,(char-code #\a) #x00 #x00 #x00 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01 #x01) :utf-32be))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(#x00 #x00 #x00 ,(char-code #\a) #x01) :utf-32be)))))))

(defun unread-char-test ()
  "Tests whether UNREAD-CHAR behaves as expected."
  (with-test ("UNREAD-CHAR behaviour.")
    (flet ((test-one-file (file-name external-format)
             (with-open-file (in (merge-pathnames file-name *this-file*)
                                 :element-type 'flex:octet)
               (let ((in (make-flexi-stream in :external-format external-format)))
                 (loop repeat 300
                       for char = (read-char in)
                       do (unread-char char in)
                          (check (char= (read-char in) char)))))))
      (loop for (file-name symbols) in *test-files*
            do (loop for symbol in symbols
                     do (loop for (file-name . external-format) in (create-file-variants file-name symbol)
                              do (test-one-file file-name external-format)))))))

(defun run-tests ()
  "Applies COMPARE-FILES to all test scenarios created with
CREATE-TEST-COMBINATIONS, runs other tests like handling of encoding
errors, shows simple statistics at the end."
  (let* ((*test-success-counter* 0)
         (compare-files-args-list (loop for (file-name symbols) in *test-files*
                                        nconc (create-test-combinations file-name symbols)))
         (no-tests (* 8 (length compare-files-args-list))))
    #+:lispworks
    (setq no-tests (* 2 no-tests))
    (dolist (*copy-function* '(copy-stream copy-stream*))
      (dolist (args compare-files-args-list)
        (apply 'compare-files args)))
    (let ((string-test-args-list (loop for (file-name symbols) in *test-files*
                                       nconc (create-test-combinations file-name symbols t))))
      (incf no-tests (length string-test-args-list))
      (dolist (args string-test-args-list)
        (apply 'string-test args)))
    (let ((read-sequence-test-args-list (loop for (file-name symbols) in *test-files*
                                              nconc (create-test-combinations file-name symbols t))))
      (incf no-tests (length read-sequence-test-args-list))
      (dolist (args read-sequence-test-args-list)
        (apply 'sequence-test args)))
    (incf no-tests 3)
    (error-handling-test)
    (incf no-tests)
    (unread-char-test)
    (format *error-output* "~%~%~:[~A of ~A tests failed..~;~*All ~A tests passed~].~%"
            (= no-tests *test-success-counter*) (- no-tests *test-success-counter*) no-tests)))
            

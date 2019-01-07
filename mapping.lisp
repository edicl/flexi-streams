;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/mapping.lisp,v 1.3 2008/05/25 19:07:53 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :flexi-streams)

(deftype octet ()
  "A shortcut for \(UNSIGNED-BYTE 8)."
  '(unsigned-byte 8))

(deftype char* ()
  "Convenience shortcut to paper over the difference between LispWorks
and the other Lisps."
  #+:lispworks 'lw:simple-char
  #-:lispworks 'character)

(deftype string* ()
  "Convenience shortcut to paper over the difference between LispWorks
and the other Lisps."
  #+:lispworks 'lw:text-string
  #-:lispworks 'string)

(deftype char-code-integer ()
  "The subtype of integers which can be returned by the function CHAR-CODE."
  #-:cmu '(integer 0 #.(1- char-code-limit))
  #+:cmu '(integer 0 65533))

(deftype code-point ()
  "The subtype of integers that's just big enough to hold all Unicode
codepoints.

See for example <http://unicode.org/glossary/#C>."
  '(mod #x110000))

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;;; Genera uses a non-standard character set where the control characters
;;; have codepoints above 127.  Also, (char-code #\Return) == (char-code #\Newline)
;;; Consequently, translation from/to external formats will not work properly
;;; if any of those codepoints is used in the external representation.

#+:genera
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +external-to-internal-codepoints+
  (let ((table (make-hash-table)))
    (setf (gethash #o010 table) #.(char-code #\Backspace))
    (setf (gethash #o011 table) #.(char-code #\Tab))
    (setf (gethash #o012 table) #.(char-code #\Linefeed))
    (setf (gethash #o014 table) #.(char-code #\Page))
    (setf (gethash #o015 table) #.(char-code #\Return))
    (setf (gethash #o177 table) #.(char-code #\Rubout))
    table))
)

#+:genera
(defconstant +internal-to-external-chars+
  (let ((table (make-hash-table)))
    (setf (gethash #\Backspace table) #.(code-char #o010))
    (setf (gethash #\Tab table)       #.(code-char #o011))
    (setf (gethash #\Linefeed table)  #.(code-char #o012))
    (setf (gethash #\Page table)      #.(code-char #o014))
    (setf (gethash #\Return table)    #.(code-char #o015))
    (setf (gethash #\Rubout table)    #.(code-char #o177))
    table))

(defun invert-table (table)
  "`Inverts' an array which maps octets to character codes to a hash
table which maps character codes to octets."
  (let ((hash (make-hash-table)))
    (loop for octet from 0
          for char-code across table
          unless (= char-code 65533)
       do (setf (gethash char-code hash) octet))
    #+:genera (maphash #'(lambda (external-code internal-code)
			   (setf (gethash internal-code hash) external-code))
		       +external-to-internal-codepoints+)
    hash))

(defun make-decoding-table (list)
  "Creates and returns an array which contains the elements in the
list LIST and has an element type that's suitable for character
codes."
  (make-array (length list)
              :element-type 'char-code-integer
              :initial-contents list))

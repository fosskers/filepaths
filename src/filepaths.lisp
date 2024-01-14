(defpackage filepaths
  (:use :cl))

(in-package :filepaths)

(defun existsp (path)
  "Does a PATH exist on the filesystem?")

(defun executablep (path)
  "Is the given PATH an executable file?")

(defun starts-with-p (path base)
  "Is a PATH prefixed by a given BASE?")

(defun ends-with-p (path child)
  "Is the final component of a PATH some given CHILD?")

(defun absolutep (path)
  "Is the given PATH a full, absolute path?"
  (uiop:absolute-pathname-p path))

#+nil
(absolutep #p"/home/colin/foo.txt")
#+nil
(absolutep #p"foo.txt")

(defun relativep (path)
  "Is the given PATH a relative one?"
  (uiop:relative-pathname-p path))

#+nil
(relativep #p"/home/colin/foo.txt")
#+nil
(relativep #p"foo.txt")

(defun directoryp (path)
  "Does the given PATH exist on the file system and point to a directory?")

(defun filep (path)
  "Does the given PATH exist on the file system and point to a normal file?")

(declaim (ftype (function ((or pathname string)) simple-string) base))
(defun base (path)
  "The non-extension, non-directory portion of the filename of a PATH."
  (let ((b (pathname-name path)))
    (if (not b)
        (error 'no-filename :path path)
        b)))

#+nil
(base #p"/foo/bar/baz.txt")
#+nil
(base "/foo/bar/baz.txt")
#+nil
(base #p"/foo/bar/ゆびわ.txt")
#+nil
(base "/foo/bar/")
#+nil
(base 7)

(defun with-base (path new)
  "Swap the base portion of a PATH with a NEW one.")

(declaim (ftype (function ((or pathname string)) simple-string) name))
(defun name (path)
  "The filename of a PATH with no other directory components."
  (let ((n (file-namestring path)))
    (if (= 0 (length n))
        (error 'no-filename :path path)
        n)))

#+nil
(name #p"baz.txt")
#+nil
(name #p"/foo/bar/baz.txt")
#+nil
(name "/foo/bar/baz.txt")
#+nil
(name #p"/foo/bar/ゆびわ.txt")
#+nil
(name #p"/foo/bar/")
#+nil
(name "")
#+nil
(name 7)

(defun with-name (path new)
  "Swap the filename portion of a PATH with a NEW one.")

(defun directory (path)
  "Yield PATH without its final component, if there is one.")

(defun with-directory (path parent)
  "Swap the directory portion of a PATH")

(defun extension (path)
  "The extension of a given PATH.")

(defun with-extension (path ext)
  "Swap the entire extension of a given PATH.")

(defun drop-extension (path)
  "Everything but the extension of a PATH.")

(defun add-extension (path ext)
  "Add an extension to the given path, even if it already has one.")

(defun join (parent child &rest components)
  "Combine two or more components together.")

(defun components (path)
  "Every component of a PATH broken up as a list.")

(defun to-string (path)
  "Convert a PATH object into string.")

(defun from-string (s)
  "Convert a string into a project filepath object.")

;; --- Conditions --- ;;

(define-condition no-filename (error)
  ((path :initarg :path :reader no-filename-path))
  (:documentation "The given path was expected to be a filename, but it may have been a directory
or empty string instead.")
  (:report (lambda (condition stream)
             (format stream "The given path was expected to be a filename: ~a"
                     (no-filename-path condition)))))

(defpackage filepaths
  (:use :cl))

(in-package :filepaths)

(defvar +empty-path+ #p"")
(defvar +filesystem-root+ #p"/")
(defvar +separator+ #\/)

(declaim (ftype (function ((or pathname string)) boolean) rootp))
(defun rootp (path)
  "Is the given PATH the root directory?"
  (or (and (stringp path)
           (string-equal "/" path))
      (and (pathnamep path)
           (equal +filesystem-root+ path))))

#+nil
(rootp #p"/")

(declaim (ftype (function ((or pathname string)) boolean) emptyp))
(defun emptyp (path)
  "Is the given PATH an empty string?"
  (or (and (stringp path)
           (= 0 (length path)))
      (and (pathnamep path)
           (equal +empty-path+ path))))

#+nil
(emptyp #p"")

(defun existsp (path)
  "Does a PATH exist on the filesystem?")

(defun executablep (path)
  "Is the given PATH an executable file?")

(defun starts-with-p (path base)
  "Is a PATH prefixed by a given BASE?")

(defun ends-with-p (path child)
  "Is the final component of a PATH some given CHILD?")

(declaim (ftype (function ((or pathname string)) boolean) absolutep))
(defun absolutep (path)
  "Yields T when the given PATH a full, absolute path."
  (if (pathnamep path)
      (eq :absolute (car (pathname-directory path)))
      (and (< 0 (length path))
           (equal +separator+ (aref path 0)))))

#+nil
(absolutep #p"/home/colin/foo.txt")
#+nil
(absolutep "/home/colin/foo.txt")
#+nil
(absolutep #p"foo.txt")
#+nil
(absolutep #p"")
#+nil
(absolutep "")

(declaim (ftype (function (pathname) boolean) relativep))
(defun relativep (path)
  "Yields T when the given PATH a relative one."
  (not (absolutep path)))

#+nil
(relativep #p"/home/colin/foo.txt")
#+nil
(relativep #p"foo.txt")

(defun directoryp (path)
  "Does the given PATH exist on the file system and point to a directory?")

(declaim (ftype (function ((or pathname string)) boolean) directory-formatted-p))
(defun directory-formatted-p (path)
  "Yields non-nil if the PATH ends in a path separator."
  (if (pathnamep path)
      (and (not (null (pathname-directory path)))
           (not (pathname-name path)))
      (equal +separator+ (char path (1- (length path))))))

#+nil
(directory-formatted-p #p"/foo/bar/")
#+nil
(directory-formatted-p #p"/foo/bar/baz.txt")
#+nil
(directory-formatted-p "/foo/bar/")
#+nil
(directory-formatted-p "/foo/bar/baz.txt")

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

(declaim (ftype (function ((or pathname string)) pathname) parent))
(defun parent (path)
  "Yield PATH without its final component, if there is one."
  (cond ((emptyp path) (error 'empty-path))
        ((rootp path)  (error 'root-no-parent))
        (t (let* ((s (to-string path))
                  (path (if (directory-formatted-p s)
                            (string-right-trim "/" s)
                            s)))
             (from-string (directory-namestring path))))))

#+nil
(parent #p"/foo/bar/baz.txt")
#+nil
(parent #p"/foo/bar/")
#+nil
(parent #p"/foo/")
#+nil
(parent #p"/")
#+nil
(parent "")

#+nil
(cdr (pathname-directory #p"/foo/bar/"))

(defun with-parent (path parent)
  "Swap the parent portion of a PATH")

(declaim (ftype (function ((or pathname string)) (or simple-string null)) extension))
(defun extension (path)
  "The extension of a given PATH."
  (pathname-type path))

#+nil
(extension #p"/foo/bar.json")
#+nil
(extension #p"/")

(defun with-extension (path ext)
  "Swap the entire extension of a given PATH.")

(defun drop-extension (path)
  "Everything but the extension of a PATH.")

(defun add-extension (path ext)
  "Add an extension to the given path, even if it already has one.")

(defun join (parent child &rest components)
  "Combine two or more components together."
  (let* ((combined   (cons child components))
         (final      (car (last combined)))
         (rest       (butlast combined))
         (abs-or-rel (if (absolutep parent) :absolute :relative)))
    (make-pathname :name (base final)
                   :type (extension final)
                   :directory (cons abs-or-rel
                                    (append (components parent)
                                            rest)))))

#+nil
(join "/foo" "bar" "baz" "test.json")
#+nil
(join #p"/bar/baz/" #p"foo.json")
#+nil
(join #p"/bar/baz" #p"foo.json")

(declaim (ftype (function ((or pathname string)) list) components))
(defun components (path)
  "Every component of a PATH broken up as a list."
  (if (directory-formatted-p path)
      (cdr (pathname-directory path))
      (let* ((ext  (extension path))
             (file (if ext (concatenate 'string (base path) "." ext) (base path))))
        (append (cdr (pathname-directory path))
                (list file)))))

#+nil
(components #p"/foo/bar/baz.json")
#+nil
(components "/foo/bar/baz.json")

(declaim (ftype (function (pathname) simple-string) to-string))
(defun to-string (path)
  "Convert a PATH object into string."
  (namestring path))

#+nil
(to-string #p"/foo/bar/baz.txt")

(declaim (ftype (function (string) pathname) from-string))
(defun from-string (s)
  "Convert a string into a proper filepath object."
  (pathname s))

#+nil
(from-string "/foo/bar/baz.txt")

;; --- Conditions --- ;;

(define-condition no-filename (error)
  ((path :initarg :path :reader no-filename-path))
  (:documentation "The given path was expected to be a filename, but it may have been a directory
or empty string instead.")
  (:report (lambda (condition stream)
             (format stream "The given path was expected to be a filename: ~a"
                     (no-filename-path condition)))))

(define-condition empty-path (error)
  ()
  (:documentation "A non-empty path was expected, but an empty one was given.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Empty path given where a concrete one was expected."))))

(define-condition root-no-parent (error)
  ()
  (:documentation "The filesystem root has no parent.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "The filesystem root has no parent."))))

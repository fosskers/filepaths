(defpackage filepaths
  (:use :cl)
  ;; --- Structural tests --- ;;
  (:export #:rootp #:emptyp
           #:starts-with-p #:ends-with-p
           #:absolutep #:relativep
           #:directoryp)
  ;; --- Construction --- ;;
  (:export #:join)
  ;; --- Component Access --- ;;
  (:export #:base #:with-base
           #:name #:with-name
           #:parent #:with-parent
           #:extension #:with-extension #:drop-extension #:add-extension)
  ;; --- Conversion --- ;;
  (:export #:components #:from-list
           #:ensure-directory #:ensure-string #:ensure-path
           #:to-string #:from-string)
  ;; --- Conditions --- ;;
  (:export #:no-filename
           #:empty-path
           #:root-no-parent)
  (:documentation "Modern and consistent filepath manipulation."))

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

(defun starts-with-p (path base)
  "Are the initial components of a PATH some BASE?"
  (let ((bools (mapcar #'equal (components path) (components base))))
    (reduce (lambda (a b) (and a b)) bools :initial-value t)))

#+nil
(starts-with-p #p"/foo/bar/baz/zing.json" "/foo/bar")

(defun ends-with-p (path child)
  "Are the final components of a PATH some given CHILD?"
  (let ((bools (mapcar #'equal
                       (reverse (components path))
                       (reverse (components child)))))
    (reduce (lambda (a b) (and a b)) bools :initial-value t)))

#+nil
(ends-with-p #p"/foo/bar/baz/zing.json" "baz/zing.json")

(declaim (ftype (function ((or pathname string)) boolean) absolutep))
(defun absolutep (path)
  "Yields T when the given PATH is a full, absolute path."
  (if (pathnamep path)
      (eq :absolute (car (pathname-directory path)))
      (and (< 0 (length path))
           (equal +separator+ (char path 0)))))

#+nil
(absolutep "/home/colin/foo.txt")

(declaim (ftype (function ((or pathname string)) boolean) relativep))
(defun relativep (path)
  "Yields T when the given PATH is a relative one."
  (not (absolutep path)))

#+nil
(relativep #p"/home/colin/foo.txt")
#+nil
(relativep #p"foo.txt")

(declaim (ftype (function ((or pathname string)) boolean) directoryp))
(defun directoryp (path)
  "Yields T if the PATH represents a directory.

Note that this only checks the formatting of the path, and does not query the
filesystem."
  (if (pathnamep path)
      (and (not (null (pathname-directory path)))
           (not (pathname-name path)))
      (equal +separator+ (char path (1- (length path))))))

#+nil
(directoryp "/foo/bar/")
#+nil
(directoryp "/foo/bar/baz.txt")

(declaim (ftype (function ((or pathname string)) simple-string) base))
(defun base (path)
  "The non-extension, non-directory portion of the filename of a PATH."
  (let ((b (pathname-name path)))
    (if (not b)
        (error 'no-filename :path path)
        (string-if-keyword-impl-specific b))))

#+nil
(base "/foo/bar/baz.txt")
#+nil
(base #p"/foo/bar/ゆびわ.txt")

(declaim (ftype (function ((or pathname string) string) pathname) with-base))
(defun with-base (path new)
  "Swap the base portion of a PATH with a NEW one. Yields a new path object."
  (let ((path (ensure-path path)))
    (make-pathname :name new
                   :type (pathname-type path)
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(with-base #p"/foo/bar/baz.txt" "jack")

(declaim (ftype (function ((or pathname string)) simple-string) name))
(defun name (path)
  "The filename of a PATH with no other directory components."
  (let ((n (file-namestring path)))
    (if (= 0 (length n))
        (error 'no-filename :path path)
        n)))

#+nil
(name "/foo/bar/baz.txt")

(declaim (ftype (function ((or pathname string) (or pathname string)) pathname) with-name))
(defun with-name (path new)
  "Swap the filename portion of a PATH with a NEW one. Yields a new path object."
  (let ((path (ensure-path path)))
    (make-pathname :name (base new)
                   :type (extension new)
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(with-name #p"/foo/bar/baz.txt" "jack.json")

(declaim (ftype (function ((or pathname string)) pathname) parent))
(defun parent (path)
  "Yield PATH without its final component, if there is one."
  (cond ((emptyp path) (error 'empty-path))
        ((rootp path)  (error 'root-no-parent))
        (t (let* ((s (ensure-string path))
                  (path (if (directoryp s)
                            (string-right-trim "/" s)
                            s)))
             (from-string (directory-namestring path))))))

#+nil
(parent "/foo/bar/baz.txt")
#+nil
(parent "/foo/bar/")
#+nil
(parent "/foo/")

(declaim (ftype (function ((or pathname string) (or pathname string)) pathname) with-parent))
(defun with-parent (path parent)
  "Swap the parent portion of a PATH."
  (join parent (name path)))

#+nil
(with-parent #p"/foo/bar/baz.json" #p"/zing")

(declaim (ftype (function ((or pathname string)) (or simple-string null)) extension))
(defun extension (path)
  "The extension of a given PATH."
  (pathname-type path))

#+nil
(extension #p"/foo/bar.json")
#+nil
(extension #p"/")

(declaim (ftype (function ((or pathname string) string) pathname) with-extension))
(defun with-extension (path ext)
  "Swap the entire extension of a given PATH. Yields a new path object."
  (let ((path (ensure-path path)))
    (if (directoryp path)
        (error 'no-filename :path path)
        (make-pathname :name (base path)
                       :type ext
                       :directory (pathname-directory path)
                       :version :newest))))

#+nil
(with-extension #p"/foo/bar/baz.txt" "json")
#+nil
(with-extension #p"/foo/bar/" "json")

(declaim (ftype (function ((or pathname string)) pathname) drop-extension))
(defun drop-extension (path)
  "Remove an extension from a PATH."
  (let* ((path (ensure-path path))
         (stem (base path))
         (ext  (extension stem))
         (name (if ext (base stem) stem)))
    ;; Similar to `add-extension', there's some cleverness here where we need to
    ;; check if we must move an "inner" extension back outward. For instance, in
    ;; the case of foo.json.zip. It looks like we're always setting `:type' to
    ;; something concrete, but this will properly be NIL in the normal case
    ;; where only one extension was present.
    (make-pathname :name name
                   :type ext
                   :directory (pathname-directory path)
                   :version :newest)))

#+nil
(drop-extension #p"/foo/bar/baz.json")
#+nil
(drop-extension #p"/foo/bar/baz.json.zip")

(declaim (ftype (function ((or pathname string) string) pathname) add-extension))
(defun add-extension (path ext)
  "Add an extension to the given path, even if it already has one."
  (let* ((path    (ensure-path path))
         (already (extension path)))
    (if already
        ;; The pathname type only wants a single extension present in the
        ;; `:type' field, or else there is strange behaviour elsewhere (for
        ;; instance involving `to-string'). The old extension must thus become
        ;; part of the stem, and the only value reported by `extension' is the
        ;; new one, not the composite. This behaviour reflects that of Rust's
        ;; standard library.
        (make-pathname :name (concatenate 'string (base path) "." already)
                       :type ext
                       :directory (pathname-directory path)
                       :version :newest)
        (with-extension path ext))))

#+nil
(add-extension #p"/foo/bar/baz.txt" "zip")

(declaim (ftype (function ((or pathname string) (or pathname string) &rest (or pathname string)) pathname) join))
(defun join (parent child &rest components)
  "Combine two or more components together."
  (let* ((parent     (ensure-path parent))
         (combined   (remove-if (lambda (s)
                                  (or (string-equal +separator+ s)
                                      (string-equal "" s)))
                                (mapcan #'components (cons child components))))
         (final      (car (last combined)))
         (rest       (butlast combined))
         (abs-or-rel (if (absolutep parent) :absolute :relative))
         (par-comps  (components parent))
         (final-base (base final)))
    (make-pathname :name (cond
                           #+sbcl
                           ((string-equal "**" final-base) (sbcl-wildcard))
                           #+(or abcl ccl allegro)
                           ((string-equal "**" final-base) final-base)
                           (t (keyword-if-special final-base)))
                   :type (extension final)
                   :version :newest
                   :directory (cons abs-or-rel
                                    (mapcar #'keyword-if-special
                                            (append (if (absolutep parent)
                                                        (cdr par-comps)
                                                        par-comps)
                                                    rest))))))

#+nil
(join "/" "foo" "bar" ".." "." ".." "baz" "stuff.json")
#+nil
#p"/foo/bar/.././../baz/stuff.json"

#+nil
(join "/foo" "bar" "**" "*.json")

#+nil
(join "/foo" "bar" "**.json")

#+nil
(join "/foo" "bar" "baz" "test.json")
#+nil
(join "/foo/bar" "baz/test.json")

(declaim (ftype (function ((or pathname string)) list) components))
(defun components (path)
  "Every component of a PATH broken up as a list."
  (cond ((emptyp path) '())
        ;; HACK 2024-06-17 Until ECL/Clasp support `**' in `:name' position.
        ;;
        ;; And not even a good hack, since it can be broken in cases where the
        ;; `**' comes at the end.
        #+(or ecl clasp)
        ((and (stringp path) (string-equal "**" path)) '("**"))
        (t (let* ((path (ensure-path path))
                  (comp (mapcar #'string-if-keyword (cdr (pathname-directory path))))
                  (list (if (directoryp path)
                            comp
                            (let* ((ext  (extension path))
                                   (file (if ext (concatenate 'string (base path) "." ext) (base path))))
                              (append comp (list file))))))
             (if (absolutep path)
                 (cons "/" list)
                 list)))))

#+nil
(components "/foo/bar/baz.json")
#+nil
(components "/foo/bar/.././../baz/stuff.json")

(declaim (ftype (function (list) pathname) from-list))
(defun from-list (list)
  "Given a LIST of path components, construct a proper pathname object."
  (if (null list)
      #p""
      (destructuring-bind (parent &rest rest) list
        (if (null rest)
            (ensure-path parent)
            (apply #'join parent (car rest) (cdr rest))))))

#+nil
(from-list '("foo" "bar" "baz"))

(declaim (ftype (function ((or pathname string)) pathname) ensure-directory))
(defun ensure-directory (path)
  "If a given PATH doesn't end in a path separator, add one."
  (let ((path (ensure-path path)))
    (if (directoryp path)
        path
        (make-pathname :name nil
                       :type nil
                       :directory (append (pathname-directory path)
                                          (list (name path)))))))

#+nil
(ensure-directory #p"/foo/bar/baz")

(declaim (ftype (function ((or pathname string)) simple-string) ensure-string))
(defun ensure-string (path)
  "A PATH is definitely a string after this."
  (if (pathnamep path) (to-string path) path))

(declaim (ftype (function (pathname) simple-string) to-string))
(defun to-string (path)
  "Convert a PATH object into string."
  (namestring path))

(declaim (ftype (function ((or pathname string)) pathname) ensure-path))
(defun ensure-path (path)
  "A PATH is definitely a pathname after this."
  (if (pathnamep path) path (from-string path)))

(declaim (ftype (function (string) pathname) from-string))
(defun from-string (s)
  "Convert a string into a proper filepath object."
  (pathname s))

#+nil
(join "/" "foo" "bar" ".." "." ".." "baz" "stuff.json")

#+nil
(pathname "/foo/bar/.././../baz/test.json")

#+nil
(pathname "/foo/bar/../baz/test.json")

#+nil
#p"/foo/bar/.././../baz/test.json"

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

;; --- Utilities --- ;;

(declaim (ftype (function (t) string) string-if-keyword-impl-specific))
(defun string-if-keyword-impl-specific (item)
  "Like `string-if-keyword' but with special consideration for implementations.
Assumed to be used internally within `base' to account for when the file base is
a wildcard character."
  (cond
    #+sbcl
    ((sb-impl::pattern-p item) "**") ; FIXME 2024-06-16 Actually check the contents.
    (t (string-if-keyword item))))

(declaim (ftype (function ((or string keyword)) string) string-if-keyword))
(defun string-if-keyword (item)
  "There are certain keywords that represent special path components. These need to
be converted back into their original string representations if present."
  (cond ((or (eq item :up) (eq item :back)) "..")
        ((eq item :wild) "*")
        ((eq item :wild-inferiors) "**")
        (t item)))

#+nil
(string-if-keyword :up)

(declaim (ftype (function (string) (or string keyword)) keyword-if-special))
(defun keyword-if-special (item)
  "Like `string-if-keyword', certain strings need to be converted to keywords
before being stored in the `:directory' portion of a pathname."
  (cond ((string-equal ".." item) :up)
        ((string-equal "**" item) :wild-inferiors)
        ((string-equal "*" item)  :wild)
        (t item)))

#+sbcl
(defun sbcl-wildcard ()
  "A SBCL-specific pattern type created when a ** appears in a path."
  (sb-impl::make-pattern '(:multi-char-wild :multi-char-wild)))

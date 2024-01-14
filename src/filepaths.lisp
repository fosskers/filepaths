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
  "Is the given PATH a full, absolute path?")

(defun relativep (path)
  "Is the given PATH a relative one?")

(defun directoryp (path)
  "Does the given PATH exist on the file system and point to a directory?")

(defun filep (path)
  "Does the given PATH exist on the file system and point to a normal file?")

(defun base (path)
  "The non-extension, non-directory portion of the filename of a PATH.")

(defun with-base (path new)
  "Swap the base portion of a PATH with a NEW one.")

(defun name (path)
  "The filename of a PATH with no other directory components.")

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

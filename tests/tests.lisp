(defpackage filepaths/tests
  (:use :cl :parachute)
  (:local-nicknames (:p :filepaths)))

(in-package :filepaths/tests)

(define-test suite)

(define-test "Structural Tests"
  :parent suite
  (true  (p:rootp "/"))
  (false (p:rootp "/foo"))
  (true  (p:emptyp ""))
  (false (p:emptyp "/foo"))
  (true  (p:absolutep "/home/colin/foo.txt"))
  (false (p:absolutep "colin/foo.txt"))
  (true  (p:absolutep "/"))
  (false (p:absolutep ""))
  (false (p:relativep "/home/colin/foo.txt"))
  (true  (p:relativep "foo.txt"))
  (true  (p:starts-with-p "/foo/bar/baz/zing.json" "/foo/bar"))
  (true  (p:ends-with-p "/foo/bar/baz/zing.json" "baz/zing.json"))
  (true  (p:directoryp "/foo/bar/"))
  (true  (p:directoryp #p"/foo/bar/"))
  (false (p:directoryp "/foo/bar/baz.txt"))
  (false (p:directoryp #p"/foo/bar/baz.txt")))

(define-test "Construction"
  :parent suite
  (is equal #p"/foo/bar/baz/test.json" (p:join "/foo" "bar" "baz" "test.json"))
  (is equal #p"/bar/baz/foo.json" (p:join #p"/bar/baz/" #p"foo.json"))
  (is equal #p"/bar/baz/foo.json" (p:join #p"/bar/baz" #p"foo.json"))
  (is equal #p"/foo/bar/baz/test.json" (p:join "/foo" "" "bar" "/" "baz" "test.json"))
  (is equal #p"/bar/baz/test.json" (p:join "/"  "bar" "baz" "test.json"))
  (is equal #p"/foo/bar/baz/test.json" (p:join "/foo/bar" "baz/test.json"))
  ;; Last component is a directory.
  ;; https://github.com/fosskers/filepaths/issues/2
  (is equal #p"foo/bar" (p:join "foo" "bar"))
  (is equal #p"foo/bar/" (p:join "foo" "bar/"))
  (is equal #p"foo/bar/baz/" (p:join "foo" "bar/baz/"))
  ;; Naughty under CCL and Allegro.
  #-(or allegro ccl abcl)
  (is equal #p"/foo/bar/.././../baz/stuff.json" (p:join "/" "foo" "bar" ".." "." ".." "baz" "stuff.json"))
  ;; Degenerate cases
  (is equal #p"/foo" (p:join "/foo"))
  (is equal #p"/foo" (p:join "/foo" "/"))
  (is equal #p"/foo" (p:join "/foo" "")))

(define-test "Wild Cards"
  :parent suite
  (is equal #p"/foo/*.*" (p:join "/foo" "*.*"))
  #-(or clasp ecl)
  (is equal #p"/foo/**.json" (p:join "/foo" "**.json"))
  (is equal #p"/foo/**/*.json" (p:join "/foo" "**" "*.json"))
  (isnt equal #p"/foo/bar/*" #p"/foo/bar/*/")
  (is equal #p"/foo/bar/*/" (p:ensure-directory #p"/foo/bar/*"))
  #-ecl
  (is equal #p"/foo/bar/**/" (p:ensure-directory #p"/foo/bar/**")))

(define-test "Component Access"
  :parent suite
  (is equal "baz" (p:base "/foo/bar/baz.txt"))
  (is equal "ゆびわ" (p:base #p"/foo/bar/ゆびわ.txt"))
  (is equal "baz.txt" (p:base "/foo/bar/baz.txt.zip"))
  (is equal "*" (p:base "/foo/bar/*.zip"))
  #-(or clasp ecl)
  (is equal "**" (p:base "/foo/bar/**.zip"))
  (fail (p:base "/foo/bar/"))
  (is equal #p"/foo/bar/jack.txt" (p:with-base "/foo/bar/baz.txt" "jack"))
  (is equal "baz.txt" (p:name "baz.txt"))
  (is equal "baz.txt" (p:name "/foo/bar/baz.txt"))
  (fail (p:name "/foo/bar/"))
  (fail (p:name ""))
  (is equal #p"/foo/bar/jack.json" (p:with-name "/foo/bar/baz.txt" "jack.json"))
  (is equal #p"/foo/bar/" (p:parent "/foo/bar/baz.txt"))
  (is equal #p"/foo/" (p:parent "/foo/bar/"))
  (is equal #p"/" (p:parent "/foo/"))
  (fail (p:parent "/"))
  (fail (p:parent ""))
  (is equal #p"/zing/baz.json" (p:with-parent "/foo/bar/baz.json" "/zing"))
  (is equal "json" (p:extension "/foo/bar.json"))
  (is equal :wild (p:extension "*.*"))
  (false (p:extension "/"))
  (is equal #p"/foo/bar/baz.json" (p:with-extension "/foo/bar/baz.txt" "json"))
  (fail (p:with-extension "/foo/bar/" "json"))
  (is equal #p"/foo/bar/baz" (p:drop-extension #p"/foo/bar/baz.json"))
  (is equal #p"/foo/bar/baz.json" (p:drop-extension #p"/foo/bar/baz.json.zip"))
  (is equal #p"/foo/bar/baz.txt.zip" (p:add-extension "/foo/bar/baz.txt" "zip"))
  (is equal #p"/foo/bar/baz.txt" (p:add-extension "/foo/bar/baz" "txt"))
  (fail (p:add-extension "/foo/bar/" "txt")))

(define-test "Conversion"
  :parent suite
  (is equal '("/" "foo" "bar" "baz.json") (p:components "/foo/bar/baz.json"))
  (is equal '("foo" "bar" "baz.json") (p:components "foo/bar/baz.json"))
  (is equal '("/") (p:components "/"))
  (is equal '() (p:components ""))
  (is equal '(".") (p:components "."))
  #-(or cmucl allegro)
  (is equal '("foo" ".") (p:components "foo/."))
  (is equal '("/" ".") (p:components "/."))
  (is equal #p"" (p:from-list '()))
  (is equal #p"foo" (p:from-list '("foo")))
  (is equal #p"foo/bar/baz" (p:from-list '("foo" "bar" "baz")))
  (let ((path #p"/foo/bar/baz/file.txt"))
    (is equal path (p:from-list (p:components path))))
  (let ((path #p"/foo/bar/.././../baz/stuff.json"))
    (is equal path (p:from-list (p:components path))))
  (let ((path #p"/foo/bar/baz/"))
    (is equal path (p:ensure-directory path)))
  (is equal #p"foobar/" (p:ensure-directory "foobar"))
  (is equal #p"/foo/bar/baz/" (p:ensure-directory "/foo/bar/baz"))
  (is equal #p"/foo/bar/baz.json/" (p:ensure-directory "/foo/bar/baz.json"))
  (of-type string (p:ensure-string #p"/foo"))
  (of-type pathname (p:ensure-path "/foo")))

#+win32
(define-test "Windows"
  :parent suite
  (is equal #p"Z:/foo/bar" (p:join #p"Z:/foo" "bar"))
  (is equal #p"Z:/foo/bar.json" (p:with-extension #p"Z:/foo/bar.txt" "json"))
  (is equal #p"Z:/foo/bar" (p:drop-extension #p"Z:/foo/bar.txt"))
  (is equal #p"Z:/foo/bar.tar.gz" (p:add-extension #p"Z:/foo/bar.tar" "gz"))
  (is equal #p"Z:/zing/baz.json" (p:with-parent #p"C:/foo/bar/baz.json" #p"Z:/zing"))
  (is equal #p"Z:/foo/bar/jack.json" (p:with-name #p"Z:/foo/bar/baz.txt" "jack.json"))
  (is equal #p"Z:/foo/bar/jack.txt" (p:with-base #p"Z:/foo/bar/baz.txt" "jack"))
  (is equal #p"Z:/foo/bar/baz/" (p:ensure-directory #p"Z:/foo/bar/baz")))

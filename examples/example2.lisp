(load "src/load.lisp")

(defmacro make-assertions (&body assertions)
  `(progn ,@(mapcar #'(lambda (x) `(assert ,x))
		    assertions)))

(def-class person
  name
  age)

(def-class researcher
  group)

(def-class (student person)
  course)

(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))

(def-class (phd-student ist-student researcher)
    thesis)

(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
  (make-assertions
   (equal (person-name s) "Paul")
   (equal (student-course s) "Informatics")))

(let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
  (make-assertions
   (ist-student? m)
   (student? m)
   (sportsman? m)
   (equal (ist-student-name m) "Maria")
   (equal (person-name m) "Maria")
   (equal (sportsman-activity m) "Tennis")
   (equal (ist-student-activity m) "Tennis")))

(let ((b (make-phd-student :name "Brian" :age 28 :course "Informatics" :activity "Soccer" :group "ESW" :thesis "Code Migration")))
  (make-assertions
   (researcher? b)
   (person? b)
   (student? b)
   (sportsman? b)
   (phd-student? b)
   (equal (phd-student-thesis b) "Code Migration")
   (equal (student-name b) "Brian")
   (equal (phd-student-group b) "ESW")
   (equal (phd-student-name b) "Brian")))

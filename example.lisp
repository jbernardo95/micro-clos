(load "src/load.lisp")

; Class Definition
(def-class person
  name
  age)

(let ((p (make-person :name "Paulo" :age 33)))
    (person-age p))

(let ((a (make-person :name "Paulo" :age 33))
        (b "I am not a person"))
    (list (person? a) (person? b)))

; Inheritance
(def-class (student person)
  course)

(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
    (list (student-name s) (student-course s)))

; Subtyping
(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
    (list (person-name s) (student-course s)))

(let ((p (make-person :name "John" :age 34))
        (s (make-student :name "Paul" :age 21 :course "Informatics")))
    (list (person? p) (student? p) (person? s) (student? s)))

; Multiple Inheritance
(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))

(let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
           (list (ist-student? m)
                 (student? m)
                 (sportsman? m)
                 (ist-student-name m)
                 (person-name m)
                 (sportsman-activity m)
                 (ist-student-activity m)))

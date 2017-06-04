# micro-clos 

This is a very basic implementation of a class system in Common Lisp.

This project was developed as a project of a programming course.

## Example

```
(def-class person
  name
  age)

(def-class (student person)
  course)

(let ((person (make-person :name "Person Name" :age 21)))
  (person? person) ; T
  (person-name person) ; "Person Name"
  (person-age person) ; 21
  (set-person-age person 35)) ; 35

(make-student :name "Persone Name" :age 21 :course "Programming")

(let ((student (make-student :name "Student Name" :age 21 :course "Computer Science")))
  (person? student) ; T
  (student? student) ; T
  (student-course student) ; "Computer Science"
  (person-age student) ; 21
  (set-student-course student "Biology") ; "Biology"
  (set-person-name student "John")) ; "John"
```

## Up and running 

This project was developed in SBCL version 1.3.18.

```
$ sbcl

* (load "src/load.lisp")

* ; Play with it
```

## License

micro-clos source code is licensed under the MIT License.

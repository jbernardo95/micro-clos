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
  (person? person) 
  (person-name person)
  (person-age person)
  (set-person-age person 35))

(make-student :name "Persone Name" :age 21 :course "Programming")

(let ((student (make-student :name "Student Name" :age 21 :course "Computer Science")))
  (person? student) 
  (student? student) 
  (student-course student)
  (person-age student)
  (set-student-course student "Biology")
  (set-person-name student "John"))
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

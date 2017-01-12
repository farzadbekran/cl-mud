;;;; cl-mud.asd

(asdf:defsystem #:cl-mud
    :description "Describe cl-mud here"
    :author "Your Name <your.name@example.com>"
    :license "Specify license here"
    :serial t
    :depends-on (#:usocket #:cl-cffi-gtk #:chanl)
    :components ((:file "package")
                 (:file "cl-mud")))


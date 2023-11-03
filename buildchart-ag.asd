;;; buildchart-ag.asd

;;; SPDX-FileCopyrightText: (C) 2023 Anthony Green <green@redhat.com>
;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:buildchart-ag
  :description "Generate interesting charts from CI/CD and infrastructure logs."
  :author "Anthony Green <green@redhat.com>"
  :license "MIT"
  :version "1"
  :serial t
  :components ((:file "package")
               (:file "buildchart-ag")
               (:file "event")
               (:file "task")
               (:file "draw")
               (:file "process"))
  :depends-on (:unix-opts :cl-json :local-time :cl-ppcre :cl-cairo2 :parse-float :str :cl-colors :plump :lquery)
  :build-operation "program-op"
  :build-pathname "buildchart-ag"
  :entry-point "buildchart-ag:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

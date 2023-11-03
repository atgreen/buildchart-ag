buildchart-atg: *.lisp *.asd Makefile
	sbcl --dynamic-space-size 2048 --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :buildchart-ag) (sb-ext:quit))"

clean:
	-rm -rf buildchart-ag .*~ *~ systems

clean:
	rm -f *~ *.wx32fsl *.fasl

run:
	sbcl --eval "(asdf:load-system :ripple)" --non-interactive

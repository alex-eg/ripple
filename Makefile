clean:
	rm -f *~ *.wx32fsl *.fasl

run:
	sbcl --eval "(asdf:operate 'asdf:load-op 'ripple)" --non-interactive

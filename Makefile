.PHONY: render test clean

render:
	dune exec ./bin/raytracer.exe > scene.ppm

test:
	dune runtest

clean:
	dune clean
	for dir in . bin lib test; do \
	  rm -f $$dir/*~ $$dir/\#*; \
	done
	rm -f scene.ppm

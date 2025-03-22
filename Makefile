.PHONY: render clean

render:
	dune exec ./bin/raytracer.exe > scene.ppm

clean:
	dune clean
	for dir in . bin lib test; do \
	  rm -f $$dir/*~ $$dir/\#*; \
	done
	rm -f scene.ppm

test-%:
	guile test/$*.scm
clean:
	rm -fr target/*

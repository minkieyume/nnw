TARGET_DIR = target

test-%:
	guile test/$*.scm

# Makefile for building devops-helper with SBCL + Quicklisp + ASDF

# Adjust this if Quicklisp is installed somewhere else:
QL := $(HOME)/quicklisp/setup.lisp

# Name of the resulting binary:
TARGET := devops-helper

# Default rule:
all: build

# Build rule:
build: $(TARGET)

$(TARGET):
	sbcl --no-sysinit --no-userinit \
	     --load "$(QL)" \
	     --eval '(ql:quickload :devops-helper)' \
	     --eval '(asdf:make :devops-helper)' \
	     --quit

# Clean rule (remove the executable and any leftover FASLs):
clean:
	rm -f $(TARGET)
	find . -type f -name '*.fasl' -delete

rebuild: clean build

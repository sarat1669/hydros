# A makefile that visits each subdirectory in this directory and runs make
# inside it.
#
# The idea here is that you can add new unikernels to your source tree
# just by creating a directory and a C file with a kmain() in it. Optionally,
# a user created Makefile can be used. If this is not desirable however, a
# generic make script is used that takes all C files and compiles them into
# a kernel named the same as the directory it is found in it.

SHELL=/bin/bash

.PHONY = all

# TODO: Make this much less gross.

all:
	find . -mindepth 2 -type f -name '*Makefile*' | sed -r 's|/[^/]+$$||' | sort \
	   | uniq | while read dir ; do make -C $$dir -f ../../utils/build_generic.mk ; done
	find . -type d '!' -exec test -e "{}/Makefile" ';' -print | while read dir \
		; do make -C $$dir -f ../../utils/build_generic.mk ; done

clean:
	find . -mindepth 2 -type f -name '*Makefile*' | sed -r 's|/[^/]+$$||' | sort \
	   | uniq | while read dir ; do make -C $$dir -f ../../utils/build_generic.mk clean ; done
	find . -type d '!' -exec test -e "{}/Makefile" ';' -print | while read dir \
		; do make -C $$dir -f ../../utils/build_generic.mk clean; done

# Radoslav (radoslav@math.cornell.edu)
# last modified: 10-29-14

.PHONY: all stable dev test genpdf

# absolute paths to packages and binary on my mac machine

pkg-1.6 = /Applications/Macaulay2-1.6/share/Macaulay2
pkg-dev = /Git/m2-main/M2/Macaulay2/packages

M2-1.6 = /Applications/Macaulay2-1.6/bin/M2
M2-dev = /Git/m2-main/M2/BUILD/rado/builds.tmp/darwin64/M2

file = SingularExamples.m2

## the actual makefile

all:

stable:
	cp $(file) $(pkg-1.6)
	$(M2-1.6) --script `pwd`/dev/updater.m2

dev:
	cp $(file) $(pkg-dev)
	$(M2-dev) --script `pwd`/dev/updater.m2

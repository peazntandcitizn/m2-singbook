# Radoslav (radoslav@math.cornell.edu)
# last modified: 09-11-14

.PHONY: all stable dev test genpdf

# absolute paths to packages and binary on my mac machine

pkg-1.6 = /Applications/Macaulay2-1.6/share/Macaulay2
pkg-dev = /Git/M2-repo/M2/Macaulay2/packages

M2-1.6 = /Applications/Macaulay2-1.6/bin/M2
M2-dev = /Git/M2-repo/M2/BUILD/rado/builds.tmp/darwin64/M2

file = RadoslavTest.m2

## the actual makefile

all:
#all: stable dev

stable:
	cp $(file) $(pkg-1.6)
	$(M2-1.6) --script `pwd`/updater.m2

dev:
	cp $(file) $(pkg-dev)
	$(M2-dev) --script `pwd`/code/updater.m2

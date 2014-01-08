
# Vacuole.js: Vacuole.hs Vacuole/Snap.ffi.js
# 	hastec $< --with-js=Vacuole/Snap.ffi.js

all: Main.js

Vacuole/Snap.hs Vacuole/Snap.ffi.js: Vacuole/Snap.ffi.hs
	haste-ffi-parser -i $< -j Vacuole/Snap.ffi.js -o Vacuole/Snap.hs



static.js: jquery.js jquery.console.uno.js d3.js snap.svg.js
	cat $^ > $@

static.min.js: static.js
	closure-compiler $< > $@


Main.js: Main.hs Vacuole/Snap.ffi.js
	hastec $< --with-js=Vacuole/Snap.ffi.js --start=asap

.PHONY: Main.js


Main.min.js: Main.js
	closure-compiler $< > $@



lib.js: jquery.js jquery.console.uno.js d3.js snap.svg.js \
		Vacuole/Snap.ffi.js snap.js Main.js
	cat $^ > $@

lib.min.js: lib.js
	closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS $< > $@





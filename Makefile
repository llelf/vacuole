
# Vacuole.js: Vacuole.hs Vacuole/Snap.ffi.js
# 	hastec $< --with-js=Vacuole/Snap.ffi.js

Vacuole/Snap.hs Vacuole/Snap.ffi.js: Vacuole/Snap.ffi.hs
	haste-ffi-parser -i $< -j Vacuole/Snap.ffi.js -o Vacuole/Snap.hs



static.js: jquery.js jquery.console.uno.js d3.js snap.svg.js
	cat $^ > $@

static.min.js: static.js
	closure-compiler $< > $@



native: jbuild
	jbuilder build links.exe -j 4
	cp _build/default/links.exe ./links

nc: native

clean: jbuild
	jbuilder clean
	rm -rf ./links

.PHONY: native nc clean
.DEFAULT_GOAL: native

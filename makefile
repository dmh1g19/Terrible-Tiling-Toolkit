build:
	nix-build
run:
	open ./result/bin/app.jsexe/index.html
both:
	nix-build && cp TTT.pdf result/bin/app.jsexe/TTT.pdf && open ./result/bin/app.jsexe/index.html
clean:
	rm -rf .firebase
	rm -rf dist-newstyle
	rm -f result
deploy: clean build
	cp TTT.pdf result/bin/app.jsexe/TTT.pdf
	firebase deploy --only hosting

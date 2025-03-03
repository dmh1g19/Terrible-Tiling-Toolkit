build:
	nix-build
run:
	open ./result/bin/app.jsexe/index.html
both:
	nix-build && open ./result/bin/app.jsexe/index.html

.PHONY: run
run:
	docker compose run --rm rust

.PHONY: install
install:
	cargo install --root bin cargo-compete

.PHONY: init
init:
	echo 2 | cargo compete init atcoder

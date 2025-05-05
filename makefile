EXE = oxide9

.PHONY: release

release:
	cargo build --release
	cp target/release/oxide9 $(EXE)
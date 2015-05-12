all: interp.bin

%.bin: %.o
	ld -lc -macosx_version_min 10.10.3 $< -o $@

%.o: %.s
	as $< -o $@


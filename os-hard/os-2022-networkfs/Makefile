obj-m += networkfs.o
networkfs-objs += entrypoint.o http.o
ccflags-y := -std=gnu11 -Wno-declaration-after-statement

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(shell pwd) modules

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(shell pwd) clean

mount: all
	insmod networkfs.ko && mount -t networkfs ee96d150-5829-41e4-8488-d94c2965719b /mnt/ct

umount: all
	umount /mnt/ct && rmmod networkfs

load: all
	insmod networkfs.ko

unload: all
	rmmod networkfs

reload: all
	umount /mnt/ct 2>/dev/null
	rmmod networkfs
	insmod networkfs.ko && mount -t networkfs ee96d150-5829-41e4-8488-d94c2965719b /mnt/ct

tests: all
	python3 -m tests BasicTestCases -f

bonus-name: all
	python3 -m tests NameTestCases -f

bonus-wr: all
	python3 -m tests WRTestCases -f

bonus-link: all
	python3 -m tests LinkTestCases -f

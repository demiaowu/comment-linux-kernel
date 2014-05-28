!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
! SYSSIZE是要加载的系统模块长度，单位是节，16字节为1节。
! 0x3000节共计192KB（3*2^(12)=3*2^(2)*2^(10)=12*2^(10)节=12*16KB=192KB）

SYSSIZE = 0x3000

!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.

! .globl用于定义随后的标识符是外部的（全局的）
.globl begtext, begdata, begbss, endtext, enddata, endbss

! .text用于定义代码段
.text
begtext:

! .data用于定义数据段
.data
begdata:

! .bss用于定义未初始化数据段（BSS是Block Started by Symbol的简称）
.bss
begbss:
.text

! SETUPLEN，setup程序所占的扇区数目
SETUPLEN = 4				! nr of setup-sectors

! bootsect的原始地址，该地址由BIOS决定。
! 0x07c0 = 7*2^(8)+c*2^(4) = 7*2^(8)+12*2^(4) = 7*2^(8)+3*4*2^(4) = (28+3)*2^(6) = 31*2^(6)，即31KB处。
BOOTSEG  = 0x07c0			! original address of boot-sector

! 程序将bootsect移动到这里。0x9000 = 9*2^(12) = 9*64*2^(6)=576*2^(6)，即576KB处。
INITSEG  = 0x9000			! we move boot here - out of the way

! 首先将setup程序加载到的地址。0x9020=576*2^(6)+2*2^(4)=576*2^(6)+(1/2)2^(6)=576.5*2^(6)，即576.5KB处。
SETUPSEG = 0x9020			! setup starts here

! 然后将setup程序转移到SYSSEG处。0x1000=2^(12)=2^(6)*2^(6)=64*2^(6)，即64KB处。
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).

! 停止加载的段地址。
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
! 根文件系统，表示第2个硬盘的第一个分区。设备号=主设备号*256+次设备号
ROOT_DEV = 0x306

! 告知链接程序，程序从start标号处开始执行。
entry start

! 移动当前段位置为0x7c0的程序到段位置0x9000处。总共移动512字节。因为本段程序被BIOS读取到段位置为0x7c0处，因此该程序移动自身。
start:
	mov	ax,#BOOTSEG	
	mov	ds,ax			!将ds段寄存器设置为0x7c0
	mov	ax,#INITSEG
	mov	es,ax			!将es段寄存器设置为0x9000
	mov	cx,#256			!设置移动记数值为256字
	sub	si,si			!设置源地址ds:si=0x7c0:0x0000
	sub	di,di			!设置目的地址es:di=ox9000:0x0000
	rep				!重复执行并递减cx的值，直到cx=0
	movw				!move word，从内存ds:si处移动cx个字（16字节）到es:di处
	jmpi	go,INITSEG		!Jump intersegment。即跳转到[INITSEG]:[go]处执行。该语句是的程序跳转到新的位置后，接着执行！

! 设置几个段寄存器
go:	mov	ax,cs			!此时cs=INITSEG=0x9000，CS，Code Segment
	mov	ds,ax			!设置ds=INITSEG=0x9000，DS，Data Segment
	mov	es,ax			!设置es=INITSEG=0x9000，ES，Extra Segment
! put stack at 0x9ff00.
	mov	ss,ax			!设置SS=INITSEG=0x9000，SS，Stack Segment
	mov	sp,#0xFF00		! arbitrary value >>512，任意大于512的值。SP，Stack Pointer

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

!读取setup模块。利用BIOS中断int 0x13将setup模块从磁盘的第二个扇区读取到0x90200开始处，共读取4个扇区。
load_setup:
	mov	dx,#0x0000		! drive 0, head 0；dh为磁头号（0），dl为驱动器号（0）
	mov	cx,#0x0002		! sector 2, track 0；表示从第二个扇区开始。
					! ch为磁道（柱面）号的低8位，cl中0到5位指示开始扇区，6到7位表示磁道号
	mov	bx,#0x0200		! address = 512, in INITSEG；es:bx指向数据缓冲区。
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
					! ah=0x02，表示读磁盘扇区到内存；al需要读取的扇区数量（4）
	int	0x13			! read it；读取操作；若出错，则Carry Flag置位，ah中包含出错码
	jnc	ok_load_setup		! ok - continue；Jump Not Carry
	
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup		!即jmp指令

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs
	mov	sectors,cx
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

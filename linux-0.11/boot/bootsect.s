!//代码注释 (C) Roger Young, 2014-05-29, rogeryoung@outlook.com
!//主要参考文献 Linux内核完全注释，内核版本0.11，赵炯 编著
! 
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!
! //SYSSIZE是要加载的系统模块长度，单位是节，16字节为1节。
! //0x3000节共计192KB（3*2^(12)=3*2^(2)*2^(10)=12*2^(10)节=12*16KB=192KB）

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

! //.globl用于定义随后的标识符是外部的（全局的）
.globl begtext, begdata, begbss, endtext, enddata, endbss

! //.text用于定义代码段
.text
begtext:

! //.data用于定义数据段
.data
begdata:

! //.bss用于定义未初始化数据段（BSS是Block Started by Symbol的简称）
.bss
begbss:
.text

! //SETUPLEN，setup程序所占的扇区数目
SETUPLEN = 4				! nr of setup-sectors

! bootsect的原始地址，该地址由BIOS决定。
! //0x07c0 = 7*2^(8)+c*2^(4) = 7*2^(8)+12*2^(4) = 7*2^(8)+3*4*2^(4) = (28+3)*2^(6) = 31*2^(6)，即31KB处。
BOOTSEG  = 0x07c0			! original address of boot-sector

! //程序将bootsect移动到这里。0x9000 = 9*2^(12) = 9*64*2^(6)=576*2^(6)，即576KB处。
INITSEG  = 0x9000			! we move boot here - out of the way

! //首先将setup程序加载到的地址。0x9020=576*2^(6)+2*2^(4)=576*2^(6)+(1/2)2^(6)=576.5*2^(6)，即576.5KB处。
SETUPSEG = 0x9020			! setup starts here

! //然后将setup程序转移到SYSSEG处。0x1000=2^(12)=2^(6)*2^(6)=64*2^(6)，即64KB处。
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).

! //停止加载的段地址。
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
! //根文件系统，表示第2个硬盘的第一个分区。设备号=主设备号*256+次设备号
ROOT_DEV = 0x306

! //告知链接程序，程序从start标号处开始执行。
entry start

! //移动当前段位置为0x7c0的程序到段位置0x9000处。总共移动512字节。因为本段程序被BIOS读取到段位置为0x7c0处，因此该程序移动自身。
start:
	mov	ax,#BOOTSEG	
	mov	ds,ax			!//将ds段寄存器设置为0x7c0
	mov	ax,#INITSEG
	mov	es,ax			!//将es段寄存器设置为0x9000
	mov	cx,#256			!//设置移动记数值为256字
	sub	si,si			!//设置源地址ds:si=0x7c0:0x0000
	sub	di,di			!//设置目的地址es:di=ox9000:0x0000
	rep				!//重复执行并递减cx的值，直到cx=0
	movw				!//move word，从内存ds:si处移动cx个字（16字节）到es:di处
	jmpi	go,INITSEG		!//Jump intersegment。即跳转到[INITSEG]:[go]处执行。该语句是的程序跳转到新的位置后，接着执行！

! //设置几个段寄存器
go:	mov	ax,cs			!//此时cs=INITSEG=0x9000，CS，Code Segment
	mov	ds,ax			!//设置ds=INITSEG=0x9000，DS，Data Segment
	mov	es,ax			!//设置es=INITSEG=0x9000，ES，Extra Segment
! put stack at 0x9ff00.
	mov	ss,ax			!//设置SS=INITSEG=0x9000，SS，Stack Segment
	mov	sp,#0xFF00		! arbitrary value >>512，//任意大于512的值。SP，Stack Pointer

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

!//读取setup模块。利用BIOS中断int 0x13将setup模块从磁盘的第二个扇区读取到0x90200开始处，共读取4个扇区。
load_setup:
	mov	dx,#0x0000		! drive 0, head 0；//dh为磁头号（0），dl为驱动器号（0）
	mov	cx,#0x0002		! sector 2, track 0；//表示从第二个扇区开始。
					! //ch为磁道（柱面）号的低8位，cl中0到5位指示开始扇区，6到7位表示磁道号
	mov	bx,#0x0200		! address = 512, in INITSEG；//es:bx指向数据缓冲区。
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
					! //ah=0x02，表示读磁盘扇区到内存；al需要读取的扇区数量（4）
	int	0x13			! read it；//读取操作；若出错，则Carry Flag置位，ah中包含出错码
	jnc	ok_load_setup		! ok - continue；Jump Not Carry
	
	!//如果出错，则复位驱动器，并重试
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup		!//即jmp指令

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track
! //获取磁盘参数

	mov	dl,#0x00		! //dl为驱动器号
	mov	ax,#0x0800		! AH=8 is get drive parameters
					! //AH=8，获取磁盘参数
	int	0x13			! //返回值ah=0，al=0，bl为驱动器类型，ch为最大磁道号的低8位，cl的0至5位为每磁道最大扇区数，cl的6至7位为最大磁道号，dh最大磁头数，dl驱动器的数量，es：di指向软驱磁盘参数表
	mov	ch,#0x00

	seg cs				! //seg cs，只影响到它的下一条语句。

	! //用于保存每磁道的扇区数目
	mov	sectors,cx		! //同上一句和起来相当于mov cs:[sectors], cx

	! //改回es的值。上个int 0x13中断更改了es的值，故改回！
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message
! //在屏幕上显示“Loading sysytem...“
! //使用BIOS中断int 0x10，功能号ah=0x03表示读取光标位置。
! //返回值ch为扫描开始线，cl为扫描结束线，dh为行号（0x00为顶端，0到24），dl为列号（0x00为最左边，0到79）
	mov	ah,#0x03		! read cursor pos
	xor	bh,bh
	int	0x10

	! //使用BIOS中的int 0x10，功能号ah=0x13表示显示字符串
	! //输入：al为放置光标的方式和规定的属性，0x01表示使用bl中的属性值，光标停在字符串结尾处
	! 	//es:bp指向所要显示的字符串起始位置；cx为要显示的字符数；bh显示页面号；bl字符属性。dh行号，dl列号。
	mov	cx,#24			! //共显示24个字符
	mov	bx,#0x0007		! page 0, attribute 7 (normal) 
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)
! //开始加载system模块到0x10000（64KB）处
	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000 //es存放system的段地址
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev	! //取508、509字节处根设备号，并判断是否已经被定义
	cmp	ax,#0
	jne	root_defined

	! //取上面保存的每磁道扇区数。如果sectors=15则说明是1.2MB的驱动器；若是sectors=18，则说明是1.44MB软驱。
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:				! //如果都不一样，则死机。
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax		! //将检查过的设备号保存到root_dev中

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG		! //Jump Intersegment，跳转到0x9020:0000处去执行。

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
! 
! //伪操作符.word定义一个2字节目标
sread:	.word 1+SETUPLEN	! sectors read of current track //当前磁道中已读扇区数目
head:	.word 0			! current head //当前磁头号
track:	.word 0			! current track //当前磁道号

! //read_it用于读取磁盘上的system模块。该程序将system模块加载到内存地址0x10000处，并确保没有跨越64KB的内存边界。
read_it:
	mov ax,es
	test ax,#0x0fff		! //首先测试输入的段值：从盘上读入的数据必须位于64KB的边界开始处，否则进入死循环
				! //test操作结果影响Zero Flag。以比特位逻辑与两个操作数，(ax) & (0xfff)
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment 
				! //bx为段内偏移值，清bx，表示当前段内存放数据的开始位置

! //判断是否已经读入全部数据。比较当前所读的段是否是系统数据末段所处的段（#ENGDEG）；若不是，继续读取。若是，则返回！
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret

ok1_read:
! //计算和验证当前磁道需要读取的扇区数目，存放在ax寄存器中。
! //根据当前磁道还未读取的扇区数以及段内数据字节开始偏移位置，计算如果全部读取这些未读扇区，所读总字节数是否会超过64KB段长度的限制。若会超过，则根据此次最多能读入的字节数，算出需要读取的扇区数。
	seg cs
	mov ax,sectors		! //取每磁道扇区数。前面保存的，见上
	sub ax,sread		! //减去当前磁道已读扇区数
	mov cx,ax		! //cx=ax，表示当前磁道未读扇区数目
	shl cx,#9		! //逻辑左移，cx=cx*512字节+段内当前偏移值(bx)
	add cx,bx		! //此次读操作后，段内共读入的字节数。add影响CF、SF、OF、PF、ZF、AF
	jnc ok2_read		! //Jump Not Carry若没有超过64KB，则跳转
	je ok2_read		! //Jump Equal
	! //若加上将读磁道上所用未读扇区时会超过64KB，则计算最多能读入的字节数。
	xor ax,ax		! 
	sub ax,bx
	shr ax,#9

ok2_read:
! //读当前磁道上指定开始扇区(cl)和需读扇区数(al)的数据到es:bx开始处。然后统计当前磁道上已经读取的扇区数，并与磁道最大扇区数sectors作比较。如果小于sectors说明当前磁道上还有扇区未读。
	call read_track
	mov cx,ax		! //cx，该次操作已读取的扇区数
	add ax,sread		! //加上当前磁道上已经读取的扇区数

	seg cs
	cmp ax,sectors		! //如果当前磁道上还有未读扇区，则跳转到ok3_read处
	jne ok3_read

	! //若该磁道的当前磁头面所有扇区已读，则读取该磁道的下一磁头面的数据。若已完成，则读下一磁道。
	mov ax,#1
	sub ax,head		! //判断当前磁头号
	jne ok4_read		! //若是0磁头，则去读1磁头面上的扇区数据
	inc track		! //否则去读下一磁道
ok4_read:
	mov head,ax		! //保存当前磁头号
	xor ax,ax		! //清当前磁道已读扇区数
ok3_read:
! //若当前磁道上还有未读扇区，则先保存当前磁道已读扇区数，然后调整存放数据处的开始位置。若小于64KB边界值，则跳转rp_read处，继续读。
	mov sread,ax		! //保存当前磁道已读扇区数
	shl cx,#9		! //上次已读扇区数*512字节
	add bx,cx		! //调整当前段内数据开始位置
	jnc rp_read
	! //否则说明已经读取64KB数据，此时调整当前段，为读下一段数据作准备。
	mov ax,es
	add ax,#0x1000		! //将段基址调整为指向下一个64KB内存开始处
	mov es,ax
	xor bx,bx		! //清段内数据开始偏移值
	jmp rp_read

read_track:
! //读当前磁道上指定开始扇区(cl)和需读扇区数(al)的数据到es:bx开始处。
	push ax
	push bx
	push cx
	push dx
	mov dx,track		! //取当前磁道号
	mov cx,sread		! //取当前磁道上已读扇区数
	inc cx			! //cl表示开始读扇区
	mov ch,dl		! //ch表示当前磁道号
	mov dx,head		! //取当前磁头号
	mov dh,dl		! //dh，磁头号
	mov dl,#0		! //dl，驱动器号
	and dx,#0x0100		! //磁头号不大于1
	mov ah,#2		! //ah=2，读磁盘扇区功能号
	int 0x13
	jc bad_rt		! //若出错，则跳转至bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	
! //读磁盘操作出错。则执行驱动器复位操作，然后重试！
	mov ax,#0
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

! //关闭软驱马达。0x3f2是软盘控制器的一个端口，是一个8位的寄存器，其4位至7位分别用于控制4个软驱（A-D）的启动和关闭，位3至位2用于允许/禁止DMA和中断请求以及启动/复位软盘控制器(FDC)，位1至位0用于选择操作的软驱。
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb			! //将al中的内容输出到dx指定的端口去。
				! //al位0，则选择A驱动器，关闭FDC，禁止DMA和中断请求，关闭马达。
	pop dx
	ret

sectors:			! //用于存放当前启动软盘每磁道的扇区数
	.word 0

msg1:					! //条用BIOS中断显示的信息，共计24个字符
	.byte 13,10			! //回车、换行符的ASCII码
	.ascii "Loading system ..."
	.byte 13,10,13,10		! //回车、换行符的ASCII码

.org 508			! //表示下面的语句从地址508（0x1FC）处开始。
root_dev:
	.word ROOT_DEV		! //存放根文件系统所在的设备号。

! //下面是启动盘具有有效引导扇区的标志。仅供BIOS中的程序加载引导扇区时识别使用。必须位于引导扇区的最后两个字节中。
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

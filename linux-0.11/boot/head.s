/*
 * 代码注释 (C) Roger Young, 2014-05-29, rogeryoung@outlook.com
 * 主要参考文献 Linux内核完全注释，内核版本0.11，赵炯 编著
 * head.s程序与内核其他程序一起被连接成system模块，位于system模块的最开始部分。
 * head.s程序采用AT&T的汇编语言格式，并且需要使用gas和gld进行编译连接。
 *
 * 主要功能：加载各个数据段寄存器;
 *         重新设置中断描述符表idt，共计256项,并使各个表项均指向一个只报错误的哑中断子程序ignore_int;
 *         重新设置全局段描述符表gdt；
 *         检测A20地址线是否已真的开启；
 *         测试是否含有数学协处理器，并在控制寄存器CR0中设置相应的标志位；
 *         设置管理内存的分页处理机制；
 *         运行main()程序。
 */

/*
 *  linux/boot/head.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 *  head.s contains the 32-bit startup code.
 *
 * NOTE!!! Startup happens at absolute address 0x00000000, which is also where
 * the page directory will exist. The startup code will be overwritten by
 * the page directory.
 */
.text
.globl _idt,_gdt,_pg_dir,_tmp_floppy_area

_pg_dir:			/*页目录表存放在这里*/

/*
 *段选择符长度为为16位。位0-1表示请求的特权级；位2用于选择全局描述符表(0)还是局部描述符表(1);
 *位3-15是描述符表项的索引，指出选择第几项描述符。
 */
startup_32:
	/*设置ds、es、fs、gs位setup.s中构造的数据段（全局段描述符表第2项）的选择符0x10*/
	movl $0x10,%eax		/*0x10（0b0001,0000）,请求特权级0，选择全局描述符表第2项
				  正好指向表中的数据段描述符表项*/
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs

	lss _stack_start,%esp	/*设置系统堆栈，ss:esp为_stack_start*/

	call setup_idt		/*设置中断描述符表*/
	call setup_gdt		/*设置全局描述符表*/

	/*重新设置ds、es、fs、gs*/
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt. CS was already
	mov %ax,%es		# reloaded in 'setup_gdt'
	mov %ax,%fs
	mov %ax,%gs

	lss _stack_start,%esp	/*重新设置系统堆栈*/
	
	/*用于测试A20数据线是否已经开启
	 *方法：向内存地址0x00000处写入任意一个数值，然后看内存地址0x100000（1M）处是否也是该数值。
	 */
	xorl %eax,%eax
1:				/*是一个局部符号构成的标号，共有10个局部符号。*/
	incl %eax		# check that A20 really IS enabled
	movl %eax,0x000000	# loop forever if it isn't
	cmpl %eax,0x100000
	je 1b
/*
 * NOTE! 486 should set bit 16, to check for write-protect in supervisor
 * mode. Then it would be unnecessary with the "verify_area()"-calls.
 * 486 users probably want to set the NE (#5) bit also, so as to use
 * int 16 for math errors.
 */

/*
 *用于检测数学协处理器芯片是否存在
 *方法：修改控制寄存器CR0，在假设存在协处理器的情况下执行一个协处理器指令，如果出错则协处理器芯片不存在。
 */
	movl %cr0,%eax		# check math chip
	andl $0x80000011,%eax	# Save PG,PE,ET
/* "orl $0x10020,%eax" here for 486 might be good */
	orl $2,%eax		# set MP
	movl %eax,%cr0
	call check_x87
	jmp after_page_tables

/*
 * We depend on ET to be correct. This checks for 287/387.
 */
check_x87:
	fninit		/*向协处理器发出初始化命令，将协处理器置于一个不受以前操作影响的已知状态。
			 如果系统存在协处理器，那么在执行fninit后，其状态字的低字节肯定为0*/
	fstsw %ax	/*取协处理器的状态字到ax寄存器中*/
	cmpb $0,%al
	je 1f		/* no coprocessor: have to set bits */
			/*如果存在，则向前跳转到标号1处，否则改写CR0*/
	movl %cr0,%eax
	xorl $6,%eax	/* reset MP, set EM */
	movl %eax,%cr0
	ret

.align 2		/*.align含义是存储边界对齐调整，
			 * 2表示把随后的代码或数据的偏移位置调整到地址最后2比特位为0的位置*/
1:	.byte 0xDB,0xE4	/* fsetpm for 287, ignored by 387 */
			/*这两个字节是80287协处理器指令fsetpm的机器码，作用将80287设置为保护模式*/
	ret

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It then loads
 *  idt. Everything that wants to install itself
 *  in the idt-table may do so themselves. Interrupts
 *  are enabled elsewhere, when we can be relatively
 *  sure everything is ok. This routine will be over-
 *  written by the page tables.
 */
setup_idt:
	lea ignore_int,%edx
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */

	lea _idt,%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt idt_descr
	ret

/*
 *  setup_gdt
 *
 *  This routines sets up a new gdt and loads it.
 *  Only two entries are currently built, the same
 *  ones that were built in init.s. The routine
 *  is VERY complicated at two whole lines, so this
 *  rather long comment is certainly needed :-).
 *  This routine will beoverwritten by the page tables.
 */
setup_gdt:
	lgdt gdt_descr
	ret

/*
 * I put the kernel page tables right after the page directory,
 * using 4 of them to span 16 Mb of physical memory. People with
 * more than 16MB will have to expand this.
 */
.org 0x1000
pg0:

.org 0x2000
pg1:

.org 0x3000
pg2:

.org 0x4000
pg3:

.org 0x5000
/*
 * tmp_floppy_area is used by the floppy-driver when DMA cannot
 * reach to a buffer-block. It needs to be aligned, so that it isn't
 * on a 64kB border.
 */
_tmp_floppy_area:
	.fill 1024,1,0

after_page_tables:
	pushl $0		# These are the parameters to main :-)
	pushl $0
	pushl $0
	pushl $L6		# return address for main, if it decides to.
	pushl $_main
	jmp setup_paging
L6:
	jmp L6			# main should never return here, but
				# just in case, we know what happens.

/* This is the default interrupt "handler" :-) */
int_msg:
	.asciz "Unknown interrupt\n\r"
.align 2
ignore_int:
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $int_msg
	call _printk
	popl %eax
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret


/*
 * Setup_paging
 *
 * This routine sets up paging by setting the page bit
 * in cr0. The page tables are set up, identity-mapping
 * the first 16MB. The pager assumes that no illegal
 * addresses are produced (ie >4Mb on a 4Mb machine).
 *
 * NOTE! Although all physical memory should be identity
 * mapped by this routine, only the kernel page functions
 * use the >1Mb addresses directly. All "normal" functions
 * use just the lower 1Mb, or the local data space, which
 * will be mapped to some other place - mm keeps track of
 * that.
 *
 * For those with more memory than 16 Mb - tough luck. I've
 * not got it, why should you :-) The source is here. Change
 * it. (Seriously - it shouldn't be too difficult. Mostly
 * change some constants etc. I left it at 16Mb, as my machine
 * even cannot be extended past that (ok, but it was cheap :-)
 * I've tried to show which constants to change by having
 * some kind of marker at them (search for "16Mb"), but I
 * won't guarantee that's all :-( )
 */
.align 2
setup_paging:
	movl $1024*5,%ecx		/* 5 pages - pg_dir+4 page tables */
	xorl %eax,%eax
	xorl %edi,%edi			/* pg_dir is at 0x000 */
	cld;rep;stosl
	movl $pg0+7,_pg_dir		/* set present bit/user r/w */
	movl $pg1+7,_pg_dir+4		/*  --------- " " --------- */
	movl $pg2+7,_pg_dir+8		/*  --------- " " --------- */
	movl $pg3+7,_pg_dir+12		/*  --------- " " --------- */
	movl $pg3+4092,%edi
	movl $0xfff007,%eax		/*  16Mb - 4096 + 7 (r/w user,p) */
	std
1:	stosl			/* fill pages backwards - more efficient :-) */
	subl $0x1000,%eax
	jge 1b
	xorl %eax,%eax		/* pg_dir is at 0x0000 */
	movl %eax,%cr3		/* cr3 - page directory start */
	movl %cr0,%eax
	orl $0x80000000,%eax
	movl %eax,%cr0		/* set paging (PG) bit */
	ret			/* this also flushes prefetch-queue */

.align 2
.word 0
idt_descr:
	.word 256*8-1		# idt contains 256 entries
	.long _idt
.align 2
.word 0
gdt_descr:
	.word 256*8-1		# so does gdt (not that that's any
	.long _gdt		# magic number, but it works for me :^)

	.align 3
_idt:	.fill 256,8,0		# idt is uninitialized

_gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a0000000fff	/* 16Mb */
	.quad 0x00c0920000000fff	/* 16Mb */
	.quad 0x0000000000000000	/* TEMPORARY - don't use */
	.fill 252,8,0			/* space for LDT's and TSS's etc */

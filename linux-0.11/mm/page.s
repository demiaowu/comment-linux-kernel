/*
 *  linux/mm/page.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * page.s contains the low-level page-exception code.
 * the real work is done in mm.c
 */
// 该文件包括页异常中断处理程序，主要分为两种：
//   一、由缺页引起的，通过调用do_no_page(error_code,address)来处理
//   二、由页写保护引起的，通过do_wp_page(error_code,address)来处理
// 其中error_code由CPU自动产生并压入堆栈
// 出现异常时访问的线性地址从CR2中取得

.globl _page_fault	// 声明为全局变量，将在traps.c中用于设置页异常描述符

_page_fault:
	xchgl %eax,(%esp)	// 取出错码到eax中
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs

	movl $0x10,%edx		// 置内核数据段选择符
	mov %dx,%ds
	mov %dx,%es
	mov %dx,%fs

	movl %cr2,%edx	// 取引起页面异常的线性地址

	pushl %edx	// 将该线性地址和出错码入栈，作为调用函数的参数
	pushl %eax

	testl $1,%eax	// 测试页存在标志P，如果不是缺页引起的异常则跳转
	jne 1f

	call _do_no_page	// 调用缺页处理函数(mm/memory.c)
	jmp 2f

1:	call _do_wp_page	// 调用写保护处理函数(mm/memory.c)
2:	addl $8,%esp	// 丢弃入栈的两个参数，弹出栈中的寄存器并退出中断
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret

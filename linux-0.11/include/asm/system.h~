//定义了设置或修改描述符/中断门等的嵌入式汇编宏

//move_to_user_mode是用于在内核初始化结束时人工切换到初始任务（任务0）中执行
// 即，从特权级0代码转移到特权级3的代码中去执行。
// 所使用的方法是模拟中断调用返回过程，即利用iret指令来实现特权级的变更和堆栈的切换。
// CPU允许低级别的代码通过门或中断、陷阱门来调用或转移到高级别的代码中运行。
#define move_to_user_mode() \
__asm__ ("movl %%esp,%%eax\n\t" \	//保存堆栈指针esp到eax中
	"pushl $0x17\n\t" \		//当前堆栈段选择符0x17入栈，指向进程0的数据段描述符，因为进程0的代码段、数据段、内核代码段、数据段4者重合，所以它指向的仍然是内核模块区域。
	"pushl %%eax\n\t" \		//把当前堆栈指针入栈。这样模拟外层堆栈的SS:ESP。由于进程0数据段选择符0x17对应的还是内核模块，和内核数据段选择符0x10的差别仅在与对应描述符的dpl和本身rpl的不同，所以外层堆栈指针指向的还是原来的堆栈即user_stack


	"pushfl\n\t" \			//标志寄存器eflags入栈
	"pushl $0x0f\n\t" \		// 进程0代码段选择符入栈，模拟返回的CS
	"pushl $1f\n\t" \		//将下面标号1的偏移地址入栈，模拟返回的EIP，也是由于4段重合，所以这里返回的CS对应的段的基地址与内核代码段基地址一样，都是0，故将返回的CS:EIP就是下面标号1处
	"iret\n" \			//中断返回。由于当前CPL=0，将返回的CS的RPL=3，所以不仅仅要改变CS，EIP，还要发生堆栈切换（但实际上堆栈还是user_stack)，同时CPL变成3。
	"1:\tmovl $0x17,%%eax\n\t" \	//把数据段寄存器的值设为进程0的数据段
	"movw %%ax,%%ds\n\t" \		//初始化段寄存器指向本局部表的数据段
	"movw %%ax,%%es\n\t" \
	"movw %%ax,%%fs\n\t" \
	"movw %%ax,%%gs" \
	:::"ax")

#define sti() __asm__ ("sti"::)		//开中断
#define cli() __asm__ ("cli"::)		//关中断
#define nop() __asm__ ("nop"::)		//空操作

#define iret() __asm__ ("iret"::)	//中断返回

//设置门描述符：根据参数中的中断或异常处理过程偏移地址addr、门描述符类型type和特权级dpl，设置位于地址gate_addr处的门描述符
// %0，由dpl、type组合成的类型标志；%1，描述符低4字节地址；%2，描述符高4字节地址；
// %3，edx，程序偏移地址addr；%4，eax，高字中含有段选择符0x8
#define _set_gate(gate_addr,type,dpl,addr) \
__asm__ ("movw %%dx,%%ax\n\t" \		//将偏移地址低字与选择符组合成描述符低4字节，eax
	"movw %0,%%dx\n\t" \		//将类型标志字与偏移高字组合成描述符高4字节，edx
	"movl %%eax,%1\n\t" \		//分别设置门描述符的低4字节和高4字节
	"movl %%edx,%2" \
	: \
	: "i" ((short) (0x8000+(dpl<<13)+(type<<8))), \
	"o" (*((char *) (gate_addr))), \
	"o" (*(4+(char *) (gate_addr))), \
	"d" ((char *) (addr)),"a" (0x00080000))

// 设置中断门函数（自动屏蔽随后的中断）
// 参数：n，中断号；add，中断程序的偏移地址
// &idt[n]，中断描述符表中中断号n对应项的偏移值
// 中断描述符的类型是14，特权级为0
#define set_intr_gate(n,addr) \
	_set_gate(&idt[n],14,0,addr)

// 设置陷阱门函数
// &idt[n]，中断描述符表中中断号n对应项的偏移值
// 中断描述符的类型是15，特权级为0
#define set_trap_gate(n,addr) \
	_set_gate(&idt[n],15,0,addr)

// 设置系统陷阱门函数
// &idt[n]，中断描述符表中中断号n对应项的偏移值
// 中断描述符的类型是15，特权级为3
#define set_system_gate(n,addr) \
	_set_gate(&idt[n],15,3,addr)

// 设置段描述符函数
#define _set_seg_desc(gate_addr,type,dpl,base,limit) {\
	*(gate_addr) = ((base) & 0xff000000) | \
		(((base) & 0x00ff0000)>>16) | \
		((limit) & 0xf0000) | \
		((dpl)<<13) | \
		(0x00408000) | \
		((type)<<8); \
	*((gate_addr)+1) = (((base) & 0x0000ffff)<<16) | \
		((limit) & 0x0ffff); }

// 在全局表中设置任务状态段/局部表描述符。状态段和局部表段的长度被设置成104字节
// 参数：n，在全局表中描述符项n所对应的地址；add，状态段/局部表所在内存的基地址
//	type，描述符中的标志类型字节
#define _set_tssldt_desc(n,addr,type) \
__asm__ ("movw $104,%1\n\t" \	//将TSS（或LDT）的长度放入描述符长度域（第0-1字节）
	"movw %%ax,%2\n\t" \	//将基地址的低字放入描述符第2-3字节
	"rorl $16,%%eax\n\t" \	//基地址高字循环移入ax中（低字则进入高字处）
	"movb %%al,%3\n\t" \	//基地址高字中低字节一如描述符第4字节
	"movb $" type ",%4\n\t" \	//标志类型字节移入描述符的第5字节
	"movb $0x00,%5\n\t" \	//描述符的第6字节置0
	"movb %%ah,%6\n\t" \	//基地址高字中高字节移入描述符第7字节
	"rorl $16,%%eax" \	//右循环16比特，eax恢复原值
	::"a" (addr), "m" (*(n)), "m" (*(n+2)), "m" (*(n+4)), \
	 "m" (*(n+5)), "m" (*(n+6)), "m" (*(n+7)) \
	)

//在全局表中设置任务状态段描述符。任务状态段描述符的类型为0x89
#define set_tss_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x89")

//在全局表中设置局部表描述符。局部表描述符的类型是0x82
#define set_ldt_desc(n,addr) _set_tssldt_desc(((char *) (n)),addr,"0x82")

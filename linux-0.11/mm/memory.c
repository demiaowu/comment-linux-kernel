/*
 *  linux/mm/memory.c
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * demand-loading started 01.12.91 - seems it is high on the list of
 * things wanted, and it should be easy to implement. - Linus
 */

/*
 * Ok, demand-loading was easy, shared pages a little bit tricker. Shared
 * pages started 02.12.91, seems to work. - Linus.
 *
 * Tested sharing by executing about 30 /bin/sh: under the old kernel it
 * would have taken more than the 6M I have free, but it worked well as
 * far as I could see.
 *
 * Also corrected some "invalidate()"s - I wasn't doing enough of them.
 */

// 进行内存分页的管理，实现对主内存区内存页面的动态分配和回收操作
// 对于内核代码和数据所占的物理内存以外的区域，使用一个字节数字mem_map[]来表示物理内存页面的状态
// 每一字节描述一个物理内存页的占用状态，其中的值表示被占用的次数，0表示对应内存空闲

#include <signal.h>	//信号头文件，定义信号符号常量、信号结构以及信号操作函数原型

#include <asm/system.h>

#include <linux/sched.h>	//定义任务结构task_struct，初始任务0的数据
#include <linux/head.h>		//定义段描述符的简单结构以及几个选择符常量
#include <linux/kernel.h>

volatile void do_exit(long code);	//进程退出函数

static inline volatile void oom(void)	//oom，out of memory，显示内存已用完，并退出
{
	printk("out of memory\n\r");
	do_exit(SIGSEGV);
}

//刷新页变换高速缓冲宏
//为了提高地址转换效率，CPU将最近使用的页表数据存放在芯片高速缓冲中，在修改页表后，需要刷新该缓冲
//这里使用重新加载页目录基址寄存器cr3的方法来进行刷新
#define invalidate() __asm__("movl %%eax,%%cr3"::"a" (0))

/* these are not to be changed without changing head.s etc */
// 以下定义可参见head.s
#define LOW_MEM 0x100000	//内存低端
#define PAGING_MEMORY (15*1024*1024)		//分页内存15M
#define PAGING_PAGES (PAGING_MEMORY>>12)	//分页后的内存页面数15M/4K
#define MAP_NR(addr) (((addr)-LOW_MEM)>>12)	//指定内存地址的页号
#define USED 100	//页面被占用标志

// 该宏用于判断给定线性地址addr是否位于当前进程的代码段中
// 4095=0xFFF，addr+4095的作用是将位于0～4095产生一个进位
// (addr)+4095)&~4095的作用是将刚得到的结果低12位置0
// (addr)+4095)&~4095，相当于去读addr所在页面的末端地址

#define CODE_SPACE(addr) ((((addr)+4095)&~4095) < \
current->start_code + current->end_code)

static long HIGH_MEMORY = 0;	// 全局变量，存放实际物理最高端内存地址

// 从form处复制一页内存（4K）到to处
// cld：清方向位，即使DF=0，决定内存地址向高地址增加
// movs：用来复制一个数据项从源字符串到目标字符串，DS:SI指向源，ES:DI指向目的。
#define copy_page(from,to) \
__asm__("cld ; rep ; movsl"::"S" (from),"D" (to),"c" (1024):"cx","di","si")

// 物理内存映射字节图（一字节代表一页内存）
static unsigned char mem_map [ PAGING_PAGES ] = {0,};

/*
 * Get physical address of first (actually last :-) free page, and mark it
 * used. If no free pages left, return 0.
 */
// 获取空闲页面，若有，则标记位已使用，否则，返回0
unsigned long get_free_page(void)
{
register unsigned long __res asm("ax");

__asm__("std ; repne ; scasb\n\t" // REPNE SCASB是不相等则重复查找的字符串，di放查找串
				//若找到，ZF=1则退出指令的执行；若没找到，已全部找遍则退出。
				//因每执行一次SCASB指令后，DI内容增（或）1
	"jne 1f\n\t"	//如果没有，则跳转到1处去，返回

	"movb $1,1(%%edi)\n\t"	//找到，将对应页面内存映射比特位设置为1
	"sall $12,%%ecx\n\t"	//将页面数*4K，为，相对页面起始地址
	"addl %2,%%ecx\n\t"		//加上低端内存地址，即实际物理起始地址
	"movl %%ecx,%%edx\n\t"	//将实际地址存入edx
	"movl $1024,%%ecx\n\t"
	"leal 4092(%%edx),%%edi\n\t"	//指向该页面末端
	"rep ; stosl\n\t"	//页面清零
	"movl %%edx,%%eax\n"	//页面地址存入eax，返回

	"1:"
	:"=a" (__res)
	:"0" (0),"i" (LOW_MEM),"c" (PAGING_PAGES),
	"D" (mem_map+PAGING_PAGES-1)	//指向mem_map[]内存字节位图的最后一个字节
	:"di","cx","dx");
return __res;
}

/*
 * Free a page of memory at physical address 'addr'. Used by
 * 'free_page_tables()'
 */
// 释放包含地址addr的一页内存
void free_page(unsigned long addr)
{
	//判断是addr是否是内核等，若是则出错
	if (addr < LOW_MEM) return;
	if (addr >= HIGH_MEMORY)
		panic("trying to free nonexistent page");
	//取addr所在的页面号
	addr -= LOW_MEM;
	addr >>= 12;
	//如果该页面对于的mem_map不为零，则减1，返回
	if (mem_map[addr]--) return;
	//否则，说明要释放空闲内存，出错
	mem_map[addr]=0;
	panic("trying to free free page");
}

/*
 * This function frees a continuos block of page tables, as needed
 * by 'exit()'. As does copy_page_tables(), this handles only 4Mb blocks.
 */
// 释放页表连续的内存块
int free_page_tables(unsigned long from,unsigned long size)
{
	unsigned long *pg_table;
	unsigned long * dir, nr;

	//检测from给出的线性基地址是否在4MB的边界处
	//0x3fffff=0b11,1111,11111111,11111111=4MB-1
	if (from & 0x3fffff)
		panic("free_page_tables called with wrong alignment");
	if (!from)
		panic("Trying to free up swapper memory space");

	//计算参数size给出的长度所占的页目录项数
	//dir是起始目录项指针，0xffc=0b1111，1111，1100
	//from>>22,为from对应的目录项号，因为每项占4字节，并且页目录项从物理地址0开始存放
	//故实际目录项指针=目录项号<<2，可得from>>20;
	//& 0xffc，确保目录项指针范围有效。
	//dir = (unsigned long *) ((from>>22)<<2);
	size = (size + 0x3fffff) >> 22;
	dir = (unsigned long *) ((from>>20) & 0xffc); /* _pg_dir = 0 */

	for ( ; size-->0 ; dir++) {
		if (!(1 & *dir))	//如果当前目录项无效（p位=0），则处理下一项
			continue;
		pg_table = (unsigned long *) (0xfffff000 & *dir);//从目录项取出页表地址
		//释放页表对应的内存页
		for (nr=0 ; nr<1024 ; nr++) {
			if (1 & *pg_table)
				free_page(0xfffff000 & *pg_table);
			*pg_table = 0;
			pg_table++;
		}
		free_page(0xfffff000 & *dir);	//释放该页表项
		*dir = 0;
	}
	invalidate();	//刷新页变换高速缓冲
	return 0;
}

/*
 *  Well, here is one of the most complicated functions in mm. It
 * copies a range of linerar addresses by copying only the pages.
 * Let's hope this is bug-free, 'cause this one I don't want to debug :-)
 *
 * Note! We don't copy just any chunks of memory - addresses have to
 * be divisible by 4Mb (one page-directory entry), as this makes the
 * function easier. It's used only by fork anyway.
 *
 * NOTE 2!! When from==0 we are copying kernel space for the first
 * fork(). Then we DONT want to copy a full page-directory entry, as
 * that would lead to some serious memory waste - we just copy the
 * first 160 pages - 640kB. Even that is more than we need, but it
 * doesn't take any more memory - we don't copy-on-write in the low
 * 1 Mb-range, so the pages can be shared with the kernel. Thus the
 * special case for nr=xxxx.
 */

// 复制指定线性地址(from)和指定长度内存(size)对应的页目录表项和页表项
int copy_page_tables(unsigned long from,unsigned long to,long size)
{
	unsigned long * from_page_table;
	unsigned long * to_page_table;
	unsigned long this_page;
	unsigned long * from_dir, * to_dir;
	unsigned long nr;

	//检测from给出的线性基地址是否在4MB的边界处
	//0x3fffff=0b11,1111,11111111,11111111=4MB-1
	if ((from&0x3fffff) || (to&0x3fffff))
		panic("copy_page_tables called with wrong alignment");

	//计算源地址和目的地址的起始目录项指针，以及要复制的内存块的页表数
	from_dir = (unsigned long *) ((from>>20) & 0xffc); /* _pg_dir = 0 */
	to_dir = (unsigned long *) ((to>>20) & 0xffc);
	size = ((unsigned) (size+0x3fffff)) >> 22;

	for( ; size-->0 ; from_dir++,to_dir++) {
		//验证当前源目录项和目的项正常
		if (1 & *to_dir)
			panic("copy_page_tables: already exist");
		if (!(1 & *from_dir))
			continue;
		//取源目录项中页表地址
		from_page_table = (unsigned long *) (0xfffff000 & *from_dir);
		//若申请不到内存，则返回，出错
		if (!(to_page_table = (unsigned long *) get_free_page()))
			return -1;	/* Out of memory, see freeing */
		//否则，设置目的目录项信息，7=0b111，表示对应页表映射的内存是用户级、可读性、存在
		*to_dir = ((unsigned long) to_page_table) | 7;
		//计算当前页目录项对应的页表所需要复制的页面项数，若是内核空间，则位160
		nr = (from==0)?0xA0:1024;
		//循环复制指定的nr个内存页面表项
		for ( ; nr-- > 0 ; from_page_table++,to_page_table++) {
			this_page = *from_page_table;
			if (!(1 & this_page))
				continue;
			this_page &= ~2;
			*to_page_table = this_page;
			if (this_page > LOW_MEM) {
				*from_page_table = this_page;
				this_page -= LOW_MEM;
				this_page >>= 12;
				mem_map[this_page]++;
			}
		}
	}
	invalidate();
	return 0;
}

/*
 * This function puts a page in memory at the wanted address.
 * It returns the physical address of the page gotten, 0 if
 * out of memory (either when trying to access page-table or
 * page.)
 */
// 将一页内存页面映射到指定线性地址处，返回页面的物理地址
unsigned long put_page(unsigned long page,unsigned long address)
{
	unsigned long tmp, *page_table;

/* NOTE !!! This uses the fact that _pg_dir=0 */

	if (page < LOW_MEM || page >= HIGH_MEMORY)
		printk("Trying to put page %p at %p\n",page,address);
	if (mem_map[(page-LOW_MEM)>>12] != 1)
		printk("mem_map disagrees with %p at %p\n",page,address);
	page_table = (unsigned long *) ((address>>20) & 0xffc);
	if ((*page_table)&1)
		page_table = (unsigned long *) (0xfffff000 & *page_table);
	else {
		if (!(tmp=get_free_page()))
			return 0;
		*page_table = tmp|7;
		page_table = (unsigned long *) tmp;
	}
	page_table[(address>>12) & 0x3ff] = page | 7;
/* no need for invalidate */
	return page;
}

//取消写保护页面函数
void un_wp_page(unsigned long * table_entry)
{
	unsigned long old_page,new_page;

	old_page = 0xfffff000 & *table_entry;
	if (old_page >= LOW_MEM && mem_map[MAP_NR(old_page)]==1) {
		*table_entry |= 2;
		invalidate();
		return;
	}
	if (!(new_page=get_free_page()))
		oom();
	if (old_page >= LOW_MEM)
		mem_map[MAP_NR(old_page)]--;
	*table_entry = new_page | 7;
	invalidate();
	copy_page(old_page,new_page);
}	

/*
 * This routine handles present pages, when users try to write
 * to a shared page. It is done by copying the page to a new address
 * and decrementing the shared-page counter for the old page.
 *
 * If it's in code space we exit with a segment error.
 */
//执行写保护页面处理
void do_wp_page(unsigned long error_code,unsigned long address)
{
#if 0
/* we cannot do this yet: the estdio library writes to code space */
/* stupid, stupid. I really want the libc.a from GNU */
	if (CODE_SPACE(address))
		do_exit(SIGSEGV);
#endif
	un_wp_page((unsigned long *)
		(((address>>10) & 0xffc) + (0xfffff000 &
		*((unsigned long *) ((address>>20) &0xffc)))));

}

// 写页面验证
void write_verify(unsigned long address)
{
	unsigned long page;

	if (!( (page = *((unsigned long *) ((address>>20) & 0xffc)) )&1))
		return;
	page &= 0xfffff000;
	page += ((address>>10) & 0xffc);
	if ((3 & *(unsigned long *) page) == 1)  /* non-writeable, present */
		un_wp_page((unsigned long *) page);
	return;
}

//取一页空闲内存并映射到指定线性地址处
void get_empty_page(unsigned long address)
{
	unsigned long tmp;

	if (!(tmp=get_free_page()) || !put_page(tmp,address)) {
		free_page(tmp);		/* 0 is ok - ignored */
		oom();
	}
}

/*
 * try_to_share() checks the page at address "address" in the task "p",
 * to see if it exists, and if it is clean. If so, share it with the current
 * task.
 *
 * NOTE! This assumes we have checked that p != current, and that they
 * share the same executable.
 */
// 尝试对当前进程指定地址处的页面进行共享处理
static int try_to_share(unsigned long address, struct task_struct * p)
{
	unsigned long from;
	unsigned long to;
	unsigned long from_page;
	unsigned long to_page;
	unsigned long phys_addr;

	from_page = to_page = ((address>>20) & 0xffc);
	from_page += ((p->start_code>>20) & 0xffc);
	to_page += ((current->start_code>>20) & 0xffc);
/* is there a page-directory at from? */
	from = *(unsigned long *) from_page;
	if (!(from & 1))
		return 0;
	from &= 0xfffff000;
	from_page = from + ((address>>10) & 0xffc);
	phys_addr = *(unsigned long *) from_page;
/* is the page clean and present? */
	if ((phys_addr & 0x41) != 0x01)
		return 0;
	phys_addr &= 0xfffff000;
	if (phys_addr >= HIGH_MEMORY || phys_addr < LOW_MEM)
		return 0;
	to = *(unsigned long *) to_page;
	if (!(to & 1))
		if (to = get_free_page())
			*(unsigned long *) to_page = to | 7;
		else
			oom();
	to &= 0xfffff000;
	to_page = to + ((address>>10) & 0xffc);
	if (1 & *(unsigned long *) to_page)
		panic("try_to_share: to_page already exists");
/* share them: write-protect */
	*(unsigned long *) from_page &= ~2;
	*(unsigned long *) to_page = *(unsigned long *) from_page;
	invalidate();
	phys_addr -= LOW_MEM;
	phys_addr >>= 12;
	mem_map[phys_addr]++;
	return 1;
}

/*
 * share_page() tries to find a process that could share a page with
 * the current one. Address is the address of the wanted page relative
 * to the current data space.
 *
 * We first check if it is at all feasible by checking executable->i_count.
 * It should be >1 if there are other tasks sharing this inode.
 */
// 共享页面处理
static int share_page(unsigned long address)
{
	struct task_struct ** p;

	if (!current->executable)
		return 0;
	if (current->executable->i_count < 2)
		return 0;
	for (p = &LAST_TASK ; p > &FIRST_TASK ; --p) {
		if (!*p)
			continue;
		if (current == *p)
			continue;
		if ((*p)->executable != current->executable)
			continue;
		if (try_to_share(address,*p))
			return 1;
	}
	return 0;
}

// 执行缺页处理
void do_no_page(unsigned long error_code,unsigned long address)
{
	int nr[4];
	unsigned long tmp;
	unsigned long page;
	int block,i;

	address &= 0xfffff000;
	tmp = address - current->start_code;
	if (!current->executable || tmp >= current->end_data) {
		get_empty_page(address);
		return;
	}
	if (share_page(tmp))
		return;
	if (!(page = get_free_page()))
		oom();
/* remember that 1 block is used for header */
	block = 1 + tmp/BLOCK_SIZE;
	for (i=0 ; i<4 ; block++,i++)
		nr[i] = bmap(current->executable,block);
	bread_page(page,current->executable->i_dev,nr);
	i = tmp + 4096 - current->end_data;
	tmp = page + 4096;
	while (i-- > 0) {
		tmp--;
		*(char *)tmp = 0;
	}
	if (put_page(page,address))
		return;
	free_page(page);
	oom();
}

// 物理内存管理初始化
void mem_init(long start_mem, long end_mem)
{
	int i;

	HIGH_MEMORY = end_mem;
	for (i=0 ; i<PAGING_PAGES ; i++)
		mem_map[i] = USED;
	i = MAP_NR(start_mem);
	end_mem -= start_mem;
	end_mem >>= 12;
	while (end_mem-->0)
		mem_map[i++]=0;
}

//计算内存空闲页面数并显示
void calc_mem(void)
{
	int i,j,k,free=0;
	long * pg_tbl;

	for(i=0 ; i<PAGING_PAGES ; i++)
		if (!mem_map[i]) free++;
	printk("%d pages free (of %d)\n\r",free,PAGING_PAGES);
	for(i=2 ; i<1024 ; i++) {
		if (1&pg_dir[i]) {
			pg_tbl=(long *) (0xfffff000 & pg_dir[i]);
			for(j=k=0 ; j<1024 ; j++)
				if (pg_tbl[j]&1)
					k++;
			printk("Pg-dir[%d] uses %d pages\n",i,k);
		}
	}
}

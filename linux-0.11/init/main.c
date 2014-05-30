/*
 *  linux/init/main.c
 *
 *  (C) 1991  Linus Torvalds
 */

#define __LIBRARY__
#include <unistd.h>
#include <time.h>

/*
 * we need this inline - forking from kernel space will result
 * in NO COPY ON WRITE (!!!), until an execve is executed. This
 * is no problem, but for the stack. This is handled by not letting
 * main() use the stack at all after fork(). Thus, no function
 * calls - which means inline code for fork too, as otherwise we
 * would use the stack upon exit from 'fork()'.
 *
 * Actually only pause and fork are needed inline, so that there
 * won't be any messing with the stack from main(), but we define
 * some others too.
 */

//main()在移动到用户模式（任务0）后执行内嵌方式的fork()和pause()，因此可保证不使用任务0的用户栈
//在执行move_to_user_mode()之后，main()就以任务0的身份运行，任务0时所有将创建子进程的父进程。

static inline _syscall0(int,fork)		//创建进程的系统调用
static inline _syscall0(int,pause)		//暂停进程的执行，直到收到一个信号
static inline _syscall1(int,setup,void *,BIOS)
static inline _syscall0(int,sync)		//更新文件系统

#include <linux/tty.h>
#include <linux/sched.h>
#include <linux/head.h>
#include <asm/system.h>
#include <asm/io.h>

#include <stddef.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#include <linux/fs.h>

static char printbuf[1024];	//用作内核显示信息的缓存

extern int vsprintf();		//送格式化输出到一字符串中
extern void init(void);
extern void blk_dev_init(void);		//块设备初始化
extern void chr_dev_init(void);		//字符设备初始化
extern void hd_init(void);		//硬盘初始化
extern void floppy_init(void);		//软驱初始化
extern void mem_init(long start, long end);	//内存管理初始化
extern long rd_init(long mem_start, int length);	//虚拟盘初始化
extern long kernel_mktime(struct tm * tm);	//计算系统开机启动时间

extern long startup_time;	//内核启动时间（秒）

/*
 * This is set up by the setup-routine at boot-time
 */
//以下三个由setup.s程序设置
#define EXT_MEM_K (*(unsigned short *)0x90002)		//1MB以后的扩展内存大小
#define DRIVE_INFO (*(struct drive_info *)0x90080)	//硬盘参数表
#define ORIG_ROOT_DEV (*(unsigned short *)0x901FC)	//根文件系统所在的设备号

/*
 * Yeah, yeah, it's ugly, but I cannot find how to do this correctly
 * and this seems to work. I anybody has more info on the real-time
 * clock I'd be interested. Most of this was trial and error, and some
 * bios-listing reading. Urghh.
 */

//该宏读取CMOS实时时钟信息，参见include/asm/io.h
//0x70是写地址端口号，0x80|addr是要读取的CMOS内存地址，0x71是读数据端口号
#define CMOS_READ(addr) ({ \
outb_p(0x80|addr,0x70); \
inb_p(0x71); \
})

// 定义宏，将BCD码转换成二进制数值。BCD码利用半个字节表示一个10进制数
// (val)&15=(val)&(0b0001111)取BCD表示的10进制个位数
// (val)>>4取BCD表示的10进制十位数
#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10)

//取CMOS实时钟信息作为开机时间，并保存到start_time中
static void time_init(void)
{
	struct tm time;

	do {
		time.tm_sec = CMOS_READ(0);
		time.tm_min = CMOS_READ(2);
		time.tm_hour = CMOS_READ(4);
		time.tm_mday = CMOS_READ(7);
		time.tm_mon = CMOS_READ(8);
		time.tm_year = CMOS_READ(9);
	} while (time.tm_sec != CMOS_READ(0));	//将时间误差控制在1秒内
	//转换成二进制数值
	BCD_TO_BIN(time.tm_sec);
	BCD_TO_BIN(time.tm_min);
	BCD_TO_BIN(time.tm_hour);
	BCD_TO_BIN(time.tm_mday);
	BCD_TO_BIN(time.tm_mon);
	BCD_TO_BIN(time.tm_year);
	time.tm_mon--;
	startup_time = kernel_mktime(&time);	//计算开机时间
}

static long memory_end = 0;		//机器具有的物理内存容量（字节数）
static long buffer_memory_end = 0;	//高速缓冲区末端地址
static long main_memory_start = 0;	//主内存开始的位置

struct drive_info { char dummy[32]; } drive_info;	//存放硬盘参数表

//内核初始化主程序
void main(void)		/* This really IS void, no error here. */
{			/* The startup routine assumes (well, ...) this */
/*
 * Interrupts are still disabled. Do necessary setups, then
 * enable them
 */
 	ROOT_DEV = ORIG_ROOT_DEV;	//ROOT_DEV定义在fs/super.c
 	drive_info = DRIVE_INFO;	//复制0x90080处的硬盘参数表
	memory_end = (1<<20) + (EXT_MEM_K<<10);		//内存大小=1MB+扩展内存*1024
	memory_end &= 0xfffff000;		//忽略不到一页(4KB)的内存数

	if (memory_end > 16*1024*1024)		//若内存超过16MB，则按16MB处理
		memory_end = 16*1024*1024;
	//设置高速缓冲区
	if (memory_end > 12*1024*1024) 
		buffer_memory_end = 4*1024*1024;
	else if (memory_end > 6*1024*1024)
		buffer_memory_end = 2*1024*1024;
	else
		buffer_memory_end = 1*1024*1024;
	//设置主内存起始位置
	main_memory_start = buffer_memory_end;

//如果定义了内存虚拟盘符号，则初始化虚拟盘，并调整主内存的其实位置和大小？
#ifdef RAMDISK
	main_memory_start += rd_init(main_memory_start, RAMDISK*1024);	// kernel/blk_drv/ramdisk.c
#endif
	mem_init(main_memory_start,memory_end);	//主内存初始化，mm/memory.c
	trap_init();	//陷阱门（硬件中断向量）初始化，kernel/traps.c
	blk_dev_init();	//块设备，kernel/blk_drv/ll_rw_blk.c
	chr_dev_init();	//字符设备，kernel/char_drv/tty_io.c
	tty_init();	//tty初始化，kernel/char_drv/tty_io.c

	time_init();	//设置开机时间

	sched_init();	//调度程序初始化，kernel/sched.c
	buffer_init(buffer_memory_end);		//缓冲管理初始化，fs/buffer.c
	hd_init();	//硬盘初始化，kernel/blk_drv/hd.c
	floppy_init();	//软驱初始化，kernel/blk_drv/floppy.c

	sti();	//初始化完成，开启中断

	move_to_user_mode();	//移动到用户模式下执行，include/asm/system.h
	if (!fork()) {		/* we count on this going ok */
		init();		//在新建的子进程（任务1）中执行
	}

//下面的代码开始以任务0的身份运行
/*
 *   NOTE!!   For any other task 'pause()' would mean we have to get a
 * signal to awaken, but task0 is the sole exception (see 'schedule()')
 * as task 0 gets activated at every idle moment (when no other tasks
 * can run). For task0 'pause()' just means we go check if some other
 * task can run, and if not we return here.
 */
	for(;;) pause();
}

//产生格式化信息并输出到stdout（1）上
static int printf(const char *fmt, ...)
{
	va_list args;
	int i;

	va_start(args, fmt);
	write(1,printbuf,i=vsprintf(printbuf, fmt, args));
	va_end(args);
	return i;
}

//读取并执行/etc/rc文件时所使用的命令行参数和环境参数
static char * argv_rc[] = { "/bin/sh", NULL };	//调用执行程序时参数的字符串数组
static char * envp_rc[] = { "HOME=/", NULL };	//调用执行程序时的环境字符串数组

//运行登陆shell时所使用的命令行参数和环境参数
static char * argv[] = { "-/bin/sh",NULL };
static char * envp[] = { "HOME=/usr/root", NULL };

//该函数运行在任务0第1次创建的子进程（任务1）中。首先对第一个将要执行的程序（shell）的环境进行初始化，然后已登录shell方式加载该程序并执行之
void init(void)
{
	int pid,i;

	//读取硬盘参数、加载虚拟盘、安装根文件系统设备
	setup((void *) &drive_info);

	//以读写方式打开"/dev/tty0"（终端控制台）
	(void) open("/dev/tty0",O_RDWR,0);	//产生句柄0，对应stdin
	(void) dup(0);		//复制句柄，产生句柄1，对应stdout
	(void) dup(0);		//复制句柄，产生句柄2，对应stderr

	//打印缓冲区块数和总字节数，每块1024字节，以及主内存区空闲内存字节数
	printf("%d buffers = %d bytes buffer space\n\r",NR_BUFFERS,
		NR_BUFFERS*BLOCK_SIZE);
	printf("Free mem: %d bytes\n\r",memory_end-main_memory_start);

	//下面的fork()用于创建一个子进程（任务2）
	//对于被创建的子进程，fork返回0值；对于父进程，fork返回子进程的pid
	if (!(pid=fork())) {
		close(0);
		if (open("/etc/rc",O_RDONLY,0))
			_exit(1);
		execve("/bin/sh",argv_rc,envp_rc);
		_exit(2);
	}

	if (pid>0)
		while (pid != wait(&i))
			/* nothing */;
	while (1) {
		if ((pid=fork())<0) {
			printf("Fork failed in init\r\n");
			continue;
		}
		if (!pid) {
			close(0);close(1);close(2);
			setsid();
			(void) open("/dev/tty0",O_RDWR,0);
			(void) dup(0);
			(void) dup(0);
			_exit(execve("/bin/sh",argv,envp));
		}
		while (1)
			if (pid == wait(&i))
				break;
		printf("\n\rchild %d died with code %04x\n\r",pid,i);
		sync();		//同步操作，刷新缓冲区
	}
	_exit(0);	/* NOTE! _exit, not exit() */
}

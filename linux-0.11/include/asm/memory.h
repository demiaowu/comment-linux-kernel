/*
 *  NOTE!!! memcpy(dest,src,n) assumes ds=es=normal data segment. This
 *  goes for all kernel functions (ds=es=kernel space, fs=local data,
 *  gs=null), as well as for all well-behaving user programs (ds=es=
 *  user data space). This is NOT a bug, as any user program that changes
 *  es deserves to die if it isn't careful.
 */

// 内存块复制。从源地址src处开始复制n个字节到目的地址dest处
#define memcpy(dest,src,n) ({ \
void * _res = dest; \
__asm__ ("cld;rep;movsb" \	//从ds:[esi]复制到es:[edi]，并且esi++，edi++
	:\
	:"D" ((long)(_res)),"S" ((long)(src)),"c" ((long) (n)) \
	:"di","si","cx"); \
_res; \
})

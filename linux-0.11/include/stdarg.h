#ifndef _STDARG_H
#define _STDARG_H

//定义va_list为一个字符指针类型
typedef char *va_list;

/* Amount of space required in an argument list for an arg of type TYPE.
   TYPE may alternatively be an expression whose type is used.  */
//定义了取整后TYPE类型的字节长度值，是int长度的倍数
/*The sizeof operator yields the size (in bytes) of its operand, 
  which may be an expression or the parenthesized name of a type*/
#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

//SPARC:Scalable Processor ARChitecture
#ifndef __sparc__
//va_start宏初始化指针AP，使其指向传给函数的可变参数表的第一个参数
#define va_start(AP, LASTARG) 						\
 (AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#else
//__builtin_saveregs:由于某些函数参数的传递是通过寄存器来的，为了使可变函数参数机制成功，该宏将寄存器中的参数复制到内存中。
#define va_start(AP, LASTARG) 						\
 (__builtin_saveregs (),						\
  AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#endif

//va_end用于被调用函数完成一次正常返回
/*从一个使用过va_start()的函数中退出之前，必须调用一次va_end()。这是因为va_start可能以某种方式修改了堆栈，这种修改可能导致返回无法完成，va_end()能将有关的修改复原。 ——《C++程序设计语言》 第3版、特别版， p139
在大多数C实现上，调用va_end与否并无区别。但是，某些版本的va_start宏为了方便对va_list的遍历，就给参数列表动态分配内存。
这样一种C实现很可能利用va_end宏来释放此前动态分配的内存；如果忘记调用宏va_end，最后得到的程序可能在某些机型上没有问题，而在另一些机型上则发生“内存泄露”。  ——《C陷阱与缺陷》， p161
*/
void va_end (va_list);		/* Defined in gnulib */
#define va_end(AP)

//va_list宏会扩展成函数参数表中下一个参数的类型和值，每次调用va_arg都会修改AP，使得下一个参数值被返回
#define va_arg(AP, TYPE)						\
 (AP += __va_rounded_size (TYPE),					\
  *((TYPE *) (AP - __va_rounded_size (TYPE))))

#endif /* _STDARG_H */

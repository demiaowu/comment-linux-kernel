/*
 *  linux/kernel/mktime.c
 *
 *  (C) 1991  Linus Torvalds
 */

#include <time.h>

/*
 * This isn't the library routine, it is only used in the kernel.
 * as such, we don't care about years<1970 etc, but assume everything
 * is ok. Similarly, TZ etc is happily ignored. We just do everything
 * as easily as possible. Let's find something public for the library
 * routines (although I think minix times is public).
 */
/*
 * PS. I hate whoever though up the year 1970 - couldn't they have gotten
 * a leap-year instead? I also hate Gregorius, pope or no. I'm grumpy.
 */
#define MINUTE 60		//一分钟的秒数
#define HOUR (60*MINUTE)	//一小时的秒数
#define DAY (24*HOUR)		//一天的秒数
#define YEAR (365*DAY)		//一年的秒数

/* interestingly, we assume leap-years */
// 以年为界线，定义了每个月开始时的秒数时间
static int month[12] = {
	0,
	DAY*(31),
	DAY*(31+29),
	DAY*(31+29+31),
	DAY*(31+29+31+30),
	DAY*(31+29+31+30+31),
	DAY*(31+29+31+30+31+30),
	DAY*(31+29+31+30+31+30+31),
	DAY*(31+29+31+30+31+30+31+31),
	DAY*(31+29+31+30+31+30+31+31+30),
	DAY*(31+29+31+30+31+30+31+31+30+31),
	DAY*(31+29+31+30+31+30+31+31+30+31+30)
};

// 该函数计算从1970-01-01 00：00：00开始到开机时经过的秒数
long kernel_mktime(struct tm * tm)
{
	long res;	//存储从1970-01-01 00：00：00开始到开机时经过的秒数
	int year;	//存储从1970年到现在经过的年数

	year = tm->tm_year - 70;	//1970年到现在经过的年数
	
	/* magic offsets (y+1) needed to get leapyears right.*/
	//1970年起算，1972年是闰年。从1970年开始到现在经过的年数year中闰年年数是1+(year-3)/4，即(year+1)/4，取整
	res = YEAR*year + DAY*((year+1)/4);	//1970年到现在，考虑闰年后，经过的秒数
	res += month[tm->tm_mon];	//加上今年到本月经过的秒数

	/* and (y+2) here. If it wasn't a leap-year, we have to adjust */
	//如果(year+2)不是闰年，则减去一天的秒数
	if (tm->tm_mon>1 && ((year+2)%4))
		res -= DAY;

	res += DAY*(tm->tm_mday-1);	//加上本月过去的天数的总秒数
	res += HOUR*tm->tm_hour;	//加上当天过去的小时
	res += MINUTE*tm->tm_min;	//加上本小时已过去的分钟数
	res += tm->tm_sec;		//加上本分钟已过去的秒数
	return res;
}

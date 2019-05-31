/*
** $Id: lobject.h,v 2.117.1.1 2017/04/19 17:39:34 roberto Exp $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "lua.h"


/*
** Extra tags for non-values
*/
#define LUA_TPROTO	LUA_NUMTAGS		/* function prototypes */
#define LUA_TDEADKEY	(LUA_NUMTAGS+1)		/* removed keys in tables */

/* 原始的数据类型只要用低四位就可以表示了，因为原始数据类型不超过16种 */

/*
** number of all possible tags (including LUA_TNONE but excluding DEADKEY)
*/
#define LUA_TOTALTAGS	(LUA_TPROTO + 2)


/*
** tags for Tagged Values have the following use of bits:
** bits 0-3: actual tag (a LUA_T* value)
** bits 4-5: variant bits
** bit 6: whether value is collectable
*/


/*
** LUA_TFUNCTION variants:
** 0 - Lua function
** 1 - light C function
** 2 - regular C function (closure)
*/

/* Variant tags for functions */
#define LUA_TLCL	(LUA_TFUNCTION | (0 << 4))  /* Lua closure */
/* light C function是指没有upvalue的函数 */
#define LUA_TLCF	(LUA_TFUNCTION | (1 << 4))  /* light C function */
#define LUA_TCCL	(LUA_TFUNCTION | (2 << 4))  /* C closure */


/* Variant tags for strings */
/*
** 字符串类型的变种标记，由字符串原始类型按位或变种掩码组成，
** 其中0表示短字符串变种掩码，0x10表示长字符串变种掩码
*/
#define LUA_TSHRSTR	(LUA_TSTRING | (0 << 4))  /* short strings */
#define LUA_TLNGSTR	(LUA_TSTRING | (1 << 4))  /* long strings */


/* Variant tags for numbers */
/*
** 数据类型的变种标记，由数据原始类型按位或变种掩码组成，
** 其中0表示浮点型变种掩码，0x10表示整型变种掩码
** LUA_TNUMFLT是表示单精度浮点数，LUA_TNUMINT表示整型
*/

#define LUA_TNUMFLT	(LUA_TNUMBER | (0 << 4))  /* float numbers */
#define LUA_TNUMINT	(LUA_TNUMBER | (1 << 4))  /* integer numbers */


/* Bit mark for collectable types */
/* 垃圾回收的掩码（0x40） */
#define BIT_ISCOLLECTABLE	(1 << 6)

/* mark a tag as collectable */
/* 给需要进行垃圾回收操作的数据类型打标签 */
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)


/*
** Common type for all collectable objects
*/
typedef struct GCObject GCObject;


/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
/*
** 需要进行GC操作的数据类型都会有一个CommonHeader宏定义的成员，并且
** 该成员在结构体定义的最开始部分。
** next成员用于指向下一个GC链表的成员。
** tt表示数据类型，即lua.h中的那些宏。
** marked表示GC相关的标记位。
*/
#define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked


/*
** Common type has only the common header
*/
struct GCObject {
  CommonHeader;
};




/*
** Tagged Values. This is the basic representation of values in Lua,
** an actual value plus a tag with its type.
*/

/*
** Union of all Lua values
*/
/*
** lua中所有可能值的联合体，通过包含GCObject *类型的成员gc，
** 可以表示某个类型的值是否需要进行垃圾回收。
*/
typedef union Value {
  GCObject *gc;    /* collectable objects */
  void *p;         /* light userdata */
  int b;           /* booleans */
  lua_CFunction f; /* light C functions */
  lua_Integer i;   /* integer numbers */
  lua_Number n;    /* float numbers */
} Value;


/*
** TValue的成员定义，tt_表示值的原始数据类型，
** 原始数据类型由GC标记位、变种类型标记位和基本类型标记位组成。
** 如果不需要进行GC，则GC标记位为0，如果无变种类型，则变种标记位为0.
*/
#define TValuefields	Value value_; int tt_

/*
** TValua的定义
** TValue是lua中所有值的基本表示，由具体的值和一个关联的原始数据类型组成。
** 数据类型存放在TValuefields中的tt_成员中。lua内部的任何数据都用TValue来表示。
*/
typedef struct lua_TValue {
  TValuefields;
} TValue;



/* macro defining a nil value */
#define NILCONSTANT	{NULL}, LUA_TNIL

/* val_宏用于获取具体值内容 */
#define val_(o)		((o)->value_)


/* raw type tag of a TValue */
/* TValue的原始类型 */
#define rttype(o)	((o)->tt_)

/* tag with no variants (bits 0-3) */
#define novariant(x)	((x) & 0x0F)

/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5) */
/*
** TValua的类型标记，包括基本类型标记和变种类型标记，变种类型标记由第4，5位表示，
** 基本类型标记由低四位表示。基本类型标记其实就是lua.h中的那些类型宏。
*/
#define ttype(o)	(rttype(o) & 0x3F)

/* type tag of a TValue with no variants (bits 0-3) */
/* 获取基本类型标记。基本类型标记其实就是lua.h中的那些类型宏。 */
#define ttnov(o)	(novariant(rttype(o)))


/* Macros to test type */
#define checktag(o,t)		(rttype(o) == (t))
#define checktype(o,t)		(ttnov(o) == (t))
#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_TNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_TNUMINT)
#define ttisnil(o)		checktag((o), LUA_TNIL)
#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)
#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))
#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))
#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
#define ttisclosure(o)		((rttype(o) & 0x1F) == LUA_TFUNCTION)
#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))
#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
#define ttislcf(o)		checktag((o), LUA_TLCF)
#define ttisfulluserdata(o)	checktag((o), ctb(LUA_TUSERDATA))
#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))
#define ttisdeadkey(o)		checktag((o), LUA_TDEADKEY)


/* Macros to access values */
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)
#define nvalue(o)	check_exp(ttisnumber(o), \
	(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))
#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)

/* tsvalue()用于获取string的内容 */
#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))
#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))
#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)
#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))
/* a dead value may get the 'gc' field, but cannot access its contents */
#define deadvalue(o)	check_exp(ttisdeadkey(o), cast(void *, val_(o).gc))

#define l_isfalse(o)	(ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))

/*
** 检查给定类型是否需要进行垃圾回收，其实LUA_TSTRING之后的数据类型
** 都要进行垃圾回收。
*/
#define iscollectable(o)	(rttype(o) & BIT_ISCOLLECTABLE)


/* Macros for internal tests */
#define righttt(obj)		(ttype(obj) == gcvalue(obj)->tt)

#define checkliveness(L,obj) \
	lua_longassert(!iscollectable(obj) || \
		(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj)))))


/* Macros to set values */
/* 设置值的原始类型 */
#define settt_(o,t)	((o)->tt_=(t))

/* 设置TValue中的lua_Number类型成员f的值 */
#define setfltvalue(obj,x) \
  { TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_TNUMFLT); }

/* 修改TValue中的lua_Number类型成员f的值 */
#define chgfltvalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }

/* 设置TValue中的lua_Integer类型成员i的值 */
#define setivalue(obj,x) \
  { TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_TNUMINT); }

/* 修改TValue中的lua_Integer类型成员i的值 */
#define chgivalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }

#define setnilvalue(obj) settt_(obj, LUA_TNIL)

/* 设置TValue中的lua_CFunction类型成员f的值 */
#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }

/* 设置TValue中的void *类型成员p的值 */
#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }

/* 设置TValue中的int类型成员b的值 */
#define setbvalue(obj,x) \
  { TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }

/* 设置TValue中的GCObject* 类型成员gc的值 */
#define setgcovalue(L,obj,x) \
  { TValue *io = (obj); GCObject *i_g=(x); \
    val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }

#define setsvalue(L,obj,x) \
  { TValue *io = (obj); TString *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
    checkliveness(L,io); }

#define setuvalue(L,obj,x) \
  { TValue *io = (obj); Udata *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TUSERDATA)); \
    checkliveness(L,io); }

#define setthvalue(L,obj,x) \
  { TValue *io = (obj); lua_State *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTHREAD)); \
    checkliveness(L,io); }

#define setclLvalue(L,obj,x) \
  { TValue *io = (obj); LClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TLCL)); \
    checkliveness(L,io); }

#define setclCvalue(L,obj,x) \
  { TValue *io = (obj); CClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TCCL)); \
    checkliveness(L,io); }

#define sethvalue(L,obj,x) \
  { TValue *io = (obj); Table *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTABLE)); \
    checkliveness(L,io); }

#define setdeadvalue(obj)	settt_(obj, LUA_TDEADKEY)



#define setobj(L,obj1,obj2) \
	{ TValue *io1=(obj1); *io1 = *(obj2); \
	  (void)L; checkliveness(L,io1); }


/*
** different types of assignments, according to destination
*/

/* from stack to (same) stack */
#define setobjs2s	setobj
/* to stack (not from same stack) */
#define setobj2s	setobj
#define setsvalue2s	setsvalue
#define sethvalue2s	sethvalue
#define setptvalue2s	setptvalue
/* from table to same table */
#define setobjt2t	setobj
/* to new object */
#define setobj2n	setobj
#define setsvalue2n	setsvalue

/* to table (define it as an expression to be used in macros) */
#define setobj2t(L,o1,o2)  ((void)L, *(o1)=*(o2), checkliveness(L,(o1)))




/*
** {======================================================
** types and prototypes
** =======================================================
*/


typedef TValue *StkId;  /* index to stack elements */

/*
**
*/


/*
** Header for string value; string bytes follow the end of this structure
** (aligned according to 'UTString'; see next).
*/
/*
** TString类型是lua中string类型的头部，string类型的具体内容紧跟在头部之后，
** 并且在内存中需要按照8字节对齐。
*/
typedef struct TString {
  CommonHeader;		/* 所有类型的公共头部 */
  /*
  ** extra成员对于短字符串来说是保留字段，但是对于长字符串来说，
  ** 是用于表明长字符串是否已经计算了hash值的标志位，1表示已计算hash值。
  */
  lu_byte extra;  /* reserved words for short strings; "has hash" for longs */
  lu_byte shrlen;  /* length for short strings */ /* 短字符串的长度 */
  unsigned int hash;	/* 该string对应的hash值，由string进行hash后得到 */
  union {
    size_t lnglen;  /* length for long strings */ /* 长字符串的长度 */
    /*
    ** lua是用散列桶来存放string的，hnext用于指向下一个具有相同hash值的string，
    ** 即具有相同hash值的字符串是用链表来串接起来的。
    */
    struct TString *hnext;  /* linked list for hash table */
  } u;
} TString;


/*
** Ensures that address after this type is always fully aligned.
*/
/* 这个联合体用于确保string的起始地址肯定是8字节对齐的。 */
typedef union UTString {
  L_Umaxalign dummy;  /* ensures maximum alignment for strings */
  TString tsv;
} UTString;


/*
** Get the actual string (array of bytes) from a 'TString'.
** (Access to 'extra' ensures that value is really a 'TString'.)
*/
/* lua中的string内容是紧跟在string头部之后的，所以这里返回的是内容的起始地址 */
#define getstr(ts)  \
  check_exp(sizeof((ts)->extra), cast(char *, (ts)) + sizeof(UTString))


/* get the actual string (array of bytes) from a Lua value */
#define svalue(o)       getstr(tsvalue(o))

/* get string length from 'TString *s' */
#define tsslen(s)	((s)->tt == LUA_TSHRSTR ? (s)->shrlen : (s)->u.lnglen)

/* get string length from 'TValue *o' */
#define vslen(o)	tsslen(tsvalue(o))


/*
** Header for userdata; memory area follows the end of this structure
** (aligned according to 'UUdata'; see next).
*/
typedef struct Udata {
  CommonHeader;
  lu_byte ttuv_;  /* user value's tag */
  struct Table *metatable;
  size_t len;  /* number of bytes */
  union Value user_;  /* user value */
} Udata;


/*
** Ensures that address after this type is always fully aligned.
*/
typedef union UUdata {
  L_Umaxalign dummy;  /* ensures maximum alignment for 'local' udata */
  Udata uv;
} UUdata;


/*
**  Get the address of memory block inside 'Udata'.
** (Access to 'ttuv_' ensures that value is really a 'Udata'.)
*/
#define getudatamem(u)  \
  check_exp(sizeof((u)->ttuv_), (cast(char*, (u)) + sizeof(UUdata)))

#define setuservalue(L,u,o) \
	{ const TValue *io=(o); Udata *iu = (u); \
	  iu->user_ = io->value_; iu->ttuv_ = rttype(io); \
	  checkliveness(L,io); }


#define getuservalue(L,u,o) \
	{ TValue *io=(o); const Udata *iu = (u); \
	  io->value_ = iu->user_; settt_(io, iu->ttuv_); \
	  checkliveness(L,io); }


/*
** Description of an upvalue for function prototypes
*/
typedef struct Upvaldesc {
  TString *name;  /* upvalue name (for debug information) */
  lu_byte instack;  /* whether it is in stack (register) */
  lu_byte idx;  /* index of upvalue (in stack or in outer function's list) */
} Upvaldesc;


/*
** Description of a local variable for function prototypes
** (used for debug information)
*/
typedef struct LocVar {
  TString *varname;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} LocVar;


/*
** Function Prototypes
*/
/* Proto结构体用于存放函数的原型信息 */
typedef struct Proto {
  CommonHeader;

  /* 函数原型中固定参数的数量 */
  lu_byte numparams;  /* number of fixed parameters */
  lu_byte is_vararg;  /* 该函数是不是可变参数的 */

  /* 该函数所需要的函数调用栈的大小 */
  lu_byte maxstacksize;  /* number of registers needed by this function */

  /* Upvaldesc *upvalues数组的大小 */
  int sizeupvalues;  /* size of 'upvalues' */

  /* TValue *k数组的大小 */
  int sizek;  /* size of 'k' */

  /* 字节码占用的内存大小 */
  int sizecode;
  int sizelineinfo;

  /* struct Proto **p指针数组的大小 */
  int sizep;  /* size of 'p' */

  /* LocVar *locvars数组的大小 */
  int sizelocvars;
  int linedefined;  /* debug information  */
  int lastlinedefined;  /* debug information  */

  /* 存放该函数使用的所有常量 */
  TValue *k;  /* constants used by the function */

  /* code指向的是该函数对应的字节码存放的地址 */
  Instruction *code;  /* opcodes */

  /* p指向的是在这个函数内定义的其他函数所对应的Proto结构体指针数组 */
  struct Proto **p;  /* functions defined inside the function */
  int *lineinfo;  /* map from opcodes to source lines (debug information) */

  /* locvars存放了该函数使用到的本地变量（即在函数内部定义的局部变量）信息 */
  LocVar *locvars;  /* information about local variables (debug information) */

  /* upvalues指向存放了该函数使用到的所有自由变量的内存地址 */
  Upvaldesc *upvalues;  /* upvalue information */
  struct LClosure *cache;  /* last-created closure with this prototype */
  TString  *source;  /* used for debug information */
  GCObject *gclist;
} Proto;



/*
** Lua Upvalues
*/
typedef struct UpVal UpVal;


/*
** Closures
*/

#define ClosureHeader \
	CommonHeader; lu_byte nupvalues; GCObject *gclist

/* C Closure对象 */
typedef struct CClosure {
  ClosureHeader;
  /* 函数指针 */
  lua_CFunction f;
  /* C Closure对应的自由变量列表 */
  TValue upvalue[1];  /* list of upvalues */
} CClosure;


typedef struct LClosure {
  ClosureHeader;
  struct Proto *p; /* 存放lua闭包对应的函数原型信息 */

	/* lua闭包所使用的自由变量列表 */
  UpVal *upvals[1];  /* list of upvalues */
} LClosure;


typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;


#define isLfunction(o)	ttisLclosure(o)

#define getproto(o)	(clLvalue(o)->p)


/*
** Tables
*/
/*
** Lua使用表来表示lua中的一切数据，Lua表分为数组和散列表部分，其中数组的索引是从1开始，
** 散列表部分可以存放任何不能存储在数组中的数据，唯一的要求是散列表的键值不能是nil。
** 虽然lua表内部分为了数组和散列表部分，但其对于使用者来说是透明的，表提供的操作方法
** 对数组和散列表来说是通用的。
*/

/*
** 散列桶中存放的Node节点所对应的key节点，TKey是一个联合体。一般情况下，如果看到一个
** 数据类型是union，那么这个数据类型是想以一种更省内存的方式来表示多种用途。但某一时刻
** 这个数据类型只会是其中的某一种用途。当TKey表示的是nk的时候，那么nk中的TValuefields
** 用于存放key节点的具体内容及其内容对应的类型，而nk中的next则用于串联同属于一个散列桶
** 中的其他Node节点；当TKey表示tvk的时候，那么此时key节点只存放key的内容及其数据类型。
*/
typedef union TKey {
  struct {
    TValuefields;

    /* next保存的是Node节点在整个散列数组中的下标，即索引值 */
    int next;  /* for chaining (offset for next node) */
  } nk;
  TValue tvk;
} TKey;


/* copy a value into a key without messing up field 'next' */
#define setnodekey(L,key,obj) \
	{ TKey *k_=(key); const TValue *io_=(obj); \
	  k_->nk.value_ = io_->value_; k_->nk.tt_ = io_->tt_; \
	  (void)L; checkliveness(L,io_); }

/*
** 散列桶中存放的节点，包括key节点和value节点，其中value节点是通用数据
** 类型TValue，而key类型则为TKey。
*/
typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;


typedef struct Table {
  CommonHeader; /* 公共头部 */
  /*
  ** flags其实是一个unsigned char类型的数据，用于表示这个表提供了哪些元方法，
  ** 一开始的时候这个flags是0，当查找一次之后，如果该表中存在某个元方法，那么
  ** 将该元方法对应的bit置位，下一次查找时只要比较这个bit就行了。
  */
  lu_byte flags;  /* 1<<p means tagmethod(p) is not present */

  /* 散列表中散列桶数组大小对2取对数的值 */
  lu_byte lsizenode;  /* log2 of size of 'node' array */

  /* 数组部分的大小 */
  unsigned int sizearray;  /* size of 'array' array */

  /* 指向数组部分的指针 */
  TValue *array;  /* array part */

  /*
  ** 指向该表的散列桶数组起始位置的指针，从lsizenode的定义可以看出散列桶数组
  ** 的大小必须是2的整数次幂，利用lisizenode求散列桶数组大小的时候可以直接通过
  ** 1 << lsizenode来实现。
  */
  Node *node;

  /* 指向该表的散列桶数组的最后一个位置的指针 */
  Node *lastfree;  /* any free position is before this position */

  /* 存放该表的元表 */
  struct Table *metatable;

  /* GC相关的链表 */
  GCObject *gclist;
} Table;



/*
** 'module' operation for hashing (size is always a power of 2)
*/
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))


#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode))


/*
** (address of) a fixed nil value
*/
#define luaO_nilobject		(&luaO_nilobject_)


LUAI_DDEC const TValue luaO_nilobject_;

/* size of buffer for 'luaO_utf8esc' function */
#define UTF8BUFFSZ	8

LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
                           const TValue *p2, TValue *res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, StkId obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);


#endif


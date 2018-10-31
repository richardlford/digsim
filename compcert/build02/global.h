#ifndef GLOBAL
#define GLOBAL
typedef unsigned long long BOOL;
typedef float REAL;
typedef long long INT8;

typedef union {
  BOOL b;
  REAL r;
  INT8 i;
} CELL;

#define REAL_ARRAY_SIZE 100
extern CELL real_array[];
extern const BOOL FALSE;
extern const BOOL TRUE;

/*
# Table of CompCert Types and Sizes

| Name   | Type               | Size    | Range of values                                                        |
|--------|--------------------|---------|------------------------------------------------------------------------|
| UCHAR  | unsigned char      | 1 byte  | 0 to 255                                                               |
| SCHAR  | signed char        | 1 byte  | −128 to 127                                                            |
| CHAR   | char               | 1 byte  | like signed char on x86 like unsigned char on PowerPC, ARM, and RISC-V |
| U16    | unsigned short     | 2 bytes | 0 to 65535                                                             |
| S16    | signed short       | 2 bytes | −32768 to 32767                                                        |
| S16    | short              | 2 bytes | −32768 to 32767                                                        |
| U32    | unsigned int       | 4 bytes | 0 to 4294967295                                                        |
| S32    | signed int         | 4 bytes | −2147483648 to 2147483647                                              |
| U32    | int                | 4 bytes | −2147483648 to 2147483647                                              |
| U32    | unsigned long      | 4 bytes | 0 to 4294967295                                                        |
| S32    | signed long        | 4 bytes | −2147483648 to 2147483647                                              |
| S32    | long               | 4 bytes | −2147483648 to 2147483647                                              |
| U64    | unsigned long long | 8 bytes | 0 to 18446744073709551615                                              |
| S64    | signed long long   | 8 bytes | −9223372036854775808 to 9223372036854775807                            |
| S64    | long long          | 8 bytes | −9223372036854775808 to 9223372036854775807                            |
| BOOL8  | _Bool              | 1 byte  | 0 or 1                                                                 |

Floating-point types follow the IEEE 754-2008 standard [11]:

| Name    | Type        |  Representation           | Size    | Mantissa | Exponent      | Binary     |
|---------|-------------|---------------------------|---------|----------|---------------|------------|
| F32     | float       | IEEE 754 single precision | 4 bytes | 23 bits  | −126 to 127   | (binary32) |
| F64     | double      | IEEE 754 double precision | 8 bytes | 52 bits  | −1022 to 1023 | (binary64) |
| F128    | long double | not supported by default; with -flongdouble option, like double             |

*/

#endif // Global common declarations

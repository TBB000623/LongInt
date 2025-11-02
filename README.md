# LongInt helper version_0.1.0

Helper Document v_0.1.1

本说明文档兼容全部版本号高于 3.6.0 的历史文件, 直至下一个说明文档更新.

## 背景

本项目为参照 2011 年信息学竞赛冬令营 PPT《理性愉悦——高精度数值计算》实现的高精度计算结构体. 本项目采用 C++完成, 以期望打到一个没有矛盾的高精度计算环境.

## 文件目录与功能简介

-   LInt.h  
    本项目的核心头文件. 主要包括了本项目的核心结构体 LInt, 用于表示高精度整数.
    附带用于加速高精度乘法的 FFT 和循环卷积函数, 以及用于支援这两个函数计算的简单的复数结构体.
-   高精度计算.cpp  
    初代版本的高精度计算文件, 初次构建的时候使用的是完全的 C 语言, 仅仅实践了朴素的算法, 所有的运算操作全部依靠结构体的指针相关的函数完成.
    现已不再更新.
-   LongInt.cpp  
    用于 DEBUG 的总的调用文件.
-   LFixed.h  
    设想为构建一个高精度定点数的结构体. 尚未实现.
-   LFloat.h  
    高精度浮点数.
-   LMath.h  
    其它高精度相关的数学函数, 主要对标`<cmath>`.

## 主要文件内容

```cpp
using u32 = unsigned;
using i64 = long long int;
using u64 = long long unsigned;
```

### LInt.h

```cpp
struct LInt{
    short sign;
    int d;
    vector<u32> num;
} example;
```

LInt 结构体的具体成员内容.

-   `sign`用来存储符号位. 合法的值包括`{-2, -1, 0, 1, 2}`, 其中分别用于表示负无穷大、负常规数、`0`或`NaN`、正常规数和正无穷大.
-   `d`用于存储其数值部分的长度, 以万进位计算. 一个十进制位数为`D`的整数, 其在本结构表示中总是满足关系式`d = Ceiling[D/4]`, 其中`Ceiling`指向上取整函数.
-   `num`是具体数位数组的`std::vector<u32>`. 从`num[0]`到`num[d-1]`分别表示`example`从低到高的各个位数, 以每 4 位分组.
-   当`num.empty()`时, 总是认定此时的`d`应该为`0`, 此时合法的`sign`值包括`{-2, 0, 2}`, 分别表示负无穷大、`NaN`（无参数初始化时的默认状态）和正无穷大.
    本项目将上述三种状况称为“非正常状态”（但是是合法的状态）, 此时成员函数`abnormal()`将返回`true`.
-   与上述三种状态对应的另外三种合法状态: 负常规数, `0`和正常规数设定上总是认为`!num.empty()`（即便数位长度为 0）. 我们定义 `0` 为 `{sign = 0, d = 0, num.assign(1, 0);}`.
-   上述六种状态之外的全部其它状态（如`num.empty() && d != 0`）认定为不合法状态, 此时本结构运行结果为未定义. 理论上成员函数`void sho()`会检查结构状态, 并在结构非合法时将成员修改为`NaN`对应状态.

```cpp
LInt (void );
LInt (bool b, int code);
LInt (int b);
LInt (i64 b);
LInt (u64 b);
LInt (const char* inString);
LInt (const string &inString_);
LInt (const LInt &A);
template <typename It>
LInt(It begin_iter, It end_iter);
```

LInt 的构造函数

-   `LInt (void );`是默认构造函数, 返回`NaN`.
-   `LInt (bool b, int code);`
-   当`b == true`时返回`0`；
-   当`b == false`时返回非正常状态, `d = sgn(code) * 2`.

-   ````cpp
        LInt (int b);
        LInt (i64 b);
        LInt (u64 b);
        ```
    接收依整型数构造.
    ````

-   ````cpp
        LInt (const char* inString);
        LInt (const string &inString_);
        ```
    接收以字符串形式表示十进制整数的构造. 不接收`NaN`和`inf`的表示.
    ````

-   `LInt (const LInt &A);` 是复制构造函数.
-   `LInt(It begin_iter, It end_iter);`将从 `[begin_iter, end_iter)` 不加检查的作为 `num` 的内容.

```cpp
LInt & operator=(const LInt &B);
LInt & operator=(const char *inString);
inline LInt & operator=(bool b);
inline LInt & operator=(int i);
inline LInt & operator=(i64 i);
inline LInt & operator=(u64 u);
```

LInt 的赋值函数

-   `LInt & operator=(const LInt &B);` LInt 的复制函数.
-   `LInt & operator=(const char *inString);` 接收由数字数位字符串构造的整数, 实现上等价于`std::swap(LInt(inString))`.
-   剩下的赋值函数在实现上等价于构造后的复制函数.

```cpp
bool operator<(const LInt &B);
bool operator<(const LInt &B);
inline bool operator>(const LInt &B);
inline bool operator!=(const LInt &B);
inline bool operator>=(const LInt &B);
inline bool operator<=(const LInt &B);
```

LInt 的比较函数

-   实际上仅实现了`bool operator<(const LInt &B);`和`bool operator<(const LInt &B);`, 其余比较函数依这两个函数做了延伸, 以保证除了`NaN`以外的其它合法状态之间是全序的.
-   对`NaN`做了特化, 保证两个比较参数中若含有`NaN`则结果必然返回`false`.

```

```

#ifndef TBBLINT_H
#define TBBLINT_H

#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>  //version:3.6.0
#include <iterator>
#include <string>
#include <vector>
typedef unsigned long long u64;
typedef long long i64;
typedef int i32;
typedef unsigned u32;
namespace tbb {
using std::cin;
using std::cout;
using std::endl;
using std::string;

int Log_2(int base) {
	int i;
	for (i = 0; ((1 << i) < base) && (i < 32); i++);
	return i;
}
// structure complex, DFT, FFT & circulate conversion

// claim
struct complex;
class complex_expotional;
extern complex_expotional root_table;

void DFT(const std::vector<complex>&, std::vector<complex>&, int, bool inv);
void circ_conv(const std::vector<double>&, const std::vector<double>&, std::vector<double>&, int);

struct LInt {
	// elements
	short sign;
	int d;
	std::vector<u32> num;
	// define function/ initial
	LInt() : sign(0), d(0), num() {}
	LInt(bool, int);
	LInt(int);
	LInt(i64);
	LInt(u64);
	LInt(const char*);
	LInt(const u32*, int);
	template <typename It>
	LInt(It, It);
	LInt(const string& inString_);

	// compare operator
	bool operator<(const LInt& B) const;
	bool operator==(const LInt& B) const;
	bool operator>(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : B < A;
	}
	bool operator!=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A == B);
	}
	bool operator>=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A < B);
	}
	bool operator<=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A > B);
	}

	// functions
	LInt abs() const;
	inline LInt abs(const LInt& b) const { return b.abs(); }

	void print() const;
	string print_str() const;
	void show(std::ostream&) const;
	int digit() const;
	bool isNaN() const { return num.empty() && d == 0 && sign == 0; }
	bool positive() const { return sign > 0; }
	bool negative() const { return sign < 0; }
	bool isinf() const { return sign == 2 || sign == -2; }
	bool zero() const { return !num.empty() && sign == 0; }
	bool meanless() const { return isNaN() || isinf(); }
	bool abnormal() const { return zero() || isinf() || isNaN(); }
	void swap(LInt& other) {
		std::swap(sign, other.sign);
		std::swap(d, other.d);
		std::swap(num, other.num);
	}

   private:  // get 10000^2d/A while A >=0
	void sho();
	LInt recip() const;

   public:
	LInt div2() const;
	LInt pow2() const { return (*this) * (*this); }

	// Operator Function
	LInt operator<<(int k) const;
	LInt operator>>(int k) const;
	LInt operator-() const;
	LInt operator+(const LInt& B) const;
	LInt operator-(const LInt& B) const { return *this + (-B); }
	LInt operator*(int B) const;
	LInt operator*(const LInt& B) const;
	LInt operator/(int B) const;
	LInt operator/(const LInt& B) const;
	LInt operator%(int B) const;
	LInt operator%(const LInt& B) const;

	LInt& operator<<=(int k);
	LInt& operator>>=(int k);
	LInt& operator+=(const LInt& B);
	LInt& operator-=(const LInt& B);
	LInt& operator*=(int p);
	LInt& operator*=(const LInt& B);
	LInt& operator/=(int p);
	LInt& operator/=(const LInt& B);
	LInt& operator%=(int B);
	LInt& operator%=(const LInt& B);

	inline LInt& operator++(void) { return *this += 1; }
	inline LInt operator++(int) {
		LInt b = *this;
		*this += 1;
		return b;
	}
	inline LInt& operator--(void) { return *this -= 1; }
	inline LInt operator--(int) {
		LInt b = *this;
		*this -= 1;
		return b;
	}
	inline u32& operator[](int k) {
		static u32 wrong = 0;
		if (zero()) return wrong = 0;
		return num[k];
		/* When *this=0 here should throw an exception, because there
		 * isn't any list to return. But for a better experience, I just
		 * return a static variable.
		 */
	}
	// Friend Function for Other Classical Class
	friend LInt operator+(int A, const LInt& B) { return B + LInt(A); }
	friend LInt operator-(int A, const LInt& B) { return -B + LInt(A); }
	friend LInt operator*(int A, const LInt& B) { return B * A; }
	friend std::ostream& operator<<(std::ostream& os, const LInt& A);
	friend std::istream& operator>>(std::istream& is, LInt& A);
	friend void swap(LInt& A, LInt& B) { A.swap(B); }
#if __cplusplus >= 201103L
	// converse to other classical type
	explicit operator bool() const { return isinf() || isNaN(); }
	explicit operator int() const {
		int temp = 0;
		for (int i = 0; i < d; i++) temp = temp * 10000 + num[i];
		if (sign < 0) temp = -temp;
		return temp;
	}
#endif
};

template <typename T = LInt>
T mul_pow10(const T&, int);

template <typename T = LInt>
T pow10(int);

template <>
LInt mul_pow10(const LInt& A, int k) {
	// return A*10^k
	if (A.isNaN() || A.zero() || A.isinf()) return A;
	int t1, divi, res;
	res = (k % 4 + 4) % 4;
	divi = (k - res) / 4;
	switch (res) {
		case (0): t1 = 1; break;
		case (1): t1 = 10; break;
		case (2): t1 = 100; break;
		default:  t1 = 1000; break;
	}
	return (divi >= 0) ? ((A * t1) << divi) : ((A * t1) >> -divi);
}
LInt mul_pow10(const int& a, int k) { return mul_pow10(LInt(a), k); }

template <>
LInt pow10(int k) {
	return mul_pow10(1, k);
}
}  // namespace tbb

#endif

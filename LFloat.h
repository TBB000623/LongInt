#ifndef TBBLFLT_H  // LFloat.h ver 3.4.1
#define TBBLFLT_H

#ifndef TBBLINT_H
#include "LInt.h"
#endif

#ifdef TBBLINT_H
#include <climits>
#include <sstream>
namespace tbb {

extern int _LFloat_prec;
struct LFloat;

extern const LFloat _LFloat_nan;

struct LFloat {
	LInt base;
	int pow;
	// define function
	LFloat(void);
	LFloat(bool, int = 0);
	LFloat(int);
	LFloat(i64);
	LFloat(u64);
	LFloat(float);
	LFloat(double);
	LFloat(long double);
	LFloat(const char*);
	LFloat(const string& S);
	LFloat(const LInt& _b, int _p = 0);

   public:
	// adjust LFloat.base to precision
	bool isNaN(void) const;
	bool positive(void) const;
	bool negative(void) const;
	bool isinf(void) const;
	bool zero(void) const;
	bool meanless(void) const;
	bool abnormal(void) const;
	void sho(void);
	int order(void) const;
	void print(void) const;
	const string print_str(void) const;
	// overload operator to calc
	LFloat operator+(void) const;
	LFloat operator-(void) const;
	LFloat operator+(LFloat) const;
	LFloat operator-(LFloat) const;
	LFloat operator*(LFloat) const;
	LFloat operator/(LFloat) const;
	LFloat& operator+=(LFloat);
	LFloat& operator-=(LFloat);
	LFloat& operator*=(LFloat);
	LFloat& operator/=(LFloat);

	LFloat operator*(int) const;
	LFloat operator/(int) const;
	// compare operator
	inline bool operator==(LFloat) const;
	inline bool operator!=(LFloat) const;
	bool operator<(LFloat) const;
	inline bool operator>(LFloat) const;
	inline bool operator<=(LFloat) const;
	inline bool operator>=(LFloat) const;
	// other functions
	LFloat pow2() const;
	// precision for LFloat
	static int precision();
	static int precision(int);
};

// IO operators
std::ostream& operator<<(std::ostream&, const tbb::LFloat&);
std::istream& operator>>(std::istream&, tbb::LFloat&);

LFloat Div_LInt(LInt, LInt);
}  // namespace tbb

#endif  // TBBLINT_H

#endif

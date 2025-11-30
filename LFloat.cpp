#include "LFloat.h"

#include <cmath>
#include <cstring>
#include <limits>
#include <sstream>

#include "LInt.h"
#include "LInt_utils.hpp"

namespace tbb {

// global variables
int _LFloat_prec = 2000;
const LFloat _LFloat_nan;

// construction
LFloat::LFloat(void) : base(), pow(0) {}
LFloat::LFloat(bool b, int code) : base(b, code), pow(0) {}
LFloat::LFloat(int i) : base(i), pow(0) {
	int zero = 0;
	for (zero = 0; zero < base.d && base.num[zero] == 0; zero++);
	base >>= zero;
	pow += zero;
}
LFloat::LFloat(i64 i) : base(i), pow(0) {
	int zero = 0;
	for (zero = 0; zero < base.d && base.num[zero] == 0; zero++);
	base >>= zero;
	pow += zero;
}
LFloat::LFloat(u64 u) : base(u), pow(0) {
	int zero = 0;
	for (zero = 0; zero < base.d && base.num[zero] == 0; zero++);
	base >>= zero;
	pow += zero;
}
LFloat::LFloat(double d) : base(false), pow(0) {
	// Handle special values first
	if (std::isnan(d)) {
		base = LInt(false);  // NaN
		pow = 0;
		return;
	}
	if (std::isinf(d)) {
		base = LInt(false, std::signbit(d) ? -1 : 1);  // -inf / +inf
		pow = 0;
		return;
	}
	if (d == 0.0) {
		base = LInt(0);
		pow = 0;
		return;
	}

	// Decompose IEEE-754 double into sign, exponent and mantissa
	u64 bits = 0;
	std::memcpy(&bits, &d, sizeof(bits));
	bool neg = ((bits >> 63) & 1ULL) != 0;
	int exp = int((bits >> 52) & 0x7FFULL);
	u64 frac = bits & ((1ULL << 52) - 1ULL);

	// Build integer mantissa and binary exponent such that
	// value = mantissa * 2^{bin_exp}
	u64 mant_u64 = 0;
	int bin_exp = 0;
	if (exp == 0) {  // subnormal
		mant_u64 = frac;
		bin_exp = 1 - 1023 - 52;
	} else {  // normal
		mant_u64 = (1ULL << 52) | frac;
		bin_exp = exp - 1023 - 52;
	}

	LInt A(mant_u64);
	if (neg) A.sign = -A.sign;

	// decimal exponent (in base 10) offset
	int dec_exp = 0;

	if (bin_exp >= 0) {
		// Multiply A by 2^{bin_exp} using fast exponentiation
		LInt mul(1);
		LInt cur(2);
		int e = bin_exp;
		while (e > 0) {
			if (e & 1) mul *= cur;
			cur *= cur;
			e >>= 1;
		}
		A *= mul;
	} else {
		// 1 / 2^n = 5^n / 10^n -> multiply numerator by 5^n and
		// subtract n from decimal exponent
		int n = -bin_exp;
		LInt mul(1);
		LInt cur(5);
		int e = n;
		while (e > 0) {
			if (e & 1) mul *= cur;
			cur *= cur;
			e >>= 1;
		}
		A *= mul;
		dec_exp -= n;
	}

	// Convert decimal exponent into groups of 4 digits (this->pow counts 10^4 groups)
	int exp_l = ((dec_exp % 4) + 4) % 4;
	if (exp_l != 0) A *= pow10(exp_l);
	dec_exp -= exp_l;
	pow = dec_exp / 4;
	base = A;
	sho();
}

// Construct from float (single precision) without using streams
LFloat::LFloat(float f) : base(false), pow(0) {
	if (std::isnan(f)) {
		base = LInt(false);
		pow = 0;
		return;
	}
	if (std::isinf(f)) {
		base = LInt(false, std::signbit(f) ? -1 : 1);
		pow = 0;
		return;
	}
	if (f == 0.0f) {
		base = LInt(0);
		pow = 0;
		return;
	}

	// Use frexp to get mantissa and exponent in binary
	int e = 0;
	long double m = std::frexpf(f, &e);
	int digits = std::numeric_limits<float>::digits;  // typically 24

	// Build integer mantissa by extracting binary digits
	LInt A(0);
	long double mm = fabsl(m);
	for (int i = 0; i < digits; ++i) {
		mm *= 2.0L;
		A *= 2;  // multiply by 2 (bit shift)
		if (mm >= 1.0L) {
			A += LInt(1);
			mm -= 1.0L;
		}
	}

	int bin_exp = e - digits;
	if (f < 0) A.sign = -A.sign;

	int dec_exp = 0;
	if (bin_exp >= 0) {
		LInt mul(1);
		LInt cur(2);
		int ee = bin_exp;
		while (ee > 0) {
			if (ee & 1) mul *= cur;
			cur *= cur;
			ee >>= 1;
		}
		A *= mul;
	} else {
		int n = -bin_exp;
		LInt mul(1);
		LInt cur(5);
		int ee = n;
		while (ee > 0) {
			if (ee & 1) mul *= cur;
			cur *= cur;
			ee >>= 1;
		}
		A *= mul;
		dec_exp -= n;
	}

	int exp_l = ((dec_exp % 4) + 4) % 4;
	if (exp_l != 0) A *= pow10(exp_l);
	dec_exp -= exp_l;
	pow = dec_exp / 4;
	base = A;
	sho();
}

// Construct from long double without using streams (build mantissa via frexpl)
LFloat::LFloat(long double ld) : base(false), pow(0) {
	if (std::isnan(ld)) {
		base = LInt(false);
		pow = 0;
		return;
	}
	if (std::isinf(ld)) {
		base = LInt(false, std::signbit(ld) ? -1 : 1);
		pow = 0;
		return;
	}
	if (ld == 0.0L) {
		base = LInt(0);
		pow = 0;
		return;
	}

	int e = 0;
	long double m = std::frexpl(ld, &e);                    // ld = m * 2^e, m in [0.5, 1)
	int digits = std::numeric_limits<long double>::digits;  // platform dependent

	LInt A(0);
	long double mm = fabsl(m);
	for (int i = 0; i < digits; ++i) {
		mm *= 2.0L;
		A *= 2;
		if (mm >= 1.0L) {
			A += LInt(1);
			mm -= 1.0L;
		}
	}

	int bin_exp = e - digits;
	if (ld < 0) A.sign = -A.sign;

	int dec_exp = 0;
	if (bin_exp >= 0) {
		LInt mul(1);
		LInt cur(2);
		int ee = bin_exp;
		while (ee > 0) {
			if (ee & 1) mul *= cur;
			cur *= cur;
			ee >>= 1;
		}
		A *= mul;
	} else {
		int n = -bin_exp;
		LInt mul(1);
		LInt cur(5);
		int ee = n;
		while (ee > 0) {
			if (ee & 1) mul *= cur;
			cur *= cur;
			ee >>= 1;
		}
		A *= mul;
		dec_exp -= n;
	}

	int exp_l = ((dec_exp % 4) + 4) % 4;
	if (exp_l != 0) A *= pow10(exp_l);
	dec_exp -= exp_l;
	pow = dec_exp / 4;
	base = A;
	sho();
}

struct FloatParseResult {
	bool valid;
	char sign;
	int integer_position;
	int dot_position;
	int fractional_position;
	int mantissa_end_position;
	int exponent_position;
	int end_position;
	std::string mantissa_string;
};

FloatParseResult parse_float_string(const char* str) {
	FloatParseResult result;
	result.valid = true;
	result.sign = 0;
	result.integer_position = -1;
	result.dot_position = -1;
	result.fractional_position = -1;
	result.mantissa_end_position = -1;
	result.exponent_position = -1;
	result.end_position = -1;
	result.mantissa_string = "";

	const char* p = str;
	// parse sign
	if (*p == '+' || *p == '-') {
		result.sign = *p;
		p++;
	} else {
		result.sign = '+';
	}
	// parse integer part
	bool has_integer = false;
	while (isdigit(*p)) {
		if (!has_integer) result.integer_position = p - str;
		has_integer = true;
		p++;
	}
	if (has_integer) {
		result.mantissa_string.append(str + result.integer_position, p - (str + result.integer_position));
		result.mantissa_end_position = p - str;
	}

	bool has_fractional = false;
	if (*p == '.') {
		result.dot_position = p - str;
		p++;
		while (isdigit(*p)) {
			if (!has_fractional) result.fractional_position = p - str;
			has_fractional = true;
			p++;
		}
		if (has_fractional) {
			result.mantissa_string.append(str + result.fractional_position, p - (str + result.fractional_position));
			result.mantissa_end_position = p - str;
		}
	}

	// 必须至少有整数或小数部分
	if (!has_integer && !has_fractional) {
		result.valid = false;
		return result;
	}

	// parse exponent part
	if (*p == 'e' || *p == 'E') {
		const char* exp_start = p;
		p++;
		if (*p == '+' || *p == '-') {
			p++;
		}
		// 检查 E 后是否有数字
		const char* exp_digit_start = p;
		while (isdigit(*p)) {
			p++;
		}
		// 只有 E 后有数字时才算有效指数
		if (p > exp_digit_start) {
			result.exponent_position = exp_start - str;
		} else {
			// E 后没有数字，回退到 E 之前
			p = exp_start;
		}
	}
	result.end_position = p - str;
	return result;
}

LFloat::LFloat(const char* float_string) : base(false), pow(0) {
	FloatParseResult parse_result = parse_float_string(float_string);
	if (!parse_result.valid) return;

	int exp_value = 0;
	if (parse_result.exponent_position != -1) {
		exp_value = std::strtol(float_string + parse_result.exponent_position + 1, nullptr, 10);
	}
	if (parse_result.fractional_position != -1) {
		exp_value -= (parse_result.mantissa_end_position - parse_result.fractional_position);
	}

	int exp_l = ((exp_value % 4) + 4) % 4;
	exp_value -= exp_l;

	string num_str(1, parse_result.sign);
	num_str += parse_result.mantissa_string;
	num_str.append(exp_l, '0');

	base = LInt(num_str);
	pow = exp_value / 4;
	sho();
}
LFloat::LFloat(string S) { *this = S.c_str(); }
LFloat::LFloat(const LInt& _b, int _p) : base(_b), pow(_p) { sho(); }

// basic proporties
bool LFloat::isNaN(void) const { return base.isNaN(); }
bool LFloat::positive(void) const { return base.positive(); }
bool LFloat::negative(void) const { return base.negative(); }
bool LFloat::isinf(void) const { return base.isinf(); }
bool LFloat::zero(void) const { return base.zero() && pow == 0; }
bool LFloat::meanless(void) const { return base.meanless(); }
bool LFloat::abnormal(void) const { return base.abnormal(); }

void LFloat::sho(void) {
	int prec = _LFloat_prec;
	if (base.abnormal()) {
		pow = 0;
		return;
	}
	if (base.d > prec) {
		int delta = base.d - prec;
		if (i64(pow) + delta > i64(INT_MAX)) pow = 0, base = LInt(false, base.sign);
		else base >>= delta, pow += delta;
	}
	int end_0;
	for (end_0 = 0; end_0 < base.d; end_0++)
		if (base[end_0] != 0) break;
	if (end_0 > 0) {
		base >>= end_0;
		if (i64(pow) + end_0 > i64(INT_MAX)) pow = 0, base = LInt(false, base.sign);
		else pow += end_0;
	}
	return;
}
int LFloat::order(void) const {
	if (isNaN()) return 0;
	if (isinf()) return positive() ? INT_MAX : INT_MIN;
	if (zero()) return INT_MIN;
	return base.digit() + 4 * pow;
}

// print
void LFloat::print(void) const {
	int dgt = base.d;
	if (base.abnormal()) {
		base.print();
		return;
	}
	if (base.negative()) putchar('-');
	if (pow >= 0) {  // 1234.p4= 1.234e19
		fast_output(base[dgt - 1]);
		for (int i = dgt - 2; i >= 0; i--) fast_output_with_padding(base[i]);
		for (int i = 0; i < pow; i++) fputs("0000", stdout);
	} else if (-pow < dgt) {  // 2563 1740 p-1= 2563.174
		fast_output(base[dgt - 1]);
		for (int i = dgt - 2; i >= -pow; i--) fast_output_with_padding(base[i]);
		putchar('.');
		for (int i = -pow - 1; i > 0; i--) fast_output_with_padding(base[i]);
		fast_output_with_padding(base[0], true);
	} else {
		putchar('0');
		putchar('.');
		for (int i = 0; i < -pow - dgt; i++) fputs("0000", stdout);
		for (int i = dgt - 1; i > 0; i--) fast_output_with_padding(base[i]);
		fast_output_with_padding(base[0], true);
	}
}
const string LFloat::print_str(void) const {
	int dgt = base.d;
	if (base.abnormal()) {
		return base.print_str();
	}
	string ans;
	ans.reserve(4 * dgt + 10);
	if (base.negative()) ans += '-';
	if (pow >= 0) {
		ans += u32_to_str(base[dgt - 1]);
		for (int i = dgt - 2; i >= 0; i--) ans += u32_to_zero_padded_string(base[i]);
		for (int i = 0; i < pow; i++) ans += "0000";
	} else if (-pow < dgt) {  // 2563 1740 p-1= 2563.174
		ans += u32_to_str(base[dgt - 1]);
		for (int i = dgt - 2; i >= -pow; i--) ans += u32_to_zero_padded_string(base[i]);
		ans += '.';
		for (int i = -pow - 1; i > 0; i--) ans += u32_to_zero_padded_string(base[i]);
		ans += u32_to_zero_padded_string(base[0], true);
	} else {
		ans += "0.";
		for (int i = 0; i < -pow - dgt; i++) ans += "0000";
		for (int i = dgt - 1; i > 0; i--) ans += u32_to_zero_padded_string(base[i]);
		ans += u32_to_zero_padded_string(base[0], true);
	}
	return ans;
}

// unary operation
LFloat LFloat::operator+(void) const { return *this; }
LFloat LFloat::operator-(void) const {
	LFloat ans(*this);
	ans.base.sign = -ans.base.sign;
	return ans;
}

// binary operation
LFloat LFloat::operator+(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	if (A.zero() || B.zero()) return A.zero() ? B : A;
	if (A.isinf()) return (A.base.sign == -B.base.sign) ? false : A;
	if (B.isinf()) return (B.base.sign == -A.base.sign) ? false : B;
	if (abs(A.pow - B.pow) > 2 * _LFloat_prec) return (A.pow > B.pow) ? A : B;
	LFloat ans;
	if (A.pow < B.pow) {
		ans.pow = A.pow, ans.base = (B.base << (B.pow - A.pow)) + A.base;
	} else {
		ans.pow = B.pow, ans.base = (A.base << (A.pow - B.pow)) + B.base;
	}
	ans.sho();
	return ans;
}

LFloat LFloat::operator-(LFloat B) const { return (*this) + (-B); }
LFloat LFloat::operator*(LFloat B) const {
	const LFloat& A = *this;
	// static int u= 0;
	// u++;	std::cerr<<u<<endl;
	if (A.abnormal() || B.abnormal()) return LFloat(A.base * B.base, 0);
	LInt ans_base = A.base * B.base;
	i64 ans_pow = i64(A.pow) + i64(B.pow);
	if (ans_pow > INT_MAX) return LFloat(LInt(false, ans_base.sign), 0);
	if (ans_pow < INT_MIN) {
		if (ans_pow + ans_base.d < INT_MIN) return 0;
		return LFloat(ans_base >> (INT_MIN - ans_pow), INT_MIN);
	}
	return LFloat(ans_base, ans_pow);
}

LFloat LFloat::operator/(LFloat B) const {
	const LFloat& A = *this;
	int n = _LFloat_prec;
	if (A.abnormal() || B.abnormal()) return LFloat(A.base / B.base, 0);
	LInt a = A.base, b = B.base;
	i64 p = A.pow, q = B.pow;
	p -= 2 * n - a.d, a <<= (2 * n - a.d);
	q -= n - b.d, b <<= (n - b.d);
	LInt c = a / b;
	i64 r = p - q;
	if (r > INT_MAX) return LFloat(LInt(false, c.sign), 0);
	if (r < INT_MIN) {
		if (r + c.d < INT_MIN) return 0;
		return LFloat(c >> (INT_MIN - r), INT_MIN);
	}
	return LFloat(c, r);
}

// comparison
bool LFloat::operator==(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	return (A.base == B.base) && (A.pow == B.pow);
}
bool LFloat::operator!=(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return true;
	return !(A == B);
}
bool LFloat::operator<(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	if (A.pow == B.pow) return A.base < B.base;
	if (A.isinf() || B.isinf()) return A.base < B.base;
	if (A.pow < B.pow) {
		LInt A_t = A.base >> (B.pow - A.pow);
		return A_t < B.base;
	} else {
		LInt B_t = B.base >> (A.pow - B.pow);
		return A.base < B_t;
	}
}
bool LFloat::operator>(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	return B < A;
}
bool LFloat::operator<=(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	return !(A > B);
}
bool LFloat::operator>=(LFloat B) const {
	const LFloat& A = *this;
	if (A.isNaN() || B.isNaN()) return false;
	return !(A < B);
}

// shorthand assignment operation
LFloat& LFloat::operator+=(LFloat B) { return *this = *this + B; }
LFloat& LFloat::operator-=(LFloat B) { return *this = *this - B; }
LFloat& LFloat::operator*=(LFloat B) { return *this = *this * B; }
LFloat& LFloat::operator/=(LFloat B) { return *this = *this / B; }

// binary operation with other P.O.D.
LFloat LFloat::operator*(int B) const {
	const LFloat& A = *this;
	if (A.isNaN() || A.zero()) return A;
	if (A.isinf() && B != 0) return B > 0 ? A : -A;
	if (A.isinf() && B == 0) return tbb::LInt(false);
	return LFloat(A.base * B, A.pow);
}

LFloat LFloat::operator/(int B) const {
	const LFloat& A = *this;
	if (A.isNaN() || (A.zero() && B == 0)) return tbb::LInt(false);  // return NaN
	if (A.isinf()) return B >= 0 ? A : -A;                           // return inf
	if (A.zero()) return 0;
	if (B == 0) return tbb::LInt(false, B);  // return inf;

	int k = _LFloat_prec + 3 - A.base.d;
	return LFloat((A.base << k) / B, A.pow - k);
}

LFloat operator+(double A, LFloat B) { return LFloat(A) + B; }
LFloat operator-(double A, LFloat B) { return LFloat(A) - B; }
LFloat operator*(double A, LFloat B) { return LFloat(A) * B; }
LFloat operator/(double A, LFloat B) { return LFloat(A) / B; }

// IO operators
std::ostream& operator<<(std::ostream& os, const tbb::LFloat& A) {
	if (!os) return os;
	if (A.abnormal()) {
		if (A.zero()) os.put('0');
		else if (A.isNaN()) os.write("NaN", 3);
		else {
			if (A.negative()) os.put('-');
			os.write("inf", 3);
		}
		return os;
	}
	// sign char
	tbb::LFloat B;
	if (A.negative()) os.put('-'), B = -A;
	else B = A;
	// get format flags
	int p = std::max(int(os.precision()), 0);
	std::ios_base::fmtflags flag = os.flags() & std::ios_base::floatfield;
	if (flag == std::ios_base::fixed) {                                  // fixed mode
		tbb::LInt B_normal = tbb::mul_pow10(B.base, 4 * B.pow + p + 1);  // A= B*10^-p; B has a digit after point
		B_normal = tbb::mul_pow10(B_normal, -1) + int((B_normal[0] % 10) >= 5);
		if (B_normal.zero()) {
			os.put('0');
			if (p > 0) {
				os.put('.');
				for (int i = 0; i < p; ++i) os.put('0');
			}
			return os;
		}  // if A=0 after scaled
		std::string B_str = B_normal.print_str();
		size_t len = B_str.length();
		if (len <= size_t(p)) {
			os.put('0');
			os.put('.');
			for (size_t t = 0; t < p - len; ++t) os.put('0');
			for (size_t t = 0; t < len; ++t) os.put(B_str[t]);
		} else {
			for (size_t i = 0; i < len - p; ++i) os.put(B_str[i]);
			if (p > 0) {
				os.put('.');
				for (int t = 0; t < p; ++t) os.put(B_str[len - p + t]);
			}
		}
		return os;
	} else if (flag == std::ios_base::scientific) {  // scientific mode
		int power_d = B.base.digit() - (p + 1) - 1;
		tbb::LInt B_normal = tbb::mul_pow10(B.base, (p + 1) - B.base.digit() + 1);
		if ((B_normal[0] % 10) >= 5) B_normal += 10;
		power_d += B_normal.digit() - (p + 1);
		B_normal = tbb::mul_pow10(B_normal, (p + 1) - B_normal.digit());
		// B_normal != 0
		std::string B_str = B_normal.print_str();
		os.put(B_str[0]);
		if (p > 0) {
			os.put('.');
			for (int t = 1; t <= p; ++t) os.put(B_str[t]);
		}
		os.put('e');
		int e_power = 4 * B.pow + power_d + p;
		os.put(e_power >= 0 ? '+' : '-');
		auto old_fill = os.fill('0');
		os << std::setw(3) << std::abs(e_power);
		os.fill(old_fill);
		return os;
	} else {  // default mode
		os << B.print_str();
	}
	return os;
}
std::istream& operator>>(std::istream& is, tbb::LFloat& A) {
	using std::isdigit;
	std::string init_str;
	is >> std::ws;
	// check stream safety
	{
		if (is.peek() == -1) {
			is.setstate(std::ios_base::eofbit);
			A = 0;
			return is;
		}
		if (!is) {
			is.setstate(std::ios_base::failbit);
			A = 0;
			return is;
		}
		char t = is.peek();
		while (t != '-' && t != '+' && !isdigit(t) && t != -1) {
			is.get();
			t = is.peek();
		}  // Ignore all char except '+', '-' and digit
		if (t == -1) {
			is.setstate(std::ios_base::failbit);
			A = 0;
			return is;
		}
	}
	if (is.peek() == '+' || is.peek() == '-') init_str.push_back(is.get());
	while (isdigit(is.peek())) init_str.push_back(is.get());
	if (is.peek() == '.') {
		init_str.push_back(is.get());
		while (isdigit(is.peek())) init_str.push_back(is.get());
		init_str.push_back('0');
	}
	if (is.peek() == 'e' || is.peek() == 'E') {
		char exp = is.get(), sign = 0;
		if (is.peek() == '+' || is.peek() == '-') sign = is.get();
		if (!isdigit(is.peek())) {
			if (!sign) is.putback(sign);
			is.putback(exp);
		} else {
			init_str.push_back(exp);
			if (sign) init_str.push_back(sign);
			while (isdigit(is.peek())) init_str.push_back(is.get());
		}
	}
	A = tbb::LFloat(init_str);
	return is;
}

LFloat LFloat::pow2() const {
	if (abnormal()) return LFloat(base.pow2(), 0);
	LInt ans_base = base.pow2();
	i64 ans_pow = 2 * i64(pow);
	if (ans_pow > INT_MAX) return LFloat(LInt(false, ans_base.sign), 0);
	if (ans_pow < INT_MIN) {
		if (ans_pow + ans_base.d < INT_MIN) return 0;
		return LFloat(ans_base >> (INT_MIN - ans_pow), INT_MIN);
	}
	return LFloat(ans_base, ans_pow);
}

// precsion
int LFloat::precision() { return tbb::_LFloat_prec; }
int LFloat::precision(int i) {
	int bef = tbb::_LFloat_prec;
	tbb::_LFloat_prec = i;
	return bef;
}

LFloat Div_LInt(LInt numerator, LInt denominator) { return LFloat(numerator) / LFloat(denominator); }

}  // namespace tbb
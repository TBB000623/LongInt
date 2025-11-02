#ifndef TBBLINT_H
#define TBBLINT_H

#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
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
struct complex {
	double x, y;

	inline double abs() const { return std::sqrt(((*this) * conj()).x); }
	inline complex conj() const { return {x, -y}; }
	inline complex left() const { return {-y, x}; }
	inline complex right() const { return {y, -x}; }
	inline complex operator-(void) const { return {-x, -y}; }
	inline complex operator+(const double& B) const { return {x + B, y}; }
	inline complex operator-(const double& B) const { return {x - B, y}; }
	inline complex operator*(const double& B) const { return {x * B, y * B}; }
	inline complex operator/(const double& B) const { return {x / B, y / B}; }
	inline complex operator+(const complex& B) const { return {x + B.x, y + B.y}; }
	inline complex operator-(const complex& B) const { return {x - B.x, y - B.y}; }
	inline complex operator*(const complex& B) const { return {(x * B.x - y * B.y), (x * B.y + y * B.x)}; }
	inline complex operator/(const complex& B) const { return (*this) * B.conj() / (B * B.conj()).x; }
	inline complex& operator+=(const complex& B) { return *this = *this + B; }
	static complex complex_exp(int i, int s) {
		const double Pi = 3.14159265358979323846;
		return {std::cos(2 * Pi * i / s), std::sin(2 * Pi * i / s)};
	}
};

class complex_expotional {
	std::vector<tbb::complex> table;
	int N = 0;

   public:
	void precompute(int n, int recompute_period = 256) {
		if (n <= 0) {
			table.clear();
			N = 0;
			return;
		}
		if (N > 0 && N % n == 0) return;
		N = n;
		table.resize(N);

		if (recompute_period <= 0) recompute_period = 1;

		const long double PI = 3.141592653589793238462643383279502884L;
		long double theta = 2.0L * PI / (long double)N;
		long double cos_step = std::cos(theta);
		long double sin_step = std::sin(theta);

		for (int base = 0; base < N; base += recompute_period) {
			long double alpha = static_cast<long double>(base) * theta;
			long double cos_base = std::cos(alpha);
			long double sin_base = std::sin(alpha);

			int left_bound = base;
			int right_bound = std::min(N, base + recompute_period);
			for (int idx = left_bound; idx < right_bound; ++idx) {
				table[idx] = {static_cast<double>(cos_base), static_cast<double>(sin_base)};
				long double cos_next = cos_base * cos_step - sin_base * sin_step;
				long double sin_next = sin_base * cos_step + cos_base * sin_step;
				cos_base = cos_next, sin_base = sin_next;
			}
		}
	}

	tbb::complex operator()(int k, int n = 0) const {
		if (n == 0) {
			if (N <= 0) {
				return tbb::complex{1, 0};
			}
			int kk = k % N;
			if (kk < 0) kk += N;
			return table[kk];
		} else {
			if (n < 0) {
				return tbb::complex{1, 0};
			}
			int kk = k % n;
			if (kk < 0) kk += n;
			if (N > 0 && N % n == 0) {
				int idx = kk * (N / n);
				return table[idx];
			}
			return tbb::complex::complex_exp(kk, n);
		}
	}
};

complex_expotional root_table;

void DFT(const std::vector<complex>& A, std::vector<complex>& a, int n, bool inv = false) {
	using std::vector;
	if (n <= 0) return;
	if (n == 1) {
		a[0] = A[0];
		return;
	}
	vector<int> factor_list;
	struct {
		int operator()(int fnum, vector<int>& flist) {
			flist.clear();
			int n = 0;
			while (fnum % 4 == 0) flist.push_back(4), fnum /= 4;
			for (int i = 2; i * i <= fnum; ++i) {
				while (fnum % i == 0) flist.push_back(i), fnum /= i;
			}
			if (fnum != 1) flist.push_back(fnum);
			return flist.size();
		}
	} Factor;
	Factor(n, factor_list);
	static vector<int> rev{};
	static vector<complex> rt;
	static complex rt_mat[10][10];
	vector<complex> temp;
	if (rev.size() != n) {
		rev.assign(n, 0);
		rev[0] = 0;
		int rev_length = 1;
		for (int i = 0; i < factor_list.size(); ++i) {
			int scale = factor_list[i];
			for (int k = 0; k < rev_length; ++k) rev[k] *= scale;
			for (int j = 1; j < scale; ++j)
				for (int k = 0; k < rev_length; ++k) rev[j * rev_length + k] = j + rev[k];
			rev_length *= scale;
		}
	}
	if (rt.size() < n) rt.resize(n);
	if (temp.size() < n) temp.resize(n);

	for (register int i = 0; i < n; i++) temp[i] = A[rev[i]];

	for (int i = 0, size = 1; i < factor_list.size(); ++i) {
		int scale = factor_list[i], new_size = size * scale;
		// std::cerr << "scale: " << scale << std::endl;
		for (int j = 0; j < new_size; ++j) rt[j] = inv ? root_table(-j, new_size) : root_table(j, new_size);
		for (int u = 0; u < scale; ++u)
			for (int v = 0; v < scale; ++v) rt_mat[u][v] = inv ? root_table(-u * v, scale) : root_table(u * v, scale);
		for (int chuck_base = 0; chuck_base < n; chuck_base += new_size) {
			for (int t = 1; t < scale; ++t) {
				for (int j = 1; j < size; ++j) temp[chuck_base + j + t * size] = temp[chuck_base + j + t * size] * rt[j * t];
			}
			for (int j = 0; j < size; ++j) {
				if (scale == 2) {
					complex x_0, x_1;
					x_0 = temp[chuck_base + j];
					x_1 = temp[chuck_base + j + size];
					temp[chuck_base + j] = x_0 + x_1;
					temp[chuck_base + j + size] = x_0 - x_1;
				} else if (scale == 3) {
					complex x_0 = temp[chuck_base + j + 0 * size];
					complex x_1 = temp[chuck_base + j + 1 * size];
					complex x_2 = temp[chuck_base + j + 2 * size];
					complex s_1 = x_1 + x_2, y_1 = complex{-s_1.x / 2, -s_1.y / 2};  // y_1 = -(1/2) * (x_1 + x_2)
					complex s_2 = ((x_1 - x_2) * (std::sqrt(3) / 2)).left();         // s_2 = (\sqrt{3}/2) * (x_1 - x_2)
					complex y_2 = inv ? (-s_2) : s_2;
					temp[chuck_base + j + 0 * size] = x_0 + s_1;
					temp[chuck_base + j + 1 * size] = x_0 + y_1 + y_2;
					temp[chuck_base + j + 2 * size] = x_0 + y_1 - y_2;
				} else if (scale == 4) {
					complex x_0 = temp[chuck_base + j + 0 * size];
					complex x_1 = temp[chuck_base + j + 1 * size];
					complex x_2 = temp[chuck_base + j + 2 * size];
					complex x_3 = temp[chuck_base + j + 3 * size];
					complex a_02 = x_0 + x_2, s_02 = x_0 - x_2;
					complex a_13 = x_1 + x_3, s_13 = x_1 - x_3;
					temp[chuck_base + j + 0 * size] = a_02 + a_13;
					temp[chuck_base + j + 2 * size] = a_02 - a_13;
					temp[chuck_base + j + 1 * size] = s_02 + (inv ? s_13.right() : s_13.left());
					temp[chuck_base + j + 3 * size] = s_02 - (inv ? s_13.right() : s_13.left());
				} else {
					static complex rot[10];
					for (int t = 0; t < scale; ++t) rot[t] = temp[chuck_base + j + t * size];
					for (int t = 0; t < scale; ++t) {
						temp[chuck_base + j + t * size] = complex{0, 0};
						for (int ti = 0; ti < scale; ++ti) temp[chuck_base + j + t * size] += rt_mat[t][ti] * rot[ti];
					}
				}
			}
		}
		// for(int i = 0; i < n; ++i)	std::cerr << rev[i] << ' ' <<  temp[i] << std::endl;
		size = new_size;
	}

	if (inv)
		for (register int i = 0; i < n; i++) temp[i] = temp[i] / n;
	a = temp;
}

void circ_conv(const std::vector<double>& A, const std::vector<double>& B, std::vector<double>& C, int n) {
	using std::vector;

	if (n <= 1024) {
		for (register int t = 0; t < n; ++t) {
			C[t] = 0;
			for (int i = 0; i <= t; ++i) C[t] += A[i] * B[t - i];
			for (int i = n - 1; i >= t + 1; ++i) C[t] += A[i] * B[n + t - i];
		}
		return;
	}

	root_table.precompute(n / 2);

	typedef complex cmxd;
	static int last_n;
	static vector<double> A_0, B_0;
	static vector<cmxd> P, Q;
	static vector<cmxd> a_0, a_1, b_0, b_1;
	static vector<cmxd> c_0, c_1;
	bool checkA, checkB, checkAB;
	int it;
	if (last_n != n) {
		P.resize(n / 2), Q.resize(n / 2);
		a_0.resize(n / 2), a_1.resize(n / 2), b_0.resize(n / 2), b_1.resize(n / 2);
		c_0.resize(n / 2), c_1.resize(n / 2);
	}
	checkA = (A_0 == A);
	if (!checkA) {
		A_0 = A;
		for (register int i = 0; i < n / 2; i++) P[i] = cmxd{A[i << 1], A[(i << 1) | 1]};
		DFT(P, P, n / 2, false);
		Q[0] = P[0].conj();
		for (register int i = 1; i < n / 2; i++) Q[i] = P[n / 2 - i].conj();
		for (register int i = 0; i < n / 2; i++) a_0[i] = (P[i] + Q[i]) / 2, a_1[i] = ((P[i] - Q[i]) / 2).left();
	}

	checkAB = (A == B);
	if (checkAB) b_0 = a_0, b_1 = a_1;
	else {
		checkB = (B_0 == B);
		if (!checkB) {
			B_0 = B;
			for (register int i = 0; i < n / 2; i++) P[i] = cmxd{B[i << 1], B[(i << 1) | 1]};
			DFT(P, P, n / 2, false);
			Q[0] = P[0].conj();
			for (register int i = 1; i < n / 2; i++) Q[i] = P[n / 2 - i].conj();
			for (register int i = 0; i < n / 2; i++) b_0[i] = (P[i] + Q[i]) / 2, b_1[i] = ((P[i] - Q[i]) / 2).left();
		}
	}

	for (register int i = 0; i < n / 2; i++) {
		c_0[i] = a_0[i] * b_0[i] + a_1[i] * b_1[i] * root_table(i, n / 2);
		c_1[i] = a_0[i] * b_1[i] + a_1[i] * b_0[i];
	}
	for (register int i = 0; i < n / 2; i++) P[i] = c_0[i] + c_1[i].right();
	DFT(P, P, n / 2, true);
	for (register int i = 0; i < n / 2; i++) C[i << 1] = P[i].x, C[(i << 1) | 1] = P[i].y;
	last_n = n;
}

struct LInt {
	// elements
	short sign;
	int d;
	std::vector<u32> num;
	// define function/ initial
	LInt(void) : sign(0), d(0), num() {}
	LInt(bool b, int code = 0) : sign(0), d(0), num(1, 0) {
		if (b == false) {
			if (code > 0) sign = 2;
			if (code < 0) sign = -2;
			num.clear();
		}
	}
	LInt(int b) : sign(0), num(1, 0) {
		i64 ub = b;
		if (b == 0) {
			d = 0;
			return;
		}
		if (b > 0) sign = 1;
		if (b < 0) sign = -1, ub = -ub;

		if (ub > 99999999) d = 3;
		else if (ub > 9999) d = 2;
		else d = 1;
		num.resize(d, 0);
		for (int i = 0; i < d && ub > 0; i++) {
			num[i] = ub % 10000;
			ub /= 10000;
		}
	}
	LInt(i64 b) : sign(0), num(1, 0) {
		if (b == 0) {
			d = 0;
			return;
		}
		u64 ub = 0;
		if (b < 0) sign = -1, ub = static_cast<u64>(-(b + 1)) + 1ull;
		if (b > 0) sign = 1, ub = static_cast<u64>(b);

		if (ub > 9999999999999999ull) d = 5;
		else if (ub > 999999999999ull) d = 4;
		else if (ub > 99999999) d = 3;
		else if (ub > 9999) d = 2;
		else d = 1;
		num.resize(d);
		for (int i = 0; i < d && ub > 0; i++) {
			num[i] = static_cast<u32>(ub % 10000);
			ub /= 10000;
		}
	}
	LInt(u64 b) : num(1, 0) {
		if (b == 0) {
			sign = d = 0;
			return;
		} else {
			sign = 1;
			if (b > 9999999999999999ull) d = 5;
			else if (b > 999999999999ull) d = 4;
			else if (b > 99999999) d = 3;
			else if (b > 9999) d = 2;
			else d = 1;
			num.resize(d);
			for (int i = 0; i < d && b > 0; i++) {
				num[i] = static_cast<u32>(b % 10000);
				b /= 10000;
			}
		}
	}
	LInt(const char* in_string) {
		int i, len;
		bool flag = true, minus = false;
		if (in_string[0] == '-') {
			minus = true;
			in_string++;
		} else if (in_string[0] == '+') {
			minus = false;
			in_string++;
		}
		len = strlen(in_string);
		// check string is +/- inf whether or not
		if (strcmp(in_string, "inf") == 0) {
			sign = minus ? -2 : 2;
			d = 0;
			num.resize(0);
			return;
		}
		// check string is full of numbers or not
		for (i = 0; i < len && flag; i++) flag = flag && ('0' <= in_string[i] && in_string[i] <= '9');
		if (!flag || len == 0) {
			d = sign = 0;
			num.clear();
			return;
		}
		// ignore all 0 at the begin of string
		for (; *in_string == '0'; in_string++, len--);
		if (*in_string == '\0') {
			d = sign = 0;
			num.resize(1, 0);
			return;
		}
		// string is a normal number
		d = (len + 3) / 4;
		num.resize(d, 0);
		sign = minus ? -1 : 1;
		int j, temp;
		for (temp = 0, i = len, j = 0; j < len; j++) {
			temp = temp * 10 + in_string[j] - '0';
			i -= 1;
			if (i % 4 == 0) num[i / 4] = temp, temp = 0;
		}
	}
	LInt(const u32* in_num, int k) : sign(1), d(k), num(in_num, in_num + k) { this->sho(); }
	template <typename It>
	LInt(It begin_iter, It end_iter) : sign(1), d(std::distance(begin_iter, end_iter)), num(begin_iter, end_iter) {
		this->sho();
	}
#if __cplusplus >= 201103L
	LInt(const LInt&) = default;
	LInt(LInt&&) noexcept = default;
	LInt(const string& inString_) : LInt(inString_.c_str()) {}
#else
	LInt(const LInt& A) : sign(A.sign), d(A.d), num(A.num) {}
	LInt(const string& inString_) : num(0) { *this = LInt(inString_.c_str()); }
#endif
	// undo function
	virtual ~LInt() {}
	// assignment operator
#if __cplusplus >= 201103L
	LInt& operator=(const LInt&) = default;
	LInt& operator=(LInt&&) noexcept = default;
#else
	LInt& operator=(const LInt& B) {
		if (this != &B) {
			sign = B.sign;
			d = B.d;
			num = B.num;
		}
		return *this;
	}
	LInt& operator=(const char* inString) {
		LInt temp(inString);
		std::swap(sign, temp.sign);
		std::swap(d, temp.d);
		std::swap(num, temp.num);
		return *this;
	}
	inline LInt& operator=(bool b) { return *this = LInt(b); }
	inline LInt& operator=(int i) { return *this = LInt(i); }
	inline LInt& operator=(i64 i) { return *this = LInt(i); }
	inline LInt& operator=(u64 u) { return *this = LInt(u); }
#endif
	// compare operator
	bool operator<(const LInt& B) const {
		const LInt& A = *this;
		if (A.isNaN() || B.isNaN()) return false;
		if (A.sign < B.sign) return true;
		if (A.sign > B.sign) return false;
		if (sign == 2 || sign == -2 || sign == 0) return false;
		if (sign == -1) {
			if (A.d != B.d) return !(A.d < B.d);
			for (int i = d - 1; i >= 0; i--)
				if (A.num[i] != B.num[i]) return !(A.num[i] < B.num[i]);
			return false;
		}
		if (sign == 1) {
			if (A.d != B.d) return A.d < B.d;
			for (int i = d - 1; i >= 0; i--)
				if (A.num[i] != B.num[i]) return A.num[i] < B.num[i];
			return false;
		}
		return false;
	}
	bool operator==(const LInt& B) const {
		const LInt& A = *this;
		if (A.isNaN() || B.isNaN()) return false;
		if (A.sign != B.sign) return false;
		if (A.d != B.d) return false;
		for (int i = d - 1; i >= 0; i--)
			if (A.num[i] != B.num[i]) return false;
		return true;
	}
	inline bool operator>(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : B < A;
	}
	inline bool operator!=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A == B);
	}
	inline bool operator>=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A < B);
	}
	inline bool operator<=(const LInt& B) const {
		const LInt& A = *this;
		return (A.isNaN() || B.isNaN()) ? false : !(A > B);
	}
	// functions
	LInt abs() const {
		LInt ans(*this);
		if (ans.sign < 0) ans.sign = -ans.sign;
		return ans;
	}
	inline LInt abs(const LInt& b) const { return b.abs(); }
	void sho() {
		if (sign == 2 || sign == -2) {  // +inf or -inf
			d = 0;
			num.clear();
			return;
		}

		if (sign == 0) {  // zero or NaN
			d = 0;
			if (!num.empty()) {
				num.assign(1, 0);  // normalize zero representation
			}
			return;
		}

		int i = d - 1;
		while (i >= 0 && num[i] == 0) --i;

		if (i < 0) {
			sign = 0;
			d = 0;
			num.assign(1, 0);
			return;
		}
		d = i + 1;
		num.resize(d);
		return;
	}

	void print() const {
#if __cplusplus >= 201103L
		auto fast_out = [](u64 a) -> void {
			if (a == 0) {
				putchar('0');
				return;
			}
			char buf[20];
			int idx = 0;
			while (a > 0) {
				buf[idx++] = char('0' + (a % 10));
				a /= 10;
			}
			for (int i = idx - 1; i >= 0; --i) putchar(buf[i]);
		};
#else
		struct {
			void operator()(u64 a) const {
				if (a == 0) {
					putchar('0');
					return;
				}
				char buf[20];
				int idx = 0;
				while (a > 0) {
					buf[idx++] = char('0' + (a % 10));
					a /= 10;
				}
				for (int i = idx - 1; i >= 0; --i) putchar(buf[i]);
			}
		} fast_out;
#endif
		if (sign == 2 || sign == -2) {
			if (sign == -2) putchar('-');
			printf("inf");
			return;
		}
		if (sign == 0) {
			if (num.empty()) printf("NaN");
			else putchar('0');
			return;
		}
		if (sign == -1) putchar('-');
		fast_out(num[d - 1]);
		for (int i = d - 2; i >= 0; i--) {
			if (num[i] < 1000) putchar('0');
			if (num[i] < 100) putchar('0');
			if (num[i] < 10) putchar('0');
			fast_out(num[i]);
		}
	}
	string print_str() const {
		if (sign == 0) return num.empty() ? string("NaN") : string("0");
		if (sign == 2 || sign == -2) return sign == -2 ? string("-inf") : string("inf");
		string ans;
		if (sign == -1) ans += '-';

		auto i2s = [](int L, char* dest = 0) -> const char* {  // converse int to classical C-style string and return it
			static char temp[20];
			if (dest == 0) dest = temp;
			if (L == 0) {
				dest[0] = '0', dest[1] = '\0';
				return dest;
			}
			char* org = dest;
			if (L < 0) dest[0] = '-', dest++, L = -L;
			u32 t = 1;
			while (t <= unsigned(L)) t *= 10;
			while (t != 1) t /= 10, *(dest++) = '0' + (L / t), L %= t;
			*dest = '\0';
			return org;
		};

		ans += i2s(num[d - 1]);
		ans.reserve(ans.size() + 4 * d);
		for (int i = d - 2; i >= 0; i--) {
			if (num[i] < 10) ans += '0';
			if (num[i] < 100) ans += '0';
			if (num[i] < 1000) ans += '0';
			ans += i2s(num[i]);
		}
		return ans;
	}
	void show() const {
		cout << "LInt:\n";
		cout << "Address: " << this << endl;
		cout << "Sign: " << (sign > 0 ? '+' : (sign < 0 ? '-' : '0')) << '\t';
		cout << "Size: " << d << endl;
		if (d > 0) {
			cout << "Detail of the list: \n";
			if (d < 100)
				for (int k = 0; k < d; k++) cout << num[k] << (k % 8 == 7 ? '\n' : '\t');
			else {
				int t = int(std::sqrt(d));
				for (int k = 0; k < t; k++) cout << num[k] << (k % 8 == 7 ? '\n' : '\t');
				cout << "...\n";
				for (int k = 0; k < t; k++) cout << num[d - t + k] << (k % 8 == 7 ? '\n' : '\t');
			}
		} else if (sign > 0) cout << "LInt = +inf\n";
		else if (sign < 0) cout << "LInt = -inf\n";
		else if (!num.empty()) cout << "LInt = 0\n";
		else cout << "LInt = NaN\n";
		cout << "show LInt end.\n";
	}
	int digit() const {
		if (isinf()) return -1;
		if (zero()) return 0;

		u32 k = num[d - 1];
		int t = 0;
		while (k > 0) k /= 10, ++t;
		return t + 4 * (d - 1);
	}
	inline bool isNaN() const { return num.empty() && d == 0 && sign == 0; }
	inline bool positive() const { return sign > 0; }
	inline bool negative() const { return sign < 0; }
	inline bool isinf() const { return sign == 2 || sign == -2; }
	inline bool zero() const { return !num.empty() && sign == 0; }
	inline bool meanless() const { return isNaN() || isinf(); }
	inline bool abnormal() const { return zero() || isinf() || isNaN(); }
	void swap(LInt& other) {
		std::swap(sign, other.sign);
		std::swap(d, other.d);
		std::swap(num, other.num);
	}

   private:  // get 10000^2d/A while A >=0
	LInt recip() const {
		LInt A(*this);
		if (A.sign < 0) {
			return LInt(false);
		}
		if (A.sign == 2) {
			return LInt(0);
		}
		if (A.sign == 0) {
			return LInt("inf");
		}
		if (A.d <= 2) {
			u64 a = 0, div = 0;
			for (int i = A.d - 1; i >= 0; i--) a *= 10000, a += A.num[i];
			if (A.d == 1) div = 100000000;
			if (A.d == 2) div = 10000000000000000ull;
			return LInt(div / a);
		}
		int k = (A.d + 2) / 2;
		LInt Ak(std::prev(A.num.end(), k), A.num.end());
		LInt _Ak = Ak.recip();
		LInt _A = ((2 * _Ak) << (A.d - k)) - ((A * _Ak * _Ak) >> (2 * k));
		LInt _rA = (LInt(1) << (2 * A.d)) - _A * A;
		for (int delta = 0x08000000; delta > 0; delta /= 2) {
			LInt temp = delta * A;
			if (_rA < 0) {
				_rA += temp;
				_A -= LInt(delta);
			} else if (temp <= _rA) {
				_rA -= temp;
				_A += LInt(delta);
			}
		}
		return _A;
	}

   public:
	LInt div2() const {
		if (sign == 2 || sign == -2 || sign == 0) return *this;
		LInt ans(*this);
		for (int i = ans.d - 1; i > 0; i--) {
			if (ans.num[i] % 2) ans.num[i]--, ans.num[i - 1] += 1e4;
			ans.num[i] /= 2;
		}
		ans.num[0] /= 2;
		ans.sho();
		return ans;
	}
	LInt pow2() const { return (*this) * (*this); }

	// Operator Function
	LInt operator<<(int k) const {
		if (abnormal()) return *this;
		if (k < 0) return LInt(false);
		if (*this == 0) return LInt(0);

		LInt ans(false);
		ans.d = d + k;
		ans.sign = sign;
		ans.num.assign(d + k, 0);
		std::copy(num.begin(), num.end(), ans.num.begin() + k);
		return ans;
	}
	LInt operator>>(int k) const {
		if (abnormal()) return *this;
		if (k < 0) return LInt(false);
		if (d <= k) return LInt(0);

		LInt ans;
		ans.sign = sign;
		ans.d = d - k;
		ans.num.assign(num.begin() + k, num.end());
		return ans;
	}
	LInt operator-() const {
		LInt ans(*this);
		ans.sign = -ans.sign;
		return ans;
	}
	LInt operator+(const LInt& B) const {
		const LInt& A = *this;
		if (A.isNaN() || B.isNaN()) return LInt(false);
		if (B.sign == 0) return A;
		if (A.sign == 0) return B;
		if (A.sign == 2 || A.sign == -2) return (A.sign == -B.sign) ? LInt(false) : A;
		if (B.sign == 2 || B.sign == -2) return (B.sign == -A.sign) ? LInt(false) : B;
		if (A.sign == B.sign) {
			LInt ans;
			ans.sign = sign;
			ans.d = std::max(d, B.d) + 1;
			ans.num.assign(ans.d, 0);
			for (int i = 0; i < ans.d; i++) {
				if (i < d) ans.num[i] += num[i];
				if (i < B.d) ans.num[i] += B.num[i];
				if (ans.num[i] > 9999) {
					ans.num[i + 1] = ans.num[i] / 10000;
					ans.num[i] %= 10000;
				}
			}
			ans.sho();
			return ans;
		}
		if (A.sign == 1 && B.sign == -1) {
			LInt D;
			D = abs(B);
			if (*this == D) {
				LInt ans(0ull);
				return ans;
			}
			if (*this > D) {
				LInt ans;
				ans.d = d;
				ans.sign = 1;
				ans.num.assign(ans.d, 0);
				int i;
				for (i = d - 1; i > 0; i--) {
					if (i < d) ans.num[i] += num[i];
					if (i < B.d) ans.num[i] -= B.num[i];
					if (ans.num[i] > 0) {
						ans.num[i]--;
						ans.num[i - 1] += 10000;
					}
				}
				ans.num[0] = ans.num[0] + num[0] - B.num[0];
				for (i = 0; i < ans.d - 1; i++) {
					ans.num[i + 1] += ans.num[i] / 10000;
					ans.num[i] %= 10000;
				}
				ans.sho();
				return ans;
			} else {
				return -((-B) + (-*this));
			}
		} else {
			return B + (*this);
		}
	}
	LInt operator-(const LInt& B) const { return *this + (-B); }
	LInt operator*(int B) const {
		const LInt& A = *this;
		if (A.isNaN()) return LInt(false);
		if (B == 0 && (A.sign == 2 || A.sign == -2)) return LInt(false);
		if (A.sign == 2 || A.sign == -2) return (B > 0) ? A : -A;
		if (A.zero() || B == 0) return 0;
		if (B == 1 || B == -1) return B == 1 ? A : -A;
		LInt ans;
		ans.num.assign(A.d + 3, 0);
		ans.d = 0;
		ans.sign = A.sign * (B < 0 ? -1 : (B > 0) ? 1 : 0);
		if (B < 0) B = -B;
		u64 temp = 0, carry = 0;
		for (int i = 0; i < A.d; i++) {
			temp = carry + (u64)A.num[i] * B;
			ans.num[i] = temp % 10000;
			carry = temp / 10000;
			ans.d++;
		}
		for (int i = A.d; carry > 0; i++) {
			ans.num[i] = carry % 10000;
			carry /= 10000;
			ans.d++;
		}
		ans.sho();
		return ans;
	}
	LInt operator*(const LInt& B) const {
		auto _conv_length = [](int n) -> int {
			// if(n < 1024)	return 1 << Log_2(n);
			n = (n + 1) / 2;
			int power_3 = 0, base_3 = 1, best_length = 0;
			i64 min_height = 0x7fffffffffffffffLL;
			do {
				base_3 *= 3, ++power_3;
				int power_2 = Log_2((n / base_3) + int((n % base_3) != 0));
				int conv_length = (1 << power_2) * base_3;
				i64 conv_height = static_cast<i64>(conv_length) * (95 * power_3 + 100 * power_2);
				if (conv_height < min_height) {
					min_height = conv_height;
					best_length = (1 << power_2) * base_3;
				}
			} while (base_3 <= n && power_3 <= 6);
			return best_length * 2;
		};

		const LInt& A = *this;
		if (A.isNaN() || B.isNaN()) return LInt(false);
		if ((A.zero() && B.isinf()) || (A.isinf() && B.zero())) return LInt(false);
		if (A.isinf()) return (B.sign > 0) ? A : -A;
		if (B.isinf()) return (A.sign > 0) ? B : -B;
		if (A.zero() || B.zero()) return 0;
		LInt ans;
		register int x, y;
		int N = _conv_length(A.d + B.d - 1);
		ans.d = N + 2;
		ans.num.assign(ans.d, 0);
		ans.sign = A.sign * B.sign;
		std::vector<double> c(N, 0);
		if (A.d <= Log_2(B.d) || Log_2(A.d) >= B.d) {
			for (x = 0; x < A.d; x++)
				for (y = 0; y < B.d; y++) c[x + y] += A.num[x] * B.num[y];
		} else {
			std::vector<double> a, b;
			a.reserve(N), b.reserve(N);
			a.assign(A.num.begin(), A.num.end());
			b.assign(B.num.begin(), B.num.end());
			a.resize(N, 0), b.resize(N, 0);
			circ_conv(a, b, c, N);
		}
		double carry = 0.0;
		for (int i = 0; i < N; i++) {
			double temp = round(c[i]) + carry;
			carry = round(temp / 10000);
			int base = (int)round(temp - carry * 10000);
			if (base < 0) base += 10000, carry -= 1.0;
			ans.num[i] = base;
		}
		ans.num[N] = unsigned(carry);
		ans.sho();
		return ans;
	}
	LInt operator/(int B) const {
		if (B == 2) return this->div2();
		const LInt& A = *this;
		if (A.isNaN()) return LInt(false);
		if (A == 0 && B == 0) return LInt(false);
		if (A == 0) return 0;
		if (B == 0) return A.positive() ? "inf" : "-inf";
		if (A.isinf()) return (B >= 0) ? A : -A;

		LInt ans;
		ans.d = d;
		ans.sign = A.sign * (B > 0 ? 1 : -1);
		ans.num.assign(d, 0);
		u32 abs_B = (B < 0 ? -B : B);
		u64 temp = 0;
		for (int i = d - 1; i >= 0; i--) {
			temp *= 10000;
			temp += num[i];
			ans.num[i] = temp / abs_B;
			temp %= abs_B;
		}
		ans.sho();
		return ans;
	}
	LInt operator/(const LInt& B) const {
		const LInt& A = *this;
		if (A.isNaN() || B.isNaN()) return LInt(false);
		if ((A == 0 && B == 0) || (A.isinf() && B.isinf())) return LInt(false);
		if (A.isinf()) return (!B.negative()) ? A : -A;
		if (B.isinf()) return 0;
		if (A == 0) return 0;
		if (B == 0) return A.positive() ? "inf" : "-inf";
		if (A.d < 25) {
			LInt mid;
			LInt high = LInt(1) << (A.d - B.d + 1);
			LInt low = LInt(1) << (A.d - B.d - 1);
			LInt abs_A = A.abs(), abs_B = B.abs();
			if (abs_A < abs_B) return 0;
			while (1) {
				mid = (low + high).div2();
				if (mid == low) break;
				if (mid * abs_B <= abs_A) low = mid;
				else high = mid;
			}
			mid.sign = A.sign * B.sign;
			mid.sho();
			return mid;
		} else {
			LInt X = A.abs(), Y = B.abs();
			if (X < Y) return LInt(0);
			if (X.d > 2 * Y.d) {
				int k = X.d - Y.d * 2;
				X <<= k;
				Y <<= k;
			}
			LInt quoti = (X * Y.recip()) >> (2 * Y.d);
			LInt remain = X - Y * quoti;
			for (int delta = 16384; delta > 0; delta /= 2) {
				LInt temp = Y * delta;
				if (temp <= remain) {
					remain -= temp;
					quoti += delta;
				}
			}
			quoti.sign = sign * B.sign;
			quoti.sho();
			return quoti;
		}
	}
	LInt operator%(int B) const {
		const LInt& A = *this;
		if (A.isNaN() || A.isinf() || B == 0) return LInt(false);
		if (A == 0) return 0;
		u32 abs_B = (B < 0 ? -B : B);
		u64 temp = 0;
		for (int i = d - 1; i >= 0; i--) {
			temp *= 10000;
			temp += num[i];
			temp %= abs_B;
		}
		i64 rem = static_cast<i64>(temp);
		if (A.sign < 0) rem = -rem;
		return LInt(rem);
	}
	LInt operator%(const LInt& B) const {
		const LInt& A = *this;
		if (A.isNaN() || B.isNaN() || A.isinf() || B == 0) return LInt(false);
		if (B.isinf()) return B.positive() ? A : -A;
		if (A == 0) return 0;
		return *this - (*this / B) * B;
	}
	LInt& operator<<=(int k) {
		if (k == 0) return *this;
		LInt temp = *this << k;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator>>=(int k) {
		LInt temp = *this >> k;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator+=(const LInt& B) {
		LInt temp = *this + B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator-=(const LInt& B) {
		LInt temp = *this - B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator*=(int p) {
		LInt temp = *this * p;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator*=(const LInt& B) {
		LInt temp = *this * B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator/=(int p) {
		LInt temp = *this / p;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator/=(const LInt& B) {
		LInt temp = *this / B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator%=(int B) {
		LInt temp = *this % B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
	LInt& operator%=(const LInt& B) {
		LInt temp = *this % B;
#if __cplusplus >= 201103L
		num = std::move(temp.num);
#else
		num.swap(temp.num);
#endif
		d = temp.d;
		sign = temp.sign;
		return *this;
	}
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
	friend std::ostream& operator<<(std::ostream& os, const LInt& A) {
		if (A.sign == 0) {
			if (A.num.empty()) os.write("NaN", 3);
			else os.put('0');
		} else {
			if (A.sign < 0) os.put('-');
			if (A.isinf()) os.write("inf", 3);
			else {
				os << A.num[A.d - 1];
				for (int i = A.d - 2; i >= 0; i--) {
					if (A.num[i] < 10) os.put('0');
					if (A.num[i] < 100) os.put('0');
					if (A.num[i] < 1000) os.put('0');
					os << A.num[i];
				}
			}
		}
		return os;
	}
	friend std::istream& operator>>(std::istream& is, LInt& A) {
		using std::isdigit;
		is >> std::ws;
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
			while (t != '-' && !isdigit(t) && t != -1) {
				is.get();
				t = is.peek();
			}
			if (t == -1) {
				is.setstate(std::ios_base::failbit);
				A = 0;
				return is;
			}
		}
		char sign = is.peek();
		if (sign == '-' || sign == '+') is.get();
		else sign = '0';
		// standard input for integer does not accept NaN and Inf
		if (!isdigit(is.peek())) {
			if (sign == '+' || sign == '-') is.unget();
			A = 0;
			is.setstate(std::ios_base::failbit);
			return is;
		}
		string in_s(1, sign);
		while (isdigit(is.peek())) in_s += is.get();
		A = in_s;
		return is;
	}
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

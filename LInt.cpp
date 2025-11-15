#include "LInt.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>

namespace tbb {

// complex and complex-precompute
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

}
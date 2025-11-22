#ifndef TBB_LINT_UTILS_HPP
#define TBB_LINT_UTILS_HPP

#include <algorithm>
#include <cstdio>
#include <string>

namespace tbb {
typedef unsigned long long u64;
typedef long long i64;
typedef int i32;
typedef unsigned u32;

inline void fast_out(u32 a, bool strip_trailing_zeros = false) {
	if (strip_trailing_zeros && a == 0) return;
	if (strip_trailing_zeros && a == 0) {
		putchar('0');
		return;
	}
	if (strip_trailing_zeros && a != 0) {
		while (a % 10 == 0) a /= 10;
	}
	char buf[20];
	int idx = 0;
	while (a > 0) {
		buf[idx++] = char('0' + (a % 10));
		a /= 10;
	}
	for (int i = idx - 1; i >= 0; --i) putchar(buf[i]);
}

inline void fast_0_out(u32 a, bool strip_trailing_zeros = false) {
	if (a < 1000u) putchar('0');
	if (a < 100u) putchar('0');
	if (a < 10u) putchar('0');
	fast_out(a, strip_trailing_zeros);
}

// forward declaration so functions below can use it
inline std::string u32_to_str(u32 a, bool strip_trailing_zeros = false);

inline std::string fast_0_out_str(u32 a, bool strip_trailing_zeros = false) {
	u32 leading_zeros = 0;
	if (a < 1000u) leading_zeros++;
	if (a < 100u) leading_zeros++;
	if (a < 10u) leading_zeros++;
	if (a < 1u) leading_zeros++;
	return std::string(leading_zeros, '0') + u32_to_str(a, strip_trailing_zeros);
}

inline std::string u32_to_str(u32 a, bool strip_trailing_zeros /*= false*/) {
	char buf[20];
	char* p = buf;

	if (strip_trailing_zeros && a == 0) {
		return std::string();
	}
	if (!strip_trailing_zeros && a == 0) {
		return "0";
	}
	if (strip_trailing_zeros && a != 0) {
		while (a % 10 == 0) a /= 10;
	}
	while (a > 0) {
		*p++ = char('0' + (a % 10));
		a /= 10;
	}
	std::reverse(buf, p);

	return std::string(buf, p);
}

}  // namespace tbb

#endif  // TBB_LINT_UTILS_HPP
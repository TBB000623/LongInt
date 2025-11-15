#include "LFloat.h"

#include <cmath>
#include <cstring>
#include <sstream>

#include "LInt.h"

namespace tbb {

int _LFloat_prec = 2000;
const LFloat _LFloat_nan;

LFloat::LFloat(void) : base(), pow(0) {}
LFloat::LFloat(bool b) : base(b), pow(0) {}
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
LFloat::LFloat(double d) : pow(0) {
	static std::stringstream sio;
	while (sio.peek() != -1) sio.get();
	sio.clear();
	sio.precision(308);
	sio << d;
	string s;
	sio >> s;
	*this = s;
}
LFloat::LFloat(const char* inString) : base(false), pow(0) {
	using std::isdigit;
	const char*& inS = inString;
	bool dot = false, scientific = false, fail = false;
	int dot_pt = 0, sci_pt = 0, exp = 0, exp_l = 0, len;
	char sign = 0;
	for (const char* t = inString; *t != '\0' && !fail; t++) {
		if (!isdigit(*t) && !(*t == '+' || *t == '-') && !(t != inS && (*t == 'E' || *t == 'e')) && *t != '.') fail = true;
		else {
			if (*t == '+' || *t == '-') {
				if (sign == 0 && t == inS) sign = *t, inString++;
				else if (scientific && t - (inS + sci_pt) == 1)
					;
				else fail = true;
			} else if (*t == 'E' || *t == 'e') {
				if (scientific) fail = true;
				if (!scientific) scientific = true, sci_pt = t - inS;
				if (sci_pt == 0) fail = true;
			} else if (*t == '.') {
				if (dot || scientific) fail = true;
				else dot = true, dot_pt = t - inS;
			}
		}
	}
	if (fail) return;
	len = strlen(inString);
	if (!scientific) sci_pt = len;
	if (!dot) dot_pt = sci_pt;
	if (scientific) exp = s2i(inS + sci_pt + 1, inS + len);
	if (sign == 0) sign = '+';
	string s0(1, sign), s1(inS, dot_pt);
	if (dot) s1 += string(inS + dot_pt + 1, sci_pt - dot_pt - 1);
	if (dot) exp -= sci_pt - dot_pt - 1;
	exp_l = ((exp % 4) + 4) % 4;
	exp -= exp_l;
	base = LInt(s0 + s1) * pow10(exp_l);
	pow = exp / 4;
	sho();
}
LFloat::LFloat(const string& S) { *this = S.c_str(); }
LFloat::LFloat(const LInt& _b, int _p = 0) : base(_b), pow(_p) { sho(); }

}  // namespace tbb
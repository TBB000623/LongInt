#ifndef TBBLFLT_H // LFloat.h ver 3.4
#define TBBLFLT_H

#ifdef TBBLINT_H
#include <climits>
#include <sstream>
namespace tbb   {
	int _LFloat_prec= 2000;
	struct LFloat   {
		LInt base;
		int pow;
	//define function
		LFloat (void ):base(),pow(0){}
		LFloat (bool b):base(b),pow(0){}
		LFloat (int);
		LFloat (i64);
		LFloat (u64);
		LFloat (double);
		LFloat (const char *);
		LFloat (const string& S)    {*this= S.c_str();}
		LFloat (const LInt& _b, int _p= 0):base(_b), pow(_p){sho();}
	public:
	//adjust LFloat.base to precision
		inline bool isNaN(void) const   {return base.isNaN();}
		inline bool positive(void) const    {return base.positive();}
		inline bool negative(void) const    {return base.negative();}
		inline bool isinf(void) const   {return base.isinf();}
		inline bool zero(void) const    {return base.zero()&&pow==0;}
		inline bool meanless(void) const    {return base.meanless();}
		inline bool abnormal(void) const    {return base.abnormal();}
		void sho(void);
		int order(void)	const	{
			if(isNaN())	return 0;
			if(isinf())	return positive() ? INT_MAX : INT_MIN;
			if(zero())	return INT_MIN;
			return base.digit() + 4*pow;
		}
		void print(void) const;
		const string print_str(void)  const;
	//overload operator to calc
		LFloat operator+(void)	const;
		LFloat operator-(void)	const;
		LFloat operator+(const LFloat &)	const;
		LFloat operator-(const LFloat &)	const;
		LFloat operator*(const LFloat &)	const;
		LFloat operator/(const LFloat &)	const;
		LFloat & operator+=(const LFloat &);
		LFloat & operator-=(const LFloat &);
		LFloat & operator*=(const LFloat &);
		LFloat & operator/=(const LFloat &);

		LFloat operator*(int)	const;
		LFloat operator/(int)	const;
	//compare operator
		inline bool operator==(const LFloat &)	const;
		inline bool operator!=(const LFloat &)	const;
		bool operator<(const LFloat &)	const;
		inline bool operator>(const LFloat &)	const;
		inline bool operator<=(const LFloat &)	const;
		inline bool operator>=(const LFloat &)	const;
	//other functions
		LFloat pow2()	const;
	//precision for LFloat
		static int precision();
		static int precision(int);
	};
}
std::ostream & operator<<(std::ostream &, const tbb::LFloat &);
std::istream & operator>>(std::istream &, tbb::LFloat &);

tbb::LFloat::LFloat(int i):base(i),pow(0){
	int zero= 0;
	for(zero= 0; zero< base.d&& base.num[zero]==0; zero++);
	base>>= zero;	pow+=zero;
}
tbb::LFloat::LFloat(i64 i):base(i),pow(0){
	int zero= 0;
	for(zero= 0; zero< base.d&& base.num[zero]==0; zero++);
	base>>= zero;    pow+=zero;
}
tbb::LFloat::LFloat(u64 u):base(u),pow(0){
	int zero= 0;
	for(zero= 0; zero< base.d&& base.num[zero]==0; zero++);
	base>>= zero;    pow+=zero;
}
tbb::LFloat::LFloat(double d):pow(0){
	static std::stringstream sio;
	while(sio.peek()!=-1)   sio.get();
	sio.clear();
	sio.precision(308);
	sio<<d;
	string s;   sio>>s;
	*this= s;
}
tbb::LFloat::LFloat(const char *inString):base(false),pow(0)    {
	using std::isdigit;
	const char* &inS= inString;
	bool dot= false, scientific= false, fail= false;
	int dot_pt= 0, sci_pt= 0, exp= 0, exp_l= 0, len;   char sign= 0;
	for(const char *t= inString; *t!='\0'&&!fail; t++)   {
		if(!isdigit(*t)&&!(*t=='+'||*t=='-')&&!(t!=inS&&(*t=='E'||*t=='e'))&&*t!='.')   fail= true;
		else    {
			if(*t=='+'||*t=='-')    {
				if(sign==0&&t==inS) sign= *t, inString++;
				else    if(scientific&&t-(inS+ sci_pt)==1);
				else    fail= true;
			}   else
			if(*t=='E'||*t=='e')	{
				if(scientific)  fail= true;
				if(!scientific) scientific= true, sci_pt= t- inS;
				if(sci_pt==0)   fail= true;
			}   else
			if(*t=='.')	{
				if(dot||scientific) fail= true;
				else    dot= true, dot_pt= t- inS;
			}
		}
	}
	if(fail)	return ;
	len= strlen(inString);
	if(!scientific) sci_pt= len;
	if(!dot)	dot_pt= sci_pt;
	if(scientific)  exp= s2i(inS+ sci_pt+ 1, inS+ len);
	if(sign==0) sign= '+';
	string s0(1, sign), s1(inS, dot_pt);
	if(dot) s1+= string(inS+ dot_pt+ 1, sci_pt- dot_pt- 1);
	if(dot) exp-=sci_pt- dot_pt- 1;
	exp_l= ((exp% 4)+ 4)% 4;	exp-= exp_l;
	base= LInt(s0+s1)* pow10(exp_l);
	pow= exp/4;
	sho();
}

void tbb::LFloat::sho(void)  {
	int prec= _LFloat_prec;
	if(base.abnormal())    {pow=0; return;}
	if(base.d> prec)   {
		int delta= base.d- prec;
		if(i64(pow)+ delta>INT_MAX) pow=0, base=(base.sign==1)?"inf":"-inf";
		else    base>>=delta,   pow+= delta;
	}
	int end_0;
	for(end_0=0; end_0<base.d; end_0++) if(base[end_0]!=0)  break;
	if(end_0>0)  {
		base>>=end_0;
		if(i64(pow)+ end_0>INT_MAX) pow=0, base=(base.sign==1)?"inf":"-inf";
		else    pow+= end_0;
	}
	return;
}
void tbb::LFloat::print(void) const {
	int dgt= base.d;
	if(base.abnormal()) {base.print();   return;}
	if(base.negative()) putchar('-');
	if(pow>=0)  {//1234.p4= 1.234e19
		Fast_out(base[dgt-1]);
		for(int i=dgt-2; i>=0; i--)	Fast_0_out(base[i]);
		for(int i=0; i<pow; i++)    printf("0000");
	}   else
	if(-pow<dgt)   {//2563 1740 p-1= 2563.174
		Fast_out(base[dgt-1]);
		for(int i=dgt-2; i>=-pow; i--)   Fast_0_out(base[i]);
		putchar('.');
		for(int i=-pow-1; i>0; i--) Fast_0_out(base[i]);
		if(base[0]%1000==0) Fast_0_out(base[0]/1000, 1);
		else    if(base[0]%100==0)  Fast_0_out(base[0]/100, 2);
		else    if(base[0]%10==0)   Fast_0_out(base[0]/10, 3);
		else	Fast_0_out(base[0]);
	}   else	{
		putchar('0');
		putchar('.');
		for(int i=0; i<-pow-dgt; i++)    printf("0000");
		for(int i=dgt-1; i>0; i--) Fast_0_out(base[i]);
		if(base[0]%1000==0) Fast_0_out(base[0]/1000, 1);
		else    if(base[0]%100==0)  Fast_0_out(base[0]/100, 2);
		else    if(base[0]%10==0)   Fast_0_out(base[0]/10, 3);
		else    Fast_0_out(base[0]);
	}
}
const std::string tbb::LFloat::print_str(void) const    {
	int dgt= base.d;
	if(base.abnormal()) {return base.print_str();}
	string ans;
	if(base.negative()) ans+='-';
	if(pow>=0)  {
		ans+= i2s(base[dgt-1]);
		for(int i=dgt-2; i>=0; i--) ans+= Fast_0_out_str(base[i]);
		for(int i=0; i<pow; i++)    ans+= "0000";
	}   else
	if(-pow<dgt)   {//2563 1740 p-1= 2563.174
		ans+= i2s(base[dgt-1]);
		for(int i=dgt-2; i>=-pow; i--)   ans+= Fast_0_out_str(base[i]);
		ans+= '.';
		for(int i=-pow-1; i>0; i--) ans+= Fast_0_out_str(base[i]);
		if(base[0]%1000==0) ans+= Fast_0_out_str(base[0]/1000, 1);
		else    if(base[0]%100==0)  ans+= Fast_0_out_str(base[0]/100, 2);
		else    if(base[0]%10==0)   ans+= Fast_0_out_str(base[0]/10, 3);
		else    ans+= Fast_0_out_str(base[0]);
	}   else    {
		ans+= "0.";
		for(int i=0; i<-pow-dgt; i++)    ans+= "0000";
		for(int i=dgt-1; i>0; i--) ans+= Fast_0_out_str(base[i]);
		if(base[0]%1000==0) ans+= Fast_0_out_str(base[0]/1000, 1);
		else    if(base[0]%100==0)  ans+= Fast_0_out_str(base[0]/100, 2);
		else    if(base[0]%10==0)   ans+= Fast_0_out_str(base[0]/10, 3);
		else    ans+= Fast_0_out_str(base[0]);
	}
	return ans;
}

tbb::LFloat tbb::LFloat::operator+(void) const	{return *this;}
tbb::LFloat tbb::LFloat::operator-(void) const	{
	LFloat ans(*this);
	ans.base.sign= -ans.base.sign;
	return ans;
}
tbb::LFloat tbb::LFloat::operator+(const LFloat &B) const	{
	const LFloat &A= *this;
	if(A.isNaN()||B.isNaN())    return false;
	if(A.zero()||B.zero())  return A.zero()? B: A;
	if(A.isinf())   return (A.base.sign== -B.base.sign)? false: A;
	if(B.isinf())   return (B.base.sign== -A.base.sign)? false: B;
	if(abs(A.pow-B.pow)>2*_LFloat_prec) return (A.pow>B.pow)? A: B;
	LFloat ans;
	if(A.pow<B.pow) {ans.pow= A.pow, ans.base= (B.base<<(B.pow- A.pow))+ A.base;}
	else            {ans.pow= B.pow, ans.base= (A.base<<(A.pow- B.pow))+ B.base;}
	ans.sho();
	return ans;
}
tbb::LFloat tbb::LFloat::operator-(const LFloat &B) const	{
	return (*this)+ (-B);
}
tbb::LFloat tbb::LFloat::operator*(const LFloat &B) const	{
	const LFloat &A= *this;
	// static int u= 0;
	// u++;	std::cerr<<u<<endl;
	if(A.abnormal()||B.abnormal())  return LFloat(A.base*B.base, 0);
	LInt ans_base= A.base* B.base;  i64 ans_pow= i64(A.pow)+ i64(B.pow);
	if(ans_pow>INT_MAX) return LFloat(LInt(false, ans_base.sign), 0);
	if(ans_pow<INT_MIN) {
		if(ans_pow+ ans_base.d< INT_MIN)    return 0;
		return LFloat(ans_base>>(INT_MIN-ans_pow), INT_MIN);
	}
	return LFloat(ans_base, ans_pow);
}
tbb::LFloat tbb::LFloat::operator/(const LFloat &B) const	{
	const LFloat &A= *this; int n= _LFloat_prec;
	if(A.abnormal()||B.abnormal())  return LFloat(A.base/B.base, 0);
	LInt a= A.base, b= B.base;  i64 p= A.pow, q= B.pow;
	p-= 2*n- a.d, a<<= (2*n- a.d);
	q-= n- b.d, b<<= (n- b.d);
	LInt c= a/b;    i64 r= p- q;
	if(r>INT_MAX)   return LFloat(LInt(false, c.sign), 0);
	if(r<INT_MIN)   {
		if(r+ c.d< INT_MIN)    return 0;
		return LFloat(c>>(INT_MIN-r), INT_MIN);
	}
	return LFloat(c, r);
}
tbb::LFloat & tbb::LFloat::operator+=(const LFloat &B)	{return *this= *this+B;}
tbb::LFloat & tbb::LFloat::operator-=(const LFloat &B)	{return *this= *this-B;}
tbb::LFloat & tbb::LFloat::operator*=(const LFloat &B)	{return *this= *this*B;}
tbb::LFloat & tbb::LFloat::operator/=(const LFloat &B)	{return *this= *this/B;}

tbb::LFloat tbb::LFloat::operator*(int B)	const	{
	const LFloat &A= *this;
	if(A.isNaN()||A.zero())	return A;
	if(A.isinf()&&B!=0)	return B>0 ? A : -A;
	if(A.isinf()&&B==0)	return tbb::LInt(false);
	return LFloat(A.base*B, A.pow);
}
tbb::LFloat tbb::LFloat::operator/(int B)	const	{
	const LFloat &A= *this;
	if(A.isNaN()|| (A.zero()&&B==0) )	return tbb::LInt(false);	//return NaN
	if(A.isinf())	return B>=0 ? A : -A;	//return inf
	if(A.zero())	return 0;
	if(B==0)	return tbb::LInt(false, B);	//return inf;

	int k= _LFloat_prec + 3 - A.base.d;
	return LFloat((A.base<<k)/B, A.pow-k);
}

tbb::LFloat operator+(double A, const tbb::LFloat & B)	{return tbb::LFloat(A)+B;}
tbb::LFloat operator-(double A, const tbb::LFloat & B)	{return tbb::LFloat(A)-B;}
tbb::LFloat operator*(double A, const tbb::LFloat & B)	{return tbb::LFloat(A)*B;}
tbb::LFloat operator/(double A, const tbb::LFloat & B)	{return tbb::LFloat(A)/B;}

tbb::LFloat tbb::LFloat::pow2()	const	{
	if(abnormal())  return LFloat(base.pow2(), 0);
	LInt ans_base= base.pow2();  i64 ans_pow= 2*i64(pow);
	if(ans_pow>INT_MAX) return LFloat(LInt(false, ans_base.sign), 0);
	if(ans_pow<INT_MIN) {
		if(ans_pow+ ans_base.d< INT_MIN)    return 0;
		return LFloat(ans_base>>(INT_MIN-ans_pow), INT_MIN);
	}
	return LFloat(ans_base, ans_pow);
}

inline bool tbb::LFloat::operator==(const LFloat &B) const	{
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return false;
	return (A.base==B.base)&&(A.pow==B.pow);
}
inline bool tbb::LFloat::operator!=(const LFloat &B) const {
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return true;
	return !(A==B);
}
bool tbb::LFloat::operator<(const LFloat &B) const {
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return false;
	if(A.pow == B.pow)	return A.base < B.base;
	if(A.isinf()||B.isinf())	return A.base < B.base;
	if(A.pow < B.pow)	{
		LInt A_t= A.base >> (B.pow-A.pow);
		return A_t < B.base;
	}
	else	{
		LInt B_t= B.base >> (A.pow-B.pow);
		return A.base < B_t;
	}
}
inline bool tbb::LFloat::operator>(const LFloat &B) const	{
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return false;
	return B < A;
}
inline bool tbb::LFloat::operator<=(const LFloat &B) const	{
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return false;
	return !(A > B);
}
inline bool tbb::LFloat::operator>=(const LFloat &B) const	{
	const LFloat & A= *this;
	if(A.isNaN()||B.isNaN())	return false;
	return !(A < B);
}

std::ostream & operator<<(std::ostream & os, const tbb::LFloat & A) {
	if(!os) return os;
	if(A.abnormal())	{
		if(A.zero())	os.put('0');
		else	if(A.isNaN())	os.write("NaN", 3);
		else	{
			if(A.negative())	os.put('-');
			os.write("inf", 3);
		}
		return os;
	}
	//sign char
	tbb::LFloat B;
	if(A.negative())	os.put('-'), B= -A;
	else B= A;
	//get format flags
	int p= std::max(int(os.precision()), 0);
	std::ios_base::fmtflags flag= os.flags() & std::ios_base::floatfield;
	if(flag == std::ios_base::fixed)	{//fixed mode
		tbb::LInt B_normal= tbb::mul_pow10(B.base, 4*B.pow+p+1); //A= B*10^-p; B has a digit after point
		B_normal= tbb::mul_pow10(B_normal, -1) + int((B_normal[0]%10)>=5);
		if(B_normal.zero())	{
			os.put('0');
			if(p>0)	{
				os.put('.');
				for(int i=0; i<p; ++i)	os.put('0');
			}
			return os;
		} // if A=0 after scaled
		std::string B_str= B_normal.print_str();
		size_t len= B_str.length();
		if(len<=size_t(p))	{
			os.put('0');	os.put('.');
			for(size_t t=0; t< p-len; ++t)	os.put('0');
			for(size_t t=0; t<len; ++t)	os.put(B_str[t]);
		}	else	{
			for(size_t i=0; i<len-p; ++i)	os.put(B_str[i]);
			if(p>0)	{
				os.put('.');
				for(int t=0; t<p; ++t)	os.put(B_str[len-p+t]);
			}
		}
		return os;
	} else
	if(flag == std::ios_base::scientific)	{//scientific mode
		int power_d= B.base.digit()-(p+1)-1;
		tbb::LInt B_normal= tbb::mul_pow10(B.base, (p+1)-B.base.digit()+1);
		if((B_normal[0]%10)>=5)	B_normal+=10;
		power_d+= B_normal.digit()-(p+1);
		B_normal= tbb::mul_pow10(B_normal, (p+1)-B_normal.digit());
		//B_normal != 0
		std::string B_str= B_normal.print_str();
		os.put(B_str[0]);
		if(p>0)	{
			os.put('.');
			for(int t=1; t<=p; ++t)	os.put(B_str[t]);
		}
		os.put('e');
		int e_power= 4*B.pow + power_d + p;
		os.put(e_power>=0?'+':'-');
		os << tbb::Fast_0_out_char(std::abs(e_power), 0, 3);
		return os;
	} else	{//default mode
		os << B.print_str();
	}
	return os;
}
std::istream & operator>>(std::istream & is, tbb::LFloat & A)	{
	using std::isdigit;
	std::string init_str;
	is >> std::ws;
	//check stream safety
	{
		if(is.peek()==-1)	{is.setstate(std::ios_base::eofbit);	A= 0;	return is;}
		if(!is)	{is.setstate(std::ios_base::failbit);	A= 0;	return is;}
		char t= is.peek();
		while(t!='-' && t!='+' && !isdigit(t) && t!=-1)	{
			is.get();	t= is.peek();
		} //Ignore all char except '+', '-' and digit
		if(t==-1)	{is.setstate(std::ios_base::failbit);	A= 0;	return is;}
	}
	if(is.peek()=='+' || is.peek()=='-')	init_str.push_back(is.get());
	while(isdigit(is.peek()))	init_str.push_back(is.get());
	if(is.peek()=='.')	{
		init_str.push_back(is.get());
		while(isdigit(is.peek()))	init_str.push_back(is.get());
		init_str.push_back('0');
	}
	if(is.peek()=='e'||is.peek()=='E')	{
		char exp= is.get(), sign= 0;
		if(is.peek()=='+'||is.peek()=='-')	sign= is.get();
		if(!isdigit(is.peek()))	{
			if(!sign)	is.putback(sign);
			is.putback(exp);
		}	else	{
			init_str.push_back(exp);
			if(sign)	init_str.push_back(sign);
			while(isdigit(is.peek()))	init_str.push_back(is.get());
		}
	}
	A= tbb::LFloat(init_str);
	return is;
}

int tbb::LFloat::precision()    {
	return tbb::_LFloat_prec;
}
int tbb::LFloat::precision(int i){
	int bef= tbb::_LFloat_prec;
	tbb::_LFloat_prec= i;
	return bef;
}
#endif // TBBLINT_H

#endif

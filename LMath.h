#define TBBLMAT_H

#ifdef debug
#include "LInt.h"
#include "LFloat.h"
#endif
#ifdef TBBLINT_H
namespace tbb {
    //Declare
    LInt _recip_2       (const LInt &);
    LInt _recip_m       (const LInt &, int);
    LInt abs            (const LInt &);
    LInt sqrt           (const LInt &);
    LInt pow            (const LInt &, int);
    LInt powrt          (const LInt &, int);
    LInt gcd            (LInt, LInt);
    LInt lcm            (LInt, LInt);
    LInt permutation    (int, int);
    LInt factorial      (int);
    //Defination
    LInt abs(const LInt &B)	{
		LInt ans(B);
		if (B.sign<0) ans.sign=-B.sign;
		return ans;
	};
    //get 10000^2k/sqrt(A) when A has 2k or 2k-1 bits
    LInt _recip_2(const LInt &A) {
        if(A.isNaN())   return false;
        if(A.d<=2)	{
            if(A.d==0)	return 0;
            u32 temp;
            if(A.d==1)	temp=A.num[0];
            else    	temp=A.num[0]+A.num[1]*10000;
            return int(100000000/std::sqrt(double(temp)));
        }
        if(A.d<=4)  {
            double temp= 0;
            for(int i= A.d-1; i>=0; i--)    temp= 10000* temp+ A.num[i];
            return u64(1e16/std::sqrt(temp));
        }
        int _2n= (A.d%2==0)?A.d:A.d+1;
        int _2k= (_2n+2)/4*2;
        LInt _A, A2k(A.num+(_2n-_2k), _2k+A.d-_2n),_A2k,_rA;
        _A2k= _recip_2(A2k);
        _A2k<<= (_2n-_2k)/2;
        _A=(3*_A2k).div2()-((_A2k*_A2k*_A2k*A).div2()>>(2*_2n));
        _rA=(LInt(1)<<(2*_2n))-A*_A*_A;
        u64 delta;
        if(_rA<0)	for(delta=1; _rA.sign<0; delta*=2)	{
            LInt temp=(_A*2*-delta+delta*delta)*A;
            _A-=LInt(delta);	_rA-=temp;
        }	else	for(delta=1; ; delta*=2)	{
            LInt temp=(_A*2+delta)*delta*A;
            if(temp>_rA)	break;
            _A+=LInt(delta);	_rA-=temp;
        }
        for(;delta>0;delta/=2)	{
            LInt temp=(_A*2+delta)*delta*A;
            if(temp<=_rA)	{_A+=LInt(delta);	_rA-=temp;}
        }
        return _A;
    }
    LInt sqrt(const LInt &base) {
		if(base.isNaN()||base.sign<0)	return false;
        if(base.sign==2)    return base;
		if(base.sign==0)	return 0;
        int _2n= (base.d%2==0)? base.d: base.d+1;
		LInt ans= (base*_recip_2(base))>>_2n;
		LInt R=base-ans*ans;
		u64 delta;
		for(delta=1;;delta*=2)	{
			LInt dR=delta*(2*ans+delta);
			if(dR>R)	break;
			R-=dR;	ans+=delta;
		}
		for(;delta>0;delta/=2)	{
			LInt dR=delta*(2*ans+delta);
			if(dR<=R)	{R-=dR;	ans+=delta;}
		}
		return ans;
	}

    LInt pow(const LInt &A, int k)  {
        if((A==0&&k==0)||A.isNaN())  return false;
        if(A.isinf()&&k>=0)   {
            if(k==0)    return false;
            if(A.sign==2&&k>0)   return A;
            return (k%2==0)?(-A):A;
        }
        if(k<0)    return 0;
        if(k==0||A==1)    return LInt(1);
        if(A.num[0]==0) {
            int lnz=0;  //lnz=the lower bit where is not 0
            for(lnz=0; lnz<A.d&&A.num[lnz]==0; lnz++);
            return pow(A>>lnz,k)<<(lnz*k);
        }
        LInt S= 1, temp= A;
        for(int i=1; i<=k; i<<=1)   {
			temp= (i==1)?A: temp*temp;
            if(i&k) S*= temp;
        }
        return S;
    }

    LInt _recip_m(const LInt &A, int m)    {
        if(A.abnormal())    return A;
        if(A.d<=m)  return (powrt(A, m)<<m)/A;
        int n= (A.d+m-1)/m;
        int k= (n+1)/2;
        LInt Ak(A.num+m*(n-k), A.d-m*(n-k));
        LInt _Ak= _recip_m(Ak, m);   _Ak<<= n-k;
        LInt _AN= pow(A, m-1), base= LInt(n*m*m);
        LInt _A= (m+1)*_Ak/m- ((_AN* pow(_Ak, m+1)/ m)>>(n*m*m));
        u64 delta= 1;
        if(_AN* pow(_A, m)< base)   for(;;delta<<=1)    {
            LInt _Ap= _A+ delta;
            if(_AN* pow(_Ap, m)> base)  break;
            swap(_A, _Ap);
        }   else    for(;;delta<<=1)    {
            _A-= delta;
            if(_AN* pow(_A, m)<= base)  break;
        }
        do {
            delta>>=1;
            LInt _Ap= _A+ delta;
            if(_AN* pow(_Ap, m)> base)  continue;
            swap(_A, _Ap);
        } while(delta>0);
        return _A;
    }
    LInt powrt(const LInt &A, int m)  {
        if(A.isNaN()||m<=0)   return false;
        if(A.zero())    return 0;
        if(A.negative()&&m%2==0)  return false;
        if(A.isinf()) return A;
        if(A.negative()&&m%2==1)    return -powrt(-A, m);
        if(A==1)    return 1;
        if(A.d<=m)  {
            int up= 10000, down= 1, mid= (up+down)/2;
            while(up-down>1) {
                LInt a= pow(LInt(mid), m);
                if(a==A)    return mid;
                (a<A? down: up)= mid;
                mid= (up+down)/2;
            }
            return down;
        }
        int n= (A.d+m-1)/m;
        LInt ans= (A* _recip_m(A, m))>>(n*m);
        u64 delta= 1;
        for(delta=1; ; delta<<= 1)  {
            if(pow(ans+delta, m)>A) break;
            ans+= delta;
        }
        for(;delta>0 ; delta>>=1)   {
            if(pow(ans+delta, m)>A) continue;
            ans+= delta;
        }
        return ans;
    }

    LInt gcd(LInt a, LInt b)  {
        if(a.isNaN()||b.isNaN())    return false;
        LInt pow= 1;
		int div= 0;
        a= abs(a);  b= abs(b);
        while(!b.zero())    {
            if(a<b)   {swap(a,b); div= 0; continue;}
			if(div==0)	div= (a.d>b.d+1)?1:-1;
            if(a[0]%2==0&&b[0]%2==0)    {a= a.div2();    b= b.div2();    pow*=2; continue;}
            if(a[0]%2==0)   {a= a.div2();   continue;}
            if(b[0]%2==0)   {b= b.div2();   continue;}
            if(div==1)	a%=b;	else	a-=b;
        }
        return a* pow;
    }
    LInt lcd(const LInt &a, const LInt &b)  {
        return (a*b)/gcd(a,b);
    }

    LInt permutation(int n, int m)    {
        if(m<0) return false;
        if(m>n||m<=0)   return 0;
        if(m==1)    return n;
        return permutation(n, m/2)* permutation(n-m/2, m-m/2);
    }
    inline LInt factorial(int n) {return permutation(n, n);}
}
#endif //TBBLINT_H
#ifdef TBBLFLT_H
namespace tbb{
    //Declare
    template<>  LFloat mul_pow10    (const LFloat &, int);
    template<>  LFloat pow10        (int);
    LFloat _sin         (const LFloat &, int);
    LFloat _cos         (const LFloat &, int);
    LFloat _tan         (const LFloat &, int);
    LFloat _pi          (void);
    LFloat pow_pow10    (const LFloat &, int);
    LFloat abs          (const LFloat &);
    LFloat sqrt         (const LFloat &);
    LFloat pow          (const LFloat &, int);
    LFloat exp          (const LFloat &);
    LFloat ln1p         (const LFloat &);
    LFloat ln           (const LFloat &);
    LFloat arctan       (const LFloat &);
    LFloat sin          (const LFloat &);
    LFloat cos          (const LFloat &);
    LFloat tan          (const LFloat &);
    LFloat trunc        (const LFloat &);
    LFloat mod          (const LFloat &, const LFloat &);

    //Defination
    LFloat abs(const LFloat & A)    {
        LFloat B= A;
        if(B.base.sign<0)   B.base.sign= -B.base.sign;
        return B;
    }
    LFloat sqrt(const LFloat & A) {
        if(A.negative()) return LFloat(LInt(false), 0);
        if(A.abnormal()) return A;
        const int n= tbb::_LFloat_prec;
        LInt u= A.base; i64 t= A.pow;
        t-=(2*n-u.d);   u<<=(2*n-u.d);
        if(t%2!=0)  t++, u>>=1;
        return LFloat(sqrt(u), t/2);
    }
    LFloat pow(const LFloat& A, int n) {
        // std::cerr<<std::scientific<<A<<'\t'<<n<<std::defaultfloat<<endl;
        if(n<0) return 1.0/pow(A, -n);
        if(A.isNaN()||(A.meanless()&&n==0))   return false;
        if(A.isinf())    {return (A.negative()&&n%2==0)? -A: A;}
        if(A.zero())    return A;
        LFloat ans= 1, base= A;
        for(unsigned l=1; l<=unsigned(n); l<<=1)   {
            if(l&n) ans= ans* base;
            base= base* base;
        }
        return ans;
    }

    // Exponential & logarithm functions
    template<>
    LFloat mul_pow10(const LFloat & x, int m)   {
        if(x.abnormal())    return x;
        int flr4, mod4;
        mod4= (m%4+4)%4;    flr4= (m-mod4)/4;
        LFloat ans= x* pow10(mod4);
        ans.pow+= flr4;
        return ans;
    }

    template<>
    LFloat pow10(int k) {return mul_pow10<LFloat>(1, k);}

    LFloat pow_pow10(const LFloat & x, int m)   {
        LFloat S= x, S_2, S_3;
        for(int i=0; i<m; ++i)  {
            S= S.pow2();
            S_2= S*S;   S_3= S_2* S; S= S_2* S_3;
        }
        return S;
    }// return x^(10^m)
    LFloat exp(const LFloat& x)   {
        if(x.isNaN())   return x;
        if(x.isinf() && x.positive())   return x;
        if(x.isinf() && x.negative())   return 0;
        if(x==0)    return 1;
        if(x<0)     return 1/exp(-x);
        if(x > (double(_LFloat_prec)+INT_MAX)*std::log(10000))   return LInt("inf");
        if(x < (double(INT_MIN))*std::log(10000))    return 0;

        int precision= _LFloat_prec * 4;
        if(x>1) {
            _LFloat_prec= (precision + 4 * (x.pow+x.base.d) )/4 + 1;
            LFloat res= pow_pow10( exp(LFloat(x.base, -x.base.d)), 4 * (x.pow+x.base.d) );
            _LFloat_prec= precision / 4;
            res.sho();
            return res;
        }
        int bound= int(std::sqrt(precision)/2.3);
        LFloat B= pow10<LFloat>(-bound);
        if(x>B) {
            int delta_pow= 4*x.pow + x.base.digit() + bound;
            _LFloat_prec= (precision + delta_pow)/4 + 1;
            LFloat x_= mul_pow10(x, -delta_pow);
            LFloat res= pow_pow10(exp(x_), delta_pow);
            _LFloat_prec= precision / 4;
            res.sho();
            return res;
        }

        _LFloat_prec= (precision + Log_2(precision))/4 + 1;
        int N= precision / bound +1;
        LFloat S= 1;
        for(int i=N; i>0; --i)  S= S/i*x + 1;
        _LFloat_prec= precision/4;
        S.sho();
        return S;
    }

    // Triangle functions
    LFloat _pi(void)    {
        static LFloat inner_pi= 3;
        static int pi_precsion= 0;
        if(_LFloat_prec < pi_precsion)  {
            LFloat ret = inner_pi;
            ret.sho();
            return ret;
        }
        pi_precsion= _LFloat_prec;
        if(_LFloat_prec > 1000) return inner_pi = 4 * arctan(1);
        static LInt numerator= 1, denominator= 1;
        static LInt iter_factorial= 1;
        static int done_iter_times= 0;
        int new_iter_times= 6.09832411290916 + 13.28771237954945 *_LFloat_prec;
        LInt delta_numerator=0, delta_denominator= 1, delta_iter_factorial= 1;
        for(int k= new_iter_times; k > done_iter_times; --k)    {
            delta_numerator+=   delta_denominator;
            delta_numerator*=   k;
            delta_denominator*= 2*k+1;
            delta_iter_factorial*=  k;
        }
        numerator= numerator * delta_denominator + iter_factorial * delta_numerator;
        denominator*= delta_denominator;
        iter_factorial*= delta_iter_factorial;
        return Div_LInt(numerator, denominator) * 2;
    }
    LFloat _sin(const LFloat& x, int k) {
        if(k<=0)    return _LFloat_nan;
        if(k%2==0)  --k;
        LFloat S= 1, x2= x * x;
        for(; k>1; k-= 2)   S = 1 - x2*S/k/(k-1);
        return x * S;
    }
    LFloat _cos(const LFloat& x, int k) {
        if(k<0) return _LFloat_nan;
        if(k%2==1)  --k;
        LFloat C= 1, x2= x * x;
        for(; k>0; k-=2)    C = 1 - x2*C/k/(k-1);
        return C;
    }
    LFloat _tan(const LFloat& x, int k) {
        if(k<=0)    return _LFloat_nan;
        LFloat S= 1, C= 1, x2= x * x;
        for(; k>1; --k)
            if(k%2) S= 1 - x2*S/k/(k-1);
            else    C= 1 - x2*C/k/(k-1);
        S *= x;
        return S / C;
    }
    LFloat sin(const LFloat& x) {
        if(x.meanless())    return _LFloat_nan;
        if(x==0)    return 0;
        if(x<0)     return -sin(-x);
        if(x >= 0.785398163397)    { // if needed
            if(x >= 2 * _pi())  return sin(mod(x, 2*_pi()));
            if(x >= _pi())      return -sin(x - _pi());
            if(x >= _pi()/2)    return sin(_pi() - x);
            if(x >= _pi()/4)    return cos(_pi()/2 - x);
        }
        struct{
            LFloat operator()(const LFloat& t) {return 3*t - 4*pow(t,3);}
        } scale_func;
        int precision = _LFloat_prec * 4;
        int n = std::ceil(6.496501949786595*std::log(precision) + 0.00384285132051876*precision);
        int k = std::ceil(-0.630929753571457 + 0.523975818572346*precision/n - 0.455119613313419*std::log(n));
        _LFloat_prec= (precision + Log_2(precision) + Log_2(k))/4 + 1;
        LFloat x0= x;
        for(int i=0; i<k; ++i)  x0/= 3;
        LFloat y0= _sin(x0, 4*n);
        for(int i=0; i<k; ++i)  y0= scale_func(y0);
        _LFloat_prec= precision / 4;
        y0.sho();
        return y0;
    }
    LFloat cos(const LFloat& x) {
        if(x.meanless())    return _LFloat_nan;
        if(x==0)    return 1;
        if(x<0)     return cos(-x);
        if(x >= 0.785398163397) { // if needed
            if(x >= 2 * _pi())  return cos(mod(x, 2*_pi()));
            if(x >= _pi())      return cos(2*_pi() - x);
            if(x >= _pi()/2)    return -cos(_pi() - x);
            if(x >= _pi()/4)    return sin(_pi()/2 - x);
        }
        struct{
            LFloat operator()(const LFloat& t) {return 2*t*t - 1;}
        } scale_func;
        int precision = _LFloat_prec * 4;
        double t= std::sqrt(precision) * 1.28878394133527;
        int k= std::ceil(t)+ 1, n= std::ceil(t/2)+ 1;
        _LFloat_prec= (precision + Log_2(precision) + Log_2(k))/4 + 12;
        LFloat x0= x;
        for(int i=0; i<k; ++i)  x0/= 2;
        LFloat y0= _cos(x0, 4*n);
        for(int i=0; i<k; ++i)  y0= scale_func(y0);
        _LFloat_prec= precision / 4;
        y0.sho();
        return y0;
    }
    LFloat tan(const LFloat& x) {
        if(x.meanless())    return _LFloat_nan;
        if(x==0)    return 0;
        return sin(x) / cos(x);
    }
    LFloat arctan(const LFloat& x)  {
        if(x.isNaN()||x.zero())   return x;
        if(x.isinf() && x.negative())   return arctan(-1)*2;
        if(x.isinf() && x.positive())   return arctan(1)*2;
        if(x<0) return -arctan(-x);
        if(x>1) return arctan((sqrt(x*x+1)-1)/x)*2;

        struct{
            LFloat operator()(const LFloat& t) {return t / (sqrt(t*t+1)+1);}
        } scale_func;
        int precision= _LFloat_prec * 4;
        int bound= int(std::sqrt(0.2006866637759875 * precision / 16));    //bound= Sqrt(2 Lg 2*p/3)
        LFloat B= pow10<LFloat>(-bound);
        int n= precision/bound +1;  //expansion terms count
        int k= 0;   //scale times

        _LFloat_prec= (precision + Log_2(precision) + Log_2(k))/4 + 1;
        LFloat x_scaled= x;
        x_scaled.sho();
        while(x_scaled > B) {   //scaling
            x_scaled= scale_func(x_scaled);
            ++k;
        }
        LFloat y_scaled= 0, x2= x_scaled * x_scaled;
        for(int i= 4*n-1; i>=1; i-=2)   y_scaled= -x2*y_scaled + LFloat(1.0)/i;
        y_scaled*= x_scaled;
        LFloat& y= y_scaled;
        for(int i= 0; i<k; ++i) y= y*2;
        _LFloat_prec= precision/4;
        y.sho();
        return y;
    }

    // translate to LInt
    LFloat trunc(const LFloat &x)   {
        if(x.abnormal())    return x;
        if(x.pow>=0)    return x;
        return x.base >> x.pow;
    }
    LFloat mod(const LFloat &x, const LFloat &y)    {
        return x - trunc(x/y) * y;
    }
}
#endif //TBBLFLT_H

#endif

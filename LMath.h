// #define debug
#ifdef debug
#include "LFloat.h"
#endif

#ifndef TBBLMAT_H //LMath.h ver:3.1.3
#define TBBLMAT_H

#include "LInt.h"
namespace tbb {
    const LInt powrt(const LInt&, int);
    LInt abs(const LInt &B)	{
		LInt ans(B);
		if (B.sign<0) ans.sign=-B.sign;
		return ans;
	};
    //get 10000^2k/sqrt(A) when A has 2k or 2k-1 bits
    LInt recip_2(const LInt &A) {
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
        _A2k= recip_2(A2k);
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
    const LInt sqrt(const LInt &base) {
		if(base.isNaN()||base.sign<0)	return false;
        if(base.sign==2)    return base;
		if(base.sign==0)	return 0;
        int _2n= (base.d%2==0)? base.d: base.d+1;
		LInt ans= (base*recip_2(base))>>_2n;
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
    const LInt pow(const LInt &A, int k)  {
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
    const LInt recip_m(const LInt &A, int m)    {
        if(A.abnormal())    return A;
        if(A.d<=m)  return (powrt(A, m)<<m)/A;
        int n= (A.d+m-1)/m;
        int k= (n+1)/2;
        LInt Ak(A.num+m*(n-k), A.d-m*(n-k));
        LInt _Ak= recip_m(Ak, m);   _Ak<<= n-k;
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
    const LInt powrt(const LInt &A, int m)  {
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
        LInt ans= (A* recip_m(A, m))>>(n*m);
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
    const LInt gcd(LInt a, LInt b)  {
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
    const LInt lcd(const LInt &a, const LInt &b)  {
        return (a*b)/gcd(a,b);
    }
    const LInt permutation(int n, int m)    {
        if(m<0) return false;
        if(m>n||m<=0)   return 0;
        if(m==1)    return n;
        return permutation(n, m/2)* permutation(n-m/2, m-m/2);
    }
    inline const LInt factorial(int n) {return permutation(n, n);}
    #ifdef TBBLFLT_H
        const LFloat sqrt(const LFloat& A) {
            if(A.negative()) return LFloat(LInt(false), 0);
            if(A.abnormal()) return A;
            const int n= tbb::_LFloat_prec;
            LInt u= A.base; i64 t= A.pow;
            t-=(2*n-u.d);   u<<=(2*n-u.d);
            if(t%2!=0)  t++, u>>=1;
            return LFloat(sqrt(u), t/2);
        }
        const LFloat pow(const LFloat& A, int n) {
            if(n<0) return LFloat(1.0)/pow(A, -n);
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
        const LFloat exp(const LFloat& A)   {
            return false;
        }
    #endif
}
#endif

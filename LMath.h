// #define debug
#ifdef debug
#include "LFloat.h"
#endif

#ifndef TBBLMAT_H //LMath.h ver:3.1
#define TBBLMAT_H

#include "LInt.h"
namespace tbb {
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
        LInt temp= pow(A, k/2);
        if(k%2) return temp*temp*A;
        else    return temp*temp;
    }
    const LInt powrt(const LInt &A, int k)  {
        if(A.isNaN()||k<=0)   return false;
        if(A.zero())    return 0;
        if(A.negative()&&k%2==0)  return false;
        if(A.isinf()) return A;
        LInt x0= pow10(4*(A.d-1)/k), x;
        while(true) {
            x= ((k+1)*x0 - pow(x0, k+1)/A)/k;
            if(x==x0)    break;
            x0= x;
        }
        const LInt &P= A;
        LInt step= 1;
        if(pow(x,k)> A) while(true) {
            LInt Q= pow(x-step, k); x-= step;
            if(Q<= P)   break;
            step*=2;
        }
        else    while(true) {
            LInt Q= pow(x+step, k);
            if(Q<= P)  x+=step, step*=2;
            else    break;
        }
        while(step>0)   {
            LInt Q= pow(x+step, k);
            if(Q<= P)  x+=step;
            step/=2;
        }
        return x;
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
    #endif
}
#endif

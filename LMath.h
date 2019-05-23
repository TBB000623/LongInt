#ifndef TBBLMAT_H
#define TBBLMAT_H

#include "D:\LongInt\LongInt\LInt.h"
namespace tbb {
    LInt abs(const LInt &B)	{
		LInt ans(B);
		if (B.sign<0) ans.sign=-B.sign;
		return ans;
	};
    LInt mul_pow10(const LInt &A, int k)    {
        if(A.isNaN()||A.zero()||A.isinf())  return A;
        int t1;
        switch(k%4) {
            case(0):    t1= 1;  break;
            case(1):    t1= 10; break;
            case(2):    t1= 100;break;
            default:    t1=1000;break;
        }
        return (A* t1)<<(k/4);
    }
    LInt pow10(int k)   {return mul_pow10(1,k);}
    //get 10000^2k/sqrt(A) when A has 2k or 2k-1 bits
    LInt recip_2(const LInt &A) {
        if(A.isNaN())   return false;
        if(A.d<=2)	{
            if(A.d==0)	return 0;
            u32 temp;
            if(A.d==1)	temp=A.num[0];
            if(A.d==2)	temp=A.num[0]+A.num[1]*10000;
            return int(pow(10000,A.d)/std::sqrt(double(temp)));
        }
        int k= (A.d+1)/2;
        LInt _A, A2k(A.num+(A.d-k),k),_A2k,_rA;
        _A2k= recip_2(A2k);
        _A2k<<= (A.d-k)/2;
        _A=(3*_A2k).div2()-((_A2k*_A2k*_A2k*A).div2()>>(2*A.d));
        _rA=(LInt(1)<<(2*A.d))-A*_A*_A;
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
		LInt ans= (base*recip_2(base))>>(base.d);
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
/*    LInt pow(const LInt &A, u32 k)    {
        using std::vector;
        using std::complex;
        using std::cerr;
        if(A==0&&k==0)  {ERROR(3); return LInt(false);}
        if(A==0)    return LInt(0);
        if(k==0||A==1)    return LInt(1);
        if(A.num[0]==0) {
            int lnz=0;  //lnz=the lower bit where is not 0
            for(lnz=0; lnz<A.d&&A.num[lnz]==0; lnz++);
            return pow(A>>lnz,k)<<(lnz*k);
        }
        LInt ans;
        u32 head= A.num[A.d-1]; if(A.d!=1)  head++;
        int N=1<<Log_2(int(ceil(k* (log10(head)/4+ A.d-1))));
        ans.d=N+1;    ans.sign=(k%2)?A.sign:1;
        ans.num= new u32[ans.d];    memset(ans.num, 0, ans.d*sizeof(u32));
        vector< complex<double> > pI(N,0),pO,aI,aO(N,0);
        for(int i=0; i<A.d; i++)  pI[i]= A.num[i];
        aI=FFT(pI, false);
        for(int i=0; i<N; i++)  aO[i]=pow(aI[i], k);
        pO=FFT(aO, true);
        cerr<<pO[0]<<endl<<pO[N-1]<<endl;
        double carry=0.0;
        for(int i=0; i<N; i++)	{
            double temp=round(real(pO[i])+carry);
            carry=round(temp/10000);
            int base=(int)round(temp-carry*10000);
            if(base<0)	base+=10000,carry-=1.0;
            ans.num[i]=base;
        }
        ans.num[N]=(unsigned)carry;
        ans.sho();
        return ans;
    }   */
    LInt pow(const LInt &A, int k)  {
        using std::cerr;
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
    LInt powrt(const LInt &A, int k)  {
        if(A.isNaN()||k<=0)   return false;
        if(A.zero())    return 0;
        if(A.ngtive()&&k%2==0)  return false;
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
}
#endif

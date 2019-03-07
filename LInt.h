#ifndef TBBLINT_H
#define TBBLINT_H

#include <iostream>	//version:2.2
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <complex>
#include <vector>
#include <stack>
#include <cstring>
#include <string>

#include "D:\LongInt\LongInt\LError.h"

#define Pi 3.14159265358979323846

//#define debug
using std::ostream;
using std::cin;
using std::cout;
using std::endl;
using std::complex;
using std::vector;
using std::string;
using std::stack;

using std::sin;
using std::cos;
using std::sqrt;
using std::pow;

typedef unsigned long long u64;
typedef long long i64;
typedef unsigned u32;

void Fast_out(u32 a)	{
	if(a<0)	putchar('-'),a=-a;
	if(a==0) {
		putchar('0');
		return;
	}
	if(a>9) {
		Fast_out(a/10);
		putchar('0'+a%10);
	} else	putchar('0'+a);
}
int Log_2(int base) {
	int i;
	for(i=0;((1<<i)<base)&(i<32);i++);
	return i;
}

unsigned BUFF_max=100000;
bool ERROR_stop=true;
namespace TBB {
	vector< complex<double> > FFT(vector< complex<double> > &X, bool flag) {
		//flag -> inverse flag
		int L=X.size();
		if(L==1)	return X;
		vector< complex<double> > X1(L/2), X2(L/2);
		vector< complex<double> > A1(L/2), A2(L/2), A(L);
		for(int i=0; i<L/2; i++)	X1[i]= X[2*i], X2[i]= X[2*i+1];
		A1= FFT(X1, flag);	A2= FFT(X2, flag);
		complex<double> w0(cos(2*Pi/L), sin(2*Pi/L)),w(1,0);
		if(flag)	w0=conj(w0);
		for(int i=0; i<L/2; i++, w*=w0)	{
			complex<double> q= A2[i]*w;
			A[i]	= A1[i]+q;
			A[i+L/2]= A1[i]-q;
		}
		if(flag)    for(int i=0; i<L; i++)  A[i]/=2;
		return A;
	}
	struct LInt	{
	//elements
		short sign;
		int d;
		u32 *num;
	public:
	//define function/initial
		LInt (void )	{
			sign=d=0;
			num=new u32[BUFF_max];
			memset(num,0,BUFF_max*sizeof(u32));
		}
		LInt (bool b)	{
			sign=d=0;
			if(b)	{
				num=new u32[BUFF_max];
				memset(num,0,BUFF_max*sizeof(u32));
			} else	num=0;
		}
		LInt (int b)	{
			if(b==0)	{sign=d=0; num=new u32[1]; num[0]=0;}
			else if(b<0) {b=-b; sign=-1;}
			else sign=1;
			if(sign!=0)	{
				if(b>99999999)	d=3;
				else	if(b>9999)	d=2;
				else	d=1;
				num=new u32[d];
				for(int i=0; i<d&&b>0; i++)	{
					num[i]=b%10000;
					b/=10000;
				}
			}
		}
		LInt (i64 b)	{
			if(b==0)	{sign=d=0; num=new u32[1]; num[0]=0;}
			else	if(b<0)	{b=-b; sign=-1;}
			else	sign=1;
			if(sign!=0)	{
				if(b>9999999999999999LL)	d=5;
				else	if(b>999999999999LL)	d=4;
				else	if(b>99999999)	d=3;
				else	if(b>9999)	d=2;
				else	d=1;
				num=new u32[d];
				for(int i=0; i<d&&b>0; i++)	{
					num[i]=b%10000;
					b/=10000;
				}
			}
		}
		LInt (u64 b)	{
			if(b==0)	{
				sign=d=0;
				num=new u32[1];
				num[0]=0;
			} else	{
				sign=1;
				if(b>9999999999999999LL)	d=5;
				else	if(b>999999999999LL)	d=4;
				else	if(b>99999999)	d=3;
				else	if(b>9999)	d=2;
				else	d=1;
				num=new u32[d];
				for(int i=0; i<d&&b>0; i++)	{
					num[i]=b%10000;
					b/=10000;
				}
			}
		}
		LInt (char* inString)	{
			int i, len;
			bool flag=true, minus=false, zero=true;
			if(inString[0]=='-')	{minus=true; inString++;}
			len=strlen(inString);
			for(i=0; i<len; i++)	{
				flag=flag&&('0'<=inString[i]&&inString[i]<='9');
				zero=zero&&('0'==inString[i]);
			}
			if(!flag)	{*this=false; return ;}
			if(zero)	{*this=0LL; return ;}
			num=new u32[BUFF_max];
			memset(num, 0, BUFF_max*sizeof(u32));
			sign=1;
			if(minus)	sign=-1;
			int j, temp;
			d=0;
			for(i=len-1,j=0; i>=0; i--)	{
				if(j==0)	temp=0;
				if(j==0)	temp += (inString[i] - '0') * 1e0;
				if(j==1)	temp += (inString[i] - '0') * 1e1;
				if(j==2)	temp += (inString[i] - '0') * 1e2;
				if(j==3)	temp += (inString[i] - '0') * 1e3;
				j++;
				if(j==4)	{j -= 4; num[d++] = temp;}
			}
			if(j>0)	num[d++] = temp;

			this->sho();
		}
		LInt (string inString_)	{
			*this=LInt(inString_.data());
			return ;
		}
		LInt (const LInt &A)	{
			sign=A.sign;
			d=A.d;
			num=new u32[d];
			for(int i=0; i<d; i++)	num[i]=A.num[i];
		}
		LInt (const u32 *inNum, int k)	{
			num= new u32[k];
			memset(num,0,k*sizeof(u32));
			d=k,sign=1;
			for(int i=0; i<k; i++)	num[i]= inNum[i];
			this->sho();
		}
		LInt (int cff, u32 pow)	{

		}
	//undo function
		~LInt()	{
			if(num!=0)	delete[] num;
		}
	//assignment operator
		LInt operator=(const LInt &B)	{
			sign=B.sign;	d=B.d;
			u32 *pt_num=num;
			num=new u32[d];
			for(int i=0; i<d; i++)	num[i]=B.num[i];
			if(pt_num)	delete[] pt_num;
			return *this;
		}
		LInt operator=(u64 b)	{
			LInt ans(b);
			*this=ans;
			return *this;
		}
		LInt operator=(i64 b)	{
			LInt ans(b);
			*this=ans;
			return *this;
		}
		LInt operator=(bool b)	{
			LInt ans(b);
			*this=ans;
			return *this;
		}
	//compare operator
		bool operator<(const LInt &B) const	{
			if(sign<B.sign)	return true;
			if(sign>B.sign)	return false;
			if(sign==-1)	{
				if(d<B.d)	return false;
				if(d>B.d)	return true;
				for(int i=d-1; i>=0; i--)	{
					if(num[i]<B.num[i])	return false;
					if(num[i]>B.num[i])	return true;
				}
				return false;
			}
			if(sign==1)	{
				if(d>B.d)	return false;
				if(d<B.d)	return true;
				for(int i=d-1; i>=0; i--)	{
					if(num[i]>B.num[i])	return false;
					if(num[i]<B.num[i])	return true;
				}
				return false;
			}
			return false;
		}
		bool operator==(const LInt &B) const	{
			if(sign!=B.sign)	return false;
			if(d!=B.d)	return false;
			for(int i=d-1; i>=0; i--)	if(num[i]!=B.num[i])	return false;
			return true;
		}
		inline bool operator>(const LInt &B) const	{
			return B<(const LInt &)*this;
		}
		inline bool operator!=(const LInt &B) const	{
			return !(*this==B);
		}
		inline bool operator>=(const LInt &B) const	{
			return !(*this<B);
		}
		inline bool operator<=(const LInt &B) const	{
			return !(*this>B);
		}
	//functions
		inline bool reset_MAX_BUFF(u32 a)	{
			BUFF_max=a;
			return BUFF_max==a;
		}
		LInt make_LInt_zero(void)	{
			LInt A(0LL);
			*this=A;
			return A;
		}
		LInt abs()	const {
			LInt ans(*this);
			if(ans.sign==-1)	ans.sign=1;
			return ans;
		}
		LInt abs(const LInt &b) const	{
			LInt A(b);
			if(A.sign==-1)	A.sign=1;
			return A;
		}
		void sho()	{
			int i=this->d-1;
			while(i>=0&&num[i]==0)	i--;
			if(i<0)	{
				if(num)delete[] num;
				num=new u32[1];
				num[0]=0;
				sign=d=0;
				return;
			}
			this->d=i+1;
			u32 *pre=this->num;
			num=new u32[this->d];
			for(int j=0; j<this->d; j++)	num[j]=pre[j];
			delete [] pre;
			return;
		}
		void print() const	{
			if(sign==0)	{
				putchar('0');
				return;
			}
			if(sign==-1)	putchar('-');
			Fast_out(num[d-1]);
			for(int i=d-2; i>=0; i--)	{
				if(num[i]<10)	putchar('0');
				if(num[i]<100)	putchar('0');
				if(num[i]<1000)	putchar('0');
				Fast_out(num[i]);
			}
		}
		string print_str()	const	{
			if(sign==0)	return string("0");
			string ans(0);
			if(sign==-1)	ans+='-';
			ans+=num[d-1];
			for(int i=d-2; i>=0; i--)	{
				if(num[i]<10)	ans+='0';
				if(num[i]<100)	ans+='0';
				if(num[i]<1000)	ans+='0';
				ans+=num[i];
			}
			return ans;
		}
			//get 10000^2d/A
		LInt recip() const	{
			LInt A(*this);
			if(A.d==0)	{ERROR(2);return LInt(false);}
			if(A.d<=2)	{
				u64 a=0,div=0;
				for(int i=A.d-1; i>=0; i--)	a*=10000, a+=A.num[i];
				if(A.d==1)	div=100000000;
				if(A.d==2)	div=10000000000000000ull;
				return LInt(div/a);
			}
			int k=(A.d+2)/2;
			LInt Ak(A.num+(A.d-k),k), _Ak, _A, _rA;
			_Ak=Ak.recip();
			_A= ((2*_Ak)<<(A.d-k))- ((A*_Ak*_Ak)>>(2*k));
			_rA= (LInt(1)<<(2*A.d))-_A*A;
			for(int delta=0x08000000; delta>0; delta/=2)	{
				LInt temp=delta*A;
				if(_rA<0)	{_rA+=temp; _A-=LInt(delta);}
				else if(temp<=_rA)	{_rA-=temp;	_A+=LInt(delta);}
			}
			return _A;
		}
			//get 10000^2k/sqrt(A) when A has 2k or 2k-1 bits
		LInt recip2() const	{
			LInt A(*this);
			if(A.d<=2)	{
				if(A.d==0)	return LInt(0);
				u32 temp;
				if(A.d==1)	temp=A.num[0];
				if(A.d==2)	temp=A.num[0]+A.num[1]*10000;
				return LInt((int)(pow(10000,A.d)/std::sqrt((double)temp)));
			}
			int k= (A.d+1)/2;
			LInt _A,A2k(A.num+(A.d-k),k),_A2k,_rA;
			_A2k= A2k.recip2();
			_A2k<<= (A.d-k)/2;
			_A=(3*_A2k).div2()-((_A2k*_A2k*_A2k*A).div2()>>(2*A.d));
			_rA=(LInt(1)<<(2*A.d))-A*_A*_A;
			u64 delta;
			if(_rA<0)	for(delta=1; _rA.sign<0; delta*=2)	{
				LInt temp=(_A*2*-delta+delta*delta)*A;
				_A-=LInt(delta);	_rA-=temp;
			}	else	for(delta=1; ;delta*=2)	{
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
		LInt div2()	const	{
			LInt ans(*this);
			for(int i=ans.d-1;i>0;i--)	{
				if(ans.num[i]%2)	ans.num[i]--,ans.num[i-1]+=1e4;
				ans.num[i]/=2;
			}
			ans.num[0]/=2;
			ans.sho();
			return ans;
		}
	//Operator Function
		LInt operator<<(int k) const {
			if(k<0)	return LInt(1);
			if(*this==0)	return LInt(0);
			LInt ans(false);
			u32 *_num = new u32[d+k];
			memset(_num, 0, (d+k)*sizeof(u32));
			for(int i=0; i<d; i++)	_num[i+k]=num[i];
			ans.num=_num;	ans.d=d+k;	ans.sign=sign;
			return ans;
		}
		LInt operator>>(int k) const {
			if(k<0)	return LInt(1);
			if(d<=k)	return LInt(0);
			LInt ans(false);
			u32 *_num = new u32[d-k];
			memset(_num, 0, (d-k)*sizeof(u32));
			for(int i=0; i<d-k; i++)	_num[i]=num[i+k];
			ans.num=_num;	ans.d=d-k;	ans.sign=sign;
			return ans;
		}
		LInt operator-() const	{
			LInt ans(*this);
			ans.sign=-ans.sign;
			return ans;
		}
		LInt operator+(const LInt &B) const	{
			if(B.sign==0)	return *this;
			if(this->sign==0)	return B;
			if(sign==B.sign)	{
				LInt C(true);
				C.sign=sign;
				C.d=std::max(d,B.d)+1;
				for(int i=0; i<C.d; i++)	{
					if(i<d)	C.num[i]+=num[i];
					if(i<B.d)	C.num[i]+=B.num[i];
					if(C.num[i]>9999)	{
						C.num[i+1]=C.num[i]/10000;
						C.num[i]%=10000;
					}
				}
				C.sho();
				return C;
			}
			if(sign==1&&B.sign==-1)	{
				LInt D;
				D=abs(B);
				if(*this==D)	{
					LInt ans(0ull);
					return ans;
				}
				if(*this>D)	{
					LInt ans(true);
					ans.d=d;
					ans.sign=1;
					int i;
					for(i=d-1; i>0; i--)	{
						if(i<d)	ans.num[i]+=num[i];
						if(i<B.d)	ans.num[i]-=B.num[i];
						if(ans.num[i]>0)	{
							ans.num[i]--;
							ans.num[i-1]+=10000;
						}
					}
					ans.num[0]=ans.num[0]+num[0]-B.num[0];
					for(i=0; i<ans.d-1; i++)	{
						ans.num[i+1]+=ans.num[i]/10000;
						ans.num[i]%=10000;
					}
					ans.sho();
					return ans;
				}
				else	{
					return -((-B)+(-*this));
				}
			}
			else	{
				return B+(*this);
			}
		}
		LInt operator-(const LInt &B) const	{
			LInt temp;
			temp=-B;
			temp=*this+temp;
			return temp;
		}
		LInt operator*(int B)	{
			LInt ans(true),A(*this);
			ans.sign= A.sign* (B<0?-1:(B>0)?1:0);
			if(B<0)	B=-B;
			u64 temp=0,carry=0;
			for(int i=0; i<A.d; i++)	{
				temp= carry+(u64)A.num[i]*B;
				ans.num[i]=temp%10000;
				carry=temp/10000;
				ans.d++;
			}
			for(int i=A.d; carry>0; i++)	{
				ans.num[i]=carry%10000;
				carry/=10000;
				ans.d++;
			}
			ans.sho();
			return ans;
		}
		LInt operator*(const LInt &B) const {
			LInt ans(true);
			int x,y;
			int N=1<<(Log_2(this->d+B.d-1));
			ans.d=N+2;
			ans.sign=this->sign*B.sign;
			vector< complex<double> > pA(N,0),pB(N,0),pC(N,0),aA(N,0),aB(N,0),aC(N,0);
			for(x=0;x<d;x++)	pA[x]=this->num[x];
			for(y=0;y<B.d;y++)	pB[y]=B.num[y];
			aA=FFT(pA,false);	aB=FFT(pB,false);
			for(int i=0; i<N; i++)	aC[i]= aA[i]* aB[i];
			pC=FFT(aC,true);
			double carry=0.0;
			for(int i=0; i<N; i++)	{
				double temp=round(real(pC[i])+carry);
				carry=round(temp/10000);
				int base=(int)round(temp-carry*10000);
				if(base<0)	base+=10000,carry-=1.0;
				ans.num[i]=base;
			}
			ans.num[N]=(unsigned)carry;
			ans.sho();
			return ans;
		}
		LInt operator/(const LInt &B) const	{
			if(d<25)	{
				if(*this==0)	return LInt(0);
				if(B==0)	{ERROR(2); return LInt(false);}
				LInt ans(true),low(true),high(true),mid(true),abs_A,abs_B;
				high=LInt(1)<<(d-B.d+1);
				low=LInt(1)<<(d-B.d-1);
				abs_A=this->abs();	abs_B=B.abs();
				if(abs_A<abs_B) return LInt(0);
				while(1) {
					mid=(low+high).div2();
					if(mid==low)	break;
					if(mid*abs_B<=abs_A)	low=mid;
					else	high=mid;
				}
				mid.sign=this->sign*B.sign;
				ans=mid;	ans.sho();
				return ans;
			}
			{
				LInt X=(*this).abs(),Y=B.abs();
				if(Y==0)	{ERROR(2);return LInt(false);}
				if(X<Y)	return LInt(0);
				if(X.d>2*Y.d)	{
					int k=X.d-Y.d*2;
					X<<=k;	Y<<=k;
				}
				LInt quoti=(X*Y.recip())>>(2*Y.d);
				LInt remain=X-Y*quoti;
				for(int delta=16384; delta>0; delta/=2)	{
					LInt temp=Y*delta;
					if(temp<=remain) {remain-=temp;	quoti+=delta;}
				}
				quoti.sign=sign*B.sign;
				quoti.sho();
				return quoti;
			}
		}
		LInt operator^(const LInt &B) const {
			if(*this==0)	return LInt(0);
			if(B==0)	{return LInt(false);}
			LInt ans(true),abs_A=abs(*this),abs_B=abs(B);
		}
		inline LInt operator%(const LInt &B) const	{
			return *this-(*this/B)*B;
		}
		inline LInt operator<<=(int k)	{
			return *this=*this<<k;
		}
		inline LInt operator>>=(int k)	{
			return *this=*this>>k;
		}
		inline LInt operator+=(const LInt &B)	{
			return *this=*this+B;
		}
		inline LInt operator-=(const LInt &B)	{
			return *this=*this-B;
		}
		inline LInt operator*=(const LInt &B)	{
			return *this=*this*B;
		}
		inline LInt operator/=(const LInt &B)	{
			return *this=*this/B;
		}
		inline LInt operator%=(const LInt &B)	{
			return *this=*this%B;
		}
	//Friend Function for Other Classical Class
		friend LInt operator+(int A, const LInt &B)	{
			return B+LInt(A);
		}
		friend LInt operator-(int A, const LInt &B)	{
			return -B+LInt(A);
		}
		friend LInt operator*(int A, const LInt &B)	{
			 return B*A;
		}
		friend ostream & operator<<(ostream &os, const LInt &A){
			if(A.sign==0)	{
				os.put('0');
			}	else	{
				if(A.sign==-1)	os.put('-');
				os<<A.num[A.d-1];
				for(int i=A.d-2; i>=0; i--)	{
					if(A.num[i]<10)	os.put('0');
					if(A.num[i]<100)	os.put('0');
					if(A.num[i]<1000)	os.put('0');
					os<<A.num[i];
				}
			}
			return os;
		}
	//Special Math Function
		LInt power(const LInt &base, u64 exp) {
			LInt ans(1),temp_square(base);
			for(int i=0; (i<64)&&(1ull<<i<=exp); i++,temp_square*=temp_square)	{
				if(exp&(1ull<<i))	ans*=temp_square;
			}
			return ans;
		}
	};
	LInt abs(const LInt &B)	{
		LInt ans(B);
		if (ans.sign==-1) ans.sign=1;
		return ans;
	};
	LInt sqrt(const LInt &base) {
		if(base.sign<0)	{ERROR(1);return LInt(false);}
		if(base.sign==0)	return LInt(0);
		LInt ans=(base*base.recip2())>>(base.d);
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
/*	extern LInt make_power_10(int num_of_zero) {
		if(num_of_zero<0)	return LInt(false);
		char inString[BUFF_max]={0};
		for(int i=1;i<=num_of_zero;i++)	inString[i]='0';
		inString[0]='1';
		return LInt(inString);
	};*/
}


#endif

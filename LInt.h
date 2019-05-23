#ifndef TBBLINT_H
#define TBBLINT_H

#include <iostream>	//version:2.4
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <complex>
#include <vector>
#include <stack>
#include <cctype>
#include <cstring>
#include <string>

typedef unsigned long long u64;
typedef long long i64;
typedef unsigned u32;

void Fast_out(u32 a)	{
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
	for(i=0; ((1<<i)<base) & (i<32) ;i++);
	return i;
}
namespace tbb	{
	const double Pi= 3.14159265358979323846;
	using std::cin;
	using std::cout;
	using std::endl;
	using std::string;
	using std::stack;

	using std::sin;
	using std::cos;
	using std::sqrt;
	using std::pow;

	// template<typename T, size_T> FFT(const std::array<T, N> &X, bool flag) {
	std::vector<std::complex<double> > FFT(const std::vector<std::complex<double> > &X, bool flag) {
		//flag -> inverse flag
		using std::complex;
		using std::vector;
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
	//define function/initial
		LInt (void ):sign(0),d(0),num(0){}
		LInt (bool b):sign(0),d(0)	{
			if(b)	{
				num=new u32[0];
			} else	num=0;
		}
		LInt (int b)	{
			if(b==0)	{sign=d=0; num=new u32[0];}
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
			if(b==0)	{sign=d=0; num=new u32[0];}
			else	if(b<0)	{b=-b; sign=-1;}	else	sign=1;
			if(sign!=0)	{
				if	(b>9999999999999999LL)	d=5;
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
			if(b==0)	{sign=d=0;	num=new u32[0];} else	{
				sign=1;
				if	(b>9999999999999999LL)	d=5;
				else	if(b>999999999999LL)	d=4;
				else	if(b>99999999)	d=3;
				else	if(b>9999)	d=2;
				else	d=1;
				num= new u32[d];
				for(int i=0; i<d&&b>0; i++)	{
					num[i]=b%10000;
					b/=10000;
				}
			}
		}
		LInt (const char* inString)	{
			int i, len;
			bool flag=true, minus=false;
			if(inString[0]=='-')	{minus=true; inString++;}
			else	if(inString[0]=='+')	{minus=false;	inString++;}
			len=strlen(inString);
			//check string is +/- inf whether or not
			if(strcmp(inString, "inf")==0)	{sign= minus? -2: 2;	d=0;	num=0;	return ;}
			//check string is full of numbers or not
			for(i=0; i<len&&flag; i++)	flag= flag&&('0'<=inString[i]&&inString[i]<='9');
			if(!flag||len== 0)	{d=sign=0;	num=0; return ;}
			//ignore all 0 at the begin of string
			for(; *inString=='0'; inString++, len--);
			if(*inString=='\0')	{d=sign=0;	num= new u32[0]; return ;}
			//string is a normal number
			d= (len+3)/4;	num= new u32[d]();
			sign= minus? -1: 1;
			int j, temp;
			for(temp=0, i=len, j=0; j<len; j++)	{
				temp= temp*10 + inString[j]-'0';
				i-=1;
				if(i%4==0)	num[i/4]= temp, temp= 0;
			}
		}
		LInt (const string &inString_)	{
			const char *inString= inString_.c_str();
			int i, len;
			bool flag=true, minus=false;
			if(inString[0]=='-')	{minus=true; inString++;}
			else	if(inString[0]=='+')	{minus=false;	inString++;}
			len=strlen(inString);
			if(strcmp(inString, "inf")==0)	{sign= minus? -2: 2;	d=0;	num=0;	return ;}
			for(i=0; i<len&&flag; i++)	flag= flag&&('0'<=inString[i]&&inString[i]<='9');
			if(!flag||len== 0)	{d=sign=0;	num=0; return ;}
			for(; *inString=='0'; inString++, len--);
			if(*inString=='\0')	{d=sign=0;	num= new u32[0]; return ;}
			d= (len+3)/4;	num= new u32[d]();
			sign= minus? -1: 1;
			int j, temp;
			for(temp=0, i=len, j=0; j<len; j++)	{
				temp= temp*10 + inString[j]-'0';
				i-=1;
				if(i%4==0)	num[i/4]= temp, temp= 0;
			}
		}
		LInt (const LInt &A):sign(A.sign), d(A.d)	{
			num= new u32[d];
			for(int i=0; i<d; i++)	num[i]= A.num[i];
		}
		LInt (const u32 *inNum, int k):sign(1){
			while(k>0 && inNum[k-1]==0)	k--, inNum++;
			d=k;	num= new u32[k];
			memset(num,0,k*sizeof(u32));
			for(int i=0; i<k; i++)	num[i]= inNum[i];
			this->sho();
		}
	//undo function
		virtual ~LInt()	{
			if(num!=0)	delete[] num;
		}
	//assignment operator
		LInt & operator=(const LInt &B)	{
			sign=B.sign;	d=B.d;
			if(num!=0)	delete[] num;
			if(B.isNaN())	num= 0;
			else	{num= new u32[d];	for(int i=0; i<d; i++)	num[i]=B.num[i];}
			return *this;
		}
	//compare operator
		bool operator<(const LInt &B) const	{//
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN())	return false;
			if(A.sign<B.sign)	return true;
			if(A.sign>B.sign)	return false;
			if(sign==2||sign==-2||sign==0)	return false;
			if(sign==-1)	{
				if(A.d!=B.d)	return !(A.d<B.d);
				for(int i=d-1; i>=0; i--)	if(A.num[i]!=B.num[i])	return !(A.num[i]<B.num[i]);
				return false;
			}
			if(sign==1)	{
				if(A.d!=B.d)	return A.d<B.d;
				for(int i=d-1; i>=0; i--)	if(A.num[i]!=B.num[i])	return A.num[i]<B.num[i];
				return false;
			}
			return false;
		}
		bool operator==(const LInt &B) const {
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN())	return false;
			if(A.sign!=B.sign)	return false;
			if(A.d!=B.d)	return false;
			for(int i=d-1; i>=0; i--)	if(A.num[i]!=B.num[i])	return false;
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
		LInt abs()	const {
			LInt ans(*this);
			if(ans.sign<0)	ans.sign=-ans.sign;
			return ans;
		}
		inline LInt abs(const LInt &b) const	{return b.abs();}
		void sho()	{
			if(sign==2||sign==-2)	{
				d=0;
				if(num!=0)	{delete[] num;	num=0;}
				return ;
			}
			if(sign==0)	{
				d=0;
				if(num!=0)	{delete[] num;	num= new u32[0];}
				return ;
			}
			int i=this->d-1;
			while(i>=0&&num[i]==0)	i--;
			if(i<0)	{
				if(num)	delete[] num;
				num=new u32[0];	sign=d=0;
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
			if(sign==2||sign==-2)	{
				if(sign==-2)	putchar('-');
				printf("inf");
			}
			if(sign==0)	{
				if(num==0)	printf("NaN");
				else	putchar('0');
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
			if(sign==0)	return num==0?string("NaN"):string("0");
			if(sign==2||sign==-2)	return sign==-2?string("-inf"):string("inf");
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
		void show()	const {
			cout<< "LInt:\n";
			cout<< "Address: "<< this<< endl;
			cout<< "Sign: "<< (sign>0?'+': (sign<0?'-':'0'))<< '\t';
			cout<< "List address: "<< num<< '\t'<< "size: "<< d<< endl;
			if(d)	{
				cout<< "Detail of the list: \n";
				if(d<100)	for(int k=0; k<d; k++)	cout<< num[k]<< (k%8==7?'\n':'\t');
				else	{
					int t= int(sqrt(d));
					for(int k=0; k<t; k++)	cout<< num[k]<< (k%8==7?'\n':'\t');
					cout<< "...\n";
					for(int k=0; k<t; k++)	cout<< num[d-t+k]<< (k%8==7?'\n':'\t');
				}
			}
			else	if(sign>0)	cout<<"LInt = +inf\n";
			else	if(sign<0)	cout<<"LInt = -inf\n";
			else	if(num!=0)	cout<<"LInt =0\n";
			else	cout<<"LInt = NaN\n";
			cout<< "show LInt end.\n";
		}
		inline bool isNaN()	const	{return (num==0)&&(d==0)&&(sign==0);}
		inline bool pstive()	const	{return sign>0;}
		inline bool ngtive()	const	{return sign<0;}
		inline bool isinf()	const	{return sign==2||sign==-2;}
		inline bool zero()	const	{return sign==0&&num!=0;}
		//get 10000^2d/A while A >=0
		LInt recip() const	{
			LInt A(*this);
			if(A.sign<0)	{return LInt(false);}
			if(A.sign==2)	{return LInt(0);}
			if(A.sign==0)	{return LInt("inf");}
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
		LInt div2()	const	{
			if(sign==2||sign==-2||sign==0)	return *this;
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
		const LInt operator<<(int k) const {
			if(sign==2||sign==-2||sign==0)	return *this;
			if(k<0)	return LInt(false);
			if(*this==0)	return LInt(0);
			LInt ans(false);
			u32 *_num = new u32[d+k]();
			for(int i=0; i<d; i++)	_num[i+k]=num[i];
			ans.num=_num;	ans.d=d+k;	ans.sign=sign;
			return ans;
		}
		const LInt operator>>(int k) const {
			if(sign==2||sign==-2||sign==0)	return *this;
			if(k<0)	return LInt(false);
			if(d<=k)	return LInt(0);
			LInt ans(false);
			u32 *_num = new u32[d-k];
			for(int i=0; i<d-k; i++)	_num[i]=num[i+k];
			ans.num=_num;	ans.d=d-k;	ans.sign=sign;
			return ans;
		}
		const LInt operator-() const	{
			LInt ans(*this);
			ans.sign=-ans.sign;
			return ans;
		}
		const LInt operator+(const LInt &B) const	{
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN())	return LInt(false);
			if(B.sign==0)	return A;
			if(A.sign==0)	return B;
			if(A.sign==2||A.sign==-2)	return (A.sign== -B.sign)?LInt(false):A;
			if(B.sign==2||B.sign==-2)	return (B.sign== -A.sign)?LInt(false):B;
			if(A.sign==B.sign)	{
				LInt C;
				C.sign=sign;
				C.d=std::max(d,B.d)+1;
				C.num= new u32[C.d]();
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
			if(A.sign==1&&B.sign==-1)	{
				LInt D;
				D=abs(B);
				if(*this==D)	{
					LInt ans(0ull);
					return ans;
				}
				if(*this>D)	{
					LInt ans;
					ans.d=d;	ans.sign=1;
					ans.num= new u32[ans.d]();
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
		const LInt operator-(const LInt &B) const	{
			return *this+(-B);
		}
		const LInt operator*(int B) const	{
			const LInt &A= *this;
			if(A.isNaN())	return false;
			if(B==0&&(A.sign==2||A.sign==-2))	return false;
			if(A.sign==2||A.sign==-2)	return (B>0)?A:-A;
			if(A.zero()||B==0)	return 0;
			LInt ans;
			ans.num= new u32[A.d+3]();
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
		const LInt operator*(const LInt &B) const {
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN())	return false;
			if((A.zero()&&B.isinf())||(A.isinf()&&B.zero()))	return false;
			if(A.isinf())	return (B.sign>0)?A:-A;
			if(B.isinf())	return (A.sign>0)?B:-B;
			if(A.zero()||B.zero())	return 0;
			using std::complex;
			using std::vector;
			LInt ans;
			int x,y;
			int N=1<<(Log_2(A.d+B.d-1));
			ans.d=N+2;	ans.num= new u32[ans.d]();
			ans.sign=A.sign*B.sign;
			vector< complex<double> > pA(N,0),pB(N,0),pC(N,0),aA(N,0),aB(N,0),aC(N,0);
			for(x=0;x<A.d;x++)	pA[x]=A.num[x];
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
			ans.num[N]=unsigned(carry);
			ans.sho();
			return ans;
		}
		const LInt operator/(int B) const	{
			if(B==2)	return this->div2();
			const LInt &A= *this;
			if(A.isNaN())	return false;
			if(A==0&&B==0)	return false;
			if(A==0)	return 0;
			if(B==0)	return A.pstive()?"inf":"-inf";
			if(A.isinf())	return (B>=0)?A:-A;

			LInt ans;	ans.d=d;
			ans.sign= A.sign* (B>0?1:-1);
			ans.num= new u32[d]();
			u32 abs_B=(B<0?-B:B);	u64 temp=0;
			for(int i=d-1; i>=0; i--)	{
				temp*= 10000;
				temp+= num[i];
				ans.num[i]= temp/abs_B;
				temp%= abs_B;
			}
			ans.sho();
			return ans;
		}
		const LInt operator/(const LInt &B) const	{
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN())	return false;
			if((A==0&&B==0)||(A.isinf()&&B.isinf()))	return false;
			if(A.isinf())	return (!B.ngtive())?A:-A;
			if(B.isinf())	return 0;
			if(A==0)	return 0;
			if(B==0)	return A.pstive()?"inf":"-inf";
			if(A.d<25)	{
				LInt mid;
				LInt high=LInt(1)<<(A.d-B.d+1);
				LInt low=LInt(1)<<(A.d-B.d-1);
				LInt abs_A=A.abs(),	abs_B=B.abs();
				if(abs_A<abs_B) return 0;
				while(1) {
					mid=(low+high).div2();
					if(mid==low)	break;
					if(mid*abs_B<=abs_A)	low=mid;
					else	high=mid;
				}
				mid.sign= A.sign* B.sign;
				mid.sho();
				return mid;
			}
			else	{
				LInt X=A.abs(), Y=B.abs();
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
		const LInt operator%(int B) const	{
			const LInt &A= *this;
			if(A.isNaN()||A.isinf()||B==0)	return false;
			if(A==0)	return 0;
			u32 abs_B=(B<0?-B:B);	u64 temp=0;
			for(int i=d-1; i>=0; i--)	{
				temp*= 10000;
				temp+= num[i];
				temp%= abs_B;
			}
			temp*= d*(B<0?-1:1);
			return temp;
		}
		const LInt operator%(const LInt &B) const	{
			const LInt &A= *this;
			if(A.isNaN()||B.isNaN()||A.isinf()||B==0)	return false;
			if(B.isinf())	return B.pstive()?A:-A;
			if(A==0)	return 0;
			return *this-(*this/B)*B;
		}
		inline LInt & operator<<=(int k)	{
			return *this=*this<<k;
		}
		inline LInt & operator>>=(int k)	{
			return *this=*this>>k;
		}
		inline LInt & operator+=(const LInt &B)	{
			return *this=*this+B;
		}
		inline LInt & operator-=(const LInt &B)	{
			return *this=*this-B;
		}
		inline LInt & operator*=(int p)	{
			return *this=(*this)*p;
		}
		inline LInt & operator*=(const LInt &B)	{
			return *this=*this*B;
		}
		inline LInt & operator/=(int p)	{
			return *this=(*this)/p;
		}
		inline LInt & operator/=(const LInt &B)	{
			return *this=*this/B;
		}
		inline LInt & operator%=(int B)	{
			return *this=*this%B;
		}
		inline LInt & operator%=(const LInt &B)	{
			return *this=*this%B;
		}
		inline LInt & operator++(void)	{
			return *this+=1;
		}
		inline const LInt operator++(int)	{
			LInt b= *this;
			*this+=1;
			return b;
		}
		inline LInt & operator--(void)	{
			return *this-=1;
		}
		inline const LInt operator--(int)	{
			LInt b= *this;
			*this-=1;
			return b;
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
		friend std::ostream & operator<<(std::ostream &os, const LInt &A)	{
			if(A.sign==0)	{
				if(A.num==0)	os.write("NaN",3);
				else	os.put('0');
			}	else	{
				if(A.sign<0)	os.put('-');
				if(A.isinf())	os.write("inf",3);
				else	{
					os<<A.num[A.d-1];
					for(int i=A.d-2; i>=0; i--)	{
						if(A.num[i]<10)	os.put('0');
						if(A.num[i]<100)	os.put('0');
						if(A.num[i]<1000)	os.put('0');
						os<<A.num[i];
					}
				}
			}
			return os;
		}
		friend std::istream & operator>>(std::istream &is, LInt &A)	{
			is>>std::ws;
			if(!is)	{is.setstate(std::ios_base::failbit);	A= 0;	return is;}
			if(is.peek()==-1)	{is.setstate(std::ios_base::eofbit);	A= 0;	return is;}
			char sign= is.peek();
			if(sign=='-'||sign=='+')	is.get();
			else	sign= '0';
			// NaN or inf?
			if(is.peek()=='N'||is.peek()=='n'||is.peek()=='I'||is.peek()=='i')	{
				char in[4]= {};	int bit;
				for(bit=0; bit<3 && is.good(); bit++)	in[bit]= is.get();
				if(sign=='0'&&(strcmp(in, "NaN")==0||strcmp(in, "nan")==0))	{
					A= false;
					return is;
				}
				if(strcmp(in, "inf")==0||strcmp(in, "Inf")==0)	{
					if(sign=='-')	A= "-inf";
					else	A= "inf";
					return is;
				}
				is.setstate(std::ios_base::goodbit);
				for(; bit>0; )	if(!is.putback(in[--bit]))	break;
				is.setstate(std::ios_base::failbit);
				A= 0;
				return is;
			}
			//just no any suitable input?
			if(!isdigit(is.peek()))	{
				if(sign=='+'||sign=='-')	is.unget();
				A= 0;	is.setstate(std::ios_base::failbit);
				return is;
			}
			string in_s(1, sign);
			while(isdigit(is.peek()))	in_s+= is.get();
			A= in_s;
			return is;
		}
	// converse to other classical type
		explicit operator bool() const	{
			return isinf()||isNaN();
		}
		explicit operator int()	const	{
			int temp= 0;
			for(int i=0; i<d ;i++)	temp= temp* 10000+ num[i];
			if(sign<0)	temp= -temp;
			return temp;
		}
	};
}


#endif

#ifndef TBBLINT_H
#define TBBLINT_H

#include <iostream>	//version:3.2
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cctype>
#include <cstring>
#include <string>

typedef unsigned long long u64;
typedef long long i64;
typedef unsigned u32;
namespace tbb	{
	const int vol= 131072;
	const double Pi= 3.14159265358979323846;
	using std::cin;
	using std::cout;
	using std::endl;
	using std::string;

	using std::sin;
	using std::cos;
	using std::sqrt;
	using std::pow;

	inline int s2i(const char *begin, const char *end) {//converse string to int
		int tmp= 0, sig= 1;
		if(*begin=='+'||*begin=='-')	sig= (*(begin++)=='+')?1:-1;
		for(const char *t= begin; t<end; t++)	tmp= tmp*10+ (*t-'0');
		return tmp*sig;
	}
	inline const char* i2s(int L, char* dest= 0)	{//converse int to classical C-style string and return it
		static char temp[20];
		if(dest==0)	dest= temp;
		if(L==0)	{
			dest[0]= '0', dest[1]= '\0';
			return dest;
		}
		char* org= dest;
		if(L<0)	dest[0]= '-', dest++, L= -L;
		u32 t= 1;
		while(t<=unsigned(L))	t*=10;
		while(t!=1)	t/=10, *(dest++)= '0'+(L/t), L%=t;
		*dest= '\0';
		return org;
	}
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
	inline void Fast_0_out(u32 a, int len= 4)	{
		if(len>=2)	if(a<10)	putchar('0');
		if(len>=3)	if(a<100)	putchar('0');
		if(len>=4)	if(a<1000)	putchar('0');
		Fast_out(a);
	}
	inline const char* Fast_0_out_char(u32 a, char* dest= 0, int len= 4)	{
		static char temp[256];	if(dest==0)	dest= temp;
		if(len>256)	len= 255;
		const char* a_str= i2s(a);
		int left_0= len- strlen(a_str);	if(left_0<0)	left_0= 0;
		for(int i=0; i<left_0; i++)	dest[i]= '0';
		for(int j=0; a_str[j]!=0; j++)	dest[left_0+j]= a_str[j];
		dest[std::max(len, int(strlen(a_str)))]= '\0';
		return dest;
	}
	inline std::string Fast_0_out_str(u32 a, int len= 4)	{
		const char* a_str= i2s(a);
		int left_0= len- strlen(a_str);	if(left_0<0)	left_0= 0;
		string ans(left_0, '0');
		return ans+ a_str;
	}
	int Log_2(int base) {
		int i;
		for(i=0; ((1<<i)<base) && (i<32) ;i++);
		return i;
	}
	inline int higdgst(int A)	{
		if(A<0)	A= -A;
		while(A/10!=0)	A/=10;
		return A;
	}
	//structure complex, DFT, FFT & circulate conversion
	struct complex {
		double x,y;
		complex(const complex &A):x(A.x), y(A.y){}
		complex(double _x= 0, double _y= 0):x(_x), y(_y){}
		complex& operator= (const double &B)	{x=B, y=0; return *this;}
		complex& operator= (const complex &B)	{x=B.x, y=B.y; return *this;}
	    inline double abs() const  {return std::sqrt(((*this)*conj()).x);}
	    inline complex conj () const {return complex(x,-y);}
	    inline complex left () const {return complex(-y,x);}
	    inline complex right() const {return complex(y,-x);}
	    inline complex operator - (void) const {return complex(-x, -y);}
	    inline complex operator + (const double &B) const {return complex(x+B, y);}
	    inline complex operator - (const double &B) const {return complex(x-B, y);}
	    inline complex operator * (const double &B) const {return complex(x*B, y*B);}
	    inline complex operator / (const double &B) const {return complex(x/B, y/B);}
	    inline complex operator + (const complex &B) const {return complex(x+B.x, y+B.y);}
	    inline complex operator - (const complex &B) const {return complex(x-B.x, y-B.y);}
	    inline complex operator * (const complex &B) const {return complex( (x*B.x-y*B.y) , (x*B.y+y*B.x) );}
	    inline complex operator / (const complex &B) const {return (*this)*B.conj()/(B*B.conj()).x;}
	    inline complex & operator += (const complex &B) {return *this= *this+ B;}
	};
	complex root_cplx[vol];
	bool pre_init= false;
	void init_root()  {
	    if(pre_init)   return ;
	    pre_init= true;
	    int i;  complex *a, *b, *c, *d;
	    double x, y;
	    complex* root= root_cplx;
	    for(i=0, a=root, b= c= root+vol/2, d= root+vol; i<=vol/4; i++, a++, b--, c++, d--)  {
	        x= std::cos(2*i*Pi/vol), y= std::sin(2*i*Pi/vol);
	        *a= complex(x, y);  *b= complex(-x, y);
	        *c= complex(-x, -y);*d= complex(x, -y);
	    }
	}
	void DFT(const complex* A, complex* a, int n, bool inv= false)  {
	    if(n==0)    {a[0]= A[0]; return ;}
	    int log= 0;
	    {int ans;   for(ans= 1; ans<n; log++, ans<<=1)  if(ans>n)   return ;}
	    if(!pre_init)   init_root();
	    static int rev[vol], last_n= 0;
	    static complex rt[vol/2+1];
	    static complex temp[vol];
	    if(last_n!=n)   {
	        rev[0]=0, rev[1]= n/2;
	        for(register int i=2; i<n; i++)  rev[i]=(rev[i&1])|(rev[i>>1]>>1);
	        last_n= n;
	    }
	    for(register int i=0; i<n; i++)   temp[i]= A[rev[i]];
	    for(register int size= 2; size<= n; size<<=1) {
	        for(register int i=0; i<size/2; i++)  rt[i]= inv?root_cplx[vol- vol/size*i]:root_cplx[vol/size*i];
	        for(int k=0; k<n; k+=size)  {
	            for(int i=0; i<size/2; i++) {
	                complex q= temp[k+size/2+i]*rt[i];
	                temp[k+size/2+i]= temp[k+i]-q;
	                temp[k+i]+=q;
	            }
	        }
	    }
	    if(inv) for(register int i=0; i<n; i++)   temp[i]= temp[i]/n;
	    for(register int i=0; i<n; i++)   a[i]= temp[i];
	}
	void circ_conv(const double* A, const double* B, double* C, int n)   {
	    if(n<=1024) {
	        for(register int k=0; k<n; k++) C[k]= 0.0;
	        for(register int i=0; i<n; i++) for(register int j=0; j<n; j++) C[(i+j)%n]+= A[i]* B[j];
	        return ;
	    }
	    typedef complex cmxd;
	    int log= 0;
	    {int ans;   for(ans= 1; ans<n; log++, ans<<=1)  if(ans>n)   return ;}
	    if(!pre_init)   init_root();
	    static cmxd P[vol/2], Q[vol/2];
	    static cmxd a_0[vol/2], a_1[vol/2], b_0[vol/2], b_1[vol/2];
	    static cmxd c_0[vol/2], c_1[vol/2];
	    for(register int i=0; i<n/2; i++)   P[i].x= A[i<<1], P[i].y= A[(i<<1)|1];
	    DFT(P, P, n/2, false);
	    Q[0]= P[0].conj();  for(register int i=1; i<n/2; i++)   Q[i]= P[n/2- i].conj();
	    for(register int i=0; i<n/2; i++)   a_0[i]= (P[i]+Q[i])/2, a_1[i]= ((P[i]-Q[i])/2).left();

	    for(register int i=0; i<n/2; i++)   P[i].x= B[i<<1], P[i].y= B[(i<<1)+1];
	    DFT(P, P, n/2, false);
	    Q[0]= P[0].conj();  for(register int i=1; i<n/2; i++)   Q[i]= P[n/2- i].conj();
	    for(register int i=0; i<n/2; i++)   b_0[i]= (P[i]+Q[i])/2, b_1[i]= ((P[i]-Q[i])/2).left();

	    for(register int i=0; i<n/2; i++)   {
	        c_0[i]= a_0[i]* b_0[i]+ a_1[i]* b_1[i]* root_cplx[2*vol/n*i];
	        c_1[i]= a_0[i]* b_1[i]+ a_1[i]* b_0[i];
	    }
	    for(register int i=0; i<n/2; i++)   P[i]= c_0[i]+ c_1[i].right();
	    DFT(P, P, n/2, true);
	    for(register int i=0; i<n/2; i++)   C[i<<1]= P[i].x, C[(i<<1)|1]= P[i].y;
	}
	struct LInt	{
	//elements
		short sign;
		int d;
		u32 *num;
	//define function/initial
		LInt (void ):sign(0),d(0),num(0){}
		LInt (bool b, int code= 0):sign(0),d(0)	{
			if(b)	{num=new u32[0];}
			else	{
				num= 0;
				if(code>0)	sign= 2;
				if(code<0)	sign= -2;
			}
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
		LInt (const string &inString_):num(0)	{
			*this= inString_.c_str();
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
		virtual ~LInt() {if(num!=0)	delete[] num;}
	//assignment operator
		LInt & operator=(const LInt &B)	{
			sign=B.sign;	d=B.d;
			if(num!=0)	delete[] num;
			if(B.isNaN())	num= 0;
			else	{num= new u32[d];	for(int i=0; i<d; i++)	num[i]=B.num[i];}
			return *this;
		}
		LInt & operator=(const char *inString) {
			LInt temp(inString);
			d= temp.d; sign= temp.sign;
			if(num!=0)	delete[] num;
			num= temp.num; temp.num=0;
			return *this;
		}
		inline LInt & operator=(bool b)  {return *this=LInt(b);}
        inline LInt & operator=(int i)  {return *this= LInt(i);}
		inline LInt & operator=(i64 i)	{return *this= LInt(i);}
		inline LInt & operator=(u64 u)	{return *this= LInt(u);}
	//compare operator
		bool operator<(const LInt &B) const	{
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
				return ;
			}
			if(sign==0)	{
				if(num==0)	printf("NaN");
				else	putchar('0');
				return;
			}
			if(sign==-1)	putchar('-');
			Fast_out(num[d-1]);
			for(int i=d-2; i>=0; i--)	Fast_0_out(num[i]);
		}
		string print_str()	const	{
			if(sign==0)	return num==0?string("NaN"):string("0");
			if(sign==2||sign==-2)	return sign==-2?string("-inf"):string("inf");
			string ans;
			if(sign==-1)	ans+='-';
			ans+=i2s(num[d-1]);
			for(int i=d-2; i>=0; i--)	{
				if(num[i]<10)	ans+='0';
				if(num[i]<100)	ans+='0';
				if(num[i]<1000)	ans+='0';
				ans+=i2s(num[i]);
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
		inline bool positive()	const	{return sign>0;}
		inline bool negative()	const	{return sign<0;}
		inline bool isinf()	const	{return sign==2||sign==-2;}
		inline bool zero()	const	{return sign==0&&num!=0;}
		inline bool meanless()	const	{return isNaN()||isinf();}
		inline bool abnormal()	const	{return zero()||isinf()||isNaN();}
		friend void swap(LInt &A, LInt &B)	{
			int temp_d; short temp_sign;	u32* temp_num;
			temp_d= A.d;	A.d= B.d;	B.d= temp_d;
			temp_sign= A.sign;	A.sign= B.sign;	B.sign= temp_sign;
			temp_num= A.num;	A.num= B.num;	B.num= temp_num;
		}
	private://get 10000^2d/A while A >=0
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
	public:
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
			if(abnormal())	return *this;
			if(k<0)	return LInt(false);
			if(*this==0)	return LInt(0);
			LInt ans(false);
			u32 *_num = new u32[d+k]();
			for(int i=0; i<d; i++)	_num[i+k]=num[i];
			ans.num=_num;	ans.d=d+k;	ans.sign=sign;
			return ans;
		}
		const LInt operator>>(int k) const {
			if(abnormal())	return *this;
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
			if(B==1||B==-1)	return B==1?A:-A;
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
			LInt ans;
			register int x,y;
			int N=1<<(Log_2(A.d+B.d-1));
			ans.d=N+2;	ans.num= new u32[ans.d]();
			ans.sign=A.sign*B.sign;
			static double a[vol], b[vol], c[vol];
			for(x=0; x<A.d; x++)	a[x]= A.num[x];
			for(y=0; y<B.d; y++)	b[y]= B.num[y];
			for(x=A.d; x<N; x++)	a[x]= 0;
			for(y=B.d; y<N; y++)	b[y]= 0;
			for(x=0; x<N; x++)		c[x]= 0;
			circ_conv(a, b, c, N);
			double carry=0.0;
			for(int i=0; i<N; i++)	{
				double temp=round(c[i])+carry;
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
			if(B==0)	return A.positive()?"inf":"-inf";
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
			if(A.isinf())	return (!B.negative())?A:-A;
			if(B.isinf())	return 0;
			if(A==0)	return 0;
			if(B==0)	return A.positive()?"inf":"-inf";
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
			if(B.isinf())	return B.positive()?A:-A;
			if(A==0)	return 0;
			return *this-(*this/B)*B;
		}
		LInt & operator<<=(int k)	{
			LInt temp= *this<<k;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator>>=(int k)	{
			LInt temp= *this>>k;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator+=(const LInt &B)	{
			LInt temp= *this+ B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator-=(const LInt &B)	{
			LInt temp= *this- B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator*=(int p)	{
			LInt temp= *this* p;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator*=(const LInt &B)	{
			LInt temp= *this* B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator/=(int p)	{
			LInt temp= *this/ p;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator/=(const LInt &B)	{
			LInt temp= *this/ B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator%=(int B)	{
			LInt temp= *this% B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
		}
		LInt & operator%=(const LInt &B)	{
			LInt temp= *this% B;
			if(num!=0)	delete[] num;
			num= temp.num;	temp.num= 0;
			d= temp.d;	sign= temp.sign;
			return *this;
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
		inline u32 & operator[](int k) const	{
			return num[k];
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
			using std::isdigit;
			is>>std::ws;
			{
				if(is.peek()==-1)	{is.setstate(std::ios_base::eofbit);	A= 0;	return is;}
				if(!is)	{is.setstate(std::ios_base::failbit);	A= 0;	return is;}
				char t= is.peek();
				while(t!='-'&&!isdigit(t)&&t!=-1)	{
					is.get();	t= is.peek();
				}
				if(t==-1)	{is.setstate(std::ios_base::failbit);	A= 0;	return is;}
			}
			char sign= is.peek();
			if(sign=='-'||sign=='+')	is.get();
			else	sign= '0';
			// NaN or inf?	//No. Like the input of float, refuse NaN
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
	#if __cplusplus >= 201103L
		explicit operator bool() const	{
			return isinf()||isNaN();
		}
		explicit operator int()	const	{
			int temp= 0;
			for(int i=0; i<d ;i++)	temp= temp* 10000+ num[i];
			if(sign<0)	temp= -temp;
			return temp;
		}
	#endif
	};
	LInt mul_pow10(const LInt &A, int k)    {
		//return A*10^k
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
}

#endif

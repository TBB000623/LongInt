#include <iostream>	//version:2.0
#include <cstdio>
#include <algorithm>
#include <queue>
#include <vector>
#include <cstring>
#include <string>
typedef unsigned long long u64;
typedef long long i64;
typedef unsigned u32;

using std::cout;
using std::endl;

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

int BUFF_max=10000;
struct LInt	{
//elements
	short sign;
	int d;
	u32 *num;
//define function
	LInt()	{
		sign=d=0;
		num=new u32[BUFF_max];
		memset(num,0,BUFF_max*sizeof(u32));
	}
	LInt(bool b)	{
		sign=d=0;
		if(b)	{
			num=new u32[BUFF_max];
			memset(num,0,BUFF_max*sizeof(u32));
		} else	num=0;
	}
	LInt(i64 b)	{
		if(b==0)	{
			sign=d=0;
			num=new u32[1];
			num[0]=0;
		} else	if(b<0)	{
			b=-b;
			sign=-1;
		} else	sign=1;
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
	LInt(u64 b)	{
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
		if(inString[0]=='-')	{
			minus=true;
			inString++;
		}
		len=strlen(inString);
		for(i=0; i<len; i++)	{
			flag=flag&&('0'<=inString[i]&&inString[i]<='9');
			zero=zero&&('0'==inString[i]);
		}
		if(!flag)	{
			*this=false;
			return ;
		}
		
		if(zero)	{
			*this=0LL;
			return ;
		}
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
			if(j==4)	{
				j -= 4;
				num[d++] = temp;
			}
		}
		if(j>0)	num[d++] = temp;
		this->sho();
	}
	LInt (const LInt &A)	{
		sign=A.sign;
		d=A.d;
		num=new u32[d];
		for(int i=0; i<d; i++)	num[i]=A.num[i];
	}
//undo function
	~LInt()	{
		if(num!=0)	delete[] num;
	}
//operator
	LInt operator=(const LInt &B)	{
		sign=B.sign;
		d=B.d;
		u32 *pt_num=num;
		num=new u32[d];
		for(int i=0; i<d; i++)	num[i]=B.num[i];
		delete[] pt_num;
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
		if(sign==0)	return false;
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
	LInt make_LInt_zero(void)	{
		LInt A(0LL);
		*this=A;
		return A;
	}
	LInt abs()	{
		if(sign==-1)	sign=1;
		return *this;
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
//operator
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
			if(*this<D)	{
				return -((-B)+(-*this));
			}
		}
		if(sign==-1&&B.sign==1)	{
			return B+(*this);
		}
	}
	LInt operator-(const LInt &B) const	{
		LInt temp;
		temp=-B;
		temp=*this+temp;
		return temp;
	}
	LInt operator*(LInt &B) const	{
		LInt ans(true);
		ans.d=this->d+B.d+1;
		ans.sign=this->sign*B.sign;
		int i;
		int x,y;
		for(x=0; x<d; x++)	for(y=0; y<B.d; y++)	{
				ans.num[x+y]+=num[x]*B.num[y];
				ans.num[x+y+1]+=ans.num[x+y]/10000;
				ans.num[x+y]%=10000;
			}
		ans.sho();
		return ans;
	}
};


int main()	{
	char in[100000];
	scanf("%s",in);
	LInt A(in);
	scanf("%s",in);
	LInt B(in);
	(A+B).print();
	std::cout<<std::endl;
	return 0;
}

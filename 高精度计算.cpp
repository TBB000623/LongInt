#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <algorithm>
#define DEBUG
typedef struct integer LInt;
typedef struct integer* ptLInt;
using namespace std;
//LInt mean Long Integer,int also means it
//Sho->Shorter
//xxxZero->xxx==0?1:0;
//Make0->return 0
//free->to free the space
//Sub->Subtance,Mul->Multiply
//Warning! poi means POstive Integer,no means Yudachi's pet phrase!

unsigned long long MAXN=100000;
//Function
struct	integer	{
	int	n;
	int	sign;
	int	*num;
};
LInt poINF,neINF;
ptLInt	zero;
ptLInt newLInt()	{
	int *pt=new int[MAXN];
	ptLInt ret=new LInt;
	ret->num=pt;
	memset(pt,0,MAXN*sizeof(int));
	return ret;
}
ptLInt intSho(ptLInt	a)	{
	if(a->n<0)	a->n=0;
	while(a->n>0 && a->num[a->n-1]==0)	a->n--;
	if(a->n==0)	a->sign=0;
//	printf("n=%d,sign=%d\n",a->n,a->sign);
	int	*pt;
	if(a->n!=0)	pt=new int[a->n];
	else	pt=new int[1];
	pt[0]=0;
	int i;
	if(a->n!=0)	for(i=0;i<a->n;i++)	pt[i]=a->num[i];
	delete [] a->num;
	a->num=pt;
	return	a;
}
int intZero(ptLInt	a)	{
	int i;
	for(i=0;i<a->n;i++)	if(a->num[i]!=0)	return 0;
	return 1;
}
ptLInt	intMake0()	{
	ptLInt	ans=new LInt;
	int *pt=new int[1];
	pt[0]=ans->sign=ans->n=0;	ans->num=pt;
	return ans;
}
ptLInt	intMPow10(int n)	{
	if(n<0)	return intMake0();
	int *num=new int[n+1];
	int i;
	for(i=0;i<n;i++)	num[i]=0;
	num[n]=1;
	ptLInt	ans=new LInt;
	ans->sign=1;	ans->n=n+1;	ans->num=num;
	return ans;
}
int intFree(ptLInt	a){
	if(a==NULL)	return 0;
	delete [] a->num;
	delete a;
	return 0;
}
int	intDiv2(ptLInt a)	{
	if(intZero(a)==1)	return 0;
	int *temp=new int[a->n];
	int i;
	for(i=0;i<a->n;i++)	temp[i]=a->num[i];
	for(i=a->n-1;i>=0;i--)	{
		a->num[i]=temp[i]/2;
		if(temp[i]%2==1 && i!=0)	temp[i-1]+=10;
	}
	delete [] temp;
	intSho(a);
	return 1;
}
ptLInt intCopy(ptLInt a){
	ptLInt ans=new LInt;
	ans->sign=a->sign;	ans->n=a->n;
	int i;
	ans->num=new int[ans->n+1];
	for(i=0;i<ans->n;i++)	ans->num[i]=a->num[i];
	intSho(ans);	intSho(a);
	return ans;
}
int	intMod10(int *a)	{
	int ans=0;
	if(*a<0)	{
		ans=-((-*a)/10+1);
		*a-=ans*10;
	}
	ans+=(*a)/10;
	*a%=10;
	return ans;
}
ptLInt intAbs(ptLInt in)	{
	ptLInt out=intCopy(in);
	if(out->sign==-1)	out->sign+=2;
	return out;
}
ptLInt intInput()	{
	char *temp=new char[MAXN];
	char *in=new char[MAXN];
	scanf("%s",temp);
	int i,sign=1;
//	printf("%s\n",temp);//test
	if(temp[0]=='-')	{
		sign=-1;
		temp++;
	}
//	printf("%s\n",temp);//test
	i=0;
	while((in[i]=temp[i])!='\0')	i++;
	temp+=(sign-1)/2;	delete [] temp;
//	printf("%s\n",in);//test
	for(i=0;i<strlen(in);i++)	if('0'>in[i]||in[i]>'9')	{
		delete [] in;
		return NULL;
	}
//	printf("%s\n",in);
	int *num=new int[MAXN];
	int len=strlen(in);
//	printf("%s\n",in);
	for(i=len-1;i>=0;i--)	num[len-1-i]=in[i]-'0';
//	printf("%s\n",in);
	delete [] in;
	LInt *ans=new LInt;
	ans->num=num;	ans->n=len;	ans->sign=sign;
	intSho(ans);
	return ans;
}
int	intOutput(ptLInt a)	{
	int i;
//	printf("ans,n=%d,sign=%d\n",a->n,a->sign);
//	for(i=0;i<a->n;i++)	printf("%d ",a->num[i]);
	if(-2>a->sign||2<a->sign)	return 1;
	if(a->n<0)	return 2;
	for(i=0;i<a->n;i++)	if(a->num[i]<0||a->num[i]>9)	return 3;
	if(a->sign==-1)	printf("-");
	if(a->n==0)	printf("0");
	else for(i=a->n-1;i>=0;i--)	printf("%d",a->num[i]);
	return 0;
}
int poiCamp(ptLInt a1,ptLInt a2)	{
	if(a1==NULL)	return 999;
	if(a2==NULL)	a2=zero;
//	printf("a1->sign-a2->sign=%d\na1->n-a2->n=%d\n",a1->sign-a2->sign,a1->n-a2->n);
	if(a1->sign!=a2->sign)	return a1->sign-a2->sign;
	if(a1->n!=a2->n)	return a1->n-a2->n;
	int i;
	for(i=a1->n-1;i>=0;i--)	if(a1->num[i]!=a2->num[i])	{
//	printf("a1->num[i]-a2->num[i]=%d,i=%d\n",a1->num[i]-a2->num[i],i);
	return a1->num[i]-a2->num[i];
	}
	return 0;
}
ptLInt poiAdd(ptLInt a1,ptLInt a2)	{
	int n1=a1->n,n2=a2->n;
	int n=((n1<n2)?n2:n1)+1;
	ptLInt ans=new LInt;
	ans->num=new int[n];
	int i;
	int *num1=new int[n];
	int *num2=new int[n];
	for(i=0;i<n;i++)	num1[i]=num2[i]=0;
	for(i=0;i<n1;i++)	num1[i]=a1->num[i];
	for(i=0;i<n2;i++)	num2[i]=a2->num[i];
	for(i=0;i<n;i++)	ans->num[i]=num1[i]+num2[i];
	for(i=0;i<n-1;i++)	ans->num[i+1]+=intMod10(&ans->num[i]);
	delete [] num1;
	delete [] num2;
	ans->n=n;
//	printf("%d\n",ans->n);
	ans->sign=a1->sign;
	intSho(ans);
//	printf("%d\n",ans->n);
	return ans;
}
ptLInt poiSub(ptLInt a1,ptLInt a2)	{
	int n1=a1->n,n2=a2->n,n=n1;
	int i;
	int *num1=new int[n];
	int *num2=new int[n];
	int *ans=new int[n];
	for(i=0;i<n;i++)	num1[i]=num2[i]=ans[i]=0;
	for(i=0;i<n;i++)	{
		if(i<n1)	num1[i]=a1->num[i];
		if(i<n2)	num2[i]=a2->num[i];
		ans[i]=num1[i]-num2[i];
	}
	for(i=0;i<n-1;i++)	ans[i+1]+=intMod10(ans+i);
	ptLInt ret=new LInt;
	ret->num=ans;	ret->sign=1;	ret->n=n;
	delete [] num1;	delete [] num2;
	intSho(ret);
	return ret;
}
ptLInt intSub(ptLInt a,ptLInt b)	{
	if(intZero(b))	return intCopy(a);
	if(intZero(a))	{
		b->sign=-b->sign;
		ptLInt ans=intCopy(b);
		b->sign=-b->sign;
		return ans;
	}
	if(poiCamp(a,b)==0)	return intMake0();
	if(a->sign==b->sign)	{
//		printf("aksdokd\n");
		if(a->sign==1)	{
			int k=poiCamp(a,b);
			if(k>0)	return poiSub(a,b);
			if(k<0)	{
				ptLInt ans=poiSub(b,a);
				ans->sign=-1;
				return ans;
			}
		}
		if(a->sign==-1)	{
			ptLInt ans;
			a->sign=b->sign=1;
			int k=poiCamp(a,b);
			if(k>=0)	ans=poiSub(a,b);
			if(k<0)	{
				ans=poiSub(b,a);
				ans->sign=-1;
			}
			a->sign=b->sign=-1;
			ans->sign=-ans->sign;
			return ans;
		}
	}
	if(a->sign!=b->sign)	return poiAdd(a,b);
}
ptLInt intAdd(ptLInt a,ptLInt b)	{
	if(intZero(a)==1)	return intCopy(b);
	if(intZero(b)==1)	return intCopy(a);
	if(a->sign==b->sign)	return poiAdd(a,b);
	ptLInt temp=intCopy(b);
	temp->sign=-temp->sign;
	ptLInt ans=intSub(a,temp);
	intFree(temp);
	return ans;
}
ptLInt intMul(ptLInt a1,ptLInt a2)	{
	int n1=a1->n,n2=a2->n,s1=a1->sign,s2=a2->sign;
	if(n1==0||s1==0||n2==0||s2==0)	return intMake0();
	if(n1==1&&a1->num[0]==1)	return intCopy(a2);
	if(n2==1&&a2->num[0]==1)	return intCopy(a1);
	int n=n1+n2;
	ptLInt ans=new LInt;
	ans->n=n;	ans->sign=s1*s2;
	ans->num=new int[n];
	int	i,x,y;
	for(i=0;i<n;i++)	ans->num[i]=0;
	for(x=0;x<n1;x++)	for(y=0;y<n2;y++)	ans->num[x+y]+=a1->num[x]*a2->num[y];
	for(i=0;i<n-1;i++)	ans->num[i+1]+=intMod10((ans->num)+i);
	intSho(ans);
	return ans;
}
ptLInt	intMul2(ptLInt a)	{
	int n=a->n,s=a->sign;
	if(n==0||s==0)	return intMake0();
	ptLInt ans=new LInt;
	ans->num=new int[n+2];
	memset(ans->num,0,(n+2)*sizeof(int));
	ans->sign=s;	ans->n=n+1;
	int i;
	for(i=0;i<n;i++)	ans->num[i]=a->num[i]*2;
	for(i=0;i<n;i++)	ans->num[i+1]+=intMod10((ans->num)+i);
	intSho(ans);
	return ans;
}
ptLInt	intDiv(ptLInt a1,ptLInt a2)	{
	int n1=a1->n,n2=a2->n;
	if(intZero(a2)==1)	return NULL;
	ptLInt k1=intAbs(a1),k2=intAbs(a2);
	if(poiCamp(a1,a2)==0)	return intMPow10(0);
	ptLInt low=intMPow10(n1-n2-1);
	ptLInt high=intMPow10(n1-n2+1);
	ptLInt mid=NULL,mul=NULL;
	while(1)	{
		intFree(mul);
		mul=NULL;
		mid=intAdd(low,high);
		intDiv2(mid);		
//		printf("(");intOutput(low);printf(",");intOutput(mid);printf(",");intOutput(high);printf(")\n");
		if(poiCamp(low,mid)==0)	break;
		mul=intMul(mid,k2);
		int cam=poiCamp(k1,mul);
//		printf("camp=%d\n",cam);
		if(cam==0)	break;
		if(cam>0)	{intFree(low);	low=mid;}
		if(cam<0)	{intFree(high);	high=mid;}
	}
//	tab1:	printf("!\n");
	intFree(high);	high=NULL;
	intFree(low);	low=NULL;
	intFree(mul);	mul=NULL;
//	intOutput(mid);	
	mid->sign=a1->sign*a2->sign;
	intFree(k1);	intFree(k2);
	return mid;
}
ptLInt intPer(ptLInt a1,ptLInt a2)	{
	ptLInt ans1=intDiv(a1,a2);
	ptLInt ans2=intMul(a2,ans1);
	ptLInt ans=intSub(a1,ans2);
	intFree(ans1);	ans1=NULL;
	intFree(ans2);	ans2=NULL;
	return ans;
}
ptLInt intToLInt(int   p)	{
	ptLInt ret=newLInt();
	ret->sign=1;
	int i=0;
	if(p==0)	{
		ret->n=ret->sign=0;
		return intSho(ret);
	}
	if(p<0)	ret->sign=-1,p=-p;
	while(p!=0)	{
		ret->num[i]=p%10;
		p/=10;
		i++;
	}
	ret->n=i;
	return ret;
}
ptLInt strToLInt(char *p)	{
	ptLInt ret=newLInt();
	int flag=0,i;
	if(p[0]=='-')	flag=-1,p++;	else	flag=1;
	int len=strlen(p);
	for(i=0;i<len;i++)	{
		if(p[i]!='0')	break;
		if(i==len-1)	flag=0;
	}
	ret->sign=flag;	ret->n=len;
	for(i=0;i<len;i++)	ret->num[len-1-i]=p[i]-'0';
	intSho(ret);
	return ret;
}
ptLInt intPow(ptLInt base,int pow)	{
	if(base==NULL||base->n*2*pow>MAXN)	return NULL;
	ptLInt ans=intMPow10(0);
	ptLInt use=intCopy(base);
	ptLInt temp=NULL;
	while(pow>0)	{
		if(pow&1)	{
			temp=intMul(ans,use);
			intFree(ans);
			ans=temp;
			temp=NULL;
		}
		pow/=2;
		temp=intMul(use,use);
		intFree(use);
		use=temp;
		temp=NULL;
	}
	intFree(use);
	return ans;
}
ptLInt intPow(ptLInt base,ptLInt pow)	{
	if(base==NULL)	return NULL;
	if(poiCamp(pow,NULL)<0)	return NULL;
	if(poiCamp(pow,NULL)==0)	return intMPow10(0);
	ptLInt k=intToLInt(base->n);
	ptLInt mul=intMul(k,pow);
	ptLInt abs=intAbs(mul);
	ptLInt max=intToLInt(MAXN);
	if(poiCamp(abs,max)>0)	{
		intFree(k);
		intFree(mul);
		intFree(abs);
		intFree(max);
		return NULL;
	}
	ptLInt ans=intMPow10(0);
	ptLInt use=intCopy(base);
	ptLInt use_pow=intCopy(pow);
	ptLInt temp=NULL;
	while(poiCamp(use_pow,NULL)>0)	{
		if(intDiv2(use_pow)==1)	{
			temp=intMul(ans,use);
			intFree(ans);
			ans=temp;
			temp=NULL;
		}
		temp=intMul(use,use);
		intFree(use);
		use=temp;
		temp=NULL;
	}
	intFree(use_pow);
	intFree(use);
	intFree(k);
	intFree(mul);
	intFree(abs);
	intFree(max);
	return ans;
}
int main()	{
	ptLInt a=intInput(),b=intInput(),c;
	c=intAdd(a,b);
	intOutput(c);
	intFree(a);
	intFree(b);
	intFree(c);
	return 0;
}

#define LOCAL
#include <iostream>	//version:2.3
#include <sstream>
#include <cstdio>
#include <algorithm>
#include <queue>
#include <vector>
#include <cstring>
#include <string>
#define SHOW	putchar('s')
#define EL	putchar('\n')
// #include "LFloat.h"
#include "LInt.h"
#include "LMath.h"
#include "LFloat.h"
using namespace tbb;
using namespace std;
LFloat sin(const LFloat &x)	{
	LFloat s=0;
	LFloat a= x;
	s= a;
	for(int k=2; k<=50; k+=2)	{
		a= a*x*x/k/(k+1);
		if((k/2)%2==1)	s= s-a;	else	s= s+a;
		// cout<<s.print_str()<<endl;
	}
	return s;
}
LFloat pi()	{
	LFloat a= 3.1, b= 3.2, m, eps("0.000000000001");
	LFloat delta= b-a;
	while((delta-eps).positive())	{
		m= (a+b)/2;
		if((sin(a)*sin(m)).positive()) a= m;	else b= m;
		delta= b-a;
		cout<<m.print_str()<<endl;
	}
	return m;
}
int main()	{
#include "localtest.h"
	string L;
	cin>>L;
	LFloat x(L);
	cout<<sin(x*pi()).print_str()<<endl;
	return 0;
}

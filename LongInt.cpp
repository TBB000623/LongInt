#include <iostream>	//version:2.2
#include <cstdio>
#include <algorithm>
#include <queue>
#include <vector>
#include <cstring>
#include <string>

#include "D:\LongInt\LongInt\LInt.h"

using namespace TBB;
using namespace std;

int main()	{
	char A[60001]={0};
	cin>>A;
	LInt LA(A);

	std::cout<<sqrt(LA)<<std::endl;
	cin.get();
	return 0;
}

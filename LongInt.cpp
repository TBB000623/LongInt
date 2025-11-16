#include <iostream>

#include "LInt.h"
using namespace std;
using namespace tbb;
int main() {
	freopen("P1919_1.in", "r", stdin);
	freopen("P1919_1.ans", "w", stdout);
	LInt A, B;
	cin >> A >> B;
	cout << A * B << endl;
	(A * B).show(cerr);
	return 0;
}
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

const ld EPS = 1e-8;

ld l = 0, r = 1e10, c;

ld f(ld x) {
    return abs(x * x + sqrtl(x) - c);
}

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/

    cin >> c;

    for (int i = 0; i < 1000; ++i) {
        ld t1 = (l + l + r) / 3;
        ld t2 = (l + r + r) / 3;
        //cout << l sp r nl;

        if (f(t2) - f(t1) < EPS) {
            l = t1;
        } else {
            r = t2;
        }
    }

    cout << setprecision(8) << fixed << r nl;
}

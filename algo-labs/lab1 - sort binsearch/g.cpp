#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

ll n, t1, t2;

bool f(ll t) {
    return (t >= t1) + (t - t1) / t1 + (t - t1) / t2 >= n;
}

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/

    cin >> n >> t1 >> t2;
    if (t1 > t2) swap(t1, t2);

    /*if (n == 1) {
        cout << t1 nl;
        return 0;
    }*/

    ll l = 0, r = 1e15;

    while (r - l > 1) {
        ll t = (l + r) / 2;
        if (f(t)) r = t;
        else l = t;
    }

    cout << r nl;
}

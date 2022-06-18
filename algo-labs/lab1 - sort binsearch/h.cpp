#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

ll w, h, n;
ll l = 0, r = 1;

bool f(ll x) {
    return (x / w) * (x / h) >= n;
}

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/

    cin >> w >> h >> n;

    while (!f(r)) r *= 2;

    while (r - l > 1) {
        ll m = (l + r) / 2;
        if (f(m)) r = m;
        else l = m;
    }

    cout << r nl;
}

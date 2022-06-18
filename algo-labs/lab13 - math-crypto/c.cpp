#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <set>
#include <map>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;
typedef long double ld;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

ll gcd (ll a, ll b, ll & x, ll & y) {
	if (a == 0) {
		x = 0; y = 1;
		return b;
	}
	ll x1, y1;
	ll d = gcd (b%a, a, x1, y1);
	x = y1 - (b / a) * x1;
	y = x1;
	return d;
}

ll a, b, n, m;

// x = a (mod n)
// x = b (mod m)
// gcd(n, m) == 1

// x = c1 * n + a
// x = c2 * m + b
// c1 * n - c2 * m = b - a

void solve() {
    cin >> a >> b >> n >> m;
    ll c1, c2;
    ll g = gcd(n, -m, c1, c2);
    c1 *= (b - a) / g;

    ll l = -10000, r = 10000;

    while (r - l > 1) {
        ll M = l + (r - l) / 2;
        ll x = a + (c1 + M * m) * n;
        if (x < 0) {
            l = M;
        } else {
            r = M;
        }
    }

    while (a + (c1 + l * m) * n > 0) {
        l--;
    }

    while (a + (c1 + l * m) * n < 0) {
        l++;
    }

    ll x = a + (c1 + l * m) * n;
    cout << x nl;


}

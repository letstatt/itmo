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

ll n, e, C;

// n = p*q
// phi(n) = (p-1)*(q-1)
// a*d = 1 - e
// M = C^d mod n

ll binpow(ll a, ll b, ll n) {
    ll res = 1;
    while (b > 0) {
        if (b & 1) {
            res = (res * a) % n;
        }
        a = (a * a) % n;
        b /= 2;
    }
    return res;
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

void solve() {
    cin >> n >> e >> C;
    ll p = 2, q;

    while (n % p != 0) {
        p++;
    }

    q = n / p;
    ll phi = (p - 1) * (q - 1);
    ll x, y;
    ll g = gcd(e, phi, x, y);
    ll d = (x % phi + phi) % phi;
    ll M = binpow(C, d, n);
    cout << M nl;
}

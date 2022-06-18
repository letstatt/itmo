#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <complex>
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

typedef complex<ld> base;

const ld PI = acos(-1);

void fft (vector<base> & a, bool invert) {
	int n = (int) a.size();
	if (n == 1)  return;

	vector<base> a0 (n/2),  a1 (n/2);
	for (int i=0, j=0; i<n; i+=2, ++j) {
		a0[j] = a[i];
		a1[j] = a[i+1];
	}
	fft (a0, invert);
	fft (a1, invert);

	ld ang = 2*PI/n * (invert ? -1 : 1);
	base w (1),  wn (cos(ang), sin(ang));
	for (int i=0; i<n/2; ++i) {
		a[i] = a0[i] + w * a1[i];
		a[i+n/2] = a0[i] - w * a1[i];
		if (invert)
			a[i] /= 2,  a[i+n/2] /= 2;
		w *= wn;
	}
}

void multiply (const vector<int> & a, const vector<int> & b, vector<int> & res) {
	vector<base> fa (a.begin(), a.end()),  fb (b.begin(), b.end());
	size_t n = 1;
	while (n < max (a.size(), b.size()))  n <<= 1;
	n <<= 1;
	fa.resize (n),  fb.resize (n);

	fft (fa, false),  fft (fb, false);
	for (size_t i=0; i<n; ++i)
		fa[i] *= fb[i];
	fft (fa, true);

	res.resize (n);
	for (size_t i=0; i<n; ++i)
		res[i] = int (fa[i].real() + 0.5);
}

int n;
vector<int> a, b, c;

void solve() {
    cin >> n;
    a.resize(n + 1);
    b.resize(n + 1);

    for (auto &i: a) cin >> i;
    for (auto &i: b) cin >> i;

    multiply(a, b, c);
    for (int i = 0; i < 2 * n + 1; ++i) {
        cout << (i < c.size() ? c[i] : 0) sq;
    }
    cout nl;
}

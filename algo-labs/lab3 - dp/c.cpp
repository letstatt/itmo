#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, k;
vector<ll> a, d, p;
// d[i] - len

void solve() {
    cin >> n;
    a.resize(n);
    d.resize(n, 1);
    p.resize(n, -1);

    for (auto &i: a) cin >> i;
    ll best = -1e18;

    for (int i = 1; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            if (d[j] + 1 > d[i] && (a[j] < a[i] || p[i] != -1 && a[j] < a[p[i]])) {
                d[i] = d[j] + 1;
                p[i] = j;
            }
        }

        if (d[i] > best) {
            best = d[i];
            k = i;
        }
    }

    vector<int> path;
    int v = k;

    while (v != -1) {
        path.push_back(a[v]);
        v = p[v];
    }

    reverse(path.begin(), path.end());

    cout << d[k] nl;
    for (auto &i: path) cout << i sq;
    cout nl;
}

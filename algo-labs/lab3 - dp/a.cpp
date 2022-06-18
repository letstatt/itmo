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
vector<int> a, d, p;

void solve() {
    cin >> n >> k;
    a.resize(n);
    d.resize(n);
    p.resize(n);

    for (int i = 1; i < n - 1; ++i) {
        cin >> a[i];
    }

    d[0] = 0;
    p[0] = -1;

    for (int i = 1; i < n; ++i) {
        d[i] = -1e9;
        int j = i - 1;
        while (j >= 0 && i - j <= k) {
            if (d[i] < d[j] + a[i]) {
                d[i] = d[j] + a[i];
                p[i] = j;
            }
            --j;
        }
    }

    vector<int> path;
    int v = n - 1;

    while (v != -1) {
        path.push_back(v + 1);
        v = p[v];
    }

    reverse(path.begin(), path.end());

    cout << d[n - 1] nl << (path.size()-1) nl;
    for (auto &i: path) cout << i sq;
    cout nl;
}

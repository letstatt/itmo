#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    freopen("choose.in", "r", stdin);
    freopen("choose.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, k;
vector<int> a;

void print() {
    for (auto &i: a) {
        cout << i sq;
    }
    cout nl;
}

void rec(int i, int last) {
    if (i == k) {
        print();
        return;
    }
    for (int j = last + 1; j <= n; ++j) {
        a[i] = j;
        rec(i + 1, j);
    }
}

void solve() {
    cin >> n >> k;
    a.resize(k);

    rec(0, 0);
}

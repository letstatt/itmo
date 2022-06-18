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
    freopen("vectors.in", "r", stdin);
    freopen("vectors.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<int> a;
vector<vector<int>> b;


void rec(int i, bool from_one) {
    if (n == i) {
        b.push_back(a);
        return;
    }
    a[i] = 0;
    rec(i + 1, false);

    if (!from_one) {
        a[i] = 1;
        rec(i + 1, true);
    }
}

void solve() {
    cin >> n;
    a.resize(n);
    rec(0, false);
    cout << b.size() nl;

    for (auto &i: b) {
        for (auto &j: i) {
            cout << j;
        }
        cout nl;
    }
}

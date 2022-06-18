#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    freopen("gray.in", "r", stdin);
    freopen("gray.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<bool> a;

void rec(int i) {
    if (n == i) {
        for (auto i: a) {
            cout << i;
        }
        cout nl;
        return;
    }
    rec(i + 1);
    a[i] = 1 - a[i];
    rec(i + 1);
}

void solve() {
    cin >> n;
    a.resize(n);
    rec(0);
}

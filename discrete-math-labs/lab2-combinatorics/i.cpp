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
    freopen("brackets.in", "r", stdin);
    freopen("brackets.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<char> a;

void print() {
    for (auto &i: a) {
        cout << i;
    }
    cout nl;
}

void rec(int i, int bal) {
    if (i == n) {
        if (bal == 0) {
            print();
        }
        return;
    }
    a[i] = '(';
    rec(i + 1, bal + 1);
    if (bal > 0) {
        a[i] = ')';
        rec(i + 1, bal - 1);
    }
}

void solve() {
    cin >> n;
    n *= 2;
    a.resize(n);
    rec(0, 0);
}

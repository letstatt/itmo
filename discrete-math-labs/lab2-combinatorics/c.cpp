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
    freopen("antigray.in", "r", stdin);
    freopen("antigray.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<int> a;

void shift() {
    for (auto &i: a) {
        i = (i + 1) % 3;
    }
}

void print() {
    for (auto &i: a) {
        cout << i;
    }
    cout nl;
}

void rec(int i) {
    if (n == i) {
        print();
        shift();
        print();
        shift();
        print();
        shift();
        return;
    }
    a[i] = 0;
    rec(i + 1);
    a[i] = 1;
    rec(i + 1);
    a[i] = 2;
    rec(i + 1);
}

void solve() {
    cin >> n;
    a.resize(n);
    rec(1);
}

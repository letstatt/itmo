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
    freopen("chaincode.in", "r", stdin);
    freopen("chaincode.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<int> a;
set<vector<int>> s;

void print() {
    for (auto &i: a) {
        cout << i;
    }
    cout nl;
}

void shift() {
    for (int i = 1; i < n; ++i) {
        a[i - 1] = a[i];
    }
}

void solve() {
    cin >> n;
    a.resize(n);
    s.insert(a);
    print();

    while (true) {
        shift();
        a[n - 1] = 1;
        if (!s.count(a)) {
            s.insert(a);
            print();
        } else {
            a[n - 1] = 0;
            if (!s.count(a)) {
                s.insert(a);
                print();
            } else {
                break;
            }
        }
    }
}

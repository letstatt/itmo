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
    freopen("permutations.in", "r", stdin);
    freopen("permutations.out", "w", stdout);
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
vector<int> a;

void print() {
    for (auto &i: a) {
        cout << i sq;
    }
    cout nl;
}

void solve() {
    cin >> n;
    a.resize(n);

    for (int i = 0; i < n; ++i) {
        a[i] = i + 1;
    }

    do {
        print();

    } while (next_permutation(a.begin(), a.end()));
}

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
    //freopen("num2brackets.in", "r", stdin);
    //freopen("num2brackets.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n, k;
vector<vector<ll>> d;

void print() {
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < b[i].size(); ++j) {
            cout << b[i][j] sq;
        }
        cout nl;
    }
    cout nl;
}


void solve() {
    cin >> n >> k;
    d.resize(2 * n, vector<ll>(n + 1));
    d[0][0] = 1;

    for (int i = 1; i < d.size(); ++i) {
        for (int j = 0; j < d[0].size() - 1; ++j) {
            d[i][j] += d[i - 1][j + 1];
            if (j > 0) d[i][j] += d[i - 1][j - 1];
        }
    }

}

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <bitset>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, m;

string print_bin(int a) {
    string s = "";
    for (int i = 0; i < n; ++i) {
        s += '0' + (a & 1);
        a >>= 1;
    }
    reverse(s.begin(), s.end());
    return s;
}

void solve() {
    cin >> n;
    m = 1 << n;

    vector<int> a(m), b(m, -1);

    for (int i = 0; i < m; ++i) {
        cin >> a[i] >> a[i];
    }

    for (int i = 0; i < m; ++i) {
        b[i] = a[0];
        for (int j = 0; j < m - i - 1; ++j) {
            a[j] = a[j] ^ a[j + 1];
        }
    }

    for (int i = 0; i < m; ++i) {
        cout << print_bin(i) sp b[i] nl;
    }
}

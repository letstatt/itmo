#include <unordered_map>
#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;
typedef long double ld;
typedef unsigned int ull;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
unordered_map<string, int> cols;
vector<vector<int>> a;
vector<int> b;
string ans;

void solve() {
    cin >> n;
    a.resize(n, vector<int>(32 + 1));
    b.resize(32 + 1);

    for (int i = 0; i < n; ++i) {
        ll x;
        cin >> x;
        for (auto &j: a[i]) {
            j = x & 1;
            x >>= 1;
        }
    }

    ll s;
    cin >> s;

    if (s == 0) {
        cout << "1&~1" nl;
        return;
    }

    for (auto &i: b) {
        i = s & 1;
        s >>= 1;
    }

    for (int i = 0; i < b.size(); ++i) { // i-th bit
        string col;

        for (int j = 0; j < n; ++j) {
            col += '0' + a[j][i];
        }

        if (!cols.count(col)) {
            cols[col] = b[i];

        } else if (cols[col] != b[i]) {
            cout << "Impossible" nl;
            return;
        }

        if (!b[i]) continue;
        else if (ans.size()) {
            ans += "|"; // append
        }

        bool f = false;

        for (int j = 0; j < n; ++j) {
            if (a[j][i]) {
                ans += (f ? "&" : "(") + to_string(j + 1);
            } else {
                ans += (f ? "&~" : "(~") + to_string(j + 1);
            }

            f = true;
        }

        ans += ")";
    }

    cout << ans nl;
}

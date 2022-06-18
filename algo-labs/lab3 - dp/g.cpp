#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n;
string s;
vector<vector<int>> d, p;

string build(int i, int j) {
    if (d[i][j] == 0) {
        return s.substr(i, j - i + 1);
    } else if (d[i][j] == j - i + 1) {
        return "";
    }

    if (p[i][j] == -1) {
        return s[i] + build(i + 1, j - 1) + s[j];
    } else {
        return build(i, p[i][j]) + build(p[i][j] + 1, j);
    }
}

void solve() {
    cin >> s;
    n = s.size();

    d.resize(n, vector<int>(n));
    p.resize(n, vector<int>(n, -1));

    for (int i = 0; i < s.size(); ++i) {
        d[i][i] = 1;
    }

    for (int i = 0; i < s.size(); ++i) {
        for (int j = i - 1; j >= 0; --j) {
            d[j][i] = 1e6;

            if (s[j] == '(' && s[i] == ')' || s[j] == '{' && s[i] == '}' || s[j] == '[' && s[i] == ']') {
                d[j][i] = d[j + 1][i - 1];
            }

            for (int k = j; k < i; ++k) {
                if (d[j][k] + d[k + 1][i] < d[j][i]) {
                    d[j][i] = d[j][k] + d[k + 1][i];
                    p[j][i] = k;
                }
            }
        }
    }

    cout << build(0, s.size() - 1) nl;
    //cout << d[0][n - 1] nl;
}

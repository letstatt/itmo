#include <algorithm>
#include <iostream>
#include <cassert>
#include <vector>
#include <cmath>
#include <set>
#include <map>

using namespace std;

typedef long long ll;
typedef unsigned long long ull;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    solve();
}

vector<int> s, lcp;
vector<int> input;
string t;
int n;

// codeforces.com/blog/entry/66540
void build(vector<int> c) {
    n = c.size();
    vector<pair<pair<int, int>, int>> t(n);
    for (int i = 1; i < n; i *= 2) {
        for (int j = 0; j < c.size(); ++j) {
            t[j] = make_pair(make_pair(c[j], c[(j + i) % n]), j);
        }
        sort(t.begin(), t.end());
        for (int cnt = 0, j = 0; j < n; ++j) {
            if (j && t[j].first != t[j - 1].first) {
                cnt++;
            }
            c[t[j].second] = cnt;
        }
    }
    s.resize(n);
    for (int i = 0; i < n; ++i) {
        s[c[i]] = i;
    }
}

// um-nik.github.io/suffix-array
void build_lcp() {
    vector<int> rev_perm(n);
    lcp.resize(n);
    for (int i = 0; i < n; ++i) {
        rev_perm[s[i]] = i;
    }
    int cur_lcp = 0;
    for (int i = 0; i < n; ++i) {
        int p = rev_perm[i];
        if (p == n - 1) {
            lcp[p] = cur_lcp = 0;
            continue;
        }
        int q = s[p + 1];
        cur_lcp = max(0, cur_lcp - 1);
        while (i + cur_lcp < n && q + cur_lcp < n && input[i + cur_lcp] == input[q + cur_lcp]) {
            cur_lcp++;
        }
        lcp[p] = cur_lcp;
    }
}

void solve() {
    cin >> t;
    input.reserve(10000);

    for (auto &i: t) {
        input.push_back(i);
    }

    input.push_back('#');

    build(input);
    build_lcp();

    /* ---------- debug start ---------- */

    /*for (auto &i: shifts) {
        cout << i sq;
    }
    cout nl;

    for (auto &i: lcp) {
        cout << i sq;
    }
    cout nl;

    for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t";
        for (int j = i; j < n; ++j) {
            cout << char(input[j]);
        }
        cout nl;
    }

    for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t" << s[i] sp "\tlcp\t" sp lcp[i] sp "\tid\t" sp get_string_id(s[i]) nl;
    }*/

    /* ---------- debug end ---------- */

    ll ans = 0;

    for (int i = 1; i < n; ++i) {
        ans += (n - s[i] - 1);
    }

    for (int i = 1; i + 1 < n; ++i) {
        ans -= lcp[i];
    }

    cout << ans nl;
}

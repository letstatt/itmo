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

vector<int> s;
vector<int> input;
string t;
int n, s_max;

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
        s_max = max(s_max, c[i]);
    }
}

void solve() {
    cin >> t;
    input.reserve(10000);

    for (auto &i: t) {
        input.push_back(i);
    }

    //input.push_back(0);

    build(input);

    /* ---------- debug start ---------- */

    /*for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t";
        for (int j = i; j < n; ++j) {
            cout << char(input[j]);
        }
        cout nl;
    }

    for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t" << s[i] sp "\t";
        if (true) {
            for (int j = s[i]; j < t.size(); ++j) {
                cout << t[j];
            }

            for (int j = 0; j < s[i]; ++j) {
                cout << t[j];
            }
        }
        cout nl;
    }*/

    /* ---------- debug end ---------- */

    int k;
    cin >> k;

    if (k > 1 + s_max) {
        cout << "IMPOSSIBLE" nl;
        return;
    }

    k--;

    for (int i = s[k]; i < t.size(); ++i) {
        cout << t[i];
    }

    for (int i = 0; i < s[k]; ++i) {
        cout << t[i];
    }
}

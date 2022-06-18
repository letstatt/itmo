#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

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

vector<int> p;
string s, t;
int n;

void solve() {
    cin >> s >> t;
    string d = s + '\n' + t;
    n = d.size();
    p.resize(n);

    p[0] = 0;
    for (int i = 1; i < n; ++i) {
        int k = p[i - 1];
        while (k > 0 && d[i] != d[k]) {
            k = p[k - 1];
        }
        if (d[i] == d[k]) k++;
        p[i] = k;
    }

    vector<int> ans;

    for (int i = s.size() + 1; i < n; ++i) {
        if (p[i] == s.size()) {
            ans.push_back(i - 2 * s.size() + 1);
        }
    }

    cout << ans.size() nl;
    for (auto &i: ans) cout << i sq;
    cout nl;
}


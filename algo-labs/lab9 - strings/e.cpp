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
string s;
int n;

void solve() {
    cin >> s;
    n = s.size();
    p.resize(n);

    if (n == 1) {
        cout << 1 nl;
        return;
    }

    p[0] = 0;
    for (int i = 1; i < n; ++i) {
        int k = p[i - 1];
        while (k > 0 && s[i] != s[k]) {
            k = p[k - 1];
        }
        if (s[i] == s[k]) k++;
        p[i] = k;
    }

    if (p.back() < n / 2.0) {
        cout << n nl;
        return;
    }

    cout << (n - p.back()) nl;
}


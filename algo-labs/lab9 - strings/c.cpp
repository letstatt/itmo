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

vector<int> z;
string s;
int n;

void solve() {
    cin >> s;
    n = s.size();
    z.resize(n);

    int left = 0, right = 0;

    for (int i = 1; i < n; ++i) {
        z[i] = max(0, min(right - i, z[i - left]));
        while (i + z[i] < n && s[z[i]] == s[i + z[i]]) z[i]++;
        if (i + z[i] > right) {
            left = i;
            right = i + z[i];
        }
    }

    for (int i = 1; i < n; ++i) {
        cout << z[i] sq;
    }
    cout nl;
}


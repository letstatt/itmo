#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <deque>
#include <random>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << endl
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<int> a;
string s;
int n;

void solve() {
    cin >> n;
    a.push_back(1);

    for (int i = 2; i <= n; ++i) {
        int l = -1, r = i - 1; // l excluded
        while (r - l > 1) {
            int m = (l + r) / 2;
            cout << "1" sp a[m] sp i nl;
            cin >> s;
            (s == "YES" ? l : r) = m;
            // what if there's no way to build correct answer?
            // pray for its correctness
        }
        a.insert(a.begin() + r, i);
    }

    cout << "0" sq;
    for (auto &i: a) {
        cout << i sq;
    }
    cout nl;
}

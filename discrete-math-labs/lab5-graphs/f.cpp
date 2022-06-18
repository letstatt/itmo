#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <iostream>
#include <vector>
#include <deque>
#include <set>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

set<int> leaves;
vector<int> d;
deque<int> p;
int n;

void solve() {
    cin >> n;
    p.resize(n - 2);
    d.resize(n, 1);

    for (auto &i: p) (cin >> i), i--, d[i]++;

    for (int i = 0; i < n; ++i) {
        if (d[i] == 1) {
            leaves.insert(i);
        }
    }

    for (int i = 0; i < n - 2; ++i) {
        int u = *leaves.begin();
        leaves.erase(u);

        int v = p[i];
        cout << u+1 sp v+1 nl;
        if (--d[v] == 1) {
            leaves.insert(v);
        }
    }

    cout << *leaves.begin()+1 sp *(++leaves.begin())+1 nl;
}

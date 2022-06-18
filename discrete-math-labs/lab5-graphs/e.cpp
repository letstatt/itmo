#include <unordered_set>
#include <unordered_map>
#include <iostream>
#include <vector>
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

unordered_map<int, unordered_set<int>> g;
vector<int> d;
int n;

void solve() {
    cin >> n;
    d.resize(n);

    for (int i = 1; i < n; ++i) {
        int a, b;
        cin >> a >> b;
        d[--a]++, d[--b]++;
        g[a].insert(b);
        g[b].insert(a);
    }

    set<int> q;

    for (int i = 0; i < n; ++i) {
        if (d[i] == 1){
            q.insert(i);
        }
    }

    while (n > 2) {
        int v = *q.begin();
        q.erase(q.begin());
        int p = *g[v].begin();
        g[p].erase(v);

        if (--d[p] == 1) {
            q.insert(p);
        }

        cout << p+1 sq;
        n -= 1;
    }
}

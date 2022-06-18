#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<pair<int, pair<int, int>>> edges;
vector<int> p;
int n, m;

int getp(int i) {
    return p[i] == i ? i : p[i] = getp(p[i]);
}

bool unite(int i, int j) {
    int a = getp(i);
    int b = getp(j);
    if (a == b) {
        return false;
    }
    if (rand() & 1) {
        swap(a, b);
    }
    p[a] = b;
    return true;
}

void solve() {
    cin >> n >> m;
    edges.resize(m);
    p.resize(n + 1);

    for (int i = 0; i <= n; ++i) {
        p[i] = i;
    }

    for (auto &i: edges) {
        cin >> i.second.first >> i.second.second >> i.first;
    }

    sort(edges.begin(), edges.end());

    ll ans = 0;

    for (auto &i: edges) {
        if (unite(i.second.first, i.second.second)) {
            ans += i.first;
        }
    }

    cout << ans nl;
}

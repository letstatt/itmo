#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;
typedef long double ld;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

char x;
int n, v;
vector<vector<int>> g;
vector<int> used, used2;
vector<int> matching;

struct node {
    int x, y, t;
};

bool kuhn(int v) {
    if (used[v]) {
        return false;
    }

    used[v] = true;

    for (auto &i: g[v]) {
        if (matching[i] == -1 || kuhn(matching[i])) {
            matching[i] = v;
            return true;
        }
    }

    return false;
}

void ufo_tracer(int v) {
    used2[v] = true;

    if (matching[v] != -1 && !used2[matching[v]]) {
        ufo_tracer(matching[v]);
    }
}

void solve() {
    cin >> n >> v;

    g.resize(n);
    used.resize(n);
    used2.resize(n);
    matching.resize(n, -1);

    vector<node> d(n);

    for (auto &i: d) {
        int h;
        cin >> h >> x >> i.t >> i.x >> i.y;
        i.t += h * 60;
    }

    sort(d.begin(), d.end(), [](const node &a, const node &b) {
        return a.t < b.t;
    });

    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            // if current ufo could move this way, add an edge
            auto h = hypot((ld) d[i].x - d[j].x, (ld) d[i].y - d[j].y);

            if (d[i].t + (h / v * 60) <= d[j].t) {
                g[i].push_back(j);
            }
        }
    }

    for (int i = 0; i < g.size(); ++i) {
        if (kuhn(i)) {
            fill(used.begin(), used.end(), 0);
        }
    }

    // backtrace

    ll ans = 0;

    for (int i = g.size() - 1; i >= 0; --i) {
        if (!used2[i]) {
            // walk on right side
            ufo_tracer(i);
            ans++;
        }
    }

    cout << ans << endl;
}

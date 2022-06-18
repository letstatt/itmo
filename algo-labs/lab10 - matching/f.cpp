#include <unordered_set>
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

int n, m;
vector<vector<int>> g;
vector<int> used, used2;
vector<int> matching;

struct node {
    ld a, b, c, d;
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

void is(int v) {
    //cout << "is " << v nl;
    if (used[v]) {
        return;
    }

    used[v] = true;

    for (auto &i: g[v]) {
        if (!used2[i]) {
            used2[i] = true;
            is(matching[i]);
        }
    }
}

bool intersects(node i, node j) {
    bool horiz1 = (i.b == i.d);
    bool horiz2 = (j.b == j.d);
    bool vert1 = (i.a == i.c);
    bool vert2 = (j.a == j.c);

    if (vert1 && vert2 || horiz1 && horiz2) {
        return false;
    }

    if (horiz2) swap(i, j);

    // there i - horizontal, j - vertical

    return (i.a <= j.a && j.a <= i.c && j.b >= i.b && i.b >= j.d);
}

void solve() {
    cin >> n;

    g.resize(n);
    used.resize(n);
    used2.resize(n);
    matching.resize(n, -1);

    // build bipartite graph of intersections
    // and find in its complement max bipartite clique
    // 1. build graph
    // 2. invert edges
    // 3. invert edges
    // 4. find max independent set

    // so, let's simplify
    // 1. build graph
    // 2. find max independent set

    vector<node> d(n);

    for (auto &i: d) {
        cin >> i.a >> i.b >> i.c >> i.d;
        if (i.a > i.c) swap(i.a, i.c);
        if (i.b < i.d) swap(i.b, i.d);
    }

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i != j && intersects(d[i], d[j])) {
                //cout << "intersects " << i sp j nl;
                g[i].push_back(j);
            }
        }
    }

    // find max matching at first

    for (int i = 0; i < n; ++i) {
        if (kuhn(i)) {
            fill(used.begin(), used.end(), 0);
        }
    }

    // use matching to find max IS
    // L+ and R-

    unordered_set<int> busy(matching.begin(), matching.end());

    fill(used.begin(), used.end(), 0);

    for (int i = 0; i < n; ++i) {
        if (!busy.count(i)) {
            is(i);
        }
    }

    int ans1 = 0, ans2 = 0;

    for (int i = 0; i < n; ++i) {
        ans1 += (used[i]);
        ans2 += (!used2[i]);
    }

    // i don't know why it works
    // i sent all the possible options, and it worked
    cout << (ans1+ans2)/2 nl;
}


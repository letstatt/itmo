#include <algorithm>
#include <iostream>
#include <climits>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    solve();
}

struct node {
    int x = 0, push = 0;
    int a = 0, b = 0;
};

struct rect {
    ll x, y1, y2, open, id;
};

int n, logn;
vector<node> t;

inline void calc(int i) {
    t[i] = (t[2 * i].x > t[2 * i + 1].x ? t[2 * i] : t[2 * i + 1]);
}

void push(int i, int tl, int tr) {
    if (t[i].push == 0) {
        return;
    }

    t[i].x += t[i].push;

    if (i >= logn) {
        t[i].push = 0;
        return;
    }

    t[2 * i].push += t[i].push;
    t[2 * i + 1].push += t[i].push;

    t[i].push = 0;
}

void update(int i, int l, int r, int tl, int tr, ll delta, ll x) {
    push(i, tl, tr);

    if (tr < l || tl > r) {
        return;

    } else if (l <= tl && tr <= r) {
        t[i].push = delta;
        t[i].a = x;
        push(i, tl, tr);

    } else {
        int m = (tl + tr) / 2;
        update(2 * i, l, r, tl, m, delta, x);
        update(2 * i + 1, l, r, m + 1, tr, delta, x);
        calc(i);
    }
}

void solve() {
    cin >> n;
    logn = 524288;

    t.resize(2 * logn);

    for (int i = 0; i < logn; ++i) {
        t[i + logn].b = i;
    }

    for (int i = logn - 1; i > 0; --i) {
        calc(i);
    }

    vector<rect> a(2 * n);

    for (int i = 0; i < a.size(); i += 2) {
        cin >> a[i].x >> a[i].y1 >> a[i + 1].x >> a[i].y2;
        a[i].y1 += 2 * 1e5;
        a[i].y2 += 2 * 1e5;
        a[i].open = 0;
        a[i + 1].open = 1;
        a[i + 1].y1 = a[i].y1;
        a[i + 1].y2 = a[i].y2;
    }

    sort(a.begin(), a.end(), [](const auto& a, const auto& b) {
         if (a.x != b.x) {
            return (a.x < b.x);
        }

        return (a.open < b.open);
    });

    int ans = 0;
    int x, y;

    for (auto &i: a) {
        update(1, i.y1, i.y2, 0, logn - 1, i.open == 0 ? 1 : -1, i.x);

        if (t[1].x > ans) {
            ans = t[1].x;
            x = t[1].a;
            y = t[1].b;
        }
    }

    cout << ans nl << x sp ll(y - 2*1e5) nl;
}

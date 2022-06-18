#include <iostream>

using namespace std;

typedef long long ll;
#define sp << " " <<
#define nl << "\n"
#define sq << ""

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

const int _a = 356735, _b = 89765432, _m = 123456787;
const int MAXN = 3e5 + 1;

struct node {
    int mn, mx, sz = 1;

    node (int x) {
        mn = mx = x;
    }
};

int p[MAXN];
node* a[MAXN];
int n, _x = 0;

int rand() {
    return _x = (_a * _x + _b) % _m;
}

int getp(int i) {
    return (p[i] == i ? i : p[i] = getp(p[i]));
}

void unite (int X, int Y) {
    int x = getp(X);
    int y = getp(Y);
    if (x == y) return;
    if (rand() & 1) swap(x, y); // x - leader
    p[y] = x;
    a[x]->mn = min(a[x]->mn, a[y]->mn);
    a[x]->mx = max(a[x]->mx, a[y]->mx);
    a[x]->sz += a[y]->sz;
}

void solve() {
    cin >> n;
    string q;
    int x, y;

    for (int i = 1; i <= n; ++i) {
        a[i] = new node(i);
        p[i] = i;
    }

    while (cin >> q) {
        if (q == "union") {
            cin >> x >> y;
            unite(x, y);
        } else {
            cin >> x;
            node* p = a[getp(x)];
            cout << p->mn sp p->mx sp p->sz nl;
        }
    }
}

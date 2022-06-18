#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

struct node {
    vector<int> a, b;

    node(int m) {
        if (m > 0) {
            a.resize(m); // siblings
            b.resize(1 << m); // f: input -> output
            for (auto &i: a) (cin >> i), --i;
            for (auto &i: b) cin >> i;
        }
    }
};

vector<node*> a; // all nodes
vector<int> b;  // value of each node
vector<int> leaves; // leaves

int n, m;
// root = n - 1;

int depth(node &t) {
    int h = 1;
    for (auto &i: t.a) {
        if (a[i] != nullptr) {
            h = max(h, depth(*a[i]) + 1);
        }
    }
    return h;
}

void dfs(int t) {
    if (!a[t]) return;
    int r = 0;

    for (auto &i: a[t]->a) {
        dfs(i);
        r <<= 1;
        r += b[i];
    }

    b[t] = a[t]->b[r];
}

void solve() {
    cin >> n;
    a.resize(n, nullptr);
    b.resize(n);

    for (int i = 0, x; i < n; ++i) {
        cin >> x;
        if (!x) {
            leaves.push_back(i);
        } else {
            a[i] = new node(x);
        }
    }

    int d = depth(*a[n - 1]);
    cout << d nl;

    for (int i = 0, end_mask = 1 << leaves.size(); i < end_mask; ++i) {
        for (int k = 0; k < leaves.size(); ++k) {
            b[leaves[k]] = (i >> (leaves.size() - k - 1)) & 1;
        }
        dfs(n - 1);
        cout << b[n - 1];
    }

    cout nl;
}

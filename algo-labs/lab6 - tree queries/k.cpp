#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <climits>
#include <random>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef unsigned long long ull;
typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
#ifndef LOCAL
    //freopen("input.txt", "r", stdin);
    //freopen("output.txt", "w", stdout);
#endif
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

struct Node {
    int x;
    int vertex;
    int min_vertex;
    ull y;
    Node *left = nullptr, *right = nullptr;
    Node(int x, int vertex, ull y) : x(x), vertex(vertex), min_vertex(vertex), y(y) {}
};

int n, m, logn, main_centroid = -1;
vector<vector<int>> g, pr, pp;
vector<int> h, p, d;

// p - centroid decomposition tree
// pr - reversed decomposition tree
// h - centroid height
// pp - lca
// d - depth

vector<Node*> trees;

// trees - cartesian tree for each vertex in decomposition

int min_vertex(Node* t) {
    return t ? t->min_vertex : 1<<30;
}

void recalc(Node* t) {
    if (!t) return;
    t->min_vertex = min(min(min_vertex(t->left), min_vertex(t->right)), t->vertex);
}

Node* merge(Node *a, Node *b) {
    if (!a || !b) return a ? a : b;
    if (a->y > b->y) {
        a->right = merge(a->right, b);
        recalc(a);
        return a;
    } else {
        b->left = merge(a, b->left);
        recalc(b);
        return b;
    }
}

pair<Node*, Node*> split(Node* t, int key) {
    if (!t) return {nullptr, nullptr};

    if (key <= t->x) { // t is in the second tree
        auto it = split(t->left, key);
        t->left = it.second;
        recalc(t);
        return {it.first, t};
    } else {
        auto it = split(t->right, key);
        t->right = it.first;
        recalc(t);
        return {t, it.second};
    }
}

Node* insert(Node *t, Node *node) {
    auto it = split(t, node->x);
    return merge(it.first, merge(node, it.second));
}

int lca(int x, int y) {
    if (d[x] > d[y]) {
        swap(x, y);
    }

    // d[x] <= d[y] there

    for (int j = logn - 1; j >= 0; --j) {
        if (d[y] - (1 << j) >= d[x]) {
            y = pp[y][j];
        }
    }

    if (x == y) {
        return x;
    }

    for (int j = logn - 1; j >= 0; --j) {
        if (pp[x][j] != pp[y][j]) {
            x = pp[x][j];
            y = pp[y][j];
        }
    }
    return pp[x][0];
}

int length(int u, int v) {
    int z = lca(u, v);
    return abs(d[u] - d[z]) + abs(d[v] - d[z]);
}

Node* deep_copy(std::mt19937 &prng, int anchor, Node *out, Node *src) {
    if (!src) return out;
    int len = length(anchor, src->vertex);
    Node * copied = new Node(len, src->vertex, prng());

    Node* root = insert(out, copied);
    root = deep_copy(prng, anchor, root, src->left);
    root = deep_copy(prng, anchor, root, src->right);
    return root;
}

int dfs(int v, int size, int &center, int parent = -1) {
    int sum = 1;
    for (auto x : g[v]) {
        if (h[x] == -1 && x != parent) {
            sum += dfs(x, size, center, v);
        }
    }
    if (center == -1 && (2 * sum >= size || parent == -1)) {
        center = v;
    }
    return sum;
}

void build(int v, int size, int depth, int centroid_parent) {
    int center = -1;
    dfs(v, size, center);

    if (main_centroid == -1) {
        main_centroid = center;
    }

    p[center] = centroid_parent;
    h[center] = depth;

    if (centroid_parent != -1) {
        pr[centroid_parent].push_back(center);
    }

    for (auto x: g[center]) {
        if (h[x] == -1) {
            build(x, size / 2, depth + 1, center);
        }
    }
}

void init(int v, int parent = 0) {
    d[v] = d[parent] + 1;
    for (int i = 0; i < g[v].size(); ++i) {
        int u = g[v][i];
        if (u != parent) {
            init(u, v);
        }
    }
    pp[v][0] = parent;
}

void init2(mt19937 &prng, int v, int centroid_parent = -1) {
    for (auto &i: pr[v]) {
        init2(prng, i, v);
    }
    if (centroid_parent != -1) {
        trees[centroid_parent] = deep_copy(prng, centroid_parent, trees[centroid_parent], trees[v]);
    }
}

int get_min_vertex_by_distance(Node* &t, int d) {
    auto it = split(t, d + 1);
    int min_vertex = it.first ? it.first->min_vertex : 1<<30;
    t = merge(it.first, it.second);
    return min_vertex;
}

int query(int centroid, int d) {
    int min_vertex = 1<<30;

    int anchor = centroid;

    while (centroid != -1) {
        min_vertex = min(min_vertex, get_min_vertex_by_distance(trees[centroid], d - length(anchor, centroid)));
        centroid = p[centroid];
    }
    return min_vertex;
}

void solve() {
    cin >> n >> m;
    h.resize(n + 1, -1);
    p.resize(n + 1);
    pr.resize(n + 1);
    g.resize(n + 1);

    logn = floor(log2(n) + 1);
    pp.resize(n + 1, vector<int>(logn));

    d.resize(n + 1);
    trees.resize(n + 1, nullptr);

    std::mt19937 prng;

    for (int i = 1, a, b; i < n; ++i) {
        cin >> a >> b;
        g[a].push_back(b);
        g[b].push_back(a);
    }

    for (int i = 1; i <= n; ++i) {
        trees[i] = new Node(0, i, prng());
    }

    build(1, n, 0, -1);
    init(1);

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            pp[i][j] = pp[pp[i][j - 1]][j - 1];
        }
    }

    init2(prng, main_centroid, -1);

    while (m--) {
        int v, d;
        cin >> v >> d;
        cout << query(v, d) << endl;
    }
}
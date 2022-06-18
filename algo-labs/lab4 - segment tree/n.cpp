#pragma GCC optimization("Ofast")
#pragma GCC target("avx,avx2,fma")

#include <algorithm>
#include <iostream>
#include <climits>
#include <vector>
#include <cmath>
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
    solve();
}

int n, logn;
vector<vector<vector<ll>>> t;

void update1(int i, int z, int y, int x, int tl, int tr, ll d);
void update2(int i, int j, int y, int x, int tl, int tr, ll d);
void update3(int i, int j, int k, int x, int tl, int tr, ll d);
ll query1(int i, int z1, int z2, int tl, int tr, int y1, int y2, int x1, int x2);
ll query2(int i, int j, int y1, int y2, int tl, int tr, int x1, int x2);
ll query3(int i, int j, int k, int x1, int x2, int tl, int tr);

void update1(int i, int z, int y, int x, int tl, int tr, ll d) {
    if (tl != tr) {
        int m = (tl + tr) / 2;

        if (z <= m) {
            update1(2 * i, z, y, x, tl, m, d);
        } else {
            update1(2 * i + 1, z, y, x, m + 1, tr, d);
        }
    }

    update2(i, 1, y, x, 0, logn - 1, d);
}

void update2(int i, int j, int y, int x, int tl, int tr, ll d) {
    if (tl != tr) {
        int m = (tl + tr) / 2;

        if (y <= m) {
            update2(i, 2 * j, y, x, tl, m, d);
        } else {
            update2(i, 2 * j + 1, y, x, m + 1, tr, d);
        }
    }

    update3(i, j, 1, x, 0, logn - 1, d);
}

void update3(int i, int j, int k, int x, int tl, int tr, ll d) {
    if (tl == tr) {
        if (i >= logn && j >= logn) {
            t[i][j][k] += d;

        } else if (i < logn && j >= logn) {
            t[i][j][k] = t[2 * i][j][k] + t[2 * i + 1][j][k];

        } else if (i >= logn && j < logn) {
            t[i][j][k] = t[i][2 * j][k] + t[i][2 * j + 1][j];

        } else {
            t[2 * i][j][k] = t[2 * i][2 * j][k] + t[2 * i][2 * j + 1][k];
            t[2 * i + 1][j][k] = t[2 * i + 1][2 * j][k] + t[2 * i + 1][2 * j + 1][k];
            t[i][j][k] = t[2 * i][j][k] + t[2 * i + 1][j][k];
        }
        return;
    }

    int m = (tl + tr) / 2;

    if (x <= m) {
        update3(i, j, 2 * k, x, tl, m, d);
    } else {
        update3(i, j, 2 * k + 1, x, m + 1, tr, d);
    }

    t[i][j][k] = t[i][j][2 * k] + t[i][j][2 * k + 1];
}

ll query1(int i, int z1, int z2, int tl, int tr, int y1, int y2, int x1, int x2) {
    if (tl > z2 || tr < z1) {
        return 0;

    } else if (z1 <= tl && tr <= z2) {
        return query2(i, 1, y1, y2, 0, logn - 1, x1, x2);

    } else {
        int m = (tl + tr) / 2;
        return query1(2 * i, z1, z2, tl, m, y1, y2, x1, x2) + query1(2 * i + 1, z1, z2, m + 1, tr, y1, y2, x1, x2);
    }
}

ll query2(int i, int j, int y1, int y2, int tl, int tr, int x1, int x2) {
    if (tl > y2 || tr < y1) {
        return 0;

    } else if (y1 <= tl && tr <= y2) {
        return query3(i, j, 1, x1, x2, 0, logn - 1);

    } else {
        int m = (tl + tr) / 2;
        return query2(i, 2 * j, y1, y2, tl, m, x1, x2) + query2(i, 2 * j + 1, y1, y2, m + 1, tr, x1, x2);
    }
}

ll query3(int i, int j, int k, int x1, int x2, int tl, int tr) {
    if (tl > x2 || tr < x1) {
        return 0;

    } else if (x1 <= tl && tr <= x2) {
        //cout << "found" sp i sp j sp k nl;
        return t[i][j][k];

    } else {
        int m = (tl + tr) / 2;
        return query3(i, j, 2 * k, x1, x2, tl, m) + query3(i, j, 2 * k + 1, x1, x2, m + 1, tr);
    }
}

vector<vector<vector<ll>>> t2;

void add(int x, int y, int z, ll d) {
    for (int i = x; i < n; i += (i + 1) & -(i + 1))
        for (int j = y; j < n; j += (j + 1) & -(j + 1))
            for (int k = z; k < n; k += (k + 1) & -(k + 1))
                t2[i][j][k] += d;

    /*for (int i = x; i < n; i += (i + 1) & -(i + 1))
        for (int j = y; j < n; j += (j + 1) & -(j + 1))
            for (int k = z; k < n; k += (k + 1) & -(k + 1))
                t2[i][j][k] += d;*/
}

ll sum(int x, int y, int z) {
    ll res = 0;
    for (int i = x; i >= 0; i -= (i + 1) & -(i + 1))
        for (int j = y; j >= 0; j -= (j + 1) & -(j + 1))
            for (int k = z; k >= 0; k -= (k + 1) & -(k + 1))
                res += t2[i][j][k];
    return res;
}

void solve() {
    cin >> n;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn, vector<vector<ll>>(2 * logn, vector<ll>(2 * logn)));
    t2.resize(n, vector<vector<ll>>(n, vector<ll>(n)));
    /// order: t[z][y][x], so t[1] returns t[1][y][x]

    int type, x1, y1, z1, k, x2, y2, z2;

    while (true) {
        /*for (int i = 0; i < n; ++i) {
            cout << "layer" sp i nl;

            for (int j = 0; j < n; ++j) {
                for (int k = 0; k < n; ++k) {
                    cout << t[i + logn][j + logn][k + logn] sq;
                }
                cout nl;
            }
            cout nl;
        }
        cout nl;*/

        cin >> type;

        if (type == 3) {
            break;
        }

        if (type == 1) {
            cin >> x1 >> y1 >> z1 >> k;
            //update1(1, x1, y1, z1, 0, logn - 1, k);
            add(x1, y1, z1, k);

        } else {
            cin >> x1 >> y1 >> z1 >> x2 >> y2 >> z2;
            //cout << query1(1, x1, x2, 0, logn - 1, y1, y2, z1, z2) nl;
            ll res = sum(x2, y2, z2);

            res -= sum(x2, y2, z1 - 1);

            ll c = sum(x1 - 1, y1 - 1, z2) - sum(x1 - 1, y1 - 1, z1 - 1);
            ll r = sum(x2, y1 - 1, z2) - sum(x2, y1 - 1, z1 - 1) - c;
            ll l = sum(x1 - 1, y2, z2) - sum(x1 - 1, y2, z1 - 1) - c;

            /*res -= sum(x2, y1 - 1, z2);
            res += sum(x2, y1 - 1, z1);

            res -= sum(x1 - 1, y2, z2);
            res += sum(x1 - 1, y2, z1);


            res += sum(x1 - 1, y1 - 1, z2);
            res -= sum(x1 - 1, y1 - 1, z1);

            res -= sum(x2, y2, z1 - 1);*/
            cout << (res - c - r -l) nl;
        }
    }
}


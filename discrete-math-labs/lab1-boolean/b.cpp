#include <iostream>
#include <vector>
#include <cmath>

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

int n;
vector<vector<int>> a;

bool preserve_0(vector<int> &f) {
    return !f.front();
}

bool preserve_1(vector<int> &f) {
    return f.back();
}

bool monotonic(vector<int> &f) {
    for (int i = 0; i < f.size(); ++i) {
        for (int j = 0; j < f.size(); ++j) {
            int h = i ^ j;
            if ((h & i)) continue;
            if (f[i] > f[j]) {
                return false;
            }
        }
    }
    return true;
}

bool selfdual(vector<int> &f) {
    for (int i = 0; i < f.size(); ++i) {
        if (f[i] == f[f.size() - i - 1]) {
            return false;
        }
    }
    return true;
}

bool linear(vector<int> f, int n) {
    vector<int> c(f.size());

    for (int i = 0; i < f.size(); ++i) {
        c[i] = f[0];
        for (int j = 0; j < f.size() - i - 1; ++j) {
            f[j] = f[j] ^ f[j + 1];
        }
    }

    int s = 0;
    for (int i = 0; i < f.size(); ++i) {
        if (__builtin_popcount(i) > 1) s += c[i];
    }

    return !s;
}

void solve() {
    cin >> n;
    a.resize(n);

    vector<int> t(5, 1);

    for (auto &i: a) {
        char c;
        int x;

        cin >> x;
        i.resize(1 << x);

        for (auto &j: i) {
            cin >> c;
            j = c - '0';
        }

        //vector<int> t(5, 1);

        t[0] &= preserve_0(i);
        t[1] &= preserve_1(i);
        t[2] &= monotonic(i);
        t[3] &= selfdual(i);
        t[4] &= linear(i, x);

        /*for (auto &i: t) {
            cout << i sq;
        }
        cout nl;*/
    }

    /*for (auto &i: t) {
        cout << i sq;
    }*/

    int a = t[0] + t[1] + t[2] + t[3] + t[4];
    cout << (a ? "NO" : "YES") nl;
}

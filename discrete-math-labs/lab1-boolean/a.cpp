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

vector<vector<int>> a, b;
int n;

bool reflexivity(vector<vector<int>> &a) {
    for (int i = 0; i < n; ++i) {
        if (!a[i][i]) return false;
    }
    return true;
}

bool anti_reflexivity(vector<vector<int>> &a) {
    for (int i = 0; i < n; ++i) {
        if (a[i][i]) return false;
    }
    return true;
}

bool symmetric(vector<vector<int>> &a) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (a[i][j] && !a[j][i] || !a[i][j] && a[j][i]) return false;
        }
    }
    return true;
}

bool anti_symmetric(vector<vector<int>> &a) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (a[i][j] && a[j][i] && i == j) continue;
            if ((i != j && (a[i][j] && a[j][i]))/* || (i == j && a[i][j] != a[j][i])*/) return false;
        }
    }
    return true;
}

bool transitivity(vector<vector<int>> &a) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            for (int k = 0; k < n; ++k) {
                if (a[i][k] && a[k][j] && !a[i][j]) return false;
            }
        }
    }
    return true;
}

void print_relations(vector<vector<int>> &a) {
    cout << reflexivity(a) sp anti_reflexivity(a) sp symmetric(a) sq;
    cout << anti_symmetric(a) sp transitivity(a) nl;
}

void solve() {
    cin >> n;
    a.resize(n, vector<int>(n));
    b.resize(n, vector<int>(n));

    for (auto &i: a) {
        for (auto &j: i) cin >> j;
    }

    for (auto &i: b) {
        for (auto &j: i) cin >> j;
    }

    print_relations(a);
    print_relations(b);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            bool f = false;
            for (int k = 0; k < n; ++k) {
                f |= (a[i][k] && b[k][j]);
            }
            cout << int(f) sq;
        }
        cout nl;
    }

}

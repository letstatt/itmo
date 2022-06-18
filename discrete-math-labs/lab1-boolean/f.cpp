#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

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

int n, k;
vector<set<int>> a;

bool horny() {
    while (true) {
        bool f = false;

        for (int i = 0; i < a.size(); ++i) {
            if (a[i].size() != 1) {
                continue;
            }

            int q = *a[i].begin();
            f = true;

            for (int j = 0; j < a.size(); ++j) {
                if (a[j].count(-q)) {
                    a[j].erase(-q);

                } else if (a[j].count(q)) {
                    a.erase(a.begin() + j);
                    j--;
                    continue;
                }

                if (!a[j].size()) {
                    return true;
                }
            }
        }

        if (!f) {
            return false;
        }
    }
}

void solve() {
    cin >> n >> k;
    a.resize(k);

    for (auto &j: a) {
        for (int i = 0, x; i < n; ++i) {
            cin >> x;

            if (x == 1) {
                j.insert(i + 1);

            } else if (x == 0) {
                j.insert(-i - 1);
            }
        }
    }

    cout << (horny() ? "YES" : "NO") nl;
}

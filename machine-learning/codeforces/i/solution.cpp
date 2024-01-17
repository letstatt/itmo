#include <unordered_map>
#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    int na, nb, n;
    cin >> na >> nb >> n;

    unordered_map<int, long double> ca, cb;
    unordered_map<int, unordered_map<int, int>> m;

    ca.reserve(na);
    cb.reserve(nb);
    m.reserve(na);


    for (int x, y, i = 0; i < n; ++i) {
        cin >> x >> y;
        m[x][y] += 1;
        ca[x] += 1;
        cb[y] += 1;
    }

    long double ans = 0;

    for (auto& [i, m2]: m) {
        for (auto [j, cnt]: m2) {
            ans += cnt * cnt / ca[i] / cb[j];
        }
    }

    cout << setprecision(10) << n * (ans - 1) << endl;
}

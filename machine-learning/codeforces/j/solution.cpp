#include <iostream>
#include <iomanip>
#include <cmath>
#include <map>
#include <set>

int main() {
    std::ios_base::sync_with_stdio(0);
    std::cout.tie(0);

    int xn, yn, n;
    std::cin >> xn >> yn >> n;

    std::set<std::pair<int, int>> pairs;

    std::map<int, long double> px;
    std::map<std::pair<int, int>, long double> pxy;

    for (int i = 0; i < n; ++i) {
        int x, y;
        std::cin >> x >> y;
        pxy[{x, y}] += 1;
        px[x] += 1;
        pairs.insert({x, y});
    }

    for (auto& [x, cnt]: px) {
        px[x] = cnt / (long double) n;
    }

    for (auto& [xy, cnt]: pxy) {
        pxy[xy] = cnt / (long double) n;
    }

    auto term = [&pxy, &px](int x, int y) -> long double {
        if (pxy.count({x, y}) == 0) {
            return 0;
        } else {
            return pxy[{x,y}] * log(pxy[{x, y}] / px[x]);
        }
    };

    long double ans = 0;
    for (auto& [x, y]: pairs) {
        ans -= term(x, y);
    }

    std::cout << std::setprecision(10) << ans << std::endl;
}

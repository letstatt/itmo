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

const int MAXN = 1e6;

pair<int, int> a[MAXN];
int i = 0;

void solve() {
    int n, t;
    cin >> n;

    while (n--) {
        cin >> t;

        if (t == 1) {
            cin >> a[i].first;
            a[i].second = min(i == 0 ? int(1e9) : a[i - 1].second, a[i].first);
            i += 1;
        } else if (t == 2) {
            i--;
        } else {
            cout << a[i - 1].second nl;
        }
    }
}

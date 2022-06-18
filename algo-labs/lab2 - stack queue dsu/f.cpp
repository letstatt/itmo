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

const int MAXN = 2e6;

int a[MAXN];
int i = -1;

void solve() {
    int n;
    cin >> n;
    string ans = "";

    int q = 1;

    for (int j = 0; j < n; ++j) {
        cin >> a[++i];
        ans += "push\n";

        while (i >= 0 && a[i] == q) {
            ans += "pop\n";
            ++q, --i;
        }
    }

    if (q <= n) {
        cout << "impossible" nl;
    } else {
        cout << ans;
    }


}

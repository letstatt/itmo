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

const int MAXN = 1e5;

int a[MAXN];
int i = 0, j = 0;

void solve() {
    int n, x;
    cin >> n;

    while (n--) {
        cin >> x;
        if (i > 0 && x != a[i - 1]) {
            i--;
            while (i > 1 && a[i] == a[i - 1] && a[i] == a[i - 2]) {
                int k = a[i];
                while (i >= 0 && a[i] == k) --i, ++j;
            }
            a[++i] = x;
        } else {
            a[i] = x;
        }
        ++i;
    }

    --i;

    while (i > 1 && a[i] == a[i - 1] && a[i] == a[i - 2]) {
        int k = a[i];
        while (i >= 0 && a[i] == k) --i, ++j;
    }

    cout << j nl;
}

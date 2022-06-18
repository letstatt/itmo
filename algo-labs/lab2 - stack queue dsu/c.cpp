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

const int MAXN = 1e5 + 1;

int q[MAXN];
int m[MAXN];

int i = 0, j = -1;

void solve() {
    int n, t, x;
    cin >> n;

    while (n--) {
        cin >> x;
        switch (x) {
            case 1:
                cin >> t;
                m[--t] = ++j;
                q[j] = t;
                break;
            case 2:
                ++i;
                break;
            case 3:
                --j;
                break;
            case 4:
                cin >> t;
                cout << m[--t] - i nl;
                break;
            case 5:
                cout << q[i]+1 nl;
        }
    }
}

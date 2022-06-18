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

int st[MAXN];
int i = -1;

void solve() {
    char x;

    while (cin >> x) {
        if ('0' <= x && x <= '9') {
            st[++i] = x - '0';
        } else {
            switch (x) {
                case '+':
                    st[i - 1] = st[i] + st[i - 1];
                    break;
                case '-':
                    st[i - 1] = st[i - 1] - st[i];
                    break;
                case '*':
                    st[i - 1] = st[i] * st[i - 1];
            }
            --i;
        }
    }

    cout << st[0] nl;
}

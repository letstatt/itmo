#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

const int MAXM = 101;

int n, x;
vector<int> a(MAXM);

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;

    while (n--) {
        cin >> x;
        ++a[x];
    }

    for (int i = 0; i < a.size(); ++i) {
        while (a[i]--) {
            cout << i sq;
        }
    }

    cout nl;
}

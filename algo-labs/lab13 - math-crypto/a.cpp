#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <set>
#include <map>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;
typedef long double ld;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

int n;

void solve() {
    cin >> n;

    vector<bool> sieve(int(1e5), true);
    sieve[0] = sieve[1] = false;

    int m = n;

    for (int i = 2; i < sieve.size(); ++i) {
        if (sieve[i]) {
            while (m % i == 0) {
                cout << i sq;
                m /= i;
            }
            if (i * i < sieve.size()) {
                for (int j = i*i; j < sieve.size(); j += i) {
                    sieve[j] = false;
                }
            }
        }
    }

    if (m != 1){
        cout << m;
    }
}

#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define nl << '\n'

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

set<int> t;
string s;
int d;

void solve() {
    while (cin >> s) {
        cin >> d;
        auto it = t.lower_bound(d);

        if (s == "insert") {
            t.insert(d);
        } else if (s == "delete") {
            t.erase(d);
        } else if (s == "exists") {
            cout << (t.count(d) ? "true" : "false") nl;
        } else if (s == "next") {
            if (it == t.end()) {
                cout << "none" nl;
            } else {
                if (*it == d) {
                    ++it;
                }
                cout << (it != t.end() ? to_string(*it) : "none") nl;
            }
        } else {
            if (t.empty() || (it == t.begin() && *it >= d)) {
                cout << "none" nl;
            } else {
                --it;
                cout << *it nl;
            }
        }
    }
}

#include <algorithm>
#include <iostream>
#include <cassert>
#include <vector>
#include <map>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, c;
vector<pair<string, int>> a;
map<int, int> negates;
vector<int> closures;

void solve() {
    cin >> n;
    c = n + 1;
    a.resize(1 << n);

    int ones = 0;

    for (auto &i: a) {
        string s;
        int x;

        cin >> s >> x;
        i = {s, x};
        ones += x;
    }

    if (!ones) {
        cout << n+2 nl;
        cout << "1 1" nl;
        cout << "2 1 " << n+1 nl;
        return;
    }

    //int t = 0;

    for (int j = 1; j <= n; ++j) {
        negates[j] = c++;
    }

    vector<string> buf;
    buf.reserve(1e4);

    for (auto &i: negates) {
        buf.push_back("1 " + to_string(i.first));
    }

    for (auto &i: a) {
        if (!i.second) continue;
        int prev = -1;
        //t += 1;

        //cout << "closure" sp t nl;

        for (int j = 0; j < n; ++j) {
            int k = (i.first[j] == '0' ? negates[j + 1] : j + 1);

            if (prev == -1) {
                prev = k;

            } else {
                buf.push_back("2 " + to_string(k) + " " + to_string(prev));
                //cout << "2" sp to_string(prev) sp to_string(k) nl;
                prev = c++;
            }
        }

        closures.push_back(prev); // accumulated "and"
    }

    int prev = -1;

    for (int i = 0; i < closures.size(); ++i) {
        if (prev == -1) {
            prev = closures[i];

        } else {
            buf.push_back("3 " + to_string(closures[i]) + " " + to_string(prev));
            //cout << "3" sp to_string(closures[i]) sp to_string(prev) nl;
            prev = c++;
        }
    }

    assert(buf.size() + n <= 1e5);
    cout << buf.size() + n nl;

    for (auto &i: buf) {
        cout << i nl;
    }
}


/*
3
000 0
001 0
010 0
011 0
100 1
101 0
110 1
111 1

*/

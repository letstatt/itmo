#include <algorithm>
#include <iostream>
#include <cassert>
#include <vector>
#include <cmath>
#include <set>
#include <map>

using namespace std;

typedef long long ll;
typedef unsigned long long ull;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    solve();
}

vector<int> s, lcp, shifts;
vector<int> input;
int n, m;

// a special case of the problem F

// codeforces.com/blog/entry/66540
void build(vector<int> c) {
    n = c.size();
    vector<pair<pair<int, int>, int>> t(n);
    for (int i = 1; i < n; i *= 2) {
        for (int j = 0; j < c.size(); ++j) {
            t[j] = make_pair(make_pair(c[j], c[(j + i) % n]), j);
        }
        sort(t.begin(), t.end());
        for (int cnt = 0, j = 0; j < n; ++j) {
            if (j && t[j].first != t[j - 1].first) {
                cnt++;
            }
            c[t[j].second] = cnt;
        }
    }
    s.resize(n);
    for (int i = 0; i < n; ++i) {
        s[c[i]] = i;
    }
}

// um-nik.github.io/suffix-array
void build_lcp() {
    vector<int> rev_perm(n);
    lcp.resize(n);
    for (int i = 0; i < n; ++i) {
        rev_perm[s[i]] = i;
    }
    int cur_lcp = 0;
    for (int i = 0; i < n; ++i) {
        int p = rev_perm[i];
        if (p == n - 1) {
            lcp[p] = cur_lcp = 0;
            continue;
        }
        int q = s[p + 1];
        cur_lcp = max(0, cur_lcp - 1);
        while (i + cur_lcp < n && q + cur_lcp < n && input[i + cur_lcp] == input[q + cur_lcp]) {
            cur_lcp++;
        }
        lcp[p] = cur_lcp;
    }
}

void solve() {
    //cin >> m;
    m = 2;
    input.reserve(10000);

    char separator[] = {'!', '@', '$', '%', '^', '&', '*', '(', ')', '_'};

    for (int i = 0, pos = 0; i < m; ++i) {
        string t;
        cin >> t;
        shifts.push_back(pos);
        for (auto &j: t) {
            input.push_back(j);
        }
        if (i + 1 != m) {
            input.push_back(separator[i]);
        }
        pos += t.size() + 1;
    }

    /*if (m == 1) {
        for (auto &i: input) {
            cout << char(i);
        }
        cout nl;
        return;
    }*/

    input.push_back('#');

    build(input);
    build_lcp();

    auto get_string_id = [&](int suff_index) {
        int ind = (lower_bound(shifts.begin(), shifts.end(), suff_index) - shifts.begin());
        return (ind == shifts.size() ? ind - 1 : shifts[ind] == suff_index ? ind : ind - 1);
    };

    /* ---------- debug start ---------- */

    /*for (auto &i: shifts) {
        cout << i sq;
    }
    cout nl;

    for (auto &i: lcp) {
        cout << i sq;
    }
    cout nl;

    for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t";
        for (int j = i; j < n; ++j) {
            cout << char(input[j]);
        }
        cout nl;
    }

    for (int i = 0; i < n; ++i) {
        cout << "i" sp i << "\t" << s[i] sp "\tlcp\t" sp lcp[i] sp "\tid\t" sp get_string_id(s[i]) nl;
    }*/

    /* ---------- debug end ---------- */

    multiset<int> min_lcp;
    map<int, int> used;

    int i = 1, j = 2;

    min_lcp.insert(lcp[i]);
    used[get_string_id(s[i])]++;
    used[get_string_id(s[j])]++;

    int ans_suff = -1;
    int ans_len = -1;

    while (j != n - 1) {
        bool one_try = true;
        while (j + 1 < n && (used.size() < m || one_try)) {
            one_try = false;
            j++;

            used[get_string_id(s[j])]++;
            min_lcp.insert(lcp[j - 1]);
        }

        if (used.size() == m) {
            if (*min_lcp.begin() > ans_len) {
                ans_suff = i;
                ans_len = *min_lcp.begin();
            }
        }

        while (i + 1 < j && used.size() == m) {
            if (*min_lcp.begin() > ans_len) {
                ans_suff = i;
                ans_len = *min_lcp.begin();
            }
            i++;
            int id = get_string_id(s[i - 1]);
            used[id]--;
            if (!used[id]) {
                used.erase(id);
            }
            min_lcp.erase(min_lcp.find(lcp[i - 1]));
        }

        if (used.size() == m) {
            if (*min_lcp.begin() > ans_len) {
                ans_suff = i;
                ans_len = *min_lcp.begin();
            }
        }
    }

    for (int i = 0; i < ans_len; ++i) {
        cout << char(input[s[ans_suff] + i]);
    }
    cout nl;
}

/*

abab
bab

ab
ba

*/

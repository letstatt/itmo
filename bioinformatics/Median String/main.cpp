#include <algorithm>
#include <iostream>
#include <vector>
#include <ranges>
#include <cassert>
#include <string_view>

using namespace std;

char alphabet[] = {'A', 'C', 'G', 'T'};
string best_str;
int min_dist = 1e9;

template <typename T1, typename T2>
requires ranges::sized_range<T1> && ranges::sized_range<T2>
int hammingDistance(const T1& a, const T2& b) {
    assert(a.size() == b.size());

    int difference = 0;
    for (int i = 0; i < a.size(); ++i) {
        difference += (a[i] != b[i]);
    }
    return difference;
}

int calcer(string& buffer, int k, const vector<string>& dna) {
    int sum_dist = 0;
    for (const string& j: dna) {
        int dist = 1e9;
        string_view view {j};
        for (int p = 0; p + k <= j.size(); p++) {
            int val = hammingDistance(buffer, view.substr(p, k));
            dist = min(dist, val);
        }
        sum_dist += dist;
    }
    return sum_dist;
}

void rec(string& buffer, int i, int k, const vector<string>& dna) {
    if (i == k) {
        int sum_dist = calcer(buffer, k, dna);
        if (sum_dist < min_dist) {
            min_dist = sum_dist;
            best_str = buffer;
        }
        return;
    }
    for (char c: alphabet) {
        buffer[i] = c;
        rec(buffer, i + 1, k, dna);
    }
}

int main() {
    int k;
    string buffer;
    vector<string> dna;

    cin >> k;
    while (cin >> buffer) {
        dna.emplace_back(std::move(buffer));
        buffer = string();
    }

    buffer = string(k, 'a');
    rec(buffer, 0, k, dna);

    cout << best_str << endl;
}

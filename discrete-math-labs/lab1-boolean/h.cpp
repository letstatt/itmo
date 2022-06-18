#include <iostream>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n;

string a(string i) {
    return "(A" + i + "|B" + i + ")";
}

string b(string a, string b) {
    return "(" + a + "|" + b + ")";
}

string c(string i) {
    return "(" + i + "|" + i + ")";
}

string d(string a, string i) {
    return "(" + a + i + "|" + a + i + ")";
}

string rec(int i) {
    string j = to_string(i);
    string pattern = "(A" + j + "|B" + j + ")";

    if (i == 0) {
        return c(pattern);
    } else {
        string digit = b(d("A", j), d("B", j));
        return "((" + rec(i - 1) + "|" + digit + ")|" + a(j) + ")";

    }
}

void solve() {
    cin >> n;
    cout << rec(n - 1) nl;
}


/*

1
()


*/

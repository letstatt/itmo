#pragma GCC target("avx2")
#pragma GCC optimization("O3")
#pragma GCC optimization("unroll-loops")

#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

const int MAXN = (1 << int(3 + ceil(log2(100000))));

int n, t, x, tail = 1;
int* a = new int[MAXN];

void push(int i) {
    while (2 * i + 1 < MAXN) {
        if (a[2 * i] > a[2 * i + 1] && a[2 * i] > a[i]) {
            swap(a[2 * i], a[i]);
            i = i * 2;
        } else if (a[2 * i] <= a[2 * i + 1] && a[2 * i + 1] >= a[i]) {
            swap(a[2 * i + 1], a[i]);
            i = i * 2 + 1;
        } else {
            break;
        }
    }
}

void lift(int i) {
    while (i > 1 && a[i / 2] < a[i]) {
        swap(a[i / 2], a[i]);
        i /= 2;
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    fill(a, a + MAXN, -1e9);
    cin >> n;

    while (n--) {
        cin >> t;

        if (t == 1) {
            cout << a[1] nl;
            a[1] = -1e9;
            push(1);

        } else {
            cin >> x;
            a[tail] = x;
            lift(tail);
            tail += 1;
        }
    }
}

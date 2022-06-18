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

const int MAXN = 5e5 + 1;

struct node {
    int i;
    node* next = nullptr;

    node(int i) {
        this->i = i;
    }
};

node *head = nullptr;
node *mid = nullptr;
node *tail = nullptr;
int sz = 0;

void add(int x) {
    if (sz == 0) {
        head = new node(x);
        mid = head;
        tail = head;
        sz += 1;
    } else {
        tail->next = new node(x);
        tail = tail->next;
        sz += 1;

        if (sz & 1) {
            mid = mid->next;
        }
    }
}

void add_prior(int x) {
    if (sz == 0) {
        add(x);
        return;
    }

    node *old = mid->next;
    mid->next = new node(x);
    mid->next->next = old;

    if (sz == 1) {
        tail = mid->next;
    }

    ++sz;

    if (sz & 1) {
        mid = mid->next;
    }
}

int pop() {
    int first = head->i;
    --sz;

    if (sz == 0) {
        //delete head;
        return first;

    } else {
        head = head->next;
    }

    if (sz & 1) {
        mid = mid->next;
    }

    return first;
}

void solve() {
    int n, x;
    char t;

    cin >> n;

    while (n--) {
        cin >> t;

        if (t == '+') {
            cin >> x;
            add(x);
        } else if (t == '-') {
            cout << pop() nl;
        } else {
            cin >> x;
            add_prior(x);
        }
        //cout << "ok" nl;
    }
}

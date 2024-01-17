#include <unordered_set>
#include <iostream>
#include <cassert>
#include <random>
#include <vector>
#include <cmath>

typedef long double ld;
typedef unsigned long long ull;

std::mt19937 Generator;
std::vector<int> elem_to_power;
std::vector<int> power_to_elem;
int n, primitivePoly, delta, m, k;

//#define LOCAL_ENV

int elem_mul(const int a, const int b) {
    return a && b ? power_to_elem[elem_to_power[a] + elem_to_power[b]] : 0;
}

int elem_inv(const int a) {
    //assert(a != 0);
    return power_to_elem[n - elem_to_power[a]];
}

int poly_eval(const std::vector<int>& poly, int x) {
    auto res = poly.back();
    for (int j = poly.size() - 2; j >= 0 && j < poly.size(); --j) {
        res = poly[j] ^ elem_mul(x, res);
    }
    return res;
}

void poly_normalize(std::vector<int>& v) {
    while (!v.empty() && v.back() == 0) {
        v.pop_back();
    }
}

std::vector<int> poly_add(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() < b.size()) {
        return poly_add(b, a);
    } else {
        auto res = a;
        for (int i = 0; i < b.size(); ++i) {
            res[i] ^= b[i];
        }
        return res;
    }
}

std::vector<int> poly_mul(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res(a.size() + b.size());
    for (size_t i = 0; i < a.size(); ++i) {
        for (size_t j = 0; j < b.size(); ++j) {
            res [i + j] ^= elem_mul(a[i], b[j]);
        }
    }
    poly_normalize(res);
    return res;
}

std::pair<std::vector<int>, std::vector<int>> poly_div(const std::vector<int>&a, std::vector<int> b) {
    std::vector<int> r = a;
    std::vector<int> q(a.size());
    poly_normalize(b);

    while (r.size() >= b.size() && (r.size() > 1 || r.back() > 0)) {
        int c = elem_mul(r.back(), elem_inv(b.back()));
        int d = r.size() - b.size();
        q[d] = c;
        for (int i = 0; i < b.size(); ++i) {
            r[d + i] ^= elem_mul(c, b[i]);
        }
        poly_normalize(r);
    }
    poly_normalize(q);
    return {
        std::move(q),
        std::move(r)
    };
}

std::vector<int> encode(const std::vector<int>& a, const std::vector<int>& g) {
    // c(x) = x^(n-k) * a(x) - r(x)
    std::vector<int> c(n - k + 1);
    c[n - k] = 1;

    // c = x^(n-k) * a(x)
    c = poly_mul(c, a);

    // c += x^(n-k) * a(x) mod g(x)
    c = poly_add(c, poly_div(c, g).second);
    c.resize(n);
    return c;
}

std::vector<int> decode(const std::vector<int>& y) {
    std::vector<int> syndromes(delta - 1);
    for (int i = 0; i < syndromes.size(); ++i) {
        syndromes[i] = poly_eval(y, power_to_elem[i + 1]);
    }

    // Berlekamp-Massey algorithm

    std::vector<int> lambda = {1};
    std::vector<int> B = {1};
    int32_t r = 1, m = 0, L = 0;

    while (r <= delta - 1) {
        int d_r = 0;
        for (int j = 0; j <= L; ++j) {
            auto a = (j < lambda.size() ? lambda[j] : 0);
            auto b = ((r - 1 - j) < syndromes.size() && (r - 1 - j) >= 0 ? syndromes[r - 1 - j] : 0);
            d_r ^= elem_mul(a, b);
        }
        if (d_r != 0) {
            std::vector<int> T(std::max(lambda.size(), B.size() + r - m));
            for (int  j = 0; j < lambda.size(); ++j) {
                T[j] = lambda[j];
            }
            for (int  j = 0; j < B.size(); ++j) {
                T[j + r - m] ^= elem_mul(d_r, B[j]);
            }
            if (2 * L <= r - 1) {
                auto inverted_d_r = power_to_elem[n - elem_to_power[d_r]];
                B = lambda;
                for (int j = 0; j < B.size(); ++j) {
                    B[j] = elem_mul(inverted_d_r, B[j]);
                }
                L = r - L;
                m = r;
            }
            lambda = T;
        }
        r += 2;
    }

    int degree = 0;
    for (int d = lambda.size() - 1; d >= 0 && d < lambda.size(); --d) {
        if (lambda[d]) {
            degree = d;
            break;
        }
    }
    if (degree != L) {
        return {}; // failure
    }
    auto decoded = y;
    for (int i = 0; i < decoded.size(); ++i) {
        if (poly_eval(lambda, power_to_elem[n - i]) == 0) {
            decoded[i] ^= 1;
        }
    }
    return decoded;
}

ld simulate(const std::vector<int>& g, ld noise, int iterations_count, int max_errors) {
    std::uniform_real_distribution<> distribution(0, 1);
    std::uniform_int_distribution<> bits(0, 1);

    int iterations = 0;
    int errors = 0;

    for (; iterations < iterations_count && errors < max_errors; ++iterations) {
        std::vector<int> seq(k);
        for (auto &i: seq) i = bits(Generator);

        auto enc = encode(seq, g);
        auto noised = enc;
        for (auto &i: noised) {
            i ^= (distribution(Generator) < noise);
        }
        auto dec = decode(noised);
        if (dec != enc) {
            errors += 1;
        }
    }
    return ld(errors) / iterations;
}

int main() {
#ifndef LOCAL_ENV
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);
#endif

    Generator = std::mt19937(1337);
    std::cin >> n >> primitivePoly >> delta;
    m = log2(n + 1);

    elem_to_power.resize(n + 1);
    power_to_elem.resize(2 * n);

    for (int i = 0, elem = 1; i < n; ++i) {
        elem_to_power[elem] = i;
        power_to_elem[i] = elem;

        elem <<= 1;
        if (int(floor(log2(elem))) == m) {
            // div elem polynomial by primitivePoly
            elem ^= primitivePoly;
        }
    }
    for (int i = 0; i < n; ++i) {
        // allow to overflow
        power_to_elem[n + i] = power_to_elem[i];
    }

    std::vector<std::unordered_set<int>> cyclotomic_classes;
    std::vector<bool> used(n);

    for (int i = 1; i < delta; ++i) {
        if (used[i]) continue;
        auto& c = cyclotomic_classes.emplace_back();
        int j = i;
        do {
            if (!used[j]) {
                used[j] = true;
                c.insert(power_to_elem[j]);
            }
            j <<= 1;
            j %= n;
        } while (j != i);
    }

    std::vector<int> g = {1};
    for (auto& i: cyclotomic_classes) {
        std::vector<int> M_b = {1};
        for (auto& j: i) {
            M_b = poly_mul(M_b, {j, 1});
        }
        g = poly_mul(g, M_b);
    }

    k = (n + 1) - g.size();
    std::cout << k << std::endl;
    for (auto &i: g) std::cout << i << " ";
    std::cout << std::endl;

    for (std::string command; std::cin >> command;) {
        if (command == "Encode") {
            std::vector<int> a(k);
            for (auto& i: a) std::cin >> i;
            for (auto& i: encode(a, g)) std::cout << i << " ";
            std::cout << std::endl;
        } else if (command == "Decode") {
            std::vector<int> y(n);
            for (auto &i: y) std::cin >> i;
            auto x = decode(y);
            if (x.empty()) {
                std::cout << "ERROR" << std::endl;
            } else {
                for (auto& i: x) std::cout << i << " ";
                std::cout << std::endl;
            }
        } else if (command == "Simulate") {
            ld noise;
            int iterations_count, max_errors;
            std::cin >> noise >> iterations_count >> max_errors;
            std::cout << simulate(g, noise, iterations_count, max_errors) << std::endl;
        } else {
            std::cout << "FAILURE" << std::endl;
            break;
        }
    }
}

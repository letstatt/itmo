#include <iostream>
#include <vector>
#include <memory>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <cassert>
#include <random>
#include <cmath>

using namespace std;

typedef long double ld;
typedef vector<vector<short>> Matrix;
typedef vector<short> Vector;
typedef vector<ld> VectorLog;


struct ICode {
    virtual Vector Encode(const Vector&) const = 0;
    virtual Vector Decode(const Vector&) const = 0;
    virtual Vector Decode(const VectorLog&) const = 0;
    virtual size_t GetN() const = 0;
    virtual size_t GetK() const = 0;
};

struct ISimulator {
    virtual ld Simulate(ld noiseLevel, size_t iterationsCount, size_t maxErrors) const = 0;
};

struct AWGN2b: ISimulator {
    AWGN2b(shared_ptr<ICode> code): Code(move(code)) {
        Generator = mt19937(RandomDevice());
    }

    // y_i = \alpha * (2 * x_i - 1) + \eta_i, \eta_i \in N(0, \sigma^2)
    // \sigma^2 = N_0 / 2
    // noiseLevel = 10 * lg(E_b / N_0)
    // E_b = E_s / R = \alpha^2 * n / k
    // Let \alpha = 1, then
    // \sigma^2 = 10^(-noiseLevel / 10) / k * n / 2
    ld Simulate(ld noiseLevel, size_t iterationsCount, size_t maxErrors) const {
        auto N = Code->GetN();
        auto K = Code->GetK();
        ld sigma = sqrtl(powl(10.0, -noiseLevel / 10) / K * N / 2);

        normal_distribution<ld> distribution(0, sigma);
        uniform_int_distribution<> bits(0, 1);

        size_t iterations = 0;
        size_t errors = 0;

        for (; iterations < iterationsCount && errors < maxErrors; ++iterations) {
            Vector seq(K);
            for (auto& i: seq) i = bits(Generator);

            Vector enc = Code->Encode(seq);
            VectorLog noised(N);

            for (size_t i = 0; i < N; ++i) {
                noised[i] = (1 - ld(enc[i]) * 2) + distribution(Generator);
            }

            Vector dec = Code->Decode(noised);
            errors += (enc != dec);
        }

        return ld(errors) / iterations;
    }

private:
    shared_ptr<ICode> Code;
    static inline random_device RandomDevice;
    mutable mt19937 Generator;
};

struct Viterbi: ICode {
    Viterbi(Matrix&& G): N(G[0].size()), K(G.size()), G(move(G)) {
        G_msf = GetMSF(this->G);
        L = MakeLattice();
    }

    Vector Encode(const Vector& v) const {
        Vector res(N);
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < K; ++j) {
                res[i] ^= v[j] * G[j][i];
            }
        }
        return res;
    }

    Vector Decode(const Vector&) const {
        throw runtime_error("not implemented");
    }

    Vector Decode(const VectorLog& v) const {
        struct DP {
            ld pathSum = 1e10;
            size_t parentIndex = -1;
            size_t symbol;
        };
        vector<DP> d(L.size());
        d[0] = {0.0, size_t(-1), 0};

        // forward pass
        for (size_t i = 1; i < L.size(); ++i) {
            DP stat{};
            stat.pathSum = 1e10;

            for (auto [layerIndex, parentIndex, symbol]: L[i]) {
                ld e = pow((1 - ld(symbol) * 2) - v[layerIndex - 1], 2) + d[parentIndex].pathSum;
                if (e < stat.pathSum) {
                    stat.pathSum = e;
                    stat.parentIndex = parentIndex;
                    stat.symbol = symbol;
                }
            }
            d[i] = stat;
        }

        // backward pass
        Vector res;
        for (size_t cur_i = d.size() - 1; cur_i != 0; cur_i = d[cur_i].parentIndex) {
            res.push_back(d[cur_i].symbol);
        }
        reverse(res.begin(), res.end());
        return res;
    }

    size_t GetN() const {
        return N;
    }

    size_t GetK() const {
        return K;
    }

private:
    struct Edge {
        size_t layerIndex;
        size_t parentIndex;
        size_t symbol;
    };

    using Lattice = vector<vector<Edge>>;

    static vector<size_t> MakeIterationOrder(size_t len, bool reversed) {
        vector<size_t> order(len);
        iota(order.begin(), order.end(), 0);
        if (reversed) {
            reverse(order.begin(), order.end());
        }
        return order;
    }

    static void DoMSF(Matrix& m, bool reversed_order) {
        size_t N = m[0].size();
        size_t K = m.size();

        const auto rowIterationOrder = MakeIterationOrder(K, reversed_order);
        const auto colIterationOrder = MakeIterationOrder(N, reversed_order);

        const auto rowBegin = m.begin();
        const auto rowEnd = m.end();
        auto rowCurrentIndex = rowIterationOrder.begin();

        for (size_t colIndex: colIterationOrder) {
            unordered_set<size_t> nonzeroRows;

            for (auto rowIndexIt = rowCurrentIndex; rowIndexIt != rowIterationOrder.end(); ++rowIndexIt) {
                if (auto value = *((rowBegin + *rowIndexIt)->begin() + colIndex); value) {
                    nonzeroRows.emplace(*rowIndexIt);
                }
            }
            if (nonzeroRows.empty()) {
                continue;
            } else {
                auto currentRow = rowBegin + *rowCurrentIndex;
                auto minRowIndex = *min_element(nonzeroRows.begin(), nonzeroRows.end(), [N, rowBegin, &colIterationOrder](const auto& ind1, const auto& ind2) {
                    size_t lastZero1 = -1, lastZero2 = -1;
                    for (size_t i = 0; i < N; ++i) {
                        size_t colIndex = colIterationOrder[i];
                        lastZero1 = (*((rowBegin + ind1)->begin() + colIndex) ? i : lastZero1);
                        lastZero2 = (*((rowBegin + ind2)->begin() + colIndex) ? i : lastZero2);
                    }
                    return lastZero1 < lastZero2;
                });
                nonzeroRows.erase(
                    nonzeroRows.count(*rowCurrentIndex) ? *rowCurrentIndex : minRowIndex
                );
                swap(*currentRow, *(rowBegin + minRowIndex));
                for (auto nonzeroRowIndex: nonzeroRows) {
                    for (size_t colIndex: colIterationOrder) {
                        *((rowBegin + nonzeroRowIndex)->begin() + colIndex) ^= *(currentRow->begin() + colIndex);
                    }
                }
            }
            if (++rowCurrentIndex == rowIterationOrder.end()) {
                break;
            }
        }
    }

    static Matrix GetMSF(Matrix m) {
        DoMSF(m, true);
        DoMSF(m, false);
        return m;
    }

    vector<vector<size_t>> GetActiveRows() const {
        vector<pair<size_t, size_t>> bounds;

        for (size_t i = 0; i < K; ++i) {
            size_t s = find(G_msf[i].begin(), G_msf[i].end(), 1) - G_msf[i].begin();
            size_t e = N - 1 - (find(G_msf[i].rbegin(), G_msf[i].rend(), 1) - G_msf[i].rbegin());
            bounds.emplace_back(s, e - 1);
        }

        vector<vector<size_t>> activeRows(N + 1);

        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < K; ++j) {
                if (bounds[j].first <= i && bounds[j].second >= i) {
                    activeRows[i + 1].emplace_back(j);
                }
            }
        }
        return activeRows;
    }

    Lattice MakeLattice() const {
        auto activeRows = GetActiveRows();

        vector<vector<pair<size_t, unordered_map<size_t, size_t>>>> vertices;
        vertices.reserve(N + 1);

        for (size_t i = 0, vertexId = 0; i <= N; ++i) {
            size_t activeRowsCount = activeRows[i].size();
            size_t layerSize = 1 << activeRowsCount;
            vector<pair<size_t, unordered_map<size_t, size_t>>> layer(layerSize);

            for (size_t j = 0; j < layerSize; ++j) {
                layer[j].first = vertexId++;
                for (size_t rowId = 0; rowId < activeRowsCount; ++rowId) {
                    layer[j].second[activeRows[i][rowId]] = ((j >> rowId) & 1);
                }
            }

            vertices.emplace_back(move(layer));
        }

        for (const auto& layer: vertices) {
            cout << layer.size() << " ";
        }
        cout << endl;

        Lattice lattice(vertices.back().back().first + 1);

        for (size_t i = 1; i <= N; ++i) {
            for (auto& [vertexId, layer]: vertices[i]) {
                for (auto& [vertexIdPrev, layerPrev]: vertices[i - 1]) {
                    bool sameRowsMatches = true;
                    for (auto [rowId, val]: layer) {
                        if (layerPrev.count(rowId) && layerPrev[rowId] != val) {
                            sameRowsMatches = false;
                            break;
                        }
                    }
                    if (!sameRowsMatches) {
                        continue;
                    }
                    size_t parity = 0;
                    for (size_t j = 0; j < K; ++j) {
                        if (layer.count(j)) {
                            parity ^= layer[j] * G_msf[j][i - 1];
                        } else if (layerPrev.count(j)) {
                            parity ^= layerPrev[j] * G_msf[j][i - 1];
                        }
                    }
                    lattice[vertexId].push_back({
                        .layerIndex = i,
                        .parentIndex = vertexIdPrev,
                        .symbol = parity
                    });
                }
            }
        }
        return lattice;
    }

    size_t N, K;
    Matrix G;
    Matrix G_msf;
    Lattice L;
};

struct Solver {
    Solver() {
        cin >> N >> K;
        Matrix G(K, Vector(N));

        for (auto &i: G) {
            for (auto &j: i) {
                cin >> j;
            }
        }

        Code = make_shared<Viterbi>(move(G));
        Simulator = make_unique<AWGN2b>(Code);
    }

    void Loop() const {
        for (string cmd; cin >> cmd;) {
            if (auto it = Commands.find(cmd); it != Commands.end()) {
                (this->*(it->second))();
            } else {
                cout << "FAILURE" << endl;
            }
        }
    }

private:
    void Encode() const {
        Vector seq(K);
        for (auto &i: seq) cin >> i;
        for (auto i: Code->Encode(seq)) cout << i << ' ';
        cout << endl;
    }

    void Decode() const {
        VectorLog seq(N);
        for (auto &i: seq) cin >> i;
        for (auto i: Code->Decode(seq)) cout << i << ' ';
        cout << endl;
    }

    void Simulate() const {
        ld noiseLevel;
        size_t iterationsCount;
        size_t maxErrors;
        cin >> noiseLevel >> iterationsCount >> maxErrors;
        cout << Simulator->Simulate(noiseLevel, iterationsCount, maxErrors) << endl;
    }

    unordered_map<string, void(Solver::*)() const> Commands = {
        {"Encode", Encode},
        {"Decode", Decode},
        {"Simulate", Simulate}
    };

    size_t N, K;
    shared_ptr<ICode> Code;
    unique_ptr<ISimulator> Simulator;
};

/*
8 4
1 1 1 1 1 1 1 1
1 1 1 1 0 0 0 0
1 1 0 0 1 1 0 0
1 0 1 0 1 0 1 0
Encode 1 0 0 0
Decode -1.0 1.0 1 1 1 1 1 1.5
Decode -10 1 1 1 1 1 1 1
Simulate 3 100000 100
Simulate 4 100000 100

*/

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    Solver{}.Loop();

    return 0;
}

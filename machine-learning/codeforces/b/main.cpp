#include <exception>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>
#include <tgmath.h>

using T = double;

struct Matrix {
    Matrix(): rows(0), columns(0) {}
    Matrix(size_t r, size_t c): rows(r), columns(c), m(r, std::vector<T>(c)) {}

    Matrix operator*(const Matrix& rhs) const {
        //assert(rows == rhs.columns);
        assert(columns == rhs.rows);

        Matrix out(rows, rhs.columns);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < rhs.columns; ++j) {
                for (size_t k = 0; k < columns; ++k) {
                    out.m[i][j] += m[i][k] * rhs.m[k][j];
                }
            }
        }
        return out;
    }

    Matrix& operator*=(const Matrix& rhs) {
        return (*this = operator*(rhs));
    }

    Matrix operator+(const Matrix& rhs) const {
        assert(rows == rhs.rows);
        assert(columns == rhs.columns);

        Matrix out = *this;
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                out.m[i][j] += rhs.m[i][j];
            }
        }
        return out;
    }

    Matrix& operator+=(const Matrix& rhs) {
        assert(rows == rhs.rows);
        assert(columns == rhs.columns);

        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                m[i][j] += rhs.m[i][j];
            }
        }
        return *this;
    }

    Matrix operator-(const Matrix& rhs) const {
        assert(rows == rhs.rows);
        assert(columns == rhs.columns);

        Matrix out = *this;
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                out.m[i][j] -= rhs.m[i][j];
            }
        }
        return out;
    }

    Matrix transpose() const {
        Matrix out(columns, rows);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                out.m[j][i] = m[i][j];
            }
        }
        return out;
    }

    Matrix hadamard(const Matrix& rhs) const {
        assert (rows == rhs.rows && columns == rhs.columns);
        Matrix out = *this;
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                out.m[i][j] *= rhs.m[i][j];
            }
        }
        return out;
    }

    Matrix& hadamard_inplace(const Matrix& rhs) {
        assert (rows == rhs.rows && columns == rhs.columns);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                m[i][j] *= rhs.m[i][j];
            }
        }
        return *this;
    }

    bool operator==(const Matrix& rhs) const {
        return m == rhs.m;
    }

    friend std::ostream& operator<<(std::ostream& out, const Matrix& matrix) {
        for (auto& row: matrix.m) {
            for (T cell: row) {
                out << std::setprecision(6) << std::fixed << cell << ' ';
            }
            out << '\n';
        }
        return out;
    }

    size_t rows;
    size_t columns;
    std::vector<std::vector<T>> m;
};

struct Function {
public:
    template <typename... Args>
    Function(Args&... deps) : args({&deps...}) {
        for (auto dep: this->args) {
            dep->inheritors += 1;
        }
    }
    Function(std::vector<Function*>&& args) : args(std::move(args)) {
        for (auto dep: this->args) {
            dep->inheritors += 1;
        }
    }

    /* Retrieve value */
    const Matrix& value() {
        if (val.rows == 0) {
            for (Function * const f: args) {
                f->value();
            }
            val = forward(args);
        }
        return val;
    }

    /* Backprop gradient */
    void backprop(Matrix&& grad) {
        if (gr.rows == 0) {
            gr = std::move(grad);
        } else {
            // not moved
            gr += grad;
        }

        if (++backprop_counter < inheritors) {
            return;
        }

        std::vector<Matrix> grads = backward(gr);
        assert (args.size() == grads.size());

        for (size_t i = 0; i < args.size(); ++i) {
            args[i]->backprop(std::move(grads[i]));
        }
    }

    const Matrix& grad() const {
        if (gr.rows == 0) {
            throw std::exception{};
        }
        return gr;
    }

    size_t is_leaf() const {
        return inheritors == 0;
    }

    void detach() const {
        for (auto dep: dependencies()) {
            dep->inheritors -= 1;
        }
    }

protected:
    /* Pass grad to dependencies */
    virtual std::vector<Matrix> backward(const Matrix& grad) = 0;

    /* Calculate value from dependencies */
    virtual Matrix forward(const std::vector<Function*>& deps) = 0;

    const std::vector<Function*>& dependencies() const {
        return args;
    }

    template <typename... Args>
    void save_context(Args&&... args) {
        assert (context.empty());
        context = {std::move(args)...};
    }

    void save_context(std::vector<Matrix>&& ctx) {
        assert (context.empty());
        context = std::move(ctx);
    }

    const std::vector<Matrix>& get_context() {
        return context;
    }

private:
    size_t inheritors = 0;
    size_t backprop_counter = 0;
    std::vector<Function*> args;
    std::vector<Matrix> context;
    Matrix val;
    Matrix gr;
};

// FUNCTIONS

struct Var : Function {

    Var() = default;

    void set_value(Matrix&& val) {
        assert (!init);
        this->val = std::move(val);
        init = true;
    }

    std::vector<Matrix> backward(const Matrix&) override {
        assert (init);
        return {};
    }

    Matrix forward(const std::vector<Function*>&) override {
        assert (init);
        return val;
    }

private:
    bool init = false;
    Matrix val;
};

struct Tanh : Function {
    Tanh(Function& f) : Function(f) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        Matrix out = get_context()[0];
        for (auto& row: out.m) {
            for (auto& cell: row) {
                cell = (T)1 - std::pow(tanhl(cell), 2);
            }
        }
        return {out.hadamard(grad)};
    }

    Matrix forward(const std::vector<Function*>& deps) override {
        assert(deps.size() == 1);
        auto& input = deps[0]->value();
        Matrix out = input;
        for (auto& row: out.m) {
            for (auto& cell: row) {
                cell = std::tanh(cell);
            }
        }
        save_context(input);
        return out;
    }
};

struct LeakyRelu: Function {

    LeakyRelu(Function& f, T negative_slope) : negative_slope(negative_slope), Function(f) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        return {grad.hadamard(get_context()[0])};
    }

    Matrix forward(const std::vector<Function*>& deps) override {
        assert (deps.size() == 1);

        auto& input = deps[0]->value();
        Matrix out(input.rows, input.columns);
        Matrix grad(input.rows, input.columns);

        for (size_t i = 0; i < input.rows; ++i) {
            for (size_t j = 0; j < input.columns; ++j) {
                T cell = input.m[i][j];
                out.m[i][j] = std::max(cell, (T)0) + negative_slope * std::min(cell, (T)0);
                grad.m[i][j] = cell >= 0 ? (T)1 : negative_slope;
            }
        }
        save_context(std::move(grad));
        return out;
    }

private:
    T negative_slope;
};

struct Mul: Function {

    Mul(Function& a, Function& b) : Function(a, b) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        auto& ctx = get_context();
        return {grad * ctx[1], ctx[0] * grad};
    }

    Matrix forward(const std::vector<Function*>& deps) override {
        assert(deps.size() == 2);
        auto& a = deps[0]->value();
        auto& b = deps[1]->value();
        save_context(a.transpose(), b.transpose());
        return a * b;
    }
};

struct Sum: Function {

    Sum(std::vector<Function*>&& deps) : Function(std::move(deps)) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        return std::vector<Matrix>(dependencies().size(), grad);
    }

    Matrix forward(const std::vector<Function*>& deps) override {
        assert(deps.size() > 0);
        Matrix res = deps[0]->value();
        for (size_t i = 1; i < deps.size(); ++i) {
            res += deps[i]->value();
        }
        return res;
    }
};

struct Hadamar: Function {

    Hadamar(std::vector<Function*>&& deps) : Function(std::move(deps)) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        auto& ctx = get_context();
        std::vector<Matrix> out;
        out.reserve(ctx.size());

        for (size_t i = 0; i < ctx.size(); ++i) {
            Matrix m = (i == 0 ? grad : ctx[0]);
            for (size_t j = 1; j < ctx.size(); ++j) {
                m.hadamard_inplace(j == i ? grad : ctx[j]);
            }
            out.emplace_back(std::move(m));
        }
        return out;
    }

    Matrix forward(const std::vector<Function*>& deps) override {
        assert(deps.size() > 0);

        std::vector<Matrix> inputs;
        inputs.reserve(deps.size());
        for (auto& dep: deps) {
            inputs.emplace_back(dep->value());
        }

        Matrix out = inputs[0];
        for (size_t i = 1; i < inputs.size(); ++i) {
            out.hadamard_inplace(inputs[i]);
        }

        save_context(std::move(inputs));
        return out;
    }
};

// MAIN

int main() {
    using namespace std;

    size_t n, m, k;
    cin >> n >> m >> k;

    std::vector<Function*> graph;
    std::vector<std::pair<size_t, size_t>> input_dims;
    graph.reserve(n);

    for (size_t i = 0; i < n; ++i) {
        string f;
        cin >> f;
        if (f == "var") {
            size_t r, c;
            cin >> r >> c;
            input_dims.emplace_back(r, c);
            graph.push_back(new Var());
        } else if (f == "tnh") {
            size_t x;
            cin >> x;
            graph.push_back(new Tanh(*graph[x - 1]));
        } else if (f == "rlu") {
            T a_1;
            size_t x;
            cin >> a_1 >> x;
            graph.push_back(new LeakyRelu(*graph[x - 1], (T)1 / a_1));
        } else if (f == "mul") {
            size_t x, y;
            cin >> x >> y;
            graph.push_back(new Mul(*graph[x - 1], *graph[y - 1]));
        } else if (f == "sum") {
            size_t n;
            cin >> n;
            std::vector<Function*> deps;
            while (n-- > 0) {
                size_t x;
                cin >> x;
                deps.emplace_back(graph[x - 1]);
            }
            graph.push_back(new Sum(std::move(deps)));
        } else if (f == "had") {
            size_t n;
            cin >> n;
            std::vector<Function*> deps;
            while (n-- > 0) {
                size_t x;
                cin >> x;
                deps.emplace_back(graph[x - 1]);
            }
            graph.push_back(new Hadamar(std::move(deps)));
        } else {
            assert (false);
        }
    }

    // remove dead-end branches
    for (size_t i = n - k - 1; i < n; --i) {
        if (graph[i]->is_leaf()) {
            graph[i]->detach();
        }
    }

    for (size_t i = 0; i < m; ++i) {
        auto [r, c] = input_dims[i];
        Matrix in(r, c);
        for (auto &row: in.m) {
            for (auto& cell: row) {
                cin >> cell;
            }
        }
        dynamic_cast<Var&>(*graph[i]).set_value(std::move(in));
    }

    std::vector<std::pair<size_t, size_t>> output_dims;

    for (size_t i = k - 1; i < k; --i) {
        auto& m = graph[graph.size() - i - 1]->value();
        output_dims.emplace_back(m.rows, m.columns);
        cout << m << endl;
    }

    for (size_t z = 0; z < k; ++z) {
        auto [rows, columns] = output_dims[z];
        Matrix grad(rows, columns);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                cin >> grad.m[i][j];
            }
        }
        graph[graph.size() - k + z]->backprop(std::move(grad));
    }

    for (int i = 0; i < m; ++i) {
        try {
            cout << graph[i]->grad() << endl;

        } catch (const std::exception& e) {
            auto [rows, columns] = input_dims[i];
            cout << Matrix(rows, columns) << endl;
        }
    }
}

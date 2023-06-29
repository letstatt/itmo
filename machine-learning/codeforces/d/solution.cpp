#include <cstddef>
#include <exception>
#include <iostream>
#include <iomanip>
#include <type_traits>
#include <utility>
#include <vector>
#include <cassert>
#include <tgmath.h>

using T = double;

struct Matrix {
    Matrix(): rows(0), columns(0) {}
    Matrix(size_t r, size_t c): rows(r), columns(c), m(r, std::vector<T>(c)) {}

    Matrix operator*(const Matrix& rhs) const {
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
                out << std::setprecision(10) << std::fixed << cell << ' ';
            }
            out << '\n';
        }
        return out;
    }

    friend std::istream& operator>>(std::istream& in, Matrix& matrix) {
        for (auto& row: matrix.m) {
            for (T& cell: row) {
                in >> cell;
            }
        }
        return in;
    }

    size_t rows;
    size_t columns;
    std::vector<std::vector<T>> m;
};

struct Function {
public:
    template <typename... Args>
    Function(Args... deps) : args({deps...}) {
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
        assert(backprop_counter == inheritors);

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

    void _set_grad(Matrix&& g) {
        this->gr = std::move(gr);
    }

    size_t is_leaf() const {
        return inheritors == 0;
    }

    void detach() const {
        for (auto dep: dependencies()) {
            dep->inheritors -= 1;
        }
    }

    std::string name() const {
        std::stringstream str;
        str << function_name() << "[";
        str << dependencies().size() << " deps,";
        str << inheritors << "inheritors ,";
        str << backprop_counter << "backprop_counter]";
        return str.str();
    }

    void _increase_inheritors() {
        inheritors += 1;
    }

protected:
    /* Pass grad to dependencies */
    virtual std::vector<Matrix> backward(const Matrix& grad) = 0;

    /* Calculate value from dependencies */
    virtual Matrix forward(const std::vector<Function*>& deps) = 0;

    virtual const char* function_name() const = 0;

    const std::vector<Function*>& dependencies() const {
        return args;
    }

    template <typename... Args>
    void save_context(Args&&... args) {
        assert (context.empty());
        context = {std::move(args)...};
    }

    template <typename... Args>
    void save_context(Args&... args) {
        assert (context.empty());
        context = {args...};
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

struct Var: Function {

    Var() = default;
    Var(Matrix&& m): val(std::move(m)), init(true) {}

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

    const char* function_name() const override {
        return "Var";
    }

private:
    bool init = false;
    Matrix val;
};

struct Tanh: Function {
    Tanh(Function* f) : Function(f) {}

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

    const char* function_name() const override {
        return "Tanh";
    }
};

struct Mul: Function {

    Mul(Function* a, Function* b) : Function(a, b) {}

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

    const char* function_name() const override {
        return "Mul";
    }
};

struct Sum: Function {
    template <typename... Args>
    Sum(Args&&... deps): Function(std::move(deps)...) {}

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

    const char* function_name() const override {
        return "Sum";
    }
};

struct Hadamard: Function {
    template <typename... Args>
    Hadamard(Args&&... deps): Function(std::move(deps)...) {}

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

    const char* function_name() const override {
        return "Hadamard";
    }
};

struct Sigmoid: Function {
    Sigmoid(Function* f) : Function(f) {}

    std::vector<Matrix> backward(const Matrix& grad) override {
        Matrix out = get_context()[0];
        for (auto& row: out.m) {
            for (auto& cell: row) {
                cell = cell * (1 - cell);
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
                cell = 1.0 / (1 + exp(-cell));
            }
        }
        save_context(out);
        return out;
    }

    const char* function_name() const override {
        return "Sigmoid";
    }
};

// MAIN

int main() {
    using namespace std;

    size_t n, m;
    cin >> n;

    vector<Function*> inputs, seqs;
    inputs.reserve(n);

    // W_f, U_f, B_f, W_i, U_i, B_i, W_o, U_o, B_o, W_c, U_c, B_c
    vector<pair<size_t, size_t>> input_dims = {
        {n, n},
        {n, n},
        {n, 1},
        {n, n},
        {n, n},
        {n, 1},
        {n, n},
        {n, n},
        {n, 1},
        {n, n},
        {n, n},
        {n, 1},
    };

    for (const auto [rows, columns]: input_dims) {
        Matrix in(rows, columns);
        cin >> in;
        inputs.push_back(new Var(move(in)));
    }

    cin >> m;
    Function *h_0, *c_0;
    {
        Matrix in_1(n, 1), in_2(n, 1);
        cin >> in_1 >> in_2;
        h_0 = new Var(move(in_1));
        c_0 = new Var(move(in_2));
    }

    // x_1, x_2, ..., x_m
    for (size_t i = 0; i < m; ++i) {
        Matrix in(n, 1);
        cin >> in;
        seqs.push_back(new Var(move(in)));
    }

    // feed forward
    auto h_prev = h_0;
    auto c_prev = c_0;
    vector<Function*> outputs;

    for (size_t t = 0; t < m; ++t) {
        // f_t = \sigma(W_f * x_t + U_f * h_{t-1} + B_f)
        auto f_t = new Sigmoid(new Sum(new Mul(inputs[0], seqs[t]), new Mul(inputs[1], h_prev), inputs[2]));

        // i_t = \sigma(W_i * x_t + U_i * h_{t-1} + B_i)
        auto i_t = new Sigmoid(new Sum(new Mul(inputs[3], seqs[t]), new Mul(inputs[4], h_prev), inputs[5]));

        // o_t = \sigma(W_o * x_t + U_o * h_{t-1} + B_o)
        auto o_t = new Sigmoid(new Sum(new Mul(inputs[6], seqs[t]), new Mul(inputs[7], h_prev), inputs[8]));
        o_t->_increase_inheritors(); // perf-ench kostyl'

        // c_t = f_t • c_{t-1} + i_t • tanh(W_c * x_t + U_c * h_{t-1} + B_c)
        auto c_t = new Sum(
            new Hadamard(f_t, c_prev),
            new Hadamard(
                i_t,
                new Tanh(new Sum(new Mul(inputs[9], seqs[t]), new Mul(inputs[10], h_prev), inputs[11]))
            )
        );

        // h_t = o_t • c_t
        auto h_t = new Hadamard(o_t, c_t);

        // memorize
        h_prev = h_t;
        c_prev = c_t;
        outputs.push_back(o_t);
    }

    // output
    for (auto o_t: outputs) {
        cout << o_t->value();
    }
    cout << h_prev->value() << c_prev->value();
    h_prev->_increase_inheritors(); // perf-ench kostyl'
    c_prev->_increase_inheritors(); // perf-ench kostyl'

    // backprop
    {
        Matrix gr_1(n, 1), gr_2(n, 1);
        cin >> gr_1 >> gr_2;
        for (size_t t = m - 1; t < m; --t) {
            Matrix gr(n, 1);
            cin >> gr;
            outputs[t]->backprop(move(gr));
        }
        c_prev->backprop(move(gr_2));
        h_prev->backprop(move(gr_1));
    }

    for (size_t t = m - 1; t < m; --t) {
        cout << seqs[t]->grad();
    }

    cout << h_0->grad() << c_0->grad();

    for (auto& m: inputs) {
        cout << m->grad();
    }
}

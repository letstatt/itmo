// I am sorry about that rubbish

#include <exception>
#include <iostream>
#include <iomanip>
#include <limits>
#include <stdexcept>
#include <type_traits>
#include <variant>
#include <vector>
#include <cassert>
#include <array>
#include <span>
#include <ranges>
#include <any>

using T = double;

struct Matrix {
    Matrix(): rows(0), columns(0) {}
    Matrix(size_t r, size_t c): rows(r), columns(c), m(r, std::vector<T>(c)) {}
    Matrix(size_t r, size_t c, T val): rows(r), columns(c), m(r, std::vector<T>(c, val)) {}

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

    Matrix operator+(T bias) const {
        Matrix out = *this;
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                out.m[i][j] += bias;
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

    Matrix& operator+=(T bias) {
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                m[i][j] += bias;
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

template <typename T>
concept MatrixType = std::is_same_v<std::remove_cvref_t<T>, Matrix>;

struct Matrix3D {
    typedef Matrix(*Mapper)(const Matrix&);
    typedef Matrix(*BiMapper)(const Matrix&, const Matrix&);
    typedef void(*BiMapperInplace)(Matrix&, const Matrix&);

    Matrix3D(size_t initial_size) : m(initial_size) {}

    template <MatrixType... Args>
    Matrix3D(Args&&... M) : m({std::forward<Args>(M)...}) {}

    Matrix3D foreach(const Matrix3D& m3d, const BiMapper& f) const {
        assert(size() == m3d.size());
        Matrix3D res(size());
        for (size_t i = 0; i < size(); ++i) {
            res.m[i] = f(m[i], m3d[i]);
        }
        return res;
    }

    Matrix3D foreach(const Mapper& f) const {
        Matrix3D res(size());
        for (size_t i = 0; i < size(); ++i) {
            res.m[i] = f(m[i]);
        }
        return res;
    }

    Matrix3D& foreach_inplace(const Matrix3D& m3d, const BiMapperInplace& f) {
        assert(size() == m3d.size());
        for (size_t i = 0; i < size(); ++i) {
            f(m[i], m3d[i]);
        }
        return *this;
    }

    Matrix3D operator+(const Matrix3D& m3d) const {
        return foreach(m3d, [](auto& a, auto& b) {return a + b;});
    }

    Matrix3D& operator+=(const Matrix3D& m3d) {
        return foreach_inplace(m3d, [](auto& a, auto& b) {a += b;});
    }

    Matrix3D operator*(const Matrix3D& m3d) const {
        return foreach(m3d, [](auto& a, auto& b) {return a * b;});
    }

    Matrix3D hadamard(const Matrix3D& m3d) const {
        return foreach(m3d, [](auto& a, auto& b) {return a.hadamard(b);});
    }

    Matrix3D& hadamard_inplace(const Matrix3D& m3d) {
        return foreach_inplace(m3d, [](auto& a, auto& b) {a.hadamard_inplace(b);});
    }

    Matrix3D transpose() const {
        return foreach([](auto& a) {return a.transpose();});
    }

    Matrix& operator[](size_t i) {
        return m[i];
    }

    const Matrix& operator[](size_t i) const {
        return m[i];
    }

    auto begin() {
        return m.begin();
    }

    auto end() {
        return m.end();
    }

    auto begin() const {
        return m.begin();
    }

    auto end() const {
        return m.end();
    }

    bool empty() const {
        return m.empty();
    }

    size_t size() const {
        return m.size();
    }

    friend std::ostream& operator<<(std::ostream& out, const Matrix3D& matrix) {
        for (auto& m: matrix.m) {
            for (auto& i: m.m) {
                for (auto j: i) {
                    out << std::setprecision(6) << std::fixed << j << ' ';
                }
            }
        }
        return out;
    }

    std::vector<Matrix> m;
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
    const Matrix3D& value() {
        if (val.empty()) {
            for (Function * const f: args) {
                f->value();
            }
            val = forward(args);
        }
        return val;
    }

    /* Retrieve value constantly */
    const Matrix3D& value() const {
        if (val.empty()) {
            throw std::runtime_error("no value!");
        }
        return val;
    }

    /* Backprop gradient */
    void backprop(Matrix3D&& grad) {
        if (gr.empty()) {
            gr = std::move(grad);
        } else {
            // not moved
            assert (gr.size() == grad.size());
            for (size_t i = 0; i < gr.size(); ++i) {
                gr[i] += grad[i];
            }
        }

        if (++backprop_counter < inheritors) {
            return;
        }

        std::vector<Matrix3D> grads = backward(gr);
        assert (args.size() == grads.size());

        for (size_t i = 0; i < args.size(); ++i) {
            args[i]->backprop(std::move(grads[i]));
        }
    }

    const Matrix3D& grad() const {
        if (gr.empty()) {
            throw std::exception{};
        }
        return gr;
    }

protected:
    /* Pass grad to dependencies */
    virtual std::vector<Matrix3D> backward(const Matrix3D& grad) = 0;

    /* Calculate value from dependencies */
    virtual Matrix3D forward(const std::vector<Function*>& deps) = 0;

    const std::vector<Function*>& dependencies() const {
        return args;
    }

    template <typename... Args>
    void save_context(Args&&... args) {
        assert (context.empty());
        context = {std::forward<Args>(args)...};
    }

    void save_context(std::vector<Matrix3D>&& ctx) {
        assert (context.empty());
        context = std::move(ctx);
    }

    template <typename... Args>
    void save_any(Args&&... args) {
        assert (any.empty());
        any = {std::forward<Args>(args)...};
    }

    const std::vector<Matrix3D>& get_context() {
        return context;
    }

    const std::vector<std::any>& get_saved_any() {
        return any;
    }

private:
    size_t inheritors = 0;
    size_t backprop_counter = 0;
    std::vector<Function*> args;
    std::vector<Matrix3D> context;
    std::vector<std::any> any;
    Matrix3D val;
    Matrix3D gr;
};

// FUNCTIONS

struct Var: Function {
    Var() = default;
    Var(Matrix3D&& m3d): init(true), val(std::move(m3d)) {}

    void set_value(Matrix3D&& val) {
        assert (!init);
        this->val = std::move(val);
        init = true;
    }

    std::vector<Matrix3D> backward(const Matrix3D&) override {
        assert (init);
        return {};
    }

    Matrix3D forward(const std::vector<Function*>&) override {
        assert (init);
        return val;
    }

private:
    bool init = false;
    Matrix3D val;
};

struct LeakyRelu: Function {

    LeakyRelu(Function& f, T negative_slope) : negative_slope(negative_slope), Function(f) {}

    std::vector<Matrix3D> backward(const Matrix3D& grad) override {
        return {grad.hadamard(get_context()[0])};
    }

    Matrix3D forward(const std::vector<Function*>& deps) override {
        assert (deps.size() == 1);

        const Matrix3D& input = deps[0]->value();
        Matrix3D out_seq(input.size());
        Matrix3D grad_seq(input.size());

        for (size_t k = 0; k < input.size(); ++k) {
            const Matrix& m = input[k];
            Matrix out(m.rows, m.columns);
            Matrix grad(m.rows, m.columns);

            for (size_t i = 0; i < m.rows; ++i) {
                for (size_t j = 0; j < m.columns; ++j) {
                    T cell = m.m[i][j];
                    out.m[i][j] = std::max(cell, (T)0) + negative_slope * std::min(cell, (T)0);
                    grad.m[i][j] = cell >= 0 ? (T)1 : negative_slope;
                }
            }
            out_seq[k] = std::move(out);
            grad_seq[k] = std::move(grad);
        }
        save_context(std::move(grad_seq));
        return out_seq;
    }

private:
    T negative_slope;
};

struct MaxPool: Function {
    MaxPool(Function& dep, size_t S): Function(dep), stride(S) {}

    std::vector<Matrix3D> backward(const Matrix3D& grad) override {
        auto max_pos_info = get_context()[0]; // copy

        for (size_t k = 0; k < grad.size(); ++k) {
            const auto& m = grad[k];
            auto& max_pos_layer = max_pos_info[k];

            for (size_t i = 0; i < m.rows; ++i) {
                for (size_t j = 0; j < m.columns; ++j) {
                    size_t i2 = i * stride;
                    size_t j2 = j * stride;

                    for (size_t p = 0; p < stride; ++p) {
                        for (size_t q = 0; q < stride; ++q) {
                            max_pos_layer.m[i2 + p][j2 + q] *= m.m[i][j];
                        }
                    }
                }
            }
        }
        return {std::move(max_pos_info)};
    }

    Matrix3D forward(const std::vector<Function*>& deps) override {
        assert (deps.size() == 1);
        auto& input = deps[0]->value();
        assert (!input.empty());
        assert (input[0].rows >= stride && input[0].columns >= stride);
        assert (input[0].rows % stride == 0);
        assert (input[0].columns % stride == 0);

        size_t cols = input[0].columns / stride;
        size_t rows = input[0].rows / stride;

        Matrix3D out_seq(input.size());
        Matrix3D max_pos_info_seq(input.size());

        for (size_t k = 0; k < input.size(); ++k) {
            Matrix out(rows, cols, std::numeric_limits<T>::lowest());
            Matrix max_pos_info(input[0].rows, input[0].columns);

            const Matrix& m = input[k];

            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < cols; ++j) {
                    T& val = out.m[i][j];
                    size_t i2 = i * stride;
                    size_t j2 = j * stride;
                    T saved_max = std::numeric_limits<T>::lowest();
                    for (size_t p = 0; p < stride; ++p) {
                        for (size_t q = 0; q < stride; ++q) {
                            val = std::max(val, m.m[i2 + p][j2 + q]);
                            saved_max = std::max(saved_max, val);
                        }
                    }
                    for (size_t p = 0; p < stride; ++p) {
                        for (size_t q = 0; q < stride; ++q) {
                            if (m.m[i2 + p][j2 + q] == saved_max) {
                                max_pos_info.m[i2 + p][j2 + q] = 1;
                            }
                        }
                    }
                }
            }

            out_seq[k] = std::move(out);
            max_pos_info_seq[k] = std::move(max_pos_info);
        }
        save_context(std::move(max_pos_info_seq));
        return out_seq;
    }

private:
    size_t stride;
};

struct Bias: Function {
    Bias(Function& dep, Var& bias_vect): Function(dep, bias_vect) {
        auto& m3d = bias_vect.value();
        for (const Matrix& m: m3d) {
            assert(m.rows == 1 && m.columns == 1);
        }
    }

    std::vector<Matrix3D> backward(const Matrix3D& grad) override {
        Matrix3D sum(grad.size());
        for (size_t k = 0; k < sum.size(); ++k) {
            T val = 0;
            for (auto& i: grad[k].m) {
                for (auto j: i) {
                    val += j;
                }
            }
            sum[k] = Matrix(1, 1, val);
        }
        return {grad, std::move(sum)};
    }

    Matrix3D forward(const std::vector<Function*>& deps) override {
        assert (deps.size() == 2);
        assert (deps[0]->value().size() == deps[1]->value().size());

        Matrix3D res = deps[0]->value();
        const Matrix3D& bias = deps[1]->value();

        for (size_t k = 0; k < res.size(); ++k) {
            res[k] += bias[k].m[0][0];
        }
        return res;
    }

    const Var* get_bias_vect() const {
        return static_cast<Var*>(dependencies()[1]);
    }
};

struct PaddingBase: Function {
    using CoordsMap = std::vector<std::vector<std::pair<size_t, size_t>>>;

    PaddingBase(Function& dep, size_t P): Function(dep), padding(P) {}

    std::vector<Matrix3D> backward(const Matrix3D& grad) override {
        assert (!grad.empty());
        size_t depth = grad.size();
        size_t rows = grad[0].rows;
        size_t cols = grad[0].columns;
        size_t input_rows = rows - 2 * padding;
        size_t input_cols = cols - 2 * padding;

        const CoordsMap& map = std::any_cast<const CoordsMap&>(get_saved_any()[0]);
        Matrix3D out;

        for (size_t k = 0; k < depth; ++k) {
            Matrix m(input_rows, input_cols);

            for (size_t i = 0; i < map.size(); ++i) {
                for (size_t j = 0; j < map[0].size(); ++j) {
                    auto [p, q] = map[i][j];
                    m.m[p][q] += grad[k].m[i][j];
                }
            }
            out.m.emplace_back(std::move(m));
        }
        return {std::move(out)};
    }

    Matrix3D forward(const std::vector<Function*>& deps) override {
        assert (deps.size() == 1);
        const Matrix3D& input = deps[0]->value();
        assert (!input.empty());

        size_t input_rows = input[0].rows;
        size_t input_cols = input[0].columns;
        size_t rows = input_rows + 2 * padding;
        size_t cols = input_cols + 2 * padding;

        CoordsMap map = coordsMap(input_rows, input_cols, padding);
        Matrix3D out;

        for (size_t k = 0; k < input.size(); ++k) {
            Matrix m(rows, cols);

            for (size_t i = 0; i < map.size(); ++i) {
                for (size_t j = 0; j < map[0].size(); ++j) {
                    auto [p, q] = map[i][j];
                    m.m[i][j] += input[k].m[p][q];
                }
            }
            out.m.emplace_back(std::move(m));
        }

        save_any(std::move(map));
        return out;
    }

protected:
    virtual CoordsMap coordsMap(size_t, size_t, size_t) const = 0;

private:
    size_t padding;
};

struct MirrorPadding: PaddingBase {
    MirrorPadding(Function& dep, size_t P): PaddingBase(dep, P) {}

    CoordsMap coordsMap(size_t input_rows, size_t input_cols, size_t padding) const override {
        size_t rows = input_rows + 2 * padding;
        size_t cols = input_cols + 2 * padding;
        size_t processed = 0;
        CoordsMap mapping;

        mapping.resize(rows, std::vector<std::pair<size_t, size_t>>(cols, {-1, -1}));

        // fill center
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < input_cols; ++j) {
                mapping[i + padding][j + padding] = {i, j};
                processed += 1;
            }
        }

        // fill horizontally
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < padding; ++j) {
                // left side
                mapping[i + padding][j] = mapping[i + padding][2 * padding - j];
                // right side
                mapping[i + padding][cols - j - 1] = mapping[i + padding][cols - 2 * padding - 1 + j];
                processed += 2;
            }
        }

        // fill vertically
        for (size_t i = 0; i < padding; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                // top
                mapping[i][j] = mapping[2 * padding - i][j];
                // bottom
                mapping[rows - 1 - i][j] = mapping[rows - 2 * padding - 1 + i][j];
                processed += 2;
            }
        }

        assert (processed == rows * cols);
        return mapping;
    }
};

struct EdgePadding: PaddingBase {
    EdgePadding(Function& dep, size_t P): PaddingBase(dep, P) {}

    CoordsMap coordsMap(size_t input_rows, size_t input_cols, size_t padding) const override {
        size_t rows = input_rows + 2 * padding;
        size_t cols = input_cols + 2 * padding;
        size_t processed = 0;
        CoordsMap mapping;

        mapping.resize(rows, std::vector<std::pair<size_t, size_t>>(cols, {-1, -1}));

        // fill center
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < input_cols; ++j) {
                mapping[i + padding][j + padding] = {i, j};
                processed += 1;
            }
        }

        // fill horizontally
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < padding; ++j) {
                // left side
                mapping[i + padding][j] = mapping[i + padding][padding];
                // right side
                mapping[i + padding][cols - j - 1] = mapping[i + padding][cols - padding - 1];
                processed += 2;
            }
        }

        // fill vertically
        for (size_t i = 0; i < padding; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                // top
                mapping[i][j] = mapping[padding][j];
                // bottom
                mapping[rows - 1 - i][j] = mapping[rows - padding - 1][j];
                processed += 2;
            }
        }

        assert (processed == rows * cols);
        return mapping;
    }
};

struct CyclicPadding: PaddingBase {
    CyclicPadding(Function& dep, size_t P): PaddingBase(dep, P) {}

    CoordsMap coordsMap(size_t input_rows, size_t input_cols, size_t padding) const override {
        size_t rows = input_rows + 2 * padding;
        size_t cols = input_cols + 2 * padding;
        size_t processed = 0;
        CoordsMap mapping;

        mapping.resize(rows, std::vector<std::pair<size_t, size_t>>(cols, {-1, -1}));

        // fill center
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < input_cols; ++j) {
                mapping[i + padding][j + padding] = {i, j};
                processed += 1;
            }
        }

        // fill horizontally
        for (size_t i = 0; i < input_rows; ++i) {
            for (size_t j = 0; j < padding; ++j) {
                // left side
                mapping[i + padding][padding - j - 1] = mapping[i + padding][padding + input_cols - j - 1];
                // right side
                mapping[i + padding][padding + input_cols + j] = mapping[i + padding][padding + j];
                processed += 2;
            }
        }

        // fill vertically
        for (size_t i = 0; i < padding; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                // top
                mapping[padding - i - 1][j] = mapping[padding + input_rows - i - 1][j];
                // bottom
                mapping[padding + input_rows + i][j] = mapping[padding + i][j];
                processed += 2;
            }
        }

        assert (processed == rows * cols);
        return mapping;
    }
};

struct Convolution: Function {
    Convolution(size_t S, std::vector<Function*>&& deps): Function(std::move(deps)), stride(S) {
        assert (dependencies().size() > 1);
    }

    std::vector<Matrix3D> backward(const Matrix3D& grad) override {
        const auto& ctx = get_context();
        const auto& input = ctx[0];

        const auto& kernels = get_kernels();
        std::vector<Matrix3D> out(kernels.size() + 1);

        size_t input_rows = input[0].rows;
        size_t input_cols = input[0].columns;
        size_t kernel_rows = kernels[0]->value()[0].rows;
        size_t kernel_cols = kernels[0]->value()[0].columns;
        size_t new_size = (input_rows - kernel_rows) / stride + 1;

        out[0] = Matrix3D(input.size());
        for (auto &m: out[0]) {
            m = Matrix(input_rows, input_cols);
        }

        for (size_t k1 = 0; k1 < kernels.size(); ++k1) {
            Matrix3D tmp_grad(input.size());

            for (size_t k2 = 0; k2 < input.size(); ++k2) {
                const auto& kernel = kernels[k1]->value()[k2];
                Matrix tmp_grad_i(kernel_rows, kernel_cols);

                for (size_t i = 0; i < new_size; ++i) {
                    for (size_t j = 0; j < new_size; ++j) {
                        size_t ii = i * stride;
                        size_t jj = j * stride;
                        for (size_t i2 = 0; i2 < kernel_rows; ++i2) {
                            for (size_t j2 = 0; j2 < kernel_cols; ++j2) {
                                tmp_grad_i.m[i2][j2] += input[k2].m[ii + i2][jj + j2] * grad[k1].m[i][j];
                                out[0][k2].m[ii + i2][jj + j2] += kernel.m[i2][j2] * grad[k1].m[i][j];
                            }
                        }
                    }
                }
                tmp_grad[k2] = std::move(tmp_grad_i);
            }
            out[k1 + 1] = std::move(tmp_grad);
        }
        return out;
    }

    Matrix3D forward(const std::vector<Function*>& deps) override {
        assert (deps.size() > 1);
        const Matrix3D& input = deps[0]->value();
        std::span<Function * const> kernels(&deps[1], &*deps.end());
        assert (input.size() == kernels[0]->value().size());

        size_t input_rows = input[0].rows;
        size_t input_cols = input[0].columns;
        size_t kernel_rows = kernels[0]->value()[0].rows;
        size_t kernel_cols = kernels[0]->value()[0].columns;

        assert (input_rows == input_cols && kernel_rows == kernel_cols);
        assert ((input_rows - kernel_rows) % stride == 0);
        size_t new_size = (input_rows - kernel_rows) / stride + 1;

        Matrix3D out(kernels.size());

        for (size_t k1 = 0; k1 < kernels.size(); ++k1) {
            Matrix tmp(new_size, new_size);
            Matrix3D tmp_grad(input.size());

            for (size_t k2 = 0; k2 < input.size(); ++k2) {
                const auto& kernel = kernels[k1]->value()[k2];

                for (size_t i = 0; i < tmp.rows; ++i) {
                    for (size_t j = 0; j < tmp.columns; ++j) {
                        size_t ii = i * stride;
                        size_t jj = j * stride;
                        for (size_t i2 = 0; i2 < kernel_rows; ++i2) {
                            for (size_t j2 = 0; j2 < kernel_cols; ++j2) {
                                tmp.m[i][j] += input[k2].m[ii + i2][jj + j2] * kernel.m[i2][j2];
                            }
                        }
                    }
                }
            }
            out[k1] = std::move(tmp);
        }
        save_context(input);
        return out;
    }

    std::span<const Function* const> get_kernels() const {
        const auto& deps = dependencies();
        return {&deps[1], &*deps.end()};
    }

private:
    const size_t stride;
};


// MAIN

int main() {
    using namespace std;

    size_t n, d, l;
    cin >> n >> d;

    Matrix3D input(d);

    for (Matrix& m: input) {
        m = Matrix(n, n);
        for (auto& i: m.m) {
            for (auto& j: i) {
                cin >> j;
            }
        }
    }

    Var* v = new Var(std::move(input));
    std::vector<std::variant<Var*, Bias*, Convolution*>> layers = {v};

    size_t last_depth = d;
    Function* last_func = v;

    string name;
    cin >> l;

    while (l-- > 0) {
        cin >> name;

        // for test purposes only
        if (name == "mirror") {
            size_t p;
            cin >> p;
            last_func = new MirrorPadding(*last_func, p);
        } else if (name == "edge") {
            size_t p;
            cin >> p;
            last_func = new EdgePadding(*last_func, p);
        } else if (name == "cyclic") {
            size_t p;
            cin >> p;
            last_func = new CyclicPadding(*last_func, p);
        }

        // task-specific layers
        else if (name == "cnvm" || name == "cnve" || name == "cnvc") {
            size_t h, k, s, p;
            cin >> h >> k >> s >> p;

            std::vector<Function*> inputs;
            inputs.emplace_back(nullptr);

            for (size_t i = 0; i < h; ++i) {
                Matrix3D v(last_depth);
                for (Matrix& m: v) {
                    m = Matrix(k, k);
                    for (auto& i: m.m) {
                        for (auto& j: i) {
                            cin >> j;
                        }
                    }
                }
                inputs.emplace_back(new Var(std::move(v)));
            }

            if (name == "cnvm") {
                inputs[0] = new MirrorPadding(*last_func, p);
            } else if (name == "cnve") {
                inputs[0] = new EdgePadding(*last_func, p);
            } else if (name == "cnvc") {
                inputs[0] = new CyclicPadding(*last_func, p);
            } else {
                throw std::runtime_error("Unknown convolution layer name: " + name);
            }

            Convolution* layer = new Convolution(s, std::move(inputs));
            layers.emplace_back(layer);
            last_func = layer;
            last_depth = h;

        } else if (name == "bias") {
            Matrix3D bias;

            for (size_t i = 0; i < last_depth; ++i) {
                Matrix m(1, 1);
                cin >> m.m[0][0];
                bias.m.emplace_back(std::move(m));
            }

            Bias* layer = new Bias(*last_func, *new Var(std::move(bias)));
            layers.emplace_back(layer);
            last_func = layer;

        } else if (name == "relu") {
            double a_1;
            cin >> a_1;
            last_func = new LeakyRelu(*last_func, T(1) / a_1);

        } else if (name == "pool") {
            size_t s;
            cin >> s;
            last_func = new MaxPool(*last_func, s);
        }
    }

    const Matrix3D& value = last_func->value();
    cout << value << endl;

    Matrix3D grad(value.size());
    for (Matrix& m: grad) {
        m = Matrix(value.m[0].rows, value.m[0].columns, 0);
        for (auto& i: m.m) {
            for (auto &j: i) {
                cin >> j;
            }
        }
    }
    last_func->backprop(std::move(grad));

    auto visitor = [](const auto& val) {
        using T = std::remove_cvref_t<decltype(val)>;

        if constexpr (std::is_same_v<T, Var*>) {
            cout << val->grad();

        } else if constexpr (std::is_same_v<T, Bias*>) {
            cout << val->get_bias_vect()->grad();

        } else if constexpr (std::is_same_v<T, Convolution*>) {
            const auto& kernels = val->get_kernels();
            for (const auto& kernel: kernels) {
                cout << kernel->grad();
            }

        } else {
            throw std::bad_variant_access{};
        }
    };

    for (auto& v: layers) {
        std::visit(visitor, v);
        cout << endl;
    }
}

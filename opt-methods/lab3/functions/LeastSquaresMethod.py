from utils.config import *

class LeastSquaresMethod:
    function = None
    p = None

    def __init__(self, points, function):
        self.function = function
        self.p = points
    
    # return vertical array of r_i
    def get_r(self, c, points):
        r = np.array([y - self.function.f(x, c) for y, *x in points])
        return r.reshape(-1, 1)

    # return scalar error value
    def get_error(self, c):
        r = self.get_r(c, self.p)
        return np.dot(r, r.T)[0][0]
    
    # return jacobian
    def jacobian(self, c):
        jac = np.ndarray(shape=(len(self.p), len(c)))
        for i in range(len(self.p)):
            jac[i] = -self.function.grad(self.p[i][1:], c).T[0]
        return jac
    
    def f(self, c):
        return self.get_error(c)
    
    def __call__(self, c):
        return self.f(c)
    
    # return least-squares method grad
    def grad(self, c):
        j = self.jacobian(c)
        return 2 * np.matmul(j.T, self.get_r(c, self.p))
    
    # return approximated hessian
    def hessian(self, c):
        jac = self.jacobian(c)
        return 2 * np.matmul(jac.T, jac)
    
    def stochastic_jacobian(self, c, points):
        jac = np.ndarray(shape=(len(points), len(c)))
        for i in range(len(points)):
            jac[i] = -self.function.grad(points[i][1:], c).T[0]
        return jac
    
    def stochastic_grad(self, c, batch_count=1):
        if not hasattr(self, "pos"):
            self.pos = 0
        batch = self.p[self.pos: self.pos + batch_count]
        self.pos = (self.pos + batch_count) % len(self.p)
        j = self.stochastic_jacobian(c, batch)
        return 2 * np.matmul(j.T, self.get_r(c, batch))

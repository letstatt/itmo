from utils.config import *

class TLeastSquares:
    f = None
    p = None
    jac = None

    def __init__(self, f, points):
        self.f = f
        self.p = points
    
    def __call__(self, c):
        self.jac = list(list(torch.tensor([i], requires_grad=True, dtype=torch.float64) for i in c) for j in self.p)
        res = []
        for i in range(len(self.p)):
            res.append((self.p[i][0] - self.f(self.p[i][1:], self.jac[i]))**2)
            res[-1].backward()
        return torch.tensor(res)
    
    def jacobian(self, *args):
        return torch.tensor(list([list(j.grad for j in i) for i in self.jac]), dtype=torch.float64)


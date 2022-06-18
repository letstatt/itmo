from utils.config import *

class TRozenbrock:
    jac = None

    def __call__(self, input):
        self.jac = [torch.tensor(i, requires_grad=True) for i in input]
        z = (1 - self.jac[0])**2 + 100 * (self.jac[1] - self.jac[0]**2)**2
        z.backward()
        return z.clone().detach()

    def jacobian(self, x):
        return torch.tensor(list(i.grad for i in self.jac))

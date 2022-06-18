from utils.config import *

class THimmelblau:
    jac = None

    def __call__(self, input):
        self.jac = [torch.tensor(i, requires_grad=True) for i in input]
        z = (self.jac[0]**2 + self.jac[1] - 11)**2 + (self.jac[0] + self.jac[1]**2 - 7)**2
        z.backward()
        return z.clone().detach()

    def jacobian(self, x):
        return torch.tensor(list(i.grad for i in self.jac))

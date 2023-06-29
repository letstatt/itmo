import torch
import os


class LeakyReLU(torch.autograd.Function):
    @staticmethod
    def forward(ctx, input: torch.Tensor, negative_slope):
        t = input.detach()
        ctx.save_for_backward(t.apply_(lambda x: 1.0 if x >= 0 else negative_slope))
        return t.apply_(lambda x: max(x, 0) + negative_slope * min(x, 0))

    @staticmethod
    def backward(ctx, grad_output):
        return ctx.saved_tensors[0] * grad_output, None


n, m, k = map(int, input().split())
vars = []
transforms = []
layers = []

for i in range(n):
    if i < m:
        var, r, c = input().split()
        vars.append((int(r), int(c)))
        assert var == "var"
    else:
        transforms.append(input().split())

for (r, c) in vars:
    tensor = torch.tensor([list(map(int, input().split())) for i in range(r)], dtype=torch.double)
    tensor.requires_grad = True
    layers.append(tensor)

for func, *args in transforms:
    if func == "tnh":
        x = int(args[0])
        layers.append(torch.tanh(layers[x - 1]))
    elif func == "rlu":
        a_1, x = map(int, args)
        layers.append(LeakyReLU.apply(layers[x - 1], 1/a_1))
    elif func == "mul":
        i, j = map(int, args)
        layers.append(torch.matmul(layers[i - 1], layers[j - 1]))
    elif func == "sum":
        _, *indexes = list(map(int, args))
        layers.append(layers[indexes[0] - 1])
        for i in indexes[1:]:
            layers[-1] = layers[-1] + layers[i - 1]
    elif func == "had":
        _, *indexes = list(map(int, args))
        layers.append(layers[indexes[0] - 1])
        for i in indexes[1:]:
            layers[-1] = layers[-1] * layers[i - 1]
    else:
        assert False

for i in range(k):
    out = layers[-k + i]
    shape = out.shape
    tensor = torch.tensor([list(map(int, input().split())) for i in range(shape[0])], dtype=torch.double)
    out.backward(gradient=tensor)


def printer(t):
    if len(t.shape) == 0:
        print(t.item())
    elif len(t.shape) == 1:
        for i in range(t.shape[0]):
            print(t[i].item(), end=' ')
        print()
    else:
        for i in range(t.shape[0]):
            for j in range(t.shape[1]):
                print(t[i][j].item(), end=' ')
            print()

for i in range(k):
    printer(layers[-k + i])

for i in range(m):
    if layers[i].grad is None:
        print(None)
        continue
    printer(layers[i].grad)

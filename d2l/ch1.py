import torch


# stride [::3, ::2]
t0 = torch.tensor([[1, 2, 3, 4], [2, 1, 3, 4], [4, 3, 2, 1], [4, 3, 1, 2]])
t1 = t0[::3, ::2]
print(t1)

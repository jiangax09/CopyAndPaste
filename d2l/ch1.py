import torch
import os
import pandas


# stride [::3, ::2]
t0 = torch.tensor([[1, 2, 3, 4], [2, 1, 3, 4], [4, 3, 2, 1], [4, 3, 1, 2]])
t1 = t0[::3, ::2]
print(t1)

os.makedirs(os.path.join('..', 'data'), exist_ok=True)
data_file = os.path.join('..', 'data', 'house_tiny.csv')
with open(data_file, 'w') as f:
    f.write('NumRooms,Alley,Price\n')
    f.write('NA,Pave,127500\n')
    f.write('2,NA,106000\n')
    f.write('4,NA,178100\n')
    f.write('NA,NA,140000\n')

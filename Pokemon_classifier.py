# -*- coding: utf-8 -*-
"""

Automatically generated by Colab.

## Download data and source folder
"""

!pip install gdown
import gdown
import zipfile
url = 'https://drive.google.com/uc?id=1CQdgTOUlY-TUoWZyxtVZxRthBhSuhDVi'
output = 'source.zip'
gdown.download(url, output, quiet=False)
with zipfile.ZipFile(output, "r") as zip_ref:
    zip_ref.extractall('.')

"""## Install package for calculating FLOPS

"""

!pip install pthflops

import os
import sys
import os.path as osp
from PIL import Image
import random
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import torchvision
import torchvision.transforms as T
import glob
import matplotlib.pyplot as plt
from pthflops import count_ops
from src.util import reset
from src.data import get_dataloader
from src.test import test

"""## Define label names, data directory, device name.

"""

label_names = ['bug', 'electric', 'fighting', 'fire', 'flying', 'grass', 'ground', 'phychic', 'poison', 'water']
data_dir = 'data'

device = 'cuda' if torch.cuda.is_available() else 'cpu'

batch_size = 1

""" DEFINE `MyModel`"""

class MyModel(nn.Module):
    def __init__(self, dim_hidden=32, dim_output = len(label_names)):
        super().__init__()

        self.conv1 = nn.Conv2d(3, dim_hidden, 5, 2, 2)
        self.conv2 = nn.Conv2d(dim_hidden, dim_hidden, 5, 2, 2)
        self.conv3 = nn.Conv2d(dim_hidden, dim_hidden, 5, 2, 2)

        self.pool = nn.MaxPool2d(2, 2)
        self.adapt_pool = nn.AdaptiveAvgPool2d(1)
        self.acti = nn.GELU()

        self.fc = nn.Linear(dim_hidden, dim_output)

    def forward(self, inp):
        B, C, H, W = inp.shape

        out = self.pool(self.acti(self.conv1(inp)))
        out = self.pool(self.acti(self.conv2(out)))
        out = self.pool(self.acti(self.conv3(out)))

        out = self.adapt_pool(out)
        out = out.view(B, -1)

        return self.fc(out)

""" Loss function.


"""

def train(model, optimizer, sample):


    criterion = nn.CrossEntropyLoss()

    model.train()

    input = sample['img'].float().to(device)
    label = sample['label'].long().to(device)

    pred = model(input)

    loss = criterion(pred, label)

    num_correct = torch.sum(torch.argmax(pred, dim=-1)==label)

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    return loss.item(), num_correct.item()

"""

DEFINE `get_optimizer`

"""

def get_optimizer(model, lr=1e-4, wd=1e-7):
    return optim.Adam(model.parameters(), lr=lr, weight_decay=wd)

"""## Repeat Training with different 10 random seeds.


"""

### Do not change below parameters ###
max_epoch = 5
num_seeds = 10
### Do not change above parameters ###


total_best_accu = []

for seed in range(num_seeds):
    reset(seed)

    train_loader, test_loader, train_dataset, test_dataset = get_dataloader(data_dir, label_names, batch_size)

    model = MyModel().to(device)

    optimizer = get_optimizer(model)

    best_accu = -100
    for epoch in range(max_epoch):
        avg_tr_loss = 0.0
        avg_tr_correct = 0.0
        for sample in train_loader:
            tr_loss, tr_correct = train(model, optimizer, sample)

            avg_tr_loss += tr_loss / len(train_loader)
            avg_tr_correct += tr_correct / len(train_dataset)

        avg_te_correct = 0.0
        for sample in test_loader:
            te_correct = test(model, sample, device)
            avg_te_correct += te_correct / len(test_dataset)

        best_accu = max(avg_te_correct, best_accu)

    print('<<<<<[SEED {}] BEST ACCU : {}>>>>>'.format(seed, best_accu))
    total_best_accu.append(best_accu)
    if seed < num_seeds-1:
        del model, optimizer


model = MyModel().to(device)
num_ops = count_ops(model, torch.rand(1, 3, 112, 112).to(device), verbose=False)

mean_accu = np.mean(total_best_accu)
accu_thres = 0.75
accu_point = min(1, np.exp(-2*(accu_thres-mean_accu)))

flops = num_ops[0]

print('*'*50)
print('Mean Accuracy :', mean_accu)
print('Accuracy Point:', accu_point)
print('*'*50)
print('Flops: ', flops)



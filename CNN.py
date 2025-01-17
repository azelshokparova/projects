# -*- coding: utf-8 -*-
""" CNN

Automatically generated by Colab.


### Import Packages
"""

import os
import csv
import numpy as np
from tqdm.notebook import tqdm
import random
import matplotlib.pyplot as plt

import torch
import torchvision
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader


print(torch.__version__)

seed = 0
random.seed(seed)
np.random.seed(seed)
torch.manual_seed(seed)
torch.cuda.manual_seed_all(seed)
torch.backends.cudnn.deterministic = True
#generate the same random set of numbers

"""### Let's load the Flowers102 dataset
in this 102 dataset, images have different sizes > we need to preprocess the dataset
"""

data_path = './Flowers' # specifying the directory
batch_size = 256

#Imagenet RGB oizel values means and std are used
mean=[0.485, 0.456, 0.406]
std=[0.229, 0.224, 0.225]


#resize, cut image into the same size + RBG pixel normalization
my_transform = torchvision.transforms.Compose([
    torchvision.transforms.ToTensor(),
    torchvision.transforms.Resize(224), #resize the smallest edge(width or height of image) to 224
    torchvision.transforms.CenterCrop((224, 224)), #to make the images squared,
    torchvision.transforms.Normalize(mean=mean, std = std) #normalized based on earlier specified mean ans std > for gaussian distribution
    ]
    )

train_dataset = torchvision.datasets.Flowers102(data_path, 'train', transform= my_transform, download=True)
test_dataset = torchvision.datasets.Flowers102(data_path, 'test', transform= my_transform, download=True)
train_dataloader = DataLoader(train_dataset, batch_size= batch_size, shuffle=True)
test_dataloader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

"""### Choose your device"""

# device = 'cpu'
device = 'cuda' if torch.cuda.is_available() else 'cpu'
print('Current Device : {}'.format(device))

"""### Sample out one data point from dataloader"""

sample = next(iter(train_dataloader))
# sample = test_dataset[1]
print(sample[0].shape) # 3 is channel (BATCH CHANNEL WIDTH HEIGHT)
print(sample[1]) # LABELS

fig, ax = plt.subplots(1, 10, figsize=(15, 4))
for plot_idx in range(10):
    ax[plot_idx].imshow(sample[0][plot_idx].permute(1, 2, 0))
    ax[plot_idx].set_title('LABEL : {}'.format(sample[1][plot_idx]))
    ax[plot_idx].set_xticks([])
    ax[plot_idx].set_yticks([])
plt.show()

"""### Define the ResNet Based Model"""

class ResNets(nn.Module):
    def __init__(self, dim_output=102): # number of classes = 102
        super(ResNets, self).__init__()
        self.resnet = torchvision.models.resnet50(weights=torchvision.models.ResNet50_Weights.DEFAULT) # call the structure of resnet #default returns the most updated resnet

        #you freezing all layers
        for p in self.resnet.parameters():
          p.requires_grad = False

        #unfreeze the parameters in layer 4
        for p in self.resnet.layer4.parameters():
          p.requires_grad = True

        #this needs to be re-trained
        #this is also unfreezed since it re-defined
        self.resnet.fc = nn.Linear(2048, dim_output) #we need to change the number of classes of resnet to the number of classes of our problem(102)
        #print(self.resnet)


    def forward(self, img):
        return self.resnet(img)

model = ResNets()
#if you want to check the structure of resnet
#Resnets()
#we freeze from 1 to 3 and

"""### Define the Model"""

model = ResNets()
model = model.to(device)

optimizer = optim.Adam(model.parameters(), lr=1e-4, weight_decay=1e-5)

print(model)

"""### Define functions for train/test"""

def train(model, optimizer, sample):
    model.train()

    criterion = nn.CrossEntropyLoss()

    optimizer.zero_grad()

    img = sample[0].float().to(device)
    label = sample[1].long().to(device)

    pred = model(img)

    num_correct = sum(torch.argmax(pred, dim=1) == label)

    pred_loss = criterion(pred, label)

    pred_loss.backward()

    optimizer.step()

    return pred_loss.item(), num_correct.item()

def test(model, sample):
    model.eval()

    criterion = nn.CrossEntropyLoss()

    with torch.no_grad():
        img = sample[0].float().to(device)
        label = sample[1].long().to(device)

        pred = model(img)
        pred_loss = criterion(pred, label)

        num_correct = sum(torch.argmax(pred, dim=1) == label)

    return pred_loss.item(), num_correct.item()

"""### Run Training"""

max_epoch = 200
tmp_path = './checkpoint.pth'

for epoch in tqdm(range(max_epoch)):
    ###Train Phase

    # Initialize Loss and Accuracy
    train_loss = 0.0
    train_accu = 0.0

    # Iterate over the train_dataloader
    for idx, sample in enumerate(train_dataloader):
        curr_loss, num_correct = train(model, optimizer, sample)
        train_loss += curr_loss / len(train_dataloader)
        train_accu += num_correct / len(train_dataset)

    torch.save(model.state_dict(), 'recent.pth')

    ### Test Phase
    # Initialize Loss and Accuracy
    test_loss = 0.0
    test_accu = 0.0

    # Iterate over the test_dataloader
    for idx, sample in enumerate(test_dataloader):
        curr_loss, num_correct = test(model, sample)
        test_loss += curr_loss / len(test_dataloader)
        test_accu += num_correct / len(test_dataset)


    print('[epoch {}] Loss train: {:.02f} test:{:.02f} / Accu train: {:.02f} test:{:.02f}'.format(epoch+1, train_loss, test_loss, train_accu, test_accu))


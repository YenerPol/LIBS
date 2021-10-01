# -*- coding: utf-8 -*-
"""
Created on Thu Sep  9 21:03:53 2021

@author: gomez
"""

import os
os.getcwd()

# Change the current working directory
os.chdir('C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H')

# first neural network with keras tutorial
from numpy import loadtxt
from keras.models import Sequential
from keras.layers import Dense

# load the dataset
dataset = loadtxt('traindata.csv', delimiter=',')
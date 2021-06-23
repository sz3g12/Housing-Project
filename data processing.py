#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 23 10:37:53 2021

@author: apple
"""

import os
import pandas as pd
import numpy as np
import requests
from pandas import json_normalize
os.chdir('/Users/apple/Desktop/ResearchProject/URA Housing')

import json

def load_n_parse(file):
    # Opening JSON file
    df = pd.read_json(file,  orient = 'records')
    df_flat = json_normalize(df.Result, sep = "-", record_path = 'transaction', meta = ["street", "project", "marketSegment"])
    return df_flat 

data_1 = load_n_parse('response_1.json')
data_2 = load_n_parse('response_2.json')
data_3 = load_n_parse('response_3.json')
data_4 = load_n_parse('response_4.json')

# append all four together and do handling
URA_df = pd.concat([data_1, data_2, data_3, data_4])
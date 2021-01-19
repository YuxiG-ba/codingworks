#!/usr/bin/env python
# coding: utf-8

# # PACKAGES

# In[5]:


import numpy as np
import pandas as pd
import nltk
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
import matplotlib.pyplot as plt
#import string
#punc=string.punctuation


# # DATA READING AND CLEANING

# In[7]:


train = pd.read_csv("train.csv")
test = pd.read_csv("test1.csv")
df.dropna(inplace=True)


# In[8]:


train


# In[9]:


test


# In[10]:


train = train.drop(columns=['day','user_id'])
test = test.drop(columns=['day','user_id'])


# In[11]:


keep_col = ['day','user_id','item_id'] 
df[keep_col]


# In[11]:


X_train = train.drop(["label"], axis = 1)
y_train = (train["label"] >0)*1
X_test = test.drop(["label"], axis =1)
y_test = (test["label"]>0)*1
y_test = y_test.values


# In[12]:


nb_class = len(set(y_train))
id_toTrain =np.array([np.where(y_train ==i)[0] for i in range(nb_class)])
size_max = [len(id_toTrain[i]) for i in range(nb_class)]
print("before resampling")
print(size_max)


# In[ ]:


blc = 150
for i in range(len(size_max)):
    if size_max[i] > blc:
        size_max[i] = int(blc*(np.log10(size_max[i]/blc)+1))
    else:
        size_max[i] = int(blc/(np.log10(blc/size_max[i])+1))

print("after resampling")
print(size_max)


# In[ ]:


from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectFromModel
from sklearn.metrics import classification_report, accuracy_score

clf = RandomForestClassifier(n_estimators=1000, random_state=42)
clf.fit(X_toTrain, y_toTrain)
y_pred = clf.predict(X_test)
print(classification_report(y_test,y_pred))


# In[ ]:


pred = clf.predict_proba(X_test)
ids = np.array(X_test["id"])
pred = np.array(pred[:,1])

results = pd.DataFrame({'id': ids, 'prob': pred}, columns=['id', 'prob'])
results


# In[ ]:


results.to_csv("prediction.csv", index =False)


# In[ ]:





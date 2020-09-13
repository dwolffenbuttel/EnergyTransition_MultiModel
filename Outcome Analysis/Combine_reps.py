#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import pickle


# In[2]:


def open_file(i):
    a_file = open("C:/Users/test/Documents/Results/Neighbourhood_results_#"+ str(i) +".pkl", "rb")
    results = pickle.load(a_file)
    a_file.close()
    return results


# In[3]:


def save_outcome(outcome, i):
    a_file = open("C:/Users/test/Documents/Results/combined_results_#"+ str(i) +".pkl", "wb")
    pickle.dump(outcome, a_file)
    a_file.close()
    print('File number '+ f'{i}\r' + ' saved', end="")


# In[4]:


def add_column_names(data):
    for i in data.columns:
        #data[i] = data[i].astype('|S500')
        data[i] = data[i].astype(float, errors = 'ignore')
    return data.rename(columns = {0:'LT-suitability', 1:'MT-suitability',2:'HT-suitability',3:'Best Primary Source', 4:'Cityheating', 5:'Net-temp',6:'Heat pumps',7:'Gas connections',8:'Average spending',9: 'Year', 10:'Municipality', 11: 'Neighbourhood', 12:'HG contract', 13: 'HP contract', 14: 'Insulation contract', 15:'Dominant system' }).sort_values(['Municipality', 'Neighbourhood','Year']).reset_index(drop = True)


# In[5]:


def combine_replications(results):
    first = True    
    for i in range(100): 
        total = add_column_names(pd.DataFrame(results[1][i][0]))
        indexers = total.iloc[:,[10,11]]
        for j in range(1,4):
            total = total.append(add_column_names(pd.DataFrame(results[1][i][j]))) 
        means = total.groupby(by = total.index).mean()
        categoricals = total[total.columns[total.dtypes == object]][['Best Primary Source','Net-temp','HG contract', 'HP contract', 'Insulation contract', 'Dominant system']].groupby(by = total.index).agg(lambda x: pd.Series.mode(x)[0])#.agg(lambda x:x.value_counts())
        outcome = indexers.merge(means, left_index = True, right_index = True).merge(categoricals, left_index = True, right_index = True).to_numpy()
        outcome = np.expand_dims(outcome, 0)
        if first == True:
            outcome_set = outcome
            first = False
        else:
            outcome_set = np.append(outcome_set, outcome, axis = 0)
        print('Current rep: '+ f'{i}\r', end="")
    return outcome_set


# In[6]:


# number of files
def process_data(i):
    results = open_file(i)
    outcome = combine_replications(results)
    save_outcome(outcome, i)


# In[7]:


# pool = multiprocessing.Pool(12)
# pool.map(process_data, range(0, 15))


# In[ ]:


from multiprocessing import Pool

if __name__ == '__main__':
    with Pool(5) as p:
        p.map(process_data, list(range(0,15)))


# In[ ]:





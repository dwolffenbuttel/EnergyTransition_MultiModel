#!/usr/bin/env python
# coding: utf-8

# In[ ]:


from MultiModelFinal7 import CombinedModel

import os
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.optimize import brentq
import math
from ema_workbench import (RealParameter, ScalarOutcome, Constant, Model)
import pyNetLogo
from datetime import datetime
from ema_workbench import (RealParameter, CategoricalParameter, ArrayOutcome, TimeSeriesOutcome, ema_logging,
                           perform_experiments, MultiprocessingEvaluator)
from ema_workbench import save_results

filepath_model = './model'
filepath_results = os.path.abspath(os.path.join(filepath_model, os.pardir)) + "\\results"


# In[ ]:


multi_model = CombinedModel('UrbanEnergyTransition', wd=filepath_model)
multi_model.vensim_model_file = 'SD_ema2.vpmx'
multi_model.netlogo_model_file = 'ABM_ema.nlogo'


# In[ ]:


start = datetime.now()

# turn on logging
ema_logging.log_to_stderr(ema_logging.INFO)

# instantiate a model

multi_model.uncertainties = [
                             # High level System uncertainties
                             RealParameter('SD_Base Investments Renewable', 0.025, 0.125),
                             RealParameter('SD_Maximum cost reduction', 0.1, 0.5),                             
                             RealParameter('SD_Green Gas investments', 1e8, 4e8), # current expectation = 1.7 (CE Delft)
                             RealParameter('SD_Gas production cost ramp[Natural Gas]',-0.001, 0.001),
                             RealParameter('SD_Gas production cost ramp[Green Gas]',-0.001, 0.001),
                             RealParameter('SD_Grey electricity cost ramp', -0.001,0.001), #minimum is -0.001
                             RealParameter('SD_Renewable Electricity cost multiplier', 0.5, 2),
                             RealParameter('SD_Foreign renewable multiplier',0.5 , 2),
                             # Economy Scenarios
                             RealParameter('SD_Expected Period', 4, 10), 
                             RealParameter('SD_Expected Amplitude', 0.01, 0.05),
                             # People uncertainties
                             RealParameter('ABM_group-behaviour', 0, 1),
                             RealParameter('ABM_Heat-company-ROI', 0.03, 0.15),
                             RealParameter('ABM_Max-income-inv-share', 0.01, 0.15),
                             RealParameter('ABM_Max-capital-inv-share', 0.01, 0.15),
                             # Low level system uncertainties
                             RealParameter('ABM_Relative-construction-capacity', 0.01, 0.1),
                             CategoricalParameter('ABM_heating-grid-cost-scenario', ['"High"', '"Low"']) # Double quotes needed for netlogo
                             ]


multi_model.levers = [RealParameter('ABM_insulation-subsidy', 0, 0.5),
                      RealParameter('ABM_LT-production-subsidy', 0, 0.5),
                      RealParameter('ABM_LT-investment-subsidy', 0, 0.5),
                      RealParameter('ABM_MT-production-subsidy', 0, 0.5),
                      RealParameter('ABM_MT-investment-subsidy', 0, 0.5),
                      RealParameter('SD_NMTU factor', 1, 2),
                      RealParameter('SD_Tax multiplier', 0, 3), 
                      CategoricalParameter('SD_CO2 tax scheme', [0, 1 , 2]),
                      CategoricalParameter('SD_Green gas transition scheme', [0, 1 , 2])
                    ]

 # Specify Municipalties to save neighbourhood data
multi_model.ABMsavelist = ['"Amsterdam"', '"Rotterdam"', '"Arnhem"', '"Zwartewaterland"'] # Double quotes needed for netlogo

multi_model.SD_subscripted_output = [['System Distributions', 'sub_mun-sys'],['Total Electricity use', 'mun_subs']]

                                # subscript options implemented: ['mun_subs','sys_subs','ins_subs','sub_mun-sys']

# FOR ABM: globals only
multi_model.outcomes = [TimeSeriesOutcome('SD_Average Gas Price'),
                        TimeSeriesOutcome('SD_Average Electricity Price'),
                        TimeSeriesOutcome('SD_Average Heat Price'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[Natural Gas]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[Green Gas]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[LT Heating Grid]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[MT Heating Grid]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[HT Heating Grid]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[Air Heat Pump]'),
                        TimeSeriesOutcome('SD_National Energy System Distribution[Ground Heat Pump]'),
                        TimeSeriesOutcome('SD_Cumulative CO2 emmissions'),
                        TimeSeriesOutcome('SD_Percentage Renewable Electricity'),
                        TimeSeriesOutcome('SD_CO2 Tax'),
                        TimeSeriesOutcome('SD_Gas Trade[Natural Gas]'),
                        TimeSeriesOutcome('SD_Gas Trade[Green Gas]'),
                        TimeSeriesOutcome('SD_Electricity Trade'),
                        ArrayOutcome('System Distributions'),
                        ArrayOutcome('Total Electricity use'),
                        ArrayOutcome('Neighbourhood Data'),
                        ArrayOutcome('Municipality Data')
                       ]

multi_model.replications = 4

#replications: 4
#scenarios: 50
#policies: 30

#results = perform_experiments(multi_model, 1, 1)

with MultiprocessingEvaluator(multi_model) as evaluator:
    results = perform_experiments(multi_model, scenarios = 50, policies = 30,
                                 evaluator=evaluator)

end = datetime.now()

print('Total experiments took ' + str(end-start))


# In[ ]:


no_categorical_results = ['SD_Average Gas Price', 'SD_Average Electricity Price', 'SD_Average Heat Price', 'SD_National Energy System Distribution[Natural Gas]', 'SD_National Energy System Distribution[Green Gas]', 'SD_National Energy System Distribution[LT Heating Grid]', 'SD_National Energy System Distribution[MT Heating Grid]', 'SD_National Energy System Distribution[HT Heating Grid]', 'SD_National Energy System Distribution[Air Heat Pump]', 'SD_National Energy System Distribution[Ground Heat Pump]', 'SD_Cumulative CO2 emmissions', 'SD_Percentage Renewable Electricity', 'SD_CO2 Tax', 'SD_Gas Trade[Natural Gas]', 'SD_Gas Trade[Green Gas]', 'SD_Electricity Trade','System Distributions', 'Total Electricity use', 'Municipality Data']


# In[ ]:


results_mean = {k: results[1][k].mean(axis = 1) for k in no_categorical_results}
# Splitting is necessary for allowing to save


# In[ ]:


save_results((results[0],results_mean), filepath_results + '/results_50scen_30_pol_290620.gz.tar')


# In[ ]:


#make batches of 100 scenarios/policies to save neighbourhood data
n = math.ceil(len(results[1]['Neighbourhood Data'])/100)

for i in range (n):
    a_file = open(filepath_results + "/Neighbourhood_results_290620_#" + str(i)+  ".pkl", "wb")
    pickle.dump((results[0],results[1]['Neighbourhood Data'][i*100:i*100+100]), a_file)
    a_file.close()
    


# In[ ]:





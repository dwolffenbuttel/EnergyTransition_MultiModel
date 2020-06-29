#!/usr/bin/env python
# coding: utf-8

# In[4]:


#!/usr/bin/env python
# coding: utf-8

# In[1]:


import math
import os

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.optimize import brentq
from datetime import datetime
import pyNetLogo

from ema_workbench import (RealParameter, ScalarOutcome, Constant, CategoricalParameter,
                           TimeSeriesOutcome, ArrayOutcome, ema_logging,
                           MultiprocessingEvaluator, SequentialEvaluator)
from ema_workbench.em_framework.model import (WorkingDirectoryModel, Replicator, SingleReplication)
from ema_workbench.connectors import vensim, netlogo
from ema_workbench.connectors.vensim import set_value
from ema_workbench.util import EMAError, CaseError
from ema_workbench.util.ema_logging import (get_module_logger, method_logger)

_logger = get_module_logger(__name__)


def get_subscripted_variables(working_directory, x):
    filepath_vensim_vars = os.path.join(working_directory, 'SD', 'vensim_vars',)
    fn = f'SD_output_{x}.csv'
    fp = os.path.join(filepath_vensim_vars, fn)
    sub_vars = pd.read_csv(fp, encoding='unicode_escape').loc[0].reset_index()['index'][1:]
    return sub_vars

def run_partial_SD_model(final_time, output_file):
    initialTime = vensim.get_val('INITIAL TIME')
    finalTime = vensim.get_val('FINAL TIME')
    timeStep = vensim.get_val('TIME STEP')
    savePer = vensim.get_val('SAVEPER')
    
    if savePer > 0:
        timeStep = savePer

    run_length = int((finalTime - initialTime) / timeStep + 1)
    
    vensim.set_value('FINAL TIME', final_time)
    vensim.run_simulation(output_file)

def extract_SD_data(output_vars, model_output):    
    output = pd.DataFrame()
    output['Time'] = vensim.get_data(model_output, 'Time')
    for i in output_vars: 
        # Fix for extrapolating missing_data
        total_time = len(output.Time)
        time_data = len (vensim.get_data(model_output, i))
        if time_data < total_time :
            val = vensim.get_data(model_output, i)
            for j in range (total_time - 1):
                val = np.append(val, val[0])
            output[i] = np.array(val)
        else:
            output[i] = vensim.get_data(model_output, i)
    return output.set_index('Time')

def get_subscript_entries(variables):
    """Takes list of Vensim subscripted output to extract subscript entries"""
    import re
    res = []
    for i in variables:
        m = re.search('START(.+?)END',i.replace('[', 'START').replace(']','END'))
        if m:
            found = m.group(1)
            res.append(found)
    return res

def get_subscripted_data(variable, subscripts, model_output):
    """Function for extracting subscripted data from Vensim. Takes variable name, 
    list (of lists) with subscript names to retrieve, and Vensim outputfile"""
    output = pd.DataFrame()
    output['Time'] = vensim.get_data(model_output, 'Time')
    
    res = []
    l = []

    # Check if subscripts are multi-dimensional
    if type(subscripts[0]) != list:
        subscript_items = subscripts  
    else:
        # For multi-dimensional subscripts, make combination of every subscript
        if len (subscripts) == 2 :
            for group in subscripts:
                l.append(len(group))

            for i in range (l[0]):
                for j in range (l[1]):
                    res.append(f'{subscripts[0][i]},{subscripts[1][j]}')
            subscript_items = res

        if len (subscripts) == 3:
            for group in subscripts:
                l.append(len(group))

            for i in range (l[0]):
                for j in range (l[1]):
                    for k in range (l[2]):
                        res.append(f'{subscripts[0][i]},{subscripts[1][j]},{subscripts[2][k]}')
            subscript_items = res
        if len (subscripts) > 3:
            raise EMAError('4th or higher dimension subscripting not implemented') 
        
    output_vars = []
    for sub in subscript_items:
        output_vars.append(f'{variable}[{sub}]' )  
    
    data = extract_SD_data(output_vars, model_output)
    data.columns = get_subscript_entries(data.columns)
    return data

def save_ABM_output(filepath_netlogo, filepath_vensim):
    systems_output = pd.DataFrame()
    systems_data = pd.read_csv(os.path.join(filepath_netlogo, 'systems.csv'),
                               header=None)
    for i in range (355):
        systems_output = systems_output.append( systems_data[i].str.split(',').apply(pd.Series))
    systems_output = systems_output.rename(columns = {1: 'Municipality', 0:'Year',2:'LT', 3:"MT",
                                                      4: "HT", 5: "AHP", 6 : "GHP", 7: 'Gas'}).set_index('Municipality').astype(float)
    
    for i in ['LT', 'MT', 'HT', "AHP", "GHP", "Gas"]:
        systems_output.reset_index().pivot_table(index = 'Year', columns = 'Municipality' , values = i).to_csv(
            os.path.join(filepath_vensim, 'ABM input', f'systems_{i}.csv'))
    energy_output = pd.DataFrame()
    
    energy_data = pd.read_csv(os.path.join(filepath_netlogo, 'energy-change.csv'), header = None)
    
    for i in range (355):
        energy_output = energy_output.append( energy_data[i].str.split(',').apply(pd.Series))
    energy_output = energy_output.rename(columns = {1: 'Municipality', 0:'Year',2:"Age0to9",
                                                    3:"Age10to19", 4:"Age20to29", 5:"Age30to39",
                                                    6:"Age40to49", 7:"Age50to59", 8:"Age60to69",
                                                    9:"Age70to79", 10:"Age80to89", 11:"Age90to99",
                                                    12:"Age100plus"}).set_index('Municipality').astype(float)
    
    for i in ["Age0to9", "Age10to19", "Age20to29", "Age30to39", "Age40to49", "Age50to59", "Age60to69", "Age70to79", "Age80to89", "Age90to99", "Age100plus"]:
        energy_output.reset_index().pivot_table(index = 'Year', columns = 'Municipality' , values = i).to_csv(
            os.path.join(filepath_vensim, 'ABM input', f'energy-change_{i}.csv'))


def save_ABM_globals(self,ticks):
    results = self.abm_globals
    for variable in self.output_variables:
        if "ABM_" in variable:
            variable = variable.replace('ABM_', '')
            results.loc[ticks,"ABM_"+ variable] = self.netlogo.report(variable)
    return results

def neighbourhood_data_transformation(data):
    oneCol = []
    colLength = len(data.columns)
    for k in range(colLength):
        oneCol.append(data[k])
    combined = pd.concat(oneCol, ignore_index=True)
    combined = combined.str.rsplit(',', expand = True, n = 12)
    indexers = combined[0].str.split(',', expand = True, n = 2).rename(columns = {0:'Year', 1:'Municipality', 2:'Neighbourhood'})
    indexers['Neighbourhood'] = indexers['Neighbourhood'].str.replace('"', '')
    combined = combined.iloc[:,1:].merge(indexers, left_index = True, right_index = True)
    contract_cols = combined.iloc[:,-6:-3].rename(columns = {10:'HG',11:'HP',12:'Insulation'})
    contract_cols.values.sort()
    for i in contract_cols.columns:
        contract_cols [i] = contract_cols[i].str.replace('[','').str.replace(']','').str.replace(str(i) + ' ', '')
    combined = combined.merge(contract_cols, left_index = True, right_index = True).drop([10,11,12],axis = 1)
    combined = combined.rename(columns = {1:'LT-suitability', 2:'MT-suitability',3:'HT-suitability',4:'Primary Source HG',5:'Cityheating', 6:'Net-temp',7:'Heat pumps',8:'Gas connections',9:'Average spending'})
    for i in combined:
        combined [i] = pd.to_numeric(combined[i], errors = 'ignore')
    combined['main heating system'] = combined[['Cityheating', 'Heat pumps', 'Gas connections']].idxmax(axis=1)

    #combined = combined.set_index(['Municipality','Neighbourhood', 'Year']).sort_index()
    return combined

def mun_data_transformation(data):
    oneCol = []
    colLength = len(data.columns)
    for k in range(colLength):
        oneCol.append(data[k])
    combined = pd.concat(oneCol, ignore_index=True)
    combined = combined.str.rsplit(',', expand = True, n = 3)
    combined = combined.pivot(index = 1, columns = 0, values = 2)
    
    return combined

class BaseCombinedModel(WorkingDirectoryModel):

    @method_logger(__name__)
    def __init__(self, name, wd=None, vensim_model_file=None,
                 netlogo_model_file=None):
        super(BaseCombinedModel, self).__init__(name, wd=wd)
        
        self.vensim_model_file = vensim_model_file
        self.netlogo_model_file = netlogo_model_file
        #self.netlogo_model_file = 'ABM_ema.nlogo'
        self.vensim_model_dir = 'SD'
        self.netlogo_model_dir = 'ABM'
        self.instantiated = False
        
    @method_logger(__name__)
    def model_init(self, policy):
        super(BaseCombinedModel, self).model_init(policy)

        # instantiate netlogo model
        if self.instantiated == False:
            self.vensim_model_file = os.path.join(self.working_directory, self.vensim_model_dir,
                                                  self.vensim_model_file)
            self.vensim_model = vensim.load_model(self.vensim_model_file)
            self.vensim_model_output = os.path.join(self.working_directory, self.vensim_model_dir,
                                                    "Energy_transition_municipality_ema_final.vdfx")


            initialTime = vensim.get_val('INITIAL TIME')
            finalTime = vensim.get_val('FINAL TIME')
            timeStep = vensim.get_val('TIME STEP')
            savePer = vensim.get_val('SAVEPER')
        
            if savePer > 0:
                timeStep = savePer
    
            self.run_length = int((finalTime - initialTime) / timeStep + 1)

            vensim.be_quiet()
        
            self.netlogo = pyNetLogo.core.NetLogoLink()
            self.netlogo.load_model(os.path.join(self.working_directory, self.netlogo_model_dir,
                                                 self.netlogo_model_file))

            #self.netlogo.command('set View-Municipality "Pekela"')
            self.netlogo.command('set View-Municipality "All"')
            self.netlogo.command('set EMA-controls True')

            self.netlogo.command('Setup')

            self.instantiated = True
            
            self.mun_subs = get_subscript_entries(get_subscripted_variables(self.working_directory, 
                                                                            'construction').to_list())
            self.sys_subs = get_subscript_entries(get_subscripted_variables(self.working_directory, 
                                                                            'systemcost-reduction').to_list())
            self.ins_subs = get_subscript_entries(get_subscripted_variables(self.working_directory, 
                                                                            'insulationcost-reduction').to_list())
            self.sub_mun_sys = [self.mun_subs, self.sys_subs]
            self.subscript_dict = {'mun_subs':self.mun_subs,'sys_subs':self.sys_subs,
                                   'ins_subs':self.ins_subs,'sub_mun-sys':self.sub_mun_sys}
        
        self._ts_output_variables = None

    @method_logger(__name__)
    def run_experiment(self, experiment):
        #netlogo = pyNetLogo.core.NetLogoLink(netlogo_home = 'C:/Program Files/Netlogo 6.1.1', netlogo_version = '6.1')
        time = datetime.now()
        full_run = True
        filepath_netlogo = os.path.join(self.working_directory, 'ABM', 'Input')
   
        ticks = 2020
        end = 2060
        
        if end != 2060:
            self.run_length = end - ticks + 1
        
        # VENSIM experiments
        for key, value in experiment.items():
            if "SD_" in key:
                key = key.replace("SD_","")
                set_value(key, value)
        
        #NETLOGO experiments

        self.netlogo.command('reset-initials')
        
        for key, value in experiment.items():
            if "ABM_" in key:
                key = key.replace("ABM_","")
                self.netlogo.command("set " + key + " " + str(value))
        
        if len (self.ABMsavelist) > 0:
            str_command = "set mun-save-list (turtle-set "
            for i in self.ABMsavelist:
                str_command += "initial-neighborhoods with [my-municipality = " + i + " ] "
            str_command += ")"
            self.netlogo.command(str_command)
        self.netlogo.command('import_constants')  
        # setup datasheet for recovering globals from ABM
        time_col = []
        for i in range (ticks, end + 1):
            time_col = np.append(time_col, i)
        self.abm_globals = pd.DataFrame(time_col).set_index(0)
                               
        while ticks <= end:
            
            # RUN SD until TICKS
            run_partial_SD_model(ticks, self.vensim_model_output)
            #get globals from SD
            output_vars = ['Average Gas Price', 'Average Heat Price', 'Average Electricity Price', 'Relocation mobility factor']
            globals_SD_output = extract_SD_data(output_vars, self.vensim_model_output)
        
            # Ask netlogo to update globals
            self.netlogo.command('set gas-price ' + str(globals_SD_output.loc[ticks]['Average Gas Price']))
            self.netlogo.command('set electricity-price ' + str(globals_SD_output.loc[ticks]['Average Electricity Price']))
            self.netlogo.command('set heat-price ' + str(globals_SD_output.loc[ticks]['Average Heat Price']))
            self.netlogo.command('set relocation-mobility ' + str(globals_SD_output.loc[ticks]['Relocation mobility factor'] ))
            
            # Retrieve subscripted output from Vensim and save to be updated in NetLogo
            get_subscripted_data('Demolition per Municipality', self.mun_subs,
                                 self.vensim_model_output).to_csv(os.path.join(filepath_netlogo,
                                                                               'SD_output_demolition.csv'))
            get_subscripted_data('New Construction', self.mun_subs,
                                 self.vensim_model_output).to_csv(os.path.join(filepath_netlogo,
                                                                               'SD_output_construction.csv'))
            get_subscripted_data('Average Heating Energy use new construction',
                                 self.mun_subs, self.vensim_model_output).to_csv(os.path.join(filepath_netlogo,
                                                                                              'SD_output_e-use_new-construction.csv'))
            get_subscripted_data('System cost reduction', self.sys_subs,
                                 self.vensim_model_output).to_csv(os.path.join(filepath_netlogo,
                                                                               'SD_output_systemcost-reduction.csv'))
            get_subscripted_data('Insulation cost reduction', self.ins_subs,
                                 self.vensim_model_output).to_csv(os.path.join(filepath_netlogo,
                                                                               'SD_output_insulationcost-reduction.csv'))
            
            # Ask netlogo to update all subscipted vars from csv's. Unfortunately directly asking netlogo to update
            # the datafiles doesn't work due to the need for strings in strings. 
            self.netlogo.command('update-SD-data-import')
        
            #RUN once
            try:
                self.netlogo.command('repeat 1 [go]')
            except:
                raise CaseError("Netlogo run not completed at " + str(ticks), experiment)
            # Save Model Output
           # systems_dict = {}

            save_ABM_globals(self, ticks)
            
            if full_run == True:
                # Do not overwrite ABM results of full run with partial run
                save_ABM_output(os.path.join(self.working_directory, 'ABM'),
                                os.path.join(self.working_directory, 'SD'))
        
            ticks += 1
        
        SD_results = self.get_sd_results(experiment)
        ABM_results = self.get_abm_results()
       
        results = SD_results
        results.update(ABM_results)
        return results


    def get_sd_results(self, experiment):        
        results = {}
        error = False
        for variable in self.output_variables:
               
            if "SD_" in variable:
                variable = variable.replace('SD_', '')
                res = vensim.get_data(self.vensim_model_output, variable)
                result, er = self.check_data(np.asarray(res))
                error = error or er
                variable = "SD_" + variable
                results[variable] = result
        
        for variable in self.SD_subscripted_output:
            sub_res = get_subscripted_data(variable[0],self.subscript_dict[variable[1]],self.vensim_model_output)
            results[variable[0]] = sub_res.transpose().to_numpy().squeeze() # reduce dimension
            
        if error:
            raise CaseError("run not completed", experiment)
            
        return results
    
    
    def get_abm_results(self):
        dict_output = {}
        for col in self.abm_globals.columns:
            dict_output[col] = np.array(self.abm_globals[col])
            
        if len (self.ABMsavelist) > 0:    
            dict_output['Neighbourhood Data'] = neighbourhood_data_transformation(pd.read_csv(os.path.join(self.working_directory, 'ABM/neighbourhood-model-outcome.csv'),header = None).transpose()).to_numpy().squeeze()
            dict_output['Municipality Data'] = mun_data_transformation(pd.read_csv(os.path.join(self.working_directory, 'ABM/municipality-model-outcome.csv'),header = None).transpose()).to_numpy().squeeze().astype(float)
        return dict_output


    def check_data(self, result):
        error = False
        if result.shape[0] != self.run_length:
            data = np.empty((self.run_length))
            data[:] = np.NAN
            data[0:result.shape[0]] = result
            result = data
            error = True
        return result, error

            
class CombinedModel(Replicator, BaseCombinedModel):
                pass


# In[ ]:


# # from Multi_model_ET import MultiModelFinal
# #from MultiModelFinal5 import CombinedModel

# import os
# import pandas as pd
# import numpy as np
# import seaborn as sns
# import matplotlib.pyplot as plt
# from scipy.optimize import brentq
# import math
# from ema_workbench import (RealParameter, ScalarOutcome, Constant, Model)
# import pyNetLogo
# from datetime import datetime
# from ema_workbench import (RealParameter, CategoricalParameter, ArrayOutcome, TimeSeriesOutcome, ema_logging,
#                            perform_experiments, MultiprocessingEvaluator)
# from ema_workbench import save_results


# In[ ]:


# filepath_model = './model'
# filepath_results = os.path.abspath(os.path.join(filepath_model, os.pardir)) + "\\results"


# In[ ]:


# multi_model = CombinedModel('UrbanEnergyTransition', wd=filepath_model)
# multi_model.vensim_model_file = 'SD_ema.vpmx'
# multi_model.netlogo_model_file = 'ABM_ema.nlogo'


# In[ ]:


# start = datetime.now()

# # turn on logging
# ema_logging.log_to_stderr(ema_logging.INFO)

# # instantiate a model

# multi_model.uncertainties = [RealParameter('SD_Base Investments Renewable', 0.025, 0.125),
#                              RealParameter('SD_Maximum cost reduction', 0.1, 0.5),
#                              RealParameter('ABM_group-behaviour', 0, 1),
#                              RealParameter('ABM_Heat-company-ROI', 0.04, 0.15),
#                              RealParameter('ABM_Max-income-inv-share', 0.05, 0.15),
#                              RealParameter('ABM_Max-capital-inv-share', 0.05, 0.15)
#                              ]


# multi_model.levers = [RealParameter('ABM_insulation-subsidy', 0, 0.5),
#                       RealParameter('ABM_LT-production-subsidy', 0, 0.5),
#                       RealParameter('ABM_LT-investment-subsidy', 0, 0.5),
#                       RealParameter('ABM_MT-production-subsidy', 0, 0.5),
#                       RealParameter('ABM_MT-investment-subsidy', 0, 0.5),
#                       RealParameter('SD_"NMTU-factor"', 1, 2),
#                       RealParameter('SD_Tax multiplier', 0, 3),
#                       CategoricalParameter('SD_"CO2-tax-scheme"', [0, 1 , 2])
#                      ]

#  # Specify Municipalties to save neighbourhood data
# multi_model.ABMsavelist = ['"Amsterdam"', '"Rotterdam"', '"Hollands Kroon"', '"Nijmegen"'] # Double quotes needed for netlogo

# multi_model.SD_subscripted_output = [['System Distributions', 'sub_mun-sys']]

# # FOR ABM: globals only
# multi_model.outcomes = [TimeSeriesOutcome('SD_Average Gas Price'),
#                         TimeSeriesOutcome('SD_Average Electricity Price'),
#                         TimeSeriesOutcome('SD_Average Heat Price'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[Natural Gas]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[Green Gas]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[LT Heating Grid]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[MT Heating Grid]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[HT Heating Grid]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[Air Heat Pump]'),
#                         TimeSeriesOutcome('SD_National Energy System Distribution[Ground Heat Pump]'),
#                         TimeSeriesOutcome('SD_"Cumulative CO2-emmissions"'),
#                         TimeSeriesOutcome('SD_Percentage Renewable Electricity'),
#                         TimeSeriesOutcome('SD_"CO2-Tax"'),
#                         ArrayOutcome('System Distributions'),
#                         ArrayOutcome('Neighbourhood Data')
#                        ]
# #multi_model._replications = 6
# #multi_model.nrreplications = 5
# #multi_model.replications = 2

# #results = perform_experiments(multi_model, 1, 1)

# with MultiprocessingEvaluator(multi_model, n_processes=2) as evaluator:
#     results = perform_experiments(multi_model, 2, 2,
#                                  evaluator=evaluator)

# end = datetime.now()

# print('Total experiments took ' + str(end-start))


# In[ ]:


# import pickle
# a_file = open(filepath_results + "/test_reps.pkl", "wb")
# pickle.dump(results, a_file)
# a_file.close()


# In[ ]:





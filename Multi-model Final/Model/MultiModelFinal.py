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

from ema_workbench import (RealParameter, ScalarOutcome, Model, Constant, TimeSeriesOutcome ,ArrayOutcome, ema_logging,
                           perform_experiments, MultiprocessingEvaluator)
from ema_workbench.em_framework import model
from ema_workbench.em_framework.model import (WorkingDirectoryModel, SingleReplication)
from ema_workbench.connectors import vensim, netlogo
from ema_workbench.connectors.vensim import set_value
from ema_workbench.em_framework import model
from ema_workbench.util import get_module_logger, EMAError, CaseError, method_logger

_logger = get_module_logger(__name__)


# In[13]:


filepath_vensim = r'SD/'
filepath_vensim_vars = r'SD/vensim_vars/'
filepath_netlogo = r'ABM/'


# In[3]:


#Works only with 64Bit Pyton and Vensim and NetLogo

def mun_subscripted_variables(x):
    sub_vars =pd.read_csv(filepath_vensim_vars + 'SD_output_' + x + '.csv', encoding = 'unicode_escape').loc[0].reset_index()['index'][1:]
    return sub_vars

def mun_subscripted_outcomes(y):
    x = mun_subscripted_variables(y)
    outcomes = []
    for m in x:
        outcomes.append(TimeSeriesOutcome(m))
    return outcomes
  
def run_partial_SD_model(final_time, self):

    model_file = self.vensim_file
    
    initialTime = vensim.get_val('INITIAL TIME')
    finalTime = vensim.get_val('FINAL TIME')
    timeStep = vensim.get_val('TIME STEP')
    savePer = vensim.get_val('SAVEPER')
    
    if savePer > 0:
        timeStep = savePer

    self.run_length = int((finalTime - initialTime) / timeStep + 2)
    #self.run_length = int((finalTime - initialTime) / timeStep + 1)
    
    vensim.set_value('FINAL TIME', final_time)
    vensim.run_simulation(model_file)

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

def get_system_cost_reduction(model_output):
    data = extract_SD_data(mun_subscripted_variables('systemcost-reduction').to_list(),model_output)
    data.columns = data.columns.str.replace('System cost reduction', '').str.replace('[','').str.replace(']','').str.replace('"', '')
    return data

def get_insulation_cost_reduction(model_output):
    data = extract_SD_data(mun_subscripted_variables('insulationcost-reduction').to_list(),model_output) 
    data.columns = data.columns.str.replace('Insulation cost reduction', '').str.replace('[','').str.replace(']','').str.replace('"', '')
    return data

def get_new_construction(model_output):
    data = extract_SD_data(mun_subscripted_variables('construction').to_list(),model_output) 
    data.columns = data.columns.str.replace('New Construction', '').str.replace('[','').str.replace(']','').str.replace('"', '')
    return data

def get_demolition(model_output):
    data = extract_SD_data(mun_subscripted_variables('demolition').to_list(),model_output) 
    data.columns = data.columns.str.replace('Demolition per municipality', '').str.replace('[','').str.replace(']','').str.replace('"', '')
    return data

def get_new_energy_use(model_output):
    data = extract_SD_data(mun_subscripted_variables('e-use_new-construction').to_list(),model_output) 
    data.columns = data.columns.str.replace('Average Heating Energy use new construction', '').str.replace('[','').str.replace(']','').str.replace('"', '')
    return data

def save_ABM_output():
    systems_output = pd.DataFrame()
    systems_data = pd.read_csv(filepath_netlogo + 'systems.csv',header = None)
    for i in range (355):
        systems_output = systems_output.append( systems_data[i].str.split(',').apply(pd.Series))
    systems_output = systems_output.rename(columns = {1: 'Municipality', 0:'Year',2:'LT', 3:"MT", 4: "HT", 5: "AHP", 6 : "GHP", 7: 'Gas'}).set_index('Municipality').astype(float)
    
    for i in ['LT', 'MT', 'HT', "AHP", "GHP", "Gas"]:
        systems_output.reset_index().pivot_table(index = 'Year', columns = 'Municipality' , values = i).to_csv(filepath_vensim + 'ABM input/systems_' + i + '.csv')
    energy_output = pd.DataFrame()
    
    energy_data = pd.read_csv(filepath_netlogo + 'energy-change.csv',header = None)
    
    for i in range (355):
        energy_output = energy_output.append( energy_data[i].str.split(',').apply(pd.Series))
    energy_output = energy_output.rename(columns = {1: 'Municipality', 0:'Year',2:"Age0to9", 3:"Age10to19", 4:"Age20to29", 5:"Age30to39", 6:"Age40to49", 7:"Age50to59", 8:"Age60to69", 9:"Age70to79", 10:"Age80to89", 11:"Age90to99", 12:"Age100plus"}).set_index('Municipality').astype(float)
    
    for i in ["Age0to9", "Age10to19", "Age20to29", "Age30to39", "Age40to49", "Age50to59", "Age60to69", "Age70to79", "Age80to89", "Age90to99", "Age100plus"]:
        energy_output.reset_index().pivot_table(index = 'Year', columns = 'Municipality' , values = i).to_csv(filepath_vensim + 'ABM input/energy-change_' + i + '.csv')

def save_ABM_globals(self,ticks):
    results = self.abm_globals
    for variable in self.output_variables:
        if "ABM_" in variable:
            variable = variable.replace('ABM_', '')
            results.loc[ticks,"ABM_"+ variable] = self.netlogo.report(variable)
    return results


def check_data(result, self):
        error = False
        if result.shape[0] != self.run_length:
            data = np.empty((self.run_length))
            data[:] = np.NAN
            data[0:result.shape[0]] = result
            result = data
            error = True
        return result, error
        
def get_sd_results(self, experiment):        
    results = {}
    error = False
    for variable in self.output_variables:
           
        if "SD_" in variable:
            variable = variable.replace('SD_', '')
            res = vensim.get_data(self.vensim_model_output, variable)
            result, er = check_data(np.asarray(res),self)
            error = error or er
            variable = "SD_" + variable
            results[variable] = result
            
                
    if error:
        raise CaseError("run not completed", experiment)
        
    return results

def get_abm_results(self):
    dict_output = {}
    for col in multi_model.abm_globals.columns:
        dict_output[col] = np.array(multi_model.abm_globals[col])
    
    return dict_output
    


# In[4]:


class BaseCombinedModel(WorkingDirectoryModel):

    @method_logger(__name__)
    def __init__(self, name, wd=None, vensim_model_file=None, netlogo_model_file=None):
        super(BaseCombinedModel, self).__init__(name, wd=wd)
        
        self.vensim_model_file = vensim_model_file
        self.netlogo_model_file = netlogo_model_file
        
    @method_logger(__name__)
    def model_init(self, policy):
        self.vensim_model = vensim.load_model(os.path.join(self.working_directory, self.vensim_model_file))
        self.vensim_model_output = os.path.join(self.working_directory, "Energy_transition_municipality_ema_final.vdfx")
        vensim.be_quiet()
        
        # instantiate netlogo model 
        self.netlogo = pyNetLogo.core.NetLogoLink(netlogo_home = 'C:/Program Files/Netlogo 6.1.1', netlogo_version = '6.1')
        self.netlogo.load_model(os.path.join(self.working_directory, "ABM_ema.nlogo"))

        full_run = True
        if full_run == True:
            self.netlogo.command('set View-Municipality "All"')
        self.netlogo.command('set EMA-controls True')
        self.netlogo.command('Setup')
        self._ts_output_variables = None

    @method_logger(__name__)
    def run_experiment(self, experiment):
        #netlogo = pyNetLogo.core.NetLogoLink(netlogo_home = 'C:/Program Files/Netlogo 6.1.1', netlogo_version = '6.1')
        
        full_run = True
        
        start = datetime.now()
    
        ticks = 2020
        end = 2060
        
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
        
        # setup datasheet for recovering globals from ABM
        time_col = []
        for i in range (ticks, end + 1):
            time_col = np.append(time_col, i)
        self.abm_globals = pd.DataFrame(time_col).set_index(0)

        while ticks <= end:
            
            # RUN SD until TICKS
            run_partial_SD_model(ticks, self)
            #get globals from SD
            output_vars = ['Average Gas Price', 'Average Heat Price', 'Average Electricity Price', 'Relocation mobility factor']
            globals_SD_output = extract_SD_data(output_vars, self.vensim_model_output)
        
            # Ask netlogo to update globals
            self.netlogo.command('set gas-price ' + str(globals_SD_output.loc[ticks]['Average Gas Price']))
            self.netlogo.command('set electricity-price ' + str(globals_SD_output.loc[ticks]['Average Electricity Price']))
            self.netlogo.command('set heat-price ' + str(globals_SD_output.loc[ticks]['Average Heat Price']))
            self.netlogo.command('set relocation-mobility ' + str(globals_SD_output.loc[ticks]['Relocation mobility factor'] ))
        
            # Retrieve subscripted output from Vensim and save to be updated in NetLogo
            get_demolition(self.vensim_model_output).to_csv(filepath_netlogo + 'Input/SD_output_demolition.csv')
            get_new_construction(self.vensim_model_output).to_csv(filepath_netlogo + 'Input/SD_output_construction.csv')
            get_new_energy_use(self.vensim_model_output).to_csv(filepath_netlogo + 'Input/SD_output_e-use_new-construction.csv')
            get_system_cost_reduction(self.vensim_model_output).to_csv(filepath_netlogo + 'Input/SD_output_systemcost-reduction.csv')
            get_insulation_cost_reduction(self.vensim_model_output).to_csv(filepath_netlogo + 'Input/SD_output_insulationcost-reduction.csv')
            
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

            save_ABM_globals(self,ticks)
            
            if full_run == True:
                # Do not overwrite ABM results of full run with partial run
                save_ABM_output()
        
            print("Current simulation year: " + f'{ticks}. ' + "Time elapsed: " + f'{datetime.now()-start}.\r' , end = "")

            ticks += 1
        
        SD_results = get_sd_results(self, experiment)
        ABM_results = get_abm_results(self)
       
        results = SD_results
        results.update(ABM_results)
        
        end = datetime.now()
        print ('\nTotal run took ' , end-start )

        return results
            
class CombinedModel(SingleReplication, BaseCombinedModel):
                pass


# In[16]:


os.path.abspath(os.path.join(filepath_vensim, os.pardir))


# In[ ]:





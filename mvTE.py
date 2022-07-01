# Import classes
from idtxl.multivariate_te import MultivariateTE
from idtxl.data import Data
from idtxl.visualise_graph import plot_network
from idtxl.visualise_graph import plot_network_comparison
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import pickle

dat=pd.read_csv('raw.csv')

d_anal=dat.drop('Dates',axis=1)
d_anal=d_anal.transpose().to_numpy()
dat_anal=Data(d_anal,dim_order='ps',normalise=False)

#  Initialise analysis object and define settings
network_analysis = MultivariateTE()
settings = {'cmi_estimator': 'JidtGaussianCMI',
            'max_lag_sources': 50,
           'min_lag_sources': 0,
          'n_perm_max_stat':1000}

# Run analysis
results = network_analysis.analyse_network(settings=settings, data=dat_anal)
# d) Plot inferred network to console and via matplotlib
results.print_edge_list(weights='max_te_lag', fdr=False)
full=plot_network(results=results, weights='max_te_lag', fdr=False)
plt.savefig("full-network.png")

# save results
import pickle
with open('full_network.pickle','wb') as f:
    pickle.dump(results,f)
    
    
# Define subperiod data
dat['date'] = pd.to_datetime(dat['Dates'],format=True)
mask = (dat['date'] >= '2020-3-23') & (dat['date'] <= '2020-12-31')
dat_ce=dat[mask]

d_anal=dat_ce.drop(['Dates','date'],axis=1)
d_anal=d_anal.transpose().to_numpy()
dat_anal=Data(d_anal,dim_order='ps',normalise=False)
#  Initialise analysis object and define settings
network_analysis = MultivariateTE()
settings = {'cmi_estimator': 'JidtGaussianCMI',
            'max_lag_sources': 50,
           'min_lag_sources': 0,
          'n_perm_max_stat':1000}

# Run analysis
results_ce = network_analysis.analyse_network(settings=settings, data=dat_anal)

# save results
import pickle
with open('ce-network.pickle','wb') as f:
    pickle.dump(results_ce,f)

# visualise
results_ce.print_edge_list(weights='max_te_lag', fdr=False)
plot_network(results=results_ce, weights='max_te_lag', fdr=False)
plt.savefig("ce-network.png")

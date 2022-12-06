
# Run all scripts in the appropriate order

# Reproduction experiment
source('scripts/rep_01_filter_data.R')
source('scripts/rep_02_decision_reproduction_plot.R')
rmarkdown::render('scripts/rep_03_ordinal_modelling_decision.Rmd')
rmarkdown::render('scripts/rep_03_ordinal_modelling_reproduction.Rmd')
source('scripts/rep_04_ordinal_models_bf_plots.R')

# DDM experiments
source('scripts/ddm_01_filter_data.R')
source('scripts/ddm_02_prepare_data_for_hddm.R')
source('scripts/ddm_03_main_plot.R')
source('scripts/ddm_04_arrows_plot.R')

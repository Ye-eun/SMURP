# SMURP

SMURP is R based traffic prediction model.


### Run SMURP ###
- Change BaseDir from GlobalParameters.R
- Run Preparation
- Run IncidentThresholdDetermination.R
- Set quantile, max.std, and min.std from SMURP_main.R

### Incident DB generation ###
- Run CTM_calibration.R to get CTM parameters for specific day of week
- Set c.vf, c.qmax, c.w, and c.kjam at IncidentGeneration.R
- Run IncidentGeneration.R

### Place Incident DB ###
- copy incidentDB into SMURP folder

### Analysis ###
- Analysis.R
# Foster Heatwave

Below is the metadata for the experimental data files associated with 'Heatwave eliminates the benefit of evolving under high temperature in an experimental model system' by Taresa J. Foster, Sydney J. Chong-King and Tess N. Grainger. Published in The American Naturalist. 

#### **Study description**

In this study, we took flour beetles (Tribolium castaneum) that had evolved for over 4 years at 25, 30, or 35°C and exposed them to either a five-day 42°C heatwave or control conditions. We then assessed the effects of historical temperature and heatwave exposure on population sizes (tracked for 18 weeks), body size, and fecundity. 

#### **Analytical workflow**

The CSV files (described below) contain the raw data used in the analysis. The three R scripts (popsizeheatwave.R, traits.R, and bodysizeexperiment.R) represent the three parts of our analysis. These scripts import the data files, clean the data, fit the statistical models, and produce the figures and statistical summaries reported in the manuscript. To reproduce the analyses, open the RStudio project (Heatwave) and run the three R scripts from beginning to end. Scripts do not need to be run in any particular order. The scripts read the input files from the data/ folder and save their outputs in the figures/ folder.

#### **Software environment**

All analyses were conducted in R version 4.5.1 (R Core Team, 2025) using RStudio Desktop 2024.12.1.

Packages: ggplot2 (version 3.5.2), cowplot (version 1.1.3), dplyr (version 2.5.0), car (version 3.1-3), tidyr (version 1.3.1), emmeans (version 1.11.2), lme4 (version 1.1-37)


#### **heatwavepopulation.csv**

\- `weeks_since_heatwave`: weeks elapsed since the end of the heatwave at the time of data collection (0, 2, 6, 12, or 18)

\- `alive`: number of live beetles counted

#### **preheatwavebodysize.csv**

\- `adapted_temp`: the temperature in °C under which beetles evolved for 49 months (25, 30, or 35)

\- `sex`: female (f) or male (m)

\- `replicate`: replicate individual beetle (1-10 for each `adapted_temp` and `sex` combination)

\- `weight`: dry weight of beetle in grams

#### **postheatwavebodysize.csv**

\- `heatwave`: the presence (1) or absence (0) of a 5 day heatwave of 42°C

\- `adapted_temp`: the temperature in °C under which beetles evolved for 49 months (25, 30, or 35)

\- `population_id`: replicate population (1-10 for each combination of `heatwave` and `adapted_temp`)

\- `sex`: female (f) or male (m)

\- `replicate`: replicate individual beetle (1-5 for each `population_id` and `sex` combination)

\- `weight`: dry weight of beetle in grams

#### **heatwavefecundity.csv**

\- `heatwave`: the presence (1) or absence (0) of a 5 day heatwave of 42°C

\- `adapted_temp`: the temperature in °C under which beetles evolved for 49 months (25, 30, or 35)

\- `population_id`: replicate population (1-10 for each combination of `heatwave` and `adapted_temp`)

\- `replicate`: replicate individual female beetle (1-3 for each `population_id`)

\- `egg_count`: number of eggs laid by each female beetle in 48 hours

\- `notes`: fecundity tubes that had no beetle (nb) or a dead beetle (db) at the time of data collection (NA for tubes with living beetles)

#### **bodysizeexperiment.csv**

\- `sex`: female (f) or male (m)

\- `size`: size category according to visual assessment (a, b, c, d, or e from smallest to largest)

\- `replicate`: replicate individual beetle (1-6 for each `size` and `sex` combination)

\- `survival_twoweeks`: the survival (s) or death (d) of beetles two weeks after a heatwave 

\- `weight`: dry weight of beetle in grams
# PyRate Tutorial \#2

#### Daniele Silvestro – Jan 2017
***  
Useful links:  
[PyRate code](https://github.com/dsilvestro/PyRate)  
[PyRate wiki](https://github.com/dsilvestro/PyRate/wiki)  
[Paleobiology Database](https://paleobiodb.org)  
[Tracer](http://tree.bio.ed.ac.uk/software/tracer/)
***



## Birth-death models with time-continuous correlates
This tutorial describes how to analyze data under birth-death models in which rates vary through time through linear or exponential correlations with a time-continuous variable. Time continuous variables may include a clade's own diversity (diversity dependence) or e.g. paleo-environmental variables such as temperature or sea level. Birth-death models with time-continuous correlates are implemented in the program "PyRateContinuous.py".

#### Generate input file for PyRateContinuous
The program does not model preservation and assumes that the times of origination and extinction of each lineage are known or have been estimated, typically in a previous PyRate analysis. Thus, the input file for PyRateContinuous.py is a simple table with the times of origination and extinction of each lineage. The table is formatted as tab-separated text file, with the first line containing column headers followed by one row for each species. Each row contains 4 columns: the first column indicates the clade assignment of species, this is only useful when using [MCDD models](https://github.com/dsilvestro/PyRate/wiki#pyratemcddpy-requires-library-pyrate_lib) and should be filled with 0s for all other analyses. The second column indicates a species numeric identifier (this values are arbitrary and only used for reference). Finally the third and fourth column contain the time of origin and extinction of each species, respectively.  

**The input files for PyRateContinuous can be generated from the _mcmc.log files of a previous PyRate analysis using the command `-ginput`.** For instance if in a previous analysis using PyRate you generated an output file named "Canidae_1_G_mcmc.log", this can be used to extract the estimated times of origination and extinction of each species using:  

`python PyRate.py -ginput .../Canidae_1_G_mcmc.log -b 100`  

where the command `-b 100` indicates that the first 100 samples should be discarded as burnin. This command generates 3 output files, which are saved in the same directory as the "Canidae_1_G_mcmc.log" file:  
  
1. A text file containing the **estimated times of origination and extinction** that will be used as input file in PyRateContinuous (e.g. "Canidae_1_G_se_est.txt")
2. A PDF file plotting the duration of each lineage and the diversity trajectory of the clade (**lineage through time plot**)
3. The R script generating the PDF file above.


#### Run a diversity-dependent birth-death model
In diversity dependence models, origination and extinction rates may correlate linearly or exponentially to the clade's own sampled (range-through) diversity. To run an analysis with a diversity dependent birth-death model you can launch PyRateContinuous providing the input data (`-d` flag) and adding the flag `-DD`:  

`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -DD`

the program implements two models of of diversity dependence defined by the flag `-m`: an **exponential model** (`-m 0`) in which speciation and extinction rates are exponential functions of the clade's diversity and a **linear model** (`-m 1`) in which speciation and extinction rates linearly correlate with diversity.  

`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -DD -m 0`

For the purpose of model testing, you can also set `-m -1` which runs a null model in which speciation and extinction rates are constant and independent of diversity.  

As in standard PyRate analyses the number of MCMC iterations and sampling frequencies can be set using the flags `-n` and `-s`, respectively.  

		Note that PyRateContinuous does not estimate times of origination and extinction nor preservation rates. This means that the number of parameters to be estimated is much smaller than in a standard PyRate analysis. Thus, setting the number of MCMC iterations between 100,000 and 1 million, will be sufficient for most data sets.

#### Output file
PyRateContinuous generate a single output file with the posterior samples of all parameters. The estimated **diversity dependence parameters** are logged to the output log file as *Gl* and *Gm* for speciation and extinction, respectively. When these parameters are significantly different from 0 (based on their 95% HPD) we consider the correlation as significantly positive or negative (depending on whether *G* >> 0 or *G* << 0). The **baseline speciation and extinction rates** (indicated by *L0* and *M0* in the log file) represent the esitmated speciation and extinction rates at the present.  

The log file can be opened in Tracer to check if convergence has been reached and inspect the mean and 95% HPDs of the parameters of interest. 

#### Plot speciation and extinction rates through time
PyRateContinuous can be used to plot marginal speciation and extinction rates through time based on the estimated baseline rates and diversity dependence parameters. To generate an RTT plot you can type:

`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -DD -m 0 -b 100 -plot .../Canidae_1_G_se_est_DD_0_exp.log -b 200`
 
This will generate an R script and a PDF file with the RTT plots showing speciation, extinction rates through time. The command `-b 200` specifies that the first 200 samples are discarded as burnin. 


#### Correlation with paleo-temperature
You can fit birth-death models where the speciation and extinction rates are changing through time as a linear or exponential function of a time-continuous variable, such as a proxy for paleo-temperature. The variable values should be provided in a simple tab-separated text file with a header (first row) and two columns indicating times and variable values (an example is provided in `PyRate-master/example_files/temperature.txt`).   

To run an analysis with temperature-dependent speciation and extinction rates you should use the command `-c` to provide the text file containing the variable:
 
`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -m 0 -c temperature.txt`


As with the diversity dependent model (see above) the flag `-m` is used to change between the default **exponential model** (`-m 0`) in which speciation and extinction rates are exponential functions of the time-continuous variable and a **linear model** (`-m 1`) in which a linear correlation is assumed.  

The time-continuous variable is by default rescaled so that its range of values equals 1. It is additionally shifted to equal 0 at the present. The estimated **correlation parameters** are saved in the output log file as *Gl* and *Gm* for speciation and extinction, respectively, and the **baseline speciation and extinction rates** (indicated by *L0* and *M0* in the log file) represent the esitmated speciation and extinction rates at the present. The rescaling of the time-continuous variable can be changed using the flag `-r`. 

Rates through time plots can be generated using the command `-plot` as shown above for the DD model, e.g.

`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -m 0 -c temperature.txt -plot .../my_logfile.log -b 100`



***
## Model testing using Thermodynamic Integration (TI)

You can use the TI algorithm to calculate the marginal likelihood of a model and **compare the fit of alternative models**. For example you can compare the fit of diversity-dependent models with linear vs exponential correlation or compare the fit of diversity-dependent models with that of temperature-dependent models. The analysis setup and model specification are the same described above and the TI algorithm is enabled by the flag  `-A 1`:

`python PyRateContinuous.py -d .../Canidae_1_G_se_est.txt -m 0 -c temperature.txt -A 1`

PyRateContinuous will run TI using 10 scaling categories by default, and the the number of iteration (as specified by the flag `-n`) corresponds to the number of MCMC iterations for each category.   

Running TI produces a single log file as output from which the marginal likelihood is calculated. Once you run the TI analyses under a range of alternative models, you can use the command `-mL` to calculate the marginal likelihoods of all models. This command expects the path to the log files and will calculate the marginal likelihood for each file in the directory with extension ".log". It is important to **specify an appropriate burnin** using the flag `-b`), for example:  

`python PyRateContinuous.py -mL .../path_to_my_logfiles -b 100`

This command will produce a single text file containing the marginal likelihoods of all models. It will also generate new log files that contain only the "cold" part of the MCMC states sampled by the TI algorithm. The content of these log files can be viewed in Tracer and used for parameter estimation. 







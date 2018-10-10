# Bristol Bay Annual Run Reconstruction
Model Creators:
* Dr. Curry J. Cunningham (University of Washington, School of Aquatic and Fishery Sciences)
* Dr. Trevor Branch (University of Washington, School of Aquatic and Fishery Sciences)

This run reconstruction model is detailed in **A general model for salmon run reconstruction that accounts for interception and and differences in availability to harvest** [Cunningham et al. (2018)](http://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2016-0360#.W76LUCdRebU).

Original Project Collaborators:
Curry J. Cunningham, Trevor A. Branch, Tyler H. Dann, Matt Smith, James E. Seeb, Lisa W. Seeb, Ray Hilborn, Lowell Fair, Tim Baker, Fred West.

## Purpose
A general run reconstruction model for the sockeye salmon fishery in Bristol Bay, Alaska. This model leverages information from age and genetic composition samples to apportion mixed-stock catches and account for interception among terminal fishing districts.

## Workflow
Each year is estimated **independently** although some information is shared among years.

  1. R code creates ADMB input files in the [/Syrah](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah) directory, based on input data in the [/R]() directory.
  2. Formal run reconstruction model estimation by AD-Model Builder (ADMB) in [/Syrah](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah) directory.
  3. Parsing annual ADMB output (.out) files into user-friendly tables and figures in the [Syrah/outputFiles](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah/outputFiles) directory.

## Structure

Directory           | Description
--------------------|-------------------------------
R                   | Houses input data files and functions to create ADMB inputs.
Syrah               | Directory for running ADMB estimation model.
Syrah/outputFiles   | Holds ADMB estimation model output (.out files) and code to generate output figures and tables.


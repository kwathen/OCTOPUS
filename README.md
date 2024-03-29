# OCTOPUS - Optimize Clinical Trials On Platforms Using Simulation Update  <img src="logo.png" align="right" alt="" width="120" />


[![Coverage Status](https://img.shields.io/codecov/c/github/kwathen/OCTOPUS/master.svg)](https://codecov.io/github/kwathen/OCTOPUS?branch=master)
[![Travis-CI Build Status](https://travis-ci.org/kwathen/OCTOPUS.svg?branch=master)](https://travis-ci.org/kwathen/OCTOPUS)
 
## License 
This package is governed by the JRD OCTOPUS License, which is the GNU General Public License V3 with additional terms. The precise license terms are located in [LICENSE](https://github.com/kwathen/OCTOPUS/blob/master/inst/LICENSE) and [GPL](https://github.com/kwathen/OCTOPUS/blob/master/inst/GPL). 


## Introduction

This project is designed to help clinical trial designers simulate a platform trial.   This project is intended for simulation purposes only.  For the purposes of this project, a platform trial is a clinical trial designed to accommodate multiple treatments or interventions added to the trial either at the beginning of the trial or any time thereafter.   Each intervention is added through an Intervention Specific Appendix (ISA).  Each ISA typically randomizes between placebo or control and one or more intervention doses (or combinations).  If trials for multiple interventions in the same disease area are considered, then a platform trial provides a unified framework for designing and running a single, multiple ISA, platform trial.  Potential benefits of a platform trial are shorter, more efficient trials due to a single framework for multiple interventions and borrowing of placebo or control patients across ISAs, thus reducing the overall number of patients treated with placebo/control.  

This package is under development. It is functional and has already been used to simulate several trials.  As case studies are created, they will be added to the Examples directory of the package to help users create the necessary structures.  The tar.gz files in this repository are included as testing versions for beta testers while as new updates and structure are added. 

Using your GitHub account, you may want to "Watch" this project for updates.   Alternatively, please contact the author to be added to the distribution list.    

In order to simulate a platform trial using this package you must specify two structures: 1) Trial Design - this structure specifies details such as the number of ISAs, number of patients for each ISA/treatment, analysis method(s) and trial monitoring scheme, 2) Simulation Design - this structure specifies how to simulate all aspects of the trial such as patient outcomes, patient accrual rates and when ISAs enter the platform.  Each of the structures is described in the sections below. 

To help increase flexibility and allow for new additions to be added by the users, S3 class and generic methods are implemented.  Many aspects can be extended such as new types of analysis, simulation of outcomes and randomization schemes.  For users not familiar with S3 or generic functions, please refer to the example document [Example S3 Generic Methods](https://github.com/kwathen/OCTOPUS/blob/master/ExampleS3Class.R).

This package is under development and is working and has been used to simulate several trials.  As case studies are created they will be added to the Examples directory of the package to help users create the necessary structures.  The tar.gz files in this repository are included as testing versions for beta testers as new updates are added. 

Please follow this project to be notified of updates.   


# Key Features

The following list of key features is intended to by a high level assessment of what can be done with the OCTOPUS package.  This packages makes extensive use of S3 classes to allow anyone to add new functionality, such as a new way of simulating patient data or the analysis that is conducted, with minimal requirements and work.

* Any number of ISAs with any number of treatments contained in the ISA
* ISA start times (times ISA enters the platform) may be fixed or random 
* Borrowing patient information between ISAs is defined for each ISA allowing great flexibility and each ISA can be different
* Analysis - Each ISA can define a different analysis and new analysis approaches are easily added
* Trial monitoring, eg when analysis are run, is triggered in a variety of way such as specified as a minimum information within an ISA, predetermined time intervals between analysis or specified times of analysis
* Patient outcomes: 
    * Any number of patient outcomes
    * Outcomes may be different between ISAs
    * New patient simulators can be added easily 
    * New analysis can be added easily to accommodate new patient outcomes
* Randomization - A few default randomizes are available, new ones can be added
  

It is useful to understand the [flow of a simulation](articles/SimulateTrialOutline.html) as well as the [key simulation objects](articles/PrimaryObjectsForSimulation.html)

The [OCTOPUS-Examples Repo](https://github.com/kwathen/OCTOPUS-Examples) provides the setup files and folders for several types of examples that many users will find helpful. 

# Trial Design Structure
A platform trial requires 2-step randomization, first the the ISA and then between the treatments within and ISA, including control/placebo.  The first step is considered the trial randomizer and the 2nd is considered the ISA randomizer and each ISA may randomize different within an ISAs.  

The trial design object is a structure with a class name equal to the desired trial randomizer.   For example,

cTrialDesign <- structure( list(  ), class='EqualRandomizer')

would create the trial design object where patients are equally randomized between ISAs.  The list( ) in the structure would contain information about the platform, such as the number of ISAs, number of patients on each ISA, ect.   Within the list is also a collection of ISA elements to define the specifics of each ISA.  More details of this object are presented in the documentation and examples.  In general, the ISAs may differ as much as needed, however, if ISAs differ too much the advantages of a platform are minimized.  
# Simulation Design Structure 

The simulation design object contains two pieces, the first is the trial design, described in the previous section, and the simulation object.  The simulation object contains a list of scenarios, such as the null and alternative, to simulate.    


## Installation 
Currently installation  this package is not available on CRAN but may be installed directly from 1) GitHub or 2) a tar.gz files located in GitHub.  The most recent version is listed under the [Versions link](https://github.com/kwathen/OCTOPUS/releases/tag/Version1.3.0.6) by selected the zip or tar.gz   

### Installation Option 1 - From GitHub
Utilizing the [remotes package](https://remotes.r-lib.org/)

```
  remotes::install_github( "kwathen/OCTOPUS")
```

### Installation Option 2 - Using a tar.gz File

Download the tar.gz file [Version 1.3.0.6](https://github.com/kwathen/OCTOPUS/releases/tag/Version1.3.0.6).
In R with the tar.gz file in your  current working directory the following command will install the package.

```
install.packages("OCTOPUS_1.3.0.6.tar.gz", repos=NULL)
```

## Usage 
In the instillation folder there is an Examples directory that contains full examples. In addition, the [OCTOPUS-Examples Repo](https://github.com/kwathen/OCTOPUS-Examples) provides the setup files and folders for several types of examples that many users will find helpful.  These example provide an R file that makes use of the [OCTOPUS::CreateProject](reference/CreateProject.html) which is typically one of the first steps in the [OCTOPUS workflow](articles/GeneralWorkflow.html)


# To Do List
Please add items or issues to the [GitHub Issues](https://github.com/kwathen/OCTOPUS/issues) so they may be addressed. 

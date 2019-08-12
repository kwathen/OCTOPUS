##################################################.
######  Version 1.2.1.1 March 6, 2018        #####
##################################################.

1.  Added dCurrentTime to randomizer so that a treatment within an ISA could have a delayed start.

2.  Added delayed randomizer to allow a treatment within and ISA to to have a delayed start.  For example, an ISA with 4 treatments but the 4th treatment only opens for enrollment 4-5 months after the ISA starts.
    Functions added: InitializeISARandomizer.DelayedStartRandomizer  RandomizeWithinISA.DelayedStartRandomizer

3.  Added tests for DelayedStartRandomizer.

4.  Added ComputePosteriorProbs.MAVTarget,
    MakeDecisionBasedOnPostProb.MAVTarget and GetBayesianCutoffs.MAVTarget, which are identical to the same functions with MAVOnly and are included so that user does not need to inherit from MAVOnly.   In future version, add a constructor to make the inheritance.

Test Results devtools::test()
√ | OK F W S | Context
√ | 19       | Test - AccrualMethods.R [4.2 s]
√ | 20       | Test GetBayesianCutoffs.R [0.2 s]
√ |  4       | Test GetBayesianCutoffs.R
√ |  3       | Test GetFinalISAAnalysisTime.R [0.2 s]
√ | 17       | Test - InitializePatientList.R [0.2 s]
√ | 81       | Test - MakeDecision.R [0.8 s]
√ |  5       | Test MakeDecisionBasedOnPostProb.R
√ |  7       | Test - MakeDecisionDoses.R [0.1 s]
√ | 207       | Test RandomizeBetweenISA.R [2.1 s]
√ | 325       | Test RandomizeWithinISA.R [5.4 s]
√ | 22       | Test - SimulateISAStartTime.R [0.2 s]
√ | 18       | Trial Monitor [0.3 s]

== Results =====================================================================
    Duration: 14.4 s

OK:       728
Failed:   0
Warnings: 0
Skipped:  0

##################################################.
######  Version 1.2.1.2 Development          #####
##################################################.

1.  Major change to include covariates and all designs to make decisions for subgroups of patients.
2.  To make printing option during the simulations easier to utilize added a global variable gnPrintDetail that is used to control print levels.
    The main purpose of this variable is used when debugging or watching trials run.   It will replace the variable cScen$nPrintDetail, but for this
    version cScen$nPrintDetail is still included for backward compatability.

# MultiAgentEC
Organ transplant modelling using expectation event calculus 

## Multi-agent files
- [AgentsAndEvents.ipynb](AgentsAndEvents.ipynb) should be read first. It contains the agent and event declarations for the organ transplant scenario, and provides some context. The prolog code from this notebook has been automatically saved to [AE.pl](AE.pl) in Deepnote.
- [Expectations.ipynb](Expectations.ipynb) provides an overview of the expectation rule examples covered.
- Individual expectation examples can be found in the [ExpectationExamples](ExpectationExamples) folder.
- For the current clauses which support discrete event calculus with Agent:Event syntax, see [dec:notation.pl](dec:notation.pl).

## Deepnote setup
- The [Dockerfile](Dockerfile) used to set up the Deepnote environment.
- For the Jswipl kernel, see [jupyter-swi-prolog](jupyter-swi-prolog). This kernel uses code from [Veracity Lab](https://github.com/veracitylab/jupyter-swi-prolog), but has additional support for running Python cells, saving prolog output and combining code from multiple cells into a single prolog file.
- The [installation notebook](init.ipynb).
- The [requirements file](requirements.txt) which is used by the installation notebook.
- For a guide on how to use the JSwipl kernel in Deepnote, see [using_jswipl.ipynb](using_jswipl.ipynb).

# Event Calculus capabilities and suitability

We can use event calculus to model events and expectations in product tracking and veracity scenarios. This demonstration makes use of 

## Issues with current set up
- We can't evaluate multiple expectations of the same type which occur for the same actor at the same time - we will only get output for one of the expectations.
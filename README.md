# MultiAgentEC
### Organ transplant modelling using expectation event calculus 
#### Kate Truman, University of Otago Computer Science 2021 / 2022 Summer Bursary


We can use event calculus (EC) to model events and expectations in product tracking and veracity scenarios. This demonstration makes use of discrete event calculus (we have discrete periods of time) to exhibit some helpful capabilities of EC in an organ donation scenario.

See [Report.md](FurtherDocumentation/Report.md) in the **FurtherDocumentation** folder for a summary of the Bursary project, and [Notes.md](FurtherDocumentation/Notes.md) in the same folder for notes made in Weeks 1 - 6 of the project.

## Notes about current set up
- We can't evaluate multiple expectations of the same type which occur for the same actor at the same time - we will only get output for one of the expectations. This feature can potentially be turned off if necessary.

## Launching in Deepnote
You can launch this repository as a Deepnote project by clicking [here](https://deepnote.com/launch?url=https%3A%2F%2Fgithub.com%2Fkatetruman%2FMultiAgentEC). 
If you wish to run any of the notebooks, you will need to set the environment to the local Dockerfile, build the Dockerfile and run the initialization notebook (init.ipynb). You may need to recomment the initialisation notebook line `sed -i '/jedi/d;/jupyter/d;' ./requirements.txt`.

Note that I am currently experiencing very slow loading times for notebooks to be set up.

Note that while individual "View in Deepnote" buttons have been added to the notebooks listed below, the Deepnote Viewer appears to have problems displaying all of the notebooks in full in this format.

## Multi-agent files

- [AgentsAndEvents.ipynb](AgentsAndEvents.ipynb) should be read first. It contains the agent and event declarations for the organ transplant scenario, and provides some context. The prolog code from this notebook has been automatically saved to [AE.pl](AE.pl) in Deepnote.
- [Expectations.ipynb](Expectations.ipynb) provides an overview of the expectation rule examples covered.
- Individual expectation examples can be found in the [ExpectationExamples](ExpectationExamples) folder.
- For the current clauses which support discrete event calculus with Agent:Event syntax, see [dec:notation.pl](dec:notation.pl).
- [DEC_Changes.pl](DEC_Changes.pl) records the changes made to dec.pl during the 10 Week Bursary period.
- [date_time.pl](date_time.pl) contains code from the [Date time package](https://github.com/fnogatz/date_time) by Falco Nogatz as using pack_install was not possible in Deepnote. Some Date time predicates are used by a notebook in the ExpectationExamples](ExpectationExamples) folder.

## Deepnote setup
- The [Dockerfile](Dockerfile) used to set up the Deepnote environment.
- For the Jswipl kernel, see [jupyter-swi-prolog](jupyter-swi-prolog). This kernel uses code from [Veracity Lab](https://github.com/veracitylab/jupyter-swi-prolog), but has additional support for running Python cells, saving prolog output and combining code from multiple cells into a single prolog file.
- The [installation notebook](init.ipynb).
- The [requirements file](requirements.txt) which is used by the installation notebook.
- For a guide on how to use the JSwipl kernel in Deepnote, see [using_jswipl.ipynb](using_jswipl.ipynb).



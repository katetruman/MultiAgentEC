# Veracity Project: Event Calculus
## University of Otago Computer Science 2021 / 2022 Summer Bursary - Kate Truman

In this project I investigated how properties of Discrete Event Calculus, with a particular focus on expectations, make it an appropriate tool to model product 
tracking scenarios such as organ transplants.

### Kernel and support file changes
I was able to use the [Juypter SWI Prolog kernel](https://github.com/veracitylab/jupyter-swi-prolog), with tweaks made and additional capabilities added by 
Stephen, Hayden and myself, to run Prolog cells in Deepnote notebooks. One of the challenges that I faced during the project was finding errors in my code given that the JSwip kernel did not provide me with most warning and error messages. In order to make it easier to transfer code from a notebook into a local text editor and consult the file with my local installation of SWI Prolog, I modified the kernel so that when a notebook is run, a file containing all of the Prolog code is produced, which can then be copied.

With Stephen's assistance I was able to extend his existing DEC support predicates to allow for agents and fluents which apply to a given actor only, create 
expections which can continue to exist independently of the expectation rules which created them, and create temporal expectations which can be adjusted based 
on other events or fluents.

### Modelled scenario
In the scenario exhibited in this repository, a monitoring system agent receives requests from hospitals to add patients to the waiting list for organ donations, as well as donation offers. The monitoring system can match waiting patients with organ donation offers, and facilitate organ transplants. The waiting list requests and donor offers are regarded as particular cases of general customer orders and resource requests which may exist in a wide variety of product tracking situations. 

We primarily focus on expectations held by the monitoring system over expectations held by the individual hospitals, although it is also possible to specify
expectation rules for those agents. The expectation rules specified can mostly be placed into three general categories - authorisation, timely communication 
/ action and compatibility / suitability. The monitoring system might expect that when another agent performs an action, the monitoring system expects them to be 
authorised in some way to do so. Agents such as the monitoring system might also expect that either manual events performed by themselves or actions performed
by others are completed within a certain time frame, and that our agent is notified about events that may affect them. For instance, the monitoring system may
expect that a hospital will regularly update them about the medical information for a patient on the organ waiting list, to make sure that their elligibility has
not changed. The monitoring system may also have many compatibility expectations around properties of potential donors and recipients, such as compatible 
blood types or medical conditions.

The notebooks in this repository firstly describe the monitoring system and example hospital agents declared and the events which can occur for them, and then explores different types of expectations that may hold for these agents, particularly the monitoring system.

### Other work and possible extensions
Before moving on to looking at a multi-agent scenario, I focused on a single hospital view. The hospital employed medical staff capable of performing actions 
such as requesting blood tests, performing diagnoses and deciding if a patient was suitable to become an organ donor. In this scenario, I created predicates
allowing the hospital to statistically assess whether doctors were unduely favouring a given medical lab, which could be evidence of fabricated results. I also 
looked at creating fluents modelling the hospital's trust in medical labs and other hospitals. A possible expansion of my current multi-agent work would be to
incorporate more of the fluents developed for a single agent view.

Over the course of a week, I made use of [Pygraphviz](https://pygraphviz.github.io/) to produce GIFs of narrative events for an individual hospital from time period zero up to a specified time period, where events linked by IDs were connected by arrows. Patient admissions, patient history requests, medical test requests, result messages, diagnoses and donor decisions were included in the diagrams. I used [MoviePy](https://pypi.org/project/moviepy/) to concatenate produced PNGs into a GIF, but I had significant issues with cloning MoviePy and using a slightly modified version in my Deepnote environment so that it worked reliably. I was forced to decouple my Prolog code and PNG production in Python from the compilation of the GIFs. I looked into using sliders to present a group of snapshots in time, but found that they were not yet supported in Deepnote. I used PyGraphviz to visualise the events and fluents belonging to different agents in the multi-agent scenario, although without the difficulties of converting produced images into a GIF. Due to time constraints, I didn't tweak my visualisation much, although it could definitely do with some further design work!

I looked into using [Logtalk](https://logtalk.org/), which is an object orientated language that supports several different implementations of Prolog, including SWI Prolog. Using Logtalk, I could have declared agent classes for which various narrative events and fluents (including expectation rules) were defined. This would have made inheritance of fluents and events more straightforward to deal with. I looked into Logtalk towards the end of the project period, and ultimately decided not to use it due to time constraints. There did not appear to be a straightforward way to install Logtalk on the JSwipl kernel, as it does not support `pack_install/1`, and when I tried using locally installed Logtalk, I found that the `abolish_table_subgoals/1` predicate did not appear to be supported. I suspected that finding a solution to these issues could take some time, so was not worth pursuing for this project given that I could add an Actor argument to support predicates for a simple multi-agent implementation. However, Logtalk could well be very useful for an extension of this work.

I also researched how to create a simple web interface for a Prolog program. I worked through some of a [guide by Anne Ogborn](https://github.com/Anniepoo/swiplwebtut/blob/master/web.adoc) on creating web interfaces in Prolog, which was useful, although potentially requiring a little more background Prolog knowledge than I have at present! Again, I didn't pursue this further, partially due to my lack of experience in Javascript, but it could be an interesting extension to the existing notebooks.

Other possible expansions of the current work include adding greater support for the compatibility and suitability rules described in the Compatibility notebook, 
and looking at the communication of expectations between agents.

### Summary
Discrete event calculus, is a useful way to model expectations held by agents in a multi-agent product tracking scenario. While this project has focussed on organ donation, the technology can be easily applied to a wide range of scenarios. While event calculus is particularly advantageous for temporal expectations, logical expressions, past and future events and fluents and interactions between agents can all be incorporated into expectation rules. Relatively low amounts of code are required to make major changes to expectation functionality, such as adding adjustable expectations which can be affected by an agent's busyness level or some other fluent. While discrete event calculus in Prolog is not a suitable tool to deal with all aspects of veracity, it can be a valuable tool for dealing with expectations.

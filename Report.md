# Veracity Project: Event Calculus
## University of Otago Computer Science 2021 / 2022 Summer Bursary - Kate Truman

In this project I investigated how properties of Discrete Event Calculus, with a particular focus on expectations, make it an appropriate tool to model product 
tracking scenarios such as organ transplants.

### Kernel and support file changes
I was able to use the [Juypter SWI Prolog kernel](https://github.com/veracitylab/jupyter-swi-prolog), with tweaks made and additional capabilities added by 
Stephen, Hayden and myself, to run Prolog cells in Deepnote notebooks. One of the challenges that I faced during the project was finding errors in my code given that the JSwip kernel did not provide me with most warning and error 
messages. In order to make it easier to transfer code from a notebook into a local text editor and consult the file with my local installation of SWI Prolog, I 
modified the kernel so that when a notebook is run, a file containing all of the Prolog code is produced, which can then be copied.

With Stephen's assistance I was able to extend his existing DEC support predicates to allow for agents and fluents which apply to a given actor only, create 
expections which can continue to exist independently of the expectation rules which created them, and create temporal expectations which can be adjusted based 
on other events or fluents.

### Modelled scenario
In the scenario exhibited in this repository, a monitoring system agent receives requests from hospitals to add patients to the waiting list for organ donations,
as well as donation offers. The monitoring system can match waiting patients with organ donation offers, and facilitate organ transplants.
The waiting list requests and donor offers are regarded as particular cases of general customer orders and resource requests which may exist in a wide variety
of product tracking situations. 

We primarily focus on expectations held by the monitoring system over expectations held by the individual hospitals, although it is also possible to specify
expectation rules for those agents. The expectation rules specified can mostly be placed into three general categories - authorisation, timely communication 
/ action and compatibility / suitability. The monitoring system might expect that when another agent performs an action, the monitoring system expects them to be 
authorised in some way to do so. Agents such as the monitoring system might also expect that either manual events performed by themselves or actions performed
by others are completed within a certain time frame, and that our agent is notified about events that may affect them. For instance, the monitoring system may
expect that a hospital will regularly update them about the medical information for a patient on the organ waiting list, to make sure that their elligibility has
not changed. The monitoring system may also have many compatibility expectations around properties of potential donors and recipients, such as compatible 
blood types or medical conditions.

Other possible expectation focus areas include

### Possible extensions
Before moving on to looking at a multi-agent scenario, I focused on a single hospital view. The hospital employed medical staff capable of performing actions 
such as requesting blood tests, performing diagnoses and deciding if a patient was suitable to become an organ donor. In this scenario, I created predicates
allowing the hospital to statistically assess whether doctors were unduely favouring a given medical lab, which could be evidence of fabricated results. I also 
looked at creating fluents modelling the hospital's trust in medical labs and other hospitals. A possible expansion of my current multi-agent work would be to
incorporate more of the fluents developed for a single agent view.

Other possible expansions of the current work include adding greater support for the compatibility and suitability rules described in the Compatibility notebook, 
developing visualisations of the multi-agent scenario shown in this repository, and looking at the communication of expectations between agents.

### Summary

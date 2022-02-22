# Notes

The following are useful queries and details from my written notes for Weeks 1 - 6 of the project. As such, they focus on an individual hospital view for the most part, but still
contain some points that may be useful to consider.

### New Zealand Scenario

*Some of the situation in New Zealand may be helpful for a modelling scenario, while other aspects may be ignored for simplicity.*

- Kidneys, lungs, livers, hearts and pancreases can be donated. 
- Some diseases make certain organs unsuitable for donation but not others.
- Brain death must be confirmed by two doctors.
- Kidneys can be transplanted in Auckland, Wellington and Christchurch.
- Hearts, livers and lungs can be transplated in Auckland.
- Waiting lists for different organs are managed by different agencies.
- Criteria for patients on a waiting list includes their age, comorbidities, lifestyle and ability to cope with complex medical therapy.
- Compatibility from donor to recipient is affected by blood type and size of the organ relative to the size of the recipient.
- The NZ Blood service carries out the blood matching tests and tissue typing tests of solid organ transplants.
- Different organs follow different organisations' guidelines.
- For an organ donation to occur, both the deceased person's recorded preferences and the preferences of their family are considered.

---

### Incorrect information
- Why can we get incorrect information?
  - Malicious actors
  - Incorrect data entry
  - Medical test failure
  - Information has been attached to the wrong patient
  - Information is actually correct, but is unusual and thus is flagged by our system rules

#### Errors
- Should we incorporate the accepted sensitivity or specificity of tests? If a patient is diagnosed with HIV, and is later declared to be disease free, what is the chance that the blood test for HIV gave an erroneous result rather than medical staff making a mistake? Are some diseases life long while others may be successfully removed?
- Can a staff member record a lab accident or suggest that a mix up may have occurred and that checking should take place?

#### Blame
- Are we able to quantify which medical diagnosis or other information is most likely to lead to a failed organ transplant if incorrect?
- If we blamed a certain source and later discover evidence which exonerates them, can we "pardon" them and improve our rating / view of them?

#### Validity checks
- What checks can we implement to quickly identify extremely erroneous data entry?
  - No birth dates more than 150 years ago, no dates in the future (except a few months in advance for prenatal records?)

#### Suspicious timing
- In order to monitor the behaviours of medical staff (nurses, doctors etc.), do hospitals encode rosters? An expectation will hold that a staff member will not perform actions outside their scheduled hours, given certain grace periods before and after their shift starting.
- Likewise, do other parties have scheduled hours of operation? Perhaps not so much hospitals, but a blood testing lab may only be open from 8 am - 6 pm, 5 days a week, for example. Thus, we might regard test results sent at 3 am as suspicious. We could use labels to check the day of the week.
- Who has the opportunity to misuse someone else's login or computer?
- Are tissue samples or medical records more likely to get accidentally swapped between patients during busy time periods?

#### Age of information
- How confident should we be in "out of date" measurements? What qualifies information as "old"?
- We could expect that medical information for patients on an organ waiting list will be updated every X units of time.

#### Unique identifiers
- How to deal with inconsistency between records with the same patient ID?
- What happens if we expect it to be the same person in reality vs. two people whose information has gotten mixed up?

#### Failed organ transplants
- What steps of the process do we investigate when an organ transplant is unsuccessful?
- Do we expect that a certain percentage of organ donations will fail?
- Do we monitor failure counts / proportions associated with given doctors, labs and / or hospitals?
- If a party is skirting close to reprimands or investigations, do we monitor them more closely for fear that they will cover up further wrongdoings?

#### Emergency / temporary rules
- Can we add expectations that only apply for a set period of time, or until a condition is met?
---

### Other Sources

#### Established sources
- Do we trust other members of our network who have many interactions with other members, more than those that are relatively reclusive?
- Do we wish to approve of sources who reply promptly? Is there such a thing as too promptly?

#### Hierarchy
- We could have some notion of governing bodies - e.g. the district health board in charge of a particular hospital.
- Do we trust sources monitored by our parent / governing body, and their child sources, more than independent parties?
- How do we deal with "friend of a friend" relationships for sources?
- Do we trust information that has been backed up by additional sources more? What if two friends give us conflicting information?
- Do we require that medical labs have been certified to perform particular tests by us or a trusted third party? How often do we expect inspections should occur? What happens if we later discover that the certifier was not trustworthy?
- Do we want to encode distance / travel time between laboratories and hospitals, and relate this to which sources we expect to be used the most?

#### Messages and Requests
- How to deal with messages or requests received with different urgencies?
- If we send a message to another hospital and expect a response back, what happens if we do not receive a response within X time periods?
- If we send a request for a test or patient history to another party, and they decline the request, do we try again with another hospital / lab? This may not be possible for a patient's medical history.
- How do we treat a party who has declined a lot of requests?
- Can we flag unusual cases for manual consideration (i.e. wait for a further happensAt(event) to affect the request status?)
- How to we treat information on decisions received from another source when there is no evidence to support them?

#### Refutations
- Can someone challenge information originating from another party? 

#### Patient histories
- Can we expect that a hospital will know where to get all of a patient's medical history from?
- Can we expect that the source will provide this information in a timely fashion?
- What if there are multiple, potentially conflicting, sources?

#### Favouritisim
- In order to tell if a medical actor is unfairly favouring a given lab / source of information over others, which could be indication of malicious collusion, we can compare the proportion of time a lab is available to provide the information of interest against other labs, and assess whether the rate at which the lab is favoured seems unlikely under fair allocation.
- How does a lab become available and unavailable, either for a particular test or in general? Opening hours and equipment functionality may be of interest.

---

### Expectations
- Can we mark violations as unlikely to be important?
- If we introduce a new expectation rule, do we want it to apply to historical records?
- If we terminate an expectation rule, do we immediately terminate existing expectations?
- Are we able to assign any quantification of confidence based on expectation violations and fulfilments, e.g. true, probably true, somewhat likely to be true, unknown, somewhat likely to be false, probably false, false?
- How do we combine ratings, if applicable?
- We want no organ wastage, but this is probably not a "realistic" expectation. We should let it hold anyway so we can view violations.
- If everything is treated as a claim, is trustworthiness a claim?
- We expect that organ transplants will be successful, and thus take place within a certain time period after a brain death occurring. This overarching expectation is the motivation for our other expectations.

#### Busyness levels
- When a hospital is busy we may wish to adjust expectations of waiting times accordingly.

#### Wait list
- For prioritising patients, we want to consider age group, time waiting, urgency of recipient condition, and rarity of donor and recipient conditions matching.

#### Donor - Recipient Matching
- Do we allow matches to be confirmed so that they cannot be pre-empted by higher priority requests?
- How do we ensure fair distribution?
- Both hospitals and any monitoring system expect fair allocation.

#### Urgency
- Do we expect that tests will be performed, organs stored and operations performed within a certain period of time for a given urgency level?
- What happens when our expectations are violated?

#### Decisions
- We expect that decisions (e.g. diagnoses, donation eligibility, brain death) will be backed up with linked evidence within a certain period of time. 
- This may require the use of logs to record historical events with unique identifiers.

#### Medical staff
- Staff are given a certain role, e.g. nurse or doctor. Different roles have different actions that they are authorised to perform. Authorisations for roles can be added or revoked, as can staff roles. (See [authorisationInternal.ipynb](https://github.com/katetruman/Expectation_EC/blob/master/authorisationInternal.ipynb)).
- Do we place staff on probation after suspicious behaviour, or monitor / treat differently new staff members?
- Do we record who a medical actor works with, their alma mater etc. so that we know of historical connections which may be present is a collusion.
- Should we expect that a patient has a consistent medical actor or group of medical actors assigned to them? Can we flag actions taken involving them by a member of an unrelated department?
- If a doctor's medical license is revoked or found to be fradulent, does a review of their historical actions take place?
- What if a review is inconclusive or incorrect?

#### Patient supplied information
- How to do we treat medical information supplied by the patient which is not (yet) backed up by further medical record?

---

### Summaries
- We want to be able to view useful statistics that can be filtered for certain hospitals, types of tests, doctors etc., while still retaining appropriate levels of information for the user's authorisation level.
- Is it sometimes more helpful to consider information from the last 6 months or 2 years rather than all historical information?
- How do we filter for a time range?
- Viewing a pattern as suspicious rather than an individual event is more likely to be accurate.
- Are we able to compare a given case to anonymised, similar cases if needed, to check for patterns or unusual behaviour?
- Can we produce a count of the number of similar cases?






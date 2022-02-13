% File: agents.pl
initially(monSys:agent("Otago")).
initially(monSys:agent("Christchurch")).
initially(monSys:agent("Wellington")).
initially(monSys:agent("Auckland")).

% File: insertDec.pl
:- discontiguous causes/3.
:- multifile causes/3.
happensAt(Hospital2, Event2, T1):- causes(Hospital1:Event1, Hospital2:Event2, T),
happensAt(Hospital1, Event1, T), T1 is T + 1.
causes(Hospital:send(Recipient, Message), Recipient:receive(Hospital, Message), _).

% File: waitList.pl
initiates(monSys:receive(Hospital, waitAddReq(ID, Organs, Details)), waiting(ID,Hospital,Details,T),T):-
\+ happensAt(monSys, viol(happ(receive(Hospital, waitAddReq(ID, Organs, Details))),_,_,_),T).
causes(monSys:fulf(happ(receive(Hospital, waitAddReq(ID, _, _))), _, _, _, _, _),
monSys:send(Hospital, waitAccept(ID)), _).
causes(monSys:viol(happ(receive(Hospital, waitAddReq(ID, _, _))), _, _, _, _, Reason),
monSys:send(Hospital, waitReject(ID, Reason)), _).

% File: waitListDeletions.pl
terminates(monSys:receive(Hospital, waitDelReq(ID,_)), waiting(ID,Hospital,_,_),_).
terminates(monSys:receive(Location, transplantOutcome(MatchID, success)),
waiting(RecipientID, RecipientHospital,WaitOrgans,_), T):- holdsAt(monSys:matched(MatchID, _, _,
RecipientHospital, RecipientID, Location, ReceiveOrgans, _), T).
initiates(monSys:receive(Location, transplantOutcome(MatchID, success)), waiting(RecipientID,
RecipientHospital,NewOrgans,T1), T):- holdsAt(monSys:matched(MatchID, _, _, RecipientHospital,
RecipientID, Location, ReceiveOrgans, _), T), holdsAt(waiting(RecipientID, RecipientHospital,
OldOrgans,T1),T), subtract(OldOrgans, ReceiveOrgans, NewOrgans), NewOrgans \= [].

% File: donationRefusal.pl
causes(monSys:noMatch(Hospital, ID, Reason), monSys:send(Hospital, offerRejected(ID, Reason)), _).

% File: matchNotification.pl
causes(monSys:match(MatchID, DonorHospital, DonorID, RecipientHospital, _, Location, Organs, Notes),
monSys:send(DonorHospital, recipientFound(MatchID, DonorID, RecipientHospital, Location, Organs, Notes)),
_).
causes(monSys:match(MatchID, DonorHospital, _, RecipientHospital, RecipientID, Location, Organs, Notes),
monSys:send(RecipientHospital, donorFound(MatchID, RecipientID, DonorHospital, Location, Organs, Notes)),
_).
causes(monSys:match(MatchID, DonorHospital, _, RecipientHospital, _, Location, Organs, Notes),
monSys:send(Location, locationSelected(MatchID, DonorHospital, RecipientHospital, Organs, Notes)),
_).
initiates(monSys:match(MatchID, DonorHospital, DonorID, RecipientHospital, RecipientID, Location,
Organs, Notes), matched(MatchID, DonorHospital, DonorID, RecipientHospital, RecipientID, Location,
Organs, Notes), _).

% File: matchRoles.pl
initiates(monSys:match(MatchID, DonorHospital, _, _, _, _, _, _), role(DonorHospital, MatchID, donor), _).
initiates(monSys:match(MatchID, RecipientHospital, _, _, _, _), role(RecipientHospital, MatchID,
recipient), _).
initiates(monSys:match(MatchID, _, _, Location, _, _), role(Location, MatchID, transplant), _).

% File: matchConfirmation.pl
happensAt(monSys, confirmationMatch(MatchID),T):- holdsAt(monSys, role(DonorHospital, MatchID, donor), T),
holdsAt(monSys, role(RecipientHospital, MatchID, recipient), T),
holdsAt(monSys, role(Location, MatchID, transplant), T),
(happensAtNarrative(monSys, receive(DonorHospital, acceptMatch(MatchID)),T);
holdsAt(monSys, matchAccepted(DonorHospital, MatchID),T)),
(happensAtNarrative(monSys, receive(RecipientHospital, acceptMatch(MatchID)),T);
holdsAt(monSys, matchAccepted(RecipientHospital, MatchID),T)),
(happensAtNarrative(monSys, receive(Location, acceptMatch(MatchID)),T);
holdsAt(monSys, matchAccepted(Location, matchID),T)).
initiates(monSys:confirmationMatch(MatchID), confirmed(MatchID), _).
causes(monSys:confirmationMatch(MatchID), monSys:send(Hospital, confirmedNotification(MatchID)), T):-
holdsAt(monSys, role(Hospital, MatchID, _),T).


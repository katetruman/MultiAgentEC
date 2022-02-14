% File: authorisationExample.pl
initially(monSys:exp_rule(happ(receive(Hospital, waitAddReq(_, _, _))), agent(Hospital), dependent,
"Authorisation":"Wait list requests should come from recognised hospital")).

% File: cell.pl
happensAtNarrative("Otago":send(monSys, waitAddReq(101, ["liver", "kidney"], [sex:"F",
dob:"01/01/2000"])),0).


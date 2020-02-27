candidate(potter).
candidate(malfoy).
candidate(weasley).
candidate(granger).

% Constraint 1
compatible(potter, weasley).
compatible(potter, granger).
compatible(malfoy, weasley).
compatible(malfoy, granger).
compatible(weasley, potter).
compatible(weasley, malfoy).
compatible(weasley, granger).
compatible(granger, potter).
compatible(granger, malfoy).
compatible(granger, weasley).

compatability(P, T, S) :- compatible(P, T), compatible(T, S), compatible(S, P).

different(potter, malfoy).
different(malfoy, potter).
different(C1, C2) :- compatible(C1, C2).

notPotter(M) :- different(potter, M).
notGranger(M) :- different(granger, M).

=(X,X).

% Constraint 2
grangerForPrez(P, T, S) :- different(granger, P), different(malfoy, T), different(malfoy, S).
grangerForPrez(P, T, S) :- =(P, granger), candidate(T), candidate(S).

% Constraint 3
friendshipIsMagic(P, T, S) :- member(potter, [P, T, S]), candidate(P), candidate(T), candidate(S).
friendshipIsMagic(P, T, S) :- notPotter(P), notPotter(T), notPotter(S), different(weasley, P), different(weasley, T), different(weasley, S).

% Constraint 4
grangerForNotSecretary(P, T, S) :- different(granger, S), candidate(P), candidate(T), candidate(S).
grangerForNotSecretary(P, T, S) :- =(S, granger), notPotter(P), notPotter(T).

% Constraint 5
weasleyForNotPresident(P, T, S) :- different(weasley, P), candidate(T), candidate(S).
weasleyForNotPresident(P, T, S) :- =(P, weasley), notGranger(T), notGranger(S).

% P, T, S = President, Treasurer, Secretary
possibleAdmins(P, T, S) :- candidate(P), candidate(T), candidate(S).


correctAdmins(P, T, S) :- 
compatability(P, T, S),
grangerForPrez(P, T, S),
friendshipIsMagic(P, T, S),
grangerForNotSecretary(P, T, S),
weasleyForNotPresident(P, T, S).

solution(P, T, S) :- possibleAdmins(P, T, S), correctAdmins(P, T, S).

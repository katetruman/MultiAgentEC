:- multifile initially/1.
:- multifile initiates/3.
:- multifile terminates/3.
:- multifile releases/3.
:- multifile progress/3.
:- multifile happensAtNarrative/2.
:- multifile holdsAt/2.
:- multifile derived_fluent/1.
:- dynamic holdsAtCached/2.
:- dynamic releasedAtCached/2.
:- dynamic cached/2.
:- dynamic happensAtNarrative/2.
:- dynamic label/2.
:- dynamic derived_fluent/1.

:- discontiguous initially/1.
:- discontiguous initiates/3.
:- discontiguous terminates/3.
:- discontiguous releases/3.
:- discontiguous happensAtNarrative/2.
:- discontiguous derived_fluent/1.

:- table(stratum/2).
stratum(1, cum_prop_delta(_,_,_)).
stratum(2, cum_prop(_,_,_)).
stratum(3, F) :-
    setof(Func/Arity,
          (Fluent,E,T,B)^(( clause(initially(Fluent), B)
                          ; clause(initiates(E, Fluent, T), B)
                          ; clause(terminates(E, Fluent, T), B)
                          ),
                          functor(Fluent, Func, Arity),
                          \+ memberchk(Func/Arity, [cum_prop_delta/3, cum_prop/3, exp/6, exp/4])
                         ),
          FAList1),
    findall(FuncArity, derived_fluent(FuncArity), FAList2),
    union(FAList1, FAList2, FAList),
    member(Func/Arity, FAList),
    functor(F, Func, Arity). % Create template for F with new vars as arguments
stratum(4, exp(_,_,_,_,_,_)).
stratum(5, exp(_,_,_)).
% If adding more strata, update num_strata/1 below.

num_strata(5).


holdsAt(F,T) :-
    stratum(N,F),
    cached(N,T),
    holdsAtCached(F,T).

holdsAt(F,T) :-
    stratum(N,F),
    \+ cached(N,T),
    holdsAtNoCache(F,T).

% Expectations now contain a status and a message. If Status is set to independent, this means that the expectation 
% will be retained even if the triggering expectation rule is terminated. If the Status is entered as dependent (or another undefined entry),
% the expectation will be terminated with the expectation rule.

holdsAtNoCache(exp(E,Status, Message), T) :-
    setof((Exp,S, M), exists(Cond,OrigExp,TriggerT)^holdsAtCached(exp(Cond,OrigExp,TriggerT,Exp,S, M), T), Exps),
    member((E,Status, Message), Exps),
    (holdsAt(exp_rule(_,E,Status,Message),T) ; Status = independent).

holdsAtNoCache(exp(C,E,TriggerT,ProgressedExp, Status, Message), T) :-
    T > 0,
    PrevT is T-1,
    holdsAtCached(exp(C,E,TriggerT,ResidualExp, Status, Message), PrevT),
    eval(ResidualExp, PrevT, SimplifiedExp),
    ( pragmatically_keep_exp(ResidualExp, SimplifiedExp)
    ; \+ member(SimplifiedExp, [true, false])
    ),
    progress(SimplifiedExp, ProgressedExp,T),
    (holdsAt(exp_rule(C,E,Status,Message),T) ; Status = independent).

holdsAtNoCache(F, -1) :-
    F = exp_rule(_,_,_,_),
    initially(F).

holdsAtNoCache(F, 0) :-
    initially(F).

holdsAtNoCache(F, T2) :-
    T2 > 0,
    T1 is T2 - 1,
    holdsAtCached(F, T1),
    \+ releasedAt(F, T1),
    \+ (happensAt(E, T1), terminates(E, F, T1)).

holdsAtNoCache(F, T2) :-
    T2 > 0,
    T1 is T2 - 1,
    happensAt(E, T1),
    initiates(E, F, T1).

holdsAtNoCache(cum_prop(P,K,V), T) :-
    TPrev is T-1,
    holdsAtCached(cum_prop(P,K,U), TPrev),
    findall(Delta, holdsAtCached(cum_prop_delta(P,K,Delta), T), Deltas),
    sum_list(Deltas, DeltasTotal),
    V is U + DeltasTotal.

% If an exp_rule holds in state T, and its condition is true there, then
% its expectation holds in state T.  Use T=-1 if rule holds initially
holdsAtNoCache(exp(C,E,T,E,Status,Message), T) :-
    T >= 0, % Special case: It's OK to check T=-1 for exp_rule/4 facts.
    holdsAtCached(exp_rule(C,E, Status, Message), T), % NOTE: Same T
    eval(C, T, true),
    PrevT is T-1,
    \+ holdsAtCached(exp(C,E,_,E, Status, Message), PrevT). % Avoid proliferation of 'always' and 'never' exps from same rule triggering in multiple time steps.

% pragmatically_keep_exp/2
% Pragmatic choices for carryng forward expectations that are violated or fulfilled
% Holds when exp should be retained to detect future fulfilments and violations
% TO DO: Decide based on a canonical form of the formula (e.g. not(always(F)) --> never(F))

pragmatically_keep_exp(always(_), false).
pragmatically_keep_exp(never(_), true).

happensAt(F,T) :- happensAtNarrative(F,T) ; happensAtInferred(F,T).

% TO DO: replace with fulf and viol closure clauses - but I've forgotten what I meant by that!
% Note: changed from having 1 timestep delay for fulf/viol to no delay

happensAtInferred(fulf(C,E,TriggerT, ResidualExp, Status, Message), T) :-
    holdsAtCached(exp(C,E,TriggerT,ResidualExp, Status, Message), T),
    eval(ResidualExp, T, true).

happensAtInferred(viol(C,E,TriggerT,ResidualExp, Status, Message), T) :-
    holdsAtCached(exp(C,E,TriggerT,ResidualExp, Status, Message), T),
    eval(ResidualExp, T, false).


happensAtInferred(conflict(P),T):-
    holdsAt(exp(eventually(P),Status, Message),T), holdsAt(exp(never(P), Status, Message),T).

% Expectations and cumulative property fluents are always released from inertia
releasedAt(exp(_,_,_), _).
releasedAt(exp(_,_,_,_,_,_), _).
releasedAt(cum_prop(_,_,_), _).
releasedAt(cum_prop_delta(_,_,_), _).

releasedAt(F, T) :-
    releasedAtCached(F, T).
releasedAt(F, T2) :-
    T1 is T2 - 1,
    releasedAtCached(F, T1),
    \+ (happensAt(E, T1),
    ( initiates(E, F, T1)
     ; terminates(E, F, T1))).
     
releasedAt(F, T2) :-
    T1 is T2 - 1,
    happensAt(E, T1),
    releases(E, F, T1),
    assert(releasedAtCached(F, T2)). % Use a cached version?

% Support for multi-valued fluent, taken from Marek Sergot-s lecture notes
terminates(E, F=_, T) :- initiates(E, F=_, T).
%F:- F=_.

holdsAtPrevLabel(F, L, T) :-
    eval('@prev'(L,F), T, true).

% TO DO: Add support below for 'or' and for additional temporal operators


eval(ExistsExpr, T, Boolean):-
    ExistsExpr = exist(_, _),
    !,
    varnumbers_names(ExistsExpr, exist(_,ExprCopy),_),
    eval(ExprCopy, T, Boolean).


eval(F, T, Boolean) :-
    functor(F, Func, Arity),
    \+ member(Func/Arity, [true/0, false/0, not/1, and/1, or/1, condition/1, (@)/1, happ/1, next/1, within/2, within/3, later/2, delay/2, before/2, preceded/2, eventually/1, always/1, never/1, until/2, '@prev'/2]),
    ( setof(F, holdsAtCached(F, T), GroundFs) ->
        Boolean = true,
        member(F, GroundFs)
    ; Boolean = false
    ).


% Evaluate true / false condition
eval(condition(X),_,Bool):- (X -> Bool = true ; Bool = false).

eval(true, _, true).
eval(false, _, false).
eval(not(F), T, NotResult) :-
    eval(F, T, Result),
    possibly_unknown_not(Result, NotResult).
eval(and(L), T, Result) :-
    map_eval(L, T, EvalResults),
    reduce_and(EvalResults, [], Result).

eval(or(L), T, Result):-
    map_eval(L, T, EvalResults),
    reduce_or(EvalResults, [], Result).

eval(@(L), T, Boolean) :-
    ( setof(L, label(L, T), Ls) ->
        Boolean = true,
        member(L, Ls)
    ; Boolean = false
    ).

eval('@prev'(L,F), T, Boolean) :-
    findall(PrevT, (label(L,PrevT), PrevT < T), PrevLTs),
    ( (sort(0, @>, PrevLTs, DescPrevLTs),
       [LatestPrevLT|_] = DescPrevLTs,
       eval(F, LatestPrevLT, true)
      ) -> Boolean = true
    ; Boolean = false
    ).
eval(happ(Event), T, Boolean) :-
    ( setof(Event, happensAt(Event,T), Es) ->
        Boolean = true,
     member(Event, Es)
    ; Boolean = false
    ).


eval(next(F), _, next(F)).

eval(preceded(F,P),T,F2):- S is T - P, (S < 0 -> false ; (holdsAt(F,S)-> F2 = true ; F2 = false)).

eval(within(F,P),T,F2):-
    (eval(F,T,true) ->
        F2 = true
    ; P < 0 -> F2 = false;
    F2 = within(F,P)).

eval(within(F,P,Type),T,F2):-
    (eval(F,T,true) ->
        F2 = true
    ; P < 0 -> F2 = false;
    F2 = within(F,P,Type)).

eval(later(F,P),T,F2):-
    (eval(F,T,true) ->
        F2 = false
    ; P < 0 -> F2 = true;
    F2 = later(F,P)).

eval(delay(F,P),T,F2):-
    (eval(F,T,true) ->
    (P = 0 ->
    F2 = true;
    F2 = false);
    (P = 0 ->
    F2 = false;
    F2 = delay(F,P))).


eval(eventually(F), T, F2) :-
    ( eval(F, T, true) ->
        F2 = true
    ; F2 = eventually(F)
    ).
eval(always(F), T, F2) :-
    ( eval(F, T, false) ->
        F2 = false
    ; F2 = always(F)
    ).
eval(never(F), T, F2) :-
    ( eval(F, T, true) ->
        F2 = false
    ; F2 = never(F)
    ).
eval(until(F1, F2), T, F) :-
    ( eval(F2, T, true) -> F = true
    ; eval(F1, T, false) -> F = false
    ; F = until(F1, F2)
    ).

eval(before(F1,F2), T, F):-
    (eval(F2,T,true) -> F = false;
     eval(F1,T, true) -> F = true;
     F = before(F1,F2)).




map_eval([], _, []).
map_eval([H|T], Time, [HResult|TResult]) :-
    eval(H, Time, HResult),
    map_eval(T, Time, TResult).


reduce_and([], [], true) :- !.
reduce_and([], Unknowns, and(Unknowns)).
reduce_and([true|Tail], Unknowns, Result) :-
    !,
    reduce_and(Tail, Unknowns, Result).
reduce_and([false|_], _, false) :-
    !.
reduce_and([Term|Tail], Unknowns, Result) :-
    append(Unknowns, [Term], Unknowns2),
    reduce_and(Tail, Unknowns2, Result).

reduce_or([],[],false):- !.
reduce_or([], Unknowns, or(Unknowns)).
reduce_or([false|Tail], Unknowns, Result):-
    !,
    reduce_or(Tail, Unknowns, Result).
reduce_or([true|_], _, true):- !.
reduce_or([Term|Tail], Unknowns, Result):-
    append(Unknowns, [Term], Unknowns2),
    reduce_or(Tail, Unknowns2, Result).

% Progression (note: partial evaluation/simplication has already been done)

progress(true, true,_).
progress(false, false,_).
progress(next(LTLFormula), LTLFormula,_).

progress(within(F1,T1), within(F1,T2),_):- T2 is T1 - 1.

progress(later(F1,T1), later(F1,T2),_):- T2 is T1 - 1.
progress(delay(F1,T1), delay(F1,T2),_):- T2 is T1 - 1.
progress(before(F1,F2), before(F1,F2),_).
progress(eventually(LTLFormula), eventually(LTLFormula),_).
progress(never(LTLFormula), never(LTLFormula),_).

possibly_unknown_not(F, not(F)) :-
    \+ member(F, [true,false]).
possibly_unknown_not(true, false).
possibly_unknown_not(false, true).

run(N) :-
    forall(between(0,N,T), tick(T)).
    %retractall(holdsAtCached(_,_))

% Must be called at all time steps from 0 onwards. Records fluents that hold at T, given fluents and events at T-1
event_recognition(T) :- tick(T). % For backwards compatibility
tick(T) :-
    % Tm2 is T - 2,
    % retractall(holdsAtCached(_, Tm2)),
    % retractall(releasedAtCached(_, Tm2)),
    num_strata(NS),
    forall((between(1,NS,N)
        %, format("** Stratum ~w**~n", [N])
       ),
            ( forall((stratum(N,F), % format("* Fluent ~w~n", [F]),
              holdsAt(F,T)),
                     %(write('Caching '), write(F), write(' at '), writeln(T),
             (holdsAtCached(F,T) -> true ;
             assert(holdsAtCached(F,T))
             )
             %)
            ),
             % format("Asserting cached(~w,~w)~n", [N,T]),
         assert(cached(N,T))
           )).

initialiseDEC :-
    retractall(holdsAtCached(_,_)),
    retractall(releasedAtCached(_,_)),
    retractall(cached(_,_)),
    abolish_table_subgoals(stratum(_,_)).

whatif(EventList, T, EventPattern, FluentPattern, InferredEvents, NextTimeMatchingFluents) :-
    ( bagof(E, happensAtNarrative(E, T), B) ->
        format("WARNING: whatif/3 called when events ~w are already declared to happen at time ~w via happensAtNarrative~n", [B,T])
    ; true
    ),
    Tm1 is T - 1,
    findall(F, holdsAtCached(F, Tm1), PrevTHoldsAtCachedList),
    findall(F, releasedAtCached(F, Tm1), PrevTReleasedAtCachedList),
    findall(E, happensAtInferred(E, T), AlreadyInferredEvents),
    forall(member(E, EventList), assert(happensAtNarrative(E, T))),
    Tp1 is T + 1,
    event_recognition(Tp1),
    findall(F, ( F = FluentPattern, holdsAt(F, Tp1) ), NextTimeMatchingFluents),
    findall(E, ( E = EventPattern,
                 ( happensAtInferred(E, T), \+ member(E, AlreadyInferredEvents)
                 ; happensAtInferred(E, Tp1)
                 )
               ), InferredEvents),
    retractall(happensAtNarrative(_, T)),
    retractall(holdsAtCached(_, Tp1)),
    retractall(releasedAtCached(_, Tp1)),
    forall(member(F, PrevTHoldsAtCachedList), assertz(holdsAtCached(F, Tm1))),
    forall(member(F, PrevTReleasedAtCachedList), assertz(releasedAtCached(F, Tm1))).

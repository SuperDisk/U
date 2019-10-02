:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

sanity_(push1(X)) -->
    "push1 ", whites, integer(X).
sanity_(pushOctothorpe(X)) -->
    "push# ", whites, integer(X).
sanity_(jump(X)) -->
    "jump ", whites, integer(X).
sanity_(jumpb(X)) -->
    "jumpb ", whites, integer(X).
sanity_(case(X)) -->
    "case ", whites, integer(X).
sanity_(goto(X)) -->
    "goto ", whites, string(X), eos.
sanity_(label(X)) -->
    "label ", whites, string(X), eos.

sanity(A, X) :-
    phrase(sanity_(X), A).
sanity(A, X) :-
    \+phrase(sanity_(X), A),
    X = comment(A).

lines(Str, X) :-
    split_string(Str, "\n", "", Ls),
    include(dif(""), Ls, Ls2),
    maplist(string_codes, Ls2, X).

sanityProg(Str, X) :-
    lines(Str, Lines),
    maplist(sanity, Lines, X).

s(comment(X), Out) :- string_codes(Out, X).
s(Term, Str2) :-
    phrase(sanity_(Term), Str),
    string_codes(Str2, Str).
unparse(Ls, Bleh) :-
    maplist(s, Ls, Bleh).

replaceReg_(X, Y, push1(X), push1(Y)).
replaceReg_(X, Y, pushOctothorpe(X), pushOctothorpe(Y)).
replaceReg_(X, Y, case(X), case(Y)).
replaceReg(What, With, In, Out) :-
    replaceReg_(What, With, In, Out).
replaceReg(What, With, In, Out) :-
    \+replaceReg_(What, With, In, Out),
    In = Out.

prependLabel_(Y, label(X), label(Out)) :-
    append(X, Y, Out).
prependLabel_(Y, goto(X), goto(Out)) :-
    append(X, Y, Out).
prependLabel(What, In, Out) :-
    prependLabel_(What,In,Out).
prependLabel(What,In,Out) :-
    \+prependLabel_(What,In,Out),
    In = Out.

rr(A, B, P1, P2) :-
    maplist(replaceReg(A,B), P1,P2).

pl(A, P1, P2) :-
    maplist(prependLabel(A), P1, P2).

ones_(0) -->
    "".
ones_(X) -->
    "1", ones_(Y), {X #= Y + 1}.

ones(X) -->
    ones_(X), {X #> 0}.

program([]) --> "".
program([push1(X) | Rest]) -->
    ones(X),
    "#",
    program(Rest).
program([pushOctothorpe(X) | Rest]) -->
    ones(X),
    "##",
    program(Rest).
program([jump(X) | Rest]) -->
    ones(X),
    "###",
    program(Rest).
program([jumpb(X) | Rest]) -->
    ones(X),
    "####",
    program(Rest).
program([case(X) | Rest]) -->
    ones(X),
    "#####",
    program(Rest).

replace(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).

case([], [], PC, NewPC) :-
    NewPC #= PC+1.
case(['1' | Rest], Rest, PC, NewPC) :-
    NewPC #= PC+2.
case(['#' | Rest], Rest, PC, NewPC) :-
    NewPC #= PC+3.

doWord(push1(Reg), PC, Registers, NewPC, NewRegs) :-
    RegNum #= Reg - 1,
    nth0(RegNum, Registers, Register),
    append(Register, ['1'], NewRegister),
    replace(Reg, Registers, NewRegister, NewRegs),
    NewPC #= PC + 1.
doWord(pushOctothorpe(Reg), PC, Registers, NewPC, NewRegs) :-
    RegNum #= Reg - 1,
    nth0(RegNum, Registers, Register),
    append(Register, ['#'], NewRegister),
    replace(Reg, Registers, NewRegister, NewRegs),
    NewPC #= PC + 1.
doWord(jump(Steps), PC, Registers, NewPC, Registers) :-
    NewPC #= PC + Steps.
doWord(jumpb(Steps), PC, Registers, NewPC, Registers) :-
    NewPC #= PC - Steps.
doWord(case(Reg), PC, Registers, NewPC, NewRegisters) :-
    RegNum #= Reg - 1,
    nth0(RegNum, Registers, Register),
    case(Register, NewRegister, PC, NewPC),
    replace(Reg, Registers, NewRegister, NewRegisters).

interp(Program, PC, Registers) :-
    length(Program, ProgSize),
    (   PC #< 0 ; PC #>= ProgSize),
    writeln(Registers).

interp(Program, PC, Registers) :-
    length(Program, ProgSize),
    PC #>= 0, PC #=< ProgSize,
    nth0(PC, Program, Instr),
    doWord(Instr, PC, Registers,
           NewPC, NewRegs),
    interp(Program, NewPC, NewRegs).

execute(Str, StartRegs) :-
    phrase(program(Prog), Str),
    interp(Prog, 0, StartRegs).

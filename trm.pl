:- use_module(library(clpfd)).

ones_(0) -->
    "".
ones_(X) -->
    "1", ones_(Y), {X #= Y + 1}.

ones(X) -->
    ones_(X), {X #> 0}.

program([]) --> "".
%% program(Rest) -->
%%     [X],
%%     {dif(X, `1`), dif(X, `#`)},
%%     program(Rest).

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

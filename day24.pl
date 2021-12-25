:- use_module(library(clpfd)).

constrain(Z, W, A, B, C, Zout) :-
    Z0 #= Z // A,
    M #<==> Z mod 26 + B #\= W,
    Zout #= Z0 * (M * 26) + M * (W + C).

day24(InputNum) :-
    Vars = [Input1,
            Input2,
            Input3,
            Input4,
            Input5,
            Input6,
            Input7,
            Input8,
            Input9,
            Input10,
            Input11,
            Input12,
            Input13,
            Input14],

    InputNum #= Input14 * 10000000000000 +
                Input13 * 1000000000000 +
                Input12 * 100000000000 +
                Input11 * 10000000000 +
                Input10 * 1000000000 +
                Input9  * 100000000 +
                Input8  * 10000000 +
                Input7  * 1000000 +
                Input6  * 100000 +
                Input5  * 10000 +
                Input4  * 1000 +
                Input3  * 100 +
                Input2  * 10 +
                Input1  * 1,

    Vars ins 1..9,
    Z = 0,
    constrain(Z,   Input1,  1,   11,   1, Z1),
    constrain(Z1,  Input2,  1,   10,  10, Z2),
    constrain(Z2,  Input3,  1,   13,   2, Z3),
    constrain(Z3,  Input4,  26, -10,  5,  Z4),
    constrain(Z4,  Input5,  1,   11,   6, Z5),
    constrain(Z5,  Input6,  1,   11,   0, Z6),
    constrain(Z6,  Input7,  1,   12,  16, Z7),
    constrain(Z7,  Input8,  26, -11,  12, Z8),
    constrain(Z8,  Input9,  26,  -7,  15, Z9),
    constrain(Z9,  Input10, 1,   13,   7, Z10),
    constrain(Z10, Input11, 26, -13,   6, Z11),
    constrain(Z11, Input12, 26,   0,   5, Z12),
    constrain(Z12, Input13, 26, -11,   6, Z13),
    constrain(Z13, Input14, 26,   0,  15, Z14),
    Z14 #= 0,
%     labeling([max(InputNum)], [InputNum]).
    %     label([InputNum])
    indomain(InputNum).

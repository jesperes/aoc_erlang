-module(aoc2015_day22).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-define(INITIAL_HP, 50).
-define(INITIAL_MANA, 500).

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2015,
                day = 22,
                name = "Wizard Simulator 20XX",
                expected = {900, 1216},
                has_input_file = false}.

-type input_type() :: {HP :: integer(), Damage :: integer()}.
-type result1_type() :: any().
-type result2_type() :: result1_type().

-spec parse(Input :: binary()) -> input_type().
parse(_) ->
    {51, 9}.

-spec solve1(Input :: input_type()) -> result1_type().
solve1({BossHP, BossDamage}) ->
    battle(player, state(?INITIAL_HP, ?INITIAL_MANA, BossHP, BossDamage, 0)).

-spec solve2(Input :: input_type()) -> result2_type().
solve2({BossHP, BossDamage}) ->
    battle(player, state(?INITIAL_HP, ?INITIAL_MANA, BossHP, BossDamage, 1)).

spells() ->
    [magic_missile, drain, shield, poison, recharge].

cost(magic_missile) ->
    53;
cost(drain) ->
    73;
cost(shield) ->
    113;
cost(poison) ->
    173;
cost(recharge) ->
    229.

effect_length(shield) ->
    6;
effect_length(poison) ->
    6;
effect_length(recharge) ->
    5.

state(HP, Mana, BossHP, BossDamage, Pen) ->
    #{boss_hp => BossHP,         % boss hit points
      boss_damage => BossDamage, % boss damage
      effects => #{},            % active effects (Spell => Timer)
      hp => HP,                  % player hit points
      mana => Mana,              % available mana
      mana_spent => 0,           % spent mana
      armor => 0,                % player armor (0 or 7)
      hp_penalty => Pen}.          % for part 2

%% battle/2 searches for the optimal player win, as measured by least
%% amount of mana spent. Returns the amount of mana spent.
battle(player, State) ->
    battle(player, State, undef).

battle(_, #{mana_spent := ManaSpent}, CurrentBest) when ManaSpent > CurrentBest ->
    %% Prune battles we know will not generate a better result
    undef;
battle(boss, #{boss_hp := BossHP} = State, _) when BossHP =< 0 ->
    maps:get(mana_spent, State);
battle(player, #{hp := HP}, _) when HP =< 0 ->
    undef;
battle(player, State, CurrentBest) ->
    S0 = apply_part2_penalty(State),
    PlayerHP = maps:get(hp, S0),
    if PlayerHP =< 0 ->
           undef;
       true ->
           S1 = apply_effects(S0),
           lists:foldl(fun(Spell, BestMana) ->
                          S2 = apply_spell(Spell, S1),
                          min(BestMana, battle(boss, S2, BestMana))
                       end,
                       CurrentBest,
                       valid_spells(S1))
    end;
battle(boss, State, CurrentBest) ->
    S0 = apply_effects(State),
    #{hp := HP,
      boss_hp := BossHP,
      armor := Armor,
      boss_damage := Damage} =
        S0,

    if BossHP =< 0 ->
           maps:get(mana_spent, S0);
       true ->
           EffDamage = max(1, Damage - Armor),
           battle(player, S0#{hp => HP - EffDamage}, CurrentBest)
    end.

%%% ============================================================
%%% Helper functions
%%% ============================================================

%% Part 2 introduces a penalty which deduces one player hp at the
%% beginning of each round.
apply_part2_penalty(State) ->
    Penalty = maps:get(hp_penalty, State),
    maps:update_with(hp, fun(V) -> V - Penalty end, State).

%% Valid spells are spells which are not currently in effect, and ones
%% which we can afford.
valid_spells(State) ->
    #{effects := Effects, mana := Mana} = State,
    lists:filter(fun(Spell) -> not maps:is_key(Spell, Effects) and (cost(Spell) =< Mana) end,
                 spells()).

%% Apply effects, decrement timers, and prune expired effects.
apply_effects(State) ->
    Effects = maps:get(effects, State),

    S0 = maps:put(armor, 0, State), %% armor is not accumulative, so reset first
    S1 = maps:fold(fun (poison, _T, StateIn) ->
                           maps:update_with(boss_hp, fun(V) -> V - 3 end, StateIn);
                       (recharge, _T, StateIn) ->
                           maps:update_with(mana, fun(V) -> V + 101 end, StateIn);
                       (shield, _T, StateIn) ->
                           maps:put(armor, 7, StateIn)
                   end,
                   S0,
                   Effects),

    Effects0 = maps:map(fun(_, Timer) -> Timer - 1 end, Effects),
    Effects1 = maps:filter(fun(_Spell, Timer) -> Timer >= 1 end, Effects0),
    S1#{effects => Effects1}.

create_effect(State, Spell) ->
    maps:update_with(effects, fun(V) -> maps:put(Spell, effect_length(Spell), V) end, State).

apply_spell(Spell, State) ->
    BossHP = maps:get(boss_hp, State),
    HP = maps:get(hp, State),

    S0 = case Spell of
             magic_missile ->
                 State#{boss_hp => BossHP - 4};
             drain ->
                 State#{boss_hp => BossHP - 2, hp => HP + 2};
             shield ->
                 create_effect(State, shield);
             poison ->
                 create_effect(State, poison);
             recharge ->
                 create_effect(State, recharge)
         end,

    S1 = maps:update_with(mana, fun(V) -> V - cost(Spell) end, S0),
    maps:update_with(mana_spent, fun(V) -> V + cost(Spell) end, S1).

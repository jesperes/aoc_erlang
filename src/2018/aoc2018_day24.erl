%% Effective power: units * attack damage
%% Effective attack damage:
%% 0 is defender is immune to damage type
%% 2 * Attacker's effective power if defender is weak to damage type
%% Otherwise, attacker's effective power.

-module(aoc2018_day24).

-define(UNIT_RE, <<"(\\d+) units each with (\\d+) hit points (\\((.*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)">>).

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 24,
                name = "Immune System Simulator 20XX",
                expected = {25088, 2002},
                has_input_file = true}.

-type input_type() :: binary().
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Input.

-spec solve1(Input :: input_type()) -> result_type().
solve1(Input) ->
    {infection_wins, {Part1, _}} = start(Input, 0),
    Part1.

-spec solve2(Input :: input_type()) -> result_type().
solve2(Input) ->
    {{immunesystem_wins, {Part2, _}}, _} = start_with_boost(Input, 0),
    Part2.

start_with_boost(Binary, Boost) ->
  case start(Binary, Boost) of
    {infection_wins, _} ->
      start_with_boost(Binary, Boost + 1);
    {immunesystem_wins, _} = X ->
      {X, {boost, Boost}}
  end.

start(Binary, ImmuneBoost) ->
  [_, _, ImmuneSystem, _, Infection] =
    re:split(binary_to_list(Binary), "(Immune System|Infection):"),
  Army1 = parse_army(ImmuneSystem, immunesystem, ImmuneBoost),
  Army2 = parse_army(Infection, infection, 0),
  fight_until_death(Army1 ++ Army2).

parse_army(Binary, ArmyName, Boost) ->
  Groups = string:tokens(binary_to_list(Binary), "\n"),
  {_, Army} =
    lists:foldl(fun(Line, {N, List}) ->
                    {N + 1, [parse_group(Line, ArmyName, N, Boost)|List]}
                end, {1, []}, Groups),
  Army.

to_i(N) -> list_to_integer(N).
to_a(N) -> list_to_atom(N).

parse_group(Line, ArmyName, Id, Boost) ->
  case re:run(Line, ?UNIT_RE) of
    {match, Captures} ->
      #{
        army => ArmyName,
        id => {ArmyName, Id},
        units => to_i(capture_group(Line, Captures, 1)),
        hp => to_i(capture_group(Line, Captures, 2)),
        damage => to_i(capture_group(Line, Captures, 5)) + Boost,
        damagetype => to_a(capture_group(Line, Captures, 6)),
        initiative => to_i(capture_group(Line, Captures, 7)),
        immune_to => parse_strengths("immune", capture_group(Line, Captures, 4)),
        weak_to => parse_strengths("weak", capture_group(Line, Captures, 4))
       };
    nomatch ->
      throw({nomatch, Line, ?UNIT_RE})
  end.

parse_strengths(What, Str) ->
  parse_strength(What,
                 lists:map(fun string:trim/1, string:tokens(Str, ";"))).

parse_strength(_, []) ->
  [];
parse_strength(What, [Strength|Rest]) ->
  Tokens = string:tokens(Strength, " ,"),
  case Tokens of
    [What, "to"|Thing] ->
      lists:map(fun to_a/1, Thing);
    _ ->
      parse_strength(What, Rest)
  end.

capture_group(Line, Captures, N) ->
  Capture = lists:nth(N + 1, Captures),
  case Capture of
    {-1, _} ->
      "";
    {Start, Len} ->
      lists:sublist(Line, Start + 1, Len)
  end.

effective_power(#{units := Units, damage := Damage}) ->
  Units * Damage.

fight_until_death(Groups) ->

  Imm = lists:filter(fun(G) -> maps:get(army, G) == immunesystem end, Groups),
  Inf = lists:filter(fun(G) -> maps:get(army, G) == infection end, Groups),

  ImmUnits = lists:sum(lists:map(fun(G) ->
                                     maps:get(units, G)
                                 end, Imm)),

  InfUnits = lists:sum(lists:map(fun(G) ->
                                     maps:get(units, G)
                                 end, Inf)),

  if (ImmUnits == 0) ->
      {infection_wins, {InfUnits, no_enemy_units}};
     (InfUnits == 0) ->
      {immunesystem_wins, {ImmUnits, no_enemy_units}};
     true ->
      G0 = fight(Groups),

      TotalUnitsBefore = lists:map(fun(G) -> maps:get(units, G) end, Groups),
      TotalUnitsAfter = lists:map(fun(G) -> maps:get(units, G) end, G0),

      %% Treat stalemates as infection victories.
      if TotalUnitsAfter == TotalUnitsBefore ->
          {infection_wins, {InfUnits, {enemy_units, ImmUnits}}};
         true ->
          fight_until_death(G0)
      end
  end.

fight(Groups) ->
  TargetSelections = select_target(Groups),
  GroupsInAttackOrder = get_groups_in_attack_order(Groups),
  G0 = attack(GroupsInAttackOrder, TargetSelections),
  remove_killed_groups(G0).

remove_killed_groups(Groups) ->
  lists:filter(fun(#{units := 0}) ->
                   false;
                  (_) ->
                   true
               end, Groups).

%% Perform an attack round. Groups is the list of all army groups
%% sorted on attack order. Targets is a map of attacker->defender ids.
%% If a group is not present in the target selections map, it does not
%% perform an attack (there was no target to find).
attack(Groups, Targets) ->
  AttackIds = lists:map(fun(X) -> maps:get(id, X) end, Groups),

  lists:foldl(fun(AttackId, Acc) ->
                  Attacker = lookup_group_by_id(AttackId, Acc),
                  case maps:is_key(AttackId, Targets) of
                    true ->
                      DefId = maps:get(AttackId, Targets),
                      Defender = lookup_group_by_id(DefId, Acc),

                      %% Compute the attack damage.
                      AD = attack_damage(Attacker, Defender),
                      Def0 = deal_damage(Defender, AD),

                      lists:map(fun(#{id := X}) when X == DefId ->
                                    Def0;
                                   (G) ->
                                    G
                                end, Acc);
                    false ->
                      Acc
                  end
              end, Groups, AttackIds).

%% The defending group only loses whole units from damage; damage is
%% always dealt in such a way that it kills the most units possible,
%% and any remaining damage to a unit that does not immediately kill
%% it is ignored. For example, if a defending group contains 10 units
%% with 10 hit points each and receives 75 damage, it loses exactly 7
%% units and is left with 3 units at full health.
deal_damage(#{units := Units, hp := HP} = Defender, AD) ->
  KilledUnits = AD div HP,
  %% KilledUnits may be negative here, meaning that the attack
  %% killed all the units in the defending group.
  RemainingUnits =
    if KilledUnits >= Units ->
        0;
       true ->
        Units - KilledUnits
    end,
  Defender#{units => RemainingUnits}.

%% This determines which group gets to choose target first, not which
%% target the group selects.
target_selection_order(G1, G2) ->
  E1 = effective_power(G1),
  E2 = effective_power(G2),
  if E1 /= E2 ->
      E1 =< E2;
     true ->
      %% tie break on initiative
      I1 = maps:get(initiative, G1),
      I2 = maps:get(initiative, G2),
      if I1 /= I2 ->
          I1 =< I2;
         true ->
          throw(no_tie_break)
      end
  end.

is_immune(DefendGroup, AttackType) ->
  ImmuneTo = maps:get(immune_to, DefendGroup),
  lists:member(AttackType, ImmuneTo).

is_weak(DefendGroup, AttackType) ->
  WeakTo = maps:get(weak_to, DefendGroup),
  lists:member(AttackType, WeakTo).

%% Compute the attack damage.
attack_damage(AttackGroup, DefendGroup) ->
  AttackType = maps:get(damagetype, AttackGroup),
  EP = effective_power(AttackGroup),
  case is_immune(DefendGroup, AttackType) of
    true ->
      0;
    false ->
      case is_weak(DefendGroup, AttackType) of
        false ->
          EP;
        true ->
          EP * 2
      end
  end.

%% Returns true iff G is an available target.
is_available_target(G, AttackingArmy, TargetSelections) ->
  IsFromEnemyArmy = (maps:get(army, G) /= AttackingArmy),
  IsNotAlreadyTargeted =
    not lists:member(maps:get(id, G), maps:values(TargetSelections)),
  IsFromEnemyArmy and IsNotAlreadyTargeted.

lookup_group_by_id(Id, GroupList) ->
  [Group] = lists:filter(fun(G) ->
                             maps:get(id, G) == Id
                         end, GroupList),
  Group.

select_target(Groups) ->
  SortedGroups =
    lists:reverse(lists:sort(fun target_selection_order/2,
                             lists:filter(fun(G) -> maps:get(units, G) > 0 end, Groups))),

  TargetSelections =
    lists:foldl(fun(Group, Acc) ->
                    Id = maps:get(id, Group),

                    RemainingGroups =
                      lists:filter(fun(G) ->
                                       Attacker = maps:get(army, Group),
                                       is_available_target(G, Attacker, Acc)
                                   end, SortedGroups),

                    %% RemainingGroupIds = lists:map(fun(G) -> maps:get(id, G) end, RemainingGroups),

                    case select_group_target(Group, RemainingGroups) of
                      notarget -> Acc;
                      Target -> maps:put(Id, maps:get(id, Target), Acc)
                    end

                end, #{}, SortedGroups),

  TargetSelections.


select_group_target(Group, AvailableEnemyGroups) ->
  %% When a group selects a target to attack, this is the order in
  %% which they select that target. (This is not the order which
  %% determines which group gets to pick target first.)
  Sorted =
    lists:reverse(
      lists:sort(
        fun(A, B) ->
            %% 1. amount of damage dealt
            AD1 = attack_damage(Group, A),
            AD2 = attack_damage(Group, B),
            if AD1 /= AD2 ->
                AD1 =< AD2;
               true ->
                %% 2. defending group's effective power
                EP1 = effective_power(A),
                EP2 = effective_power(B),
                if EP1 /= EP2 ->
                    EP1 =< EP2;
                   true ->
                    %% 3. defending group with highest initiative
                    Init1 = maps:get(initiative, A),
                    Init2 = maps:get(initiative, B),
                    Init1 =< Init2
                end
            end
        end,
        AvailableEnemyGroups)),

  %% If a group cannot deal any damage, it should not select a
  %% target.
  SortedWithDamage =
    lists:filter(fun(G) ->
                     attack_damage(Group, G) /= 0
                 end, Sorted),

  case SortedWithDamage of
    [Target|_] ->
      Target;
    _ ->
      notarget
  end.

%% Return groups in the order in which they perform their attacks.
get_groups_in_attack_order(Groups) ->
  lists:reverse(lists:sort(fun(A, B)  ->
                               maps:get(initiative, A) =< maps:get(initiative, B)
                           end, Groups)).

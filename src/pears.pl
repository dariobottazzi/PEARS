%
% PEARS - Prolog Event-Action Rule System, Copyright 2025, Dario Bottazzi
%
% Redistribution and use in source and binary forms, with or without modification, are permitted  provided  that  the
% following conditions are met:
% 1.  Redistributions  of  source  code  must  retain  the above  copyright  notice,  this list of conditions and the 
% following disclaimer.
% 2.  Redistributions in binary form must reproduce the above copyright notice,  this  list  of  conditions  and  the 
% following disclaimer in the documentation and/or other materials provided with the distribution.
% 3.  Neither the name of the copyright holder nor the names of  its contributors  may be used to endorse or  promote 
% products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS  AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, 
% INCLUDING, BUT NOT LIMITED TO,  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  ARE 
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,  PROCUREMENT  OF SUBSTITUTE  GOODS  OR 
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY  OF  LIABILITY, 
% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY  WAY OUT OF THE 
% USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
% =============================================================================
% PEARS implements Event-Condition-Action rules for Prolog
% A comprehensive rule engine that processes events and executes actions
% when specified conditions are met. 
% 
% Key Features:
% - Dynamic rule management with add/remove/update operations
% - Flexible event pattern matching with unification
% - Compound condition evaluation (AND, OR, NOT, arithmetic)
% - Working memory for persistent fact storage
% - Rete algorithm implementation for efficient rule matching
% - Comprehensive testing framework with proper pass/fail logic
% - Event history and logging capabilities
% - Interactive help system with complete command reference
% =============================================================================

% Main rule definition predicate
% rule(RuleId, Event, Condition, Action)
% - RuleId: unique identifier for the rule
% - Event: the trigger event
% - Condition: condition that must be satisfied
% - Action: action to execute when event occurs and condition is met

:- dynamic rule/4.
:- dynamic fact/1.
:- dynamic event_history/2.

% ===================================================================
% Core Rule Engine
% ===================================================================

% Process an event: check all rules and execute matching actions
process_event(Event) :-
    process_event(Event, []).

% Process event with context (additional facts or variables)
process_event(Event, Context) :-
    log_event(Event),
    findall(
        action(RuleId, Action),
        (
            rule(RuleId, EventPattern, Condition, Action),
            match_event(Event, EventPattern),
            evaluate_condition(Condition, Context)
        ),
        Actions
    ),
    execute_actions(Actions, Context).

% Match event against pattern (supports unification)
match_event(Event, EventPattern) :-
    Event =.. [EventName|EventArgs],
    EventPattern =.. [PatternName|PatternArgs],
    EventName = PatternName,
    match_args(EventArgs, PatternArgs).

match_args([], []).
match_args([H1|T1], [H2|T2]) :-
    (var(H2) -> H1 = H2 ; H1 = H2),
    match_args(T1, T2).

% Evaluate condition with context
evaluate_condition(true, _) :- !.
evaluate_condition(Condition, Context) :-
    (   append(Context, [], _) ->
        maplist(assert_temp, Context)
    ;   true
    ),
    call(Condition),
    (   append(Context, [], _) ->
        maplist(retract_temp, Context)
    ;   true
    ).

assert_temp(Fact) :- assertz(Fact).
retract_temp(Fact) :- retract(Fact).

% Execute list of actions
execute_actions([], _).
execute_actions([action(RuleId, Action)|Rest], Context) :-
    execute_action(Action, RuleId, Context),
    execute_actions(Rest, Context).

% Execute individual action
execute_action(Action, RuleId, Context) :-
    (   callable(Action) ->
        (   catch(call(Action), Error, handle_action_error(RuleId, Action, Error)) ->
            log_action_success(RuleId, Action)
        ;   log_action_failure(RuleId, Action)
        )
    ;   log_action_invalid(RuleId, Action)
    ).

% ===================================================================
% Rule Management
% ===================================================================

% Add a new rule
add_rule(RuleId, Event, Condition, Action) :-
    (   rule(RuleId, _, _, _) ->
        format('Warning: Rule ~w already exists. Use update_rule/4 to modify.~n', [RuleId]),
        fail
    ;   assertz(rule(RuleId, Event, Condition, Action)),
        format('Rule ~w added successfully.~n', [RuleId])
    ).

% Remove a rule
remove_rule(RuleId) :-
    (   retract(rule(RuleId, _, _, _)) ->
        format('Rule ~w removed successfully.~n', [RuleId])
    ;   format('Rule ~w not found.~n', [RuleId])
    ).

% Update an existing rule
update_rule(RuleId, Event, Condition, Action) :-
    (   retract(rule(RuleId, _, _, _)) ->
        assertz(rule(RuleId, Event, Condition, Action)),
        format('Rule ~w updated successfully.~n', [RuleId])
    ;   format('Rule ~w not found. Use add_rule/4 to create new rule.~n', [RuleId])
    ).

% List all rules
list_rules :-
    findall(rule(Id, Event, Condition, Action), rule(Id, Event, Condition, Action), Rules),
    (   Rules = [] ->
        format('No rules defined.~n')
    ;   format('Current rules:~n'),
        forall(member(rule(Id, Event, Condition, Action), Rules),
               format('  Rule ~w: ~w -> ~w if ~w~n', [Id, Event, Action, Condition]))
    ).

% Check if a rule exists
rule_exists(RuleId) :-
    rule(RuleId, _, _, _).

% ===================================================================
% Fact Management
% ===================================================================

% Add a fact to the knowledge base
add_fact(Fact) :-
    (   fact(Fact) ->
        format('Fact ~w already exists.~n', [Fact])
    ;   assertz(fact(Fact)),
        format('Fact ~w added.~n', [Fact])
    ).

% Remove a fact from the knowledge base
remove_fact(Fact) :-
    (   retract(fact(Fact)) ->
        format('Fact ~w removed.~n', [Fact])
    ;   format('Fact ~w not found.~n', [Fact])
    ).

% List all facts
list_facts :-
    findall(Fact, fact(Fact), Facts),
    (   Facts = [] ->
        format('No facts in knowledge base.~n')
    ;   format('Current facts:~n'),
        forall(member(Fact, Facts), format('  ~w~n', [Fact]))
    ).

% ===================================================================
% Event History and Logging
% ===================================================================

% Log an event with timestamp
log_event(Event) :-
    get_time(Timestamp),
    assertz(event_history(Event, Timestamp)).

% Get event history
get_event_history(Events) :-
    findall(event(Event, Time), event_history(Event, Time), Events).

% Clear event history
clear_event_history :-
    retractall(event_history(_, _)),
    format('Event history cleared.~n').

% Logging predicates
log_action_success(RuleId, Action) :-
    format('Rule ~w: Action ~w executed successfully.~n', [RuleId, Action]).

log_action_failure(RuleId, Action) :-
    format('Rule ~w: Action ~w failed to execute.~n', [RuleId, Action]).

log_action_invalid(RuleId, Action) :-
    format('Rule ~w: Invalid action ~w.~n', [RuleId, Action]).

handle_action_error(RuleId, Action, Error) :-
    format('Rule ~w: Error executing action ~w: ~w~n', [RuleId, Action, Error]).

% ===================================================================
% Rule Utilities and Helpers
% ===================================================================

% Test if an event would trigger any rules (without executing actions)
would_trigger(Event, TriggeredRules) :-
    findall(
        RuleId,
        (
            rule(RuleId, EventPattern, Condition, _),
            match_event(Event, EventPattern),
            evaluate_condition(Condition, [])
        ),
        TriggeredRules
    ).

% Find rules by event pattern
rules_for_event(EventPattern, Rules) :-
    findall(
        rule(Id, Event, Condition, Action),
        (
            rule(Id, Event, Condition, Action),
            match_event(EventPattern, Event)
        ),
        Rules
    ).

% Enable/disable rules (by adding/removing enabled/1 condition)
enable_rule(RuleId) :-
    add_fact(enabled(RuleId)).

disable_rule(RuleId) :-
    remove_fact(enabled(RuleId)).

% Check if rule is enabled (use in conditions)
is_enabled(RuleId) :-
    fact(enabled(RuleId)).

% ===================================================================
% Example Usage and Templates
% ===================================================================

% Example rules demonstrating the system
load_examples :-
    % Simple rule: if temperature is high, turn on AC
    add_rule(temp_control, 
             temperature_reading(Temp), 
             Temp > 25, 
             turn_on_ac),
    
    % Rule with multiple conditions
    add_rule(security_alert,
             motion_detected(Location),
             (fact(armed), \+ fact(authorized_person(Location))),
             send_security_alert(Location)),
    
    % Rule that creates new facts
    add_rule(stock_low,
             item_sold(Item, Quantity),
             (fact(stock(Item, CurrentStock)), NewStock is CurrentStock - Quantity, NewStock < 10),
             (retract(fact(stock(Item, _))), add_fact(stock(Item, NewStock)), add_fact(reorder_needed(Item)))),
    
    % Enable all example rules
    enable_rule(temp_control),
    enable_rule(security_alert),
    enable_rule(stock_low),
    
    format('Example rules loaded.~n').

% Clear all rules and facts (useful for testing)
clear_all :-
    retractall(rule(_, _, _, _)),
    retractall(fact(_)),
    retractall(event_history(_, _)),
    format('All rules, facts, and event history cleared.~n').

% ===================================================================
% API Summary for Users
% ===================================================================

% Main predicates to use:
% - add_rule(Id, Event, Condition, Action): Add a new rule
% - process_event(Event): Process an event and execute matching actions
% - add_fact(Fact): Add a fact to the knowledge base
% - list_rules: Show all rules
% - list_facts: Show all facts
% - load_examples: Load example rules for testing

help :-
    format('PEARS - Prolog Event-Action Rule System - Available Commands:~n'),
    format('~nBasic Rule Management:~n'),
    format('  add_rule(Id, Event, Condition, Action) - Add a new rule~n'),
    format('  remove_rule(Id) - Remove a rule~n'),
    format('  update_rule(Id, Event, Condition, Action) - Update existing rule~n'),
    format('  list_rules - List all rules~n'),
    format('  process_event(Event) - Process an event~n'),
    format('  would_trigger(Event, Rules) - Test which rules an event would trigger~n'),
    format('  enable_rule(Id)/disable_rule(Id) - Enable/disable rules~n'),
    format('~nFact Management:~n'),
    format('  add_fact(Fact) - Add a fact~n'),
    format('  remove_fact(Fact) - Remove a fact~n'),
    format('  list_facts - List all facts~n'),
    format('~nExamples and Testing:~n'),
    format('  load_examples - Load example rules~n'),
    format('  load_compound_examples - Load compound condition examples~n'),
    format('  test_compound_conditions - Test compound conditions~n'),
    format('  clear_all - Clear all rules and facts~n'),
    format('~nRete Algorithm Commands:~n'),
    format('  init_rete - Initialize Rete network~n'),
    format('  build_rete_network - Rebuild Rete network~n'),
    format('  process_event_rete(Event) - Process event with Rete~n'),
    format('  add_rule_rete(Id, Event, Condition, Action) - Add rule with Rete~n'),
    format('  add_fact_rete(Fact) - Add fact with Rete optimization~n'),
    format('  rete_stats - Show Rete network statistics~n'),
    format('  show_rete_network - Show detailed network structure~n'),
    format('  show_rete_network_detailed - Show network with memory contents~n'),
    format('  invalidate_rete_network - Force network rebuild~n'),
    format('  test_consecutive - Test consecutive events~n'),
    format('  test_consecutive_fresh - Test consecutive events with fresh processing~n'),
    format('  test_consecutive_persistent - Test consecutive events with persistent memory~n'),
    format('~nCompound Conditions Support:~n'),
    format('  - Logical AND: (Cond1, Cond2, Cond3)~n'),
    format('  - Logical OR: (Cond1; Cond2; Cond3)~n'),
    format('  - Negation: \\+ Condition~n'),
    format('  - Arithmetic: Var > 25, X is Y + 1~n'),
    format('  - If-then-else: (Test -> Then; Else)~n'),
    format('  - Nested: ((A, B); (C, D))~n'),
    format('~nSystem Commands:~n'),
    format('  help - Show this help~n').

% ===================================================================
% RETE ALGORITHM IMPLEMENTATION
% Optimized rule matching using the Rete algorithm
% ===================================================================

% Rete network node types
:- dynamic alpha_node/3.        % alpha_node(NodeId, Pattern, Facts)
:- dynamic beta_node/4.         % beta_node(NodeId, LeftParent, RightParent, Matches)
:- dynamic production_node/3.   % production_node(NodeId, RuleId, Action)
:- dynamic beta_to_production/2. % beta_to_production(BetaNodeId, ProductionNodeId)
:- dynamic rule_condition/2.    % rule_condition(RuleId, Condition)
:- dynamic rete_network/1.      % rete_network(built/not_built)
:- dynamic working_memory/1.    % working_memory(Fact)
:- dynamic alpha_memory/2.      % alpha_memory(NodeId, MatchingFacts)
:- dynamic beta_memory/2.       % beta_memory(NodeId, MatchingTokens)
:- dynamic node_counter/1.      % node_counter(NextId)

% Initialize Rete system
init_rete :-
    retractall(alpha_node(_, _, _)),
    retractall(beta_node(_, _, _, _)),
    retractall(production_node(_, _, _)),
    retractall(beta_to_production(_, _)),
    retractall(rule_condition(_, _)),
    retractall(rete_network(_)),
    retractall(working_memory(_)),
    retractall(alpha_memory(_, _)),
    retractall(beta_memory(_, _)),
    retractall(node_counter(_)),
    assertz(node_counter(1)),
    assertz(rete_network(not_built)),
    format('Rete network initialized.~n').

% Get next node ID
next_node_id(Id) :-
    retract(node_counter(Id)),
    NextId is Id + 1,
    assertz(node_counter(NextId)).

% ===================================================================
% Rete Network Construction
% ===================================================================

% Build the Rete network from all current rules
build_rete_network :-
    init_rete,
    findall(rule(Id, Event, Condition, Action), rule(Id, Event, Condition, Action), Rules),
    length(Rules, RuleCount),
    maplist(compile_rule_to_rete, Rules),
    retractall(rete_network(_)),
    assertz(rete_network(built)),
    format('Rete network built with ~w rules.~n', [RuleCount]).

% Compile a single rule into Rete network nodes
compile_rule_to_rete(rule(RuleId, Event, Condition, Action)) :-
    % Create alpha node for the event pattern
    next_node_id(AlphaId),
    assertz(alpha_node(AlphaId, Event, [])),
    assertz(alpha_memory(AlphaId, [])),
    
    % Parse condition into conjuncts
    parse_condition(Condition, Conjuncts),
    
    % Create alpha nodes for each condition conjunct that's a fact pattern
    create_condition_nodes(Conjuncts, ConditionNodes),
    
    % Create beta network to join event with conditions
    AllNodes = [AlphaId|ConditionNodes],
    create_beta_network(AllNodes, BetaNodeId),
    
    % Create production node
    next_node_id(ProdId),
    assertz(production_node(ProdId, RuleId, Action)),
    assertz(beta_memory(ProdId, [])),
    
    % Link beta network to production - store the connection
    assertz(beta_to_production(BetaNodeId, ProdId)),
    
    % Store the full condition for evaluation
    assertz(rule_condition(RuleId, Condition)).

% Parse condition into list of conjuncts
parse_condition(true, []) :- !.
parse_condition((A, B), Conjuncts) :- !,
    parse_condition(A, ConjunctsA),
    parse_condition(B, ConjunctsB),
    append(ConjunctsA, ConjunctsB, Conjuncts).
parse_condition(Condition, [Condition]).

% Create alpha nodes for fact patterns in conditions
create_condition_nodes([], []).
create_condition_nodes([Conjunct|Rest], [NodeId|RestNodes]) :-
    is_fact_pattern(Conjunct), !,
    next_node_id(NodeId),
    assertz(alpha_node(NodeId, Conjunct, [])),
    assertz(alpha_memory(NodeId, [])),
    create_condition_nodes(Rest, RestNodes).
create_condition_nodes([_|Rest], RestNodes) :-
    % Skip non-fact patterns (arithmetic, etc.)
    create_condition_nodes(Rest, RestNodes).

% Check if a condition is a fact pattern
is_fact_pattern(fact(Pattern)) :- 
    compound(Pattern), !.
is_fact_pattern(Pattern) :-
    compound(Pattern),
    Pattern \= (_>_),
    Pattern \= (_<_),
    Pattern \= (_=_),
    Pattern \= (_is_),
    Pattern \= (\+_).

% Create beta network for joining multiple alpha nodes
create_beta_network([NodeId], NodeId) :- !.
create_beta_network([Left, Right|Rest], FinalNodeId) :-
    next_node_id(BetaId),
    assertz(beta_node(BetaId, Left, Right, [])),
    assertz(beta_memory(BetaId, [])),
    create_beta_network([BetaId|Rest], FinalNodeId).

% Link beta network to production node
link_to_production(BetaNodeId, ProdId) :-
    % This creates the final connection - could be enhanced with more metadata
    true.

% ===================================================================
% Rete Network Execution
% ===================================================================

% Process event using Rete network
process_event_rete(Event) :-
    (   rete_network(built) ->
        true
    ;   build_rete_network
    ),
    add_to_working_memory(Event),
    propagate_through_alpha_nodes(Event).
    % Note: Rule activation and execution happens during propagation

% Collect and execute activated rules (placeholder - rules are executed during propagation)
collect_and_execute_activated_rules :-
    % In our current implementation, rules are executed immediately during
    % the propagation phase in activate_production/4, so this is a no-op
    true.

% Add fact to working memory
add_to_working_memory(Fact) :-
    % Don't add if already exists - this prevents duplicate processing
    (   working_memory(Fact) ->
        format('~w already in working memory - skipping~n', [Fact])
    ;   assertz(working_memory(Fact)),
        format('Added ~w to working memory~n', [Fact])
    ).

% Remove fact from working memory
remove_from_working_memory(Fact) :-
    retract(working_memory(Fact)),
    retract_from_alpha_nodes(Fact),
    retract_from_beta_nodes(Fact).

% Propagate new fact through alpha nodes
propagate_through_alpha_nodes(Fact) :-
    forall(
        (alpha_node(NodeId, Pattern, _), matches_pattern(Fact, Pattern)),
        add_to_alpha_memory(NodeId, Fact)
    ).

% Check if fact matches pattern
matches_pattern(Fact, Pattern) :-
    catch(
        (copy_term(Pattern, PatternCopy),
         Fact = PatternCopy),
        Error,
        (format('Pattern matching error: ~w~n', [Error]), fail)
    ).

% Add fact to alpha node memory
add_to_alpha_memory(NodeId, Fact) :-
    % Check if fact is already in this alpha node's memory
    (   alpha_memory(NodeId, Facts),
        member(Fact, Facts) ->
        format('~w already in alpha node ~w - skipping~n', [Fact, NodeId])
    ;   % Process the fact - it's new to this alpha node
        format('Processing ~w in alpha node ~w~n', [Fact, NodeId]),
        propagate_to_beta_nodes(NodeId, Fact),
        % Also check for direct production node connections
        check_direct_production_activation(NodeId, Fact),
        % Update memory
        (   alpha_memory(NodeId, Facts) ->
            retract(alpha_memory(NodeId, Facts)),
            assertz(alpha_memory(NodeId, [Fact|Facts]))
        ;   assertz(alpha_memory(NodeId, [Fact]))
        )
    ).

% Check for direct alpha->production connections (rules with simple conditions)
check_direct_production_activation(AlphaNodeId, Fact) :-
    forall(
        beta_to_production(AlphaNodeId, ProdId),
        (production_node(ProdId, RuleId, Action),
         Token = token([Fact]),
         activate_production(ProdId, RuleId, Action, Token))
    ).

% Propagate to beta nodes
propagate_to_beta_nodes(AlphaNodeId, Fact) :-
    forall(
        beta_node(BetaId, AlphaNodeId, RightParent, _),
        join_in_beta_node(BetaId, left, Fact, RightParent)
    ),
    forall(
        beta_node(BetaId, LeftParent, AlphaNodeId, _),
        join_in_beta_node(BetaId, right, Fact, LeftParent)
    ).

% Join operation in beta node
join_in_beta_node(BetaId, Side, NewFact, OtherParent) :-
    (   alpha_memory(OtherParent, OtherFacts) ->
        forall(
            member(OtherFact, OtherFacts),
            (   Side = left ->
                create_token(NewFact, OtherFact, Token)
            ;   create_token(OtherFact, NewFact, Token)
            ),
            add_to_beta_memory(BetaId, Token)
        )
    ;   true
    ).

% Create token (combination of facts)
create_token(Fact1, Fact2, token(Fact1, Fact2)).

% Add token to beta memory
add_to_beta_memory(BetaId, Token) :-
    % Check if token already exists in beta memory
    (   beta_memory(BetaId, Tokens),
        member(Token, Tokens) ->
        format('Token ~w already in beta node ~w - skipping~n', [Token, BetaId])
    ;   % Process new token
        format('Processing token ~w in beta node ~w~n', [Token, BetaId]),
        propagate_beta_token(BetaId, Token),
        % Update memory
        (   beta_memory(BetaId, Tokens) ->
            retract(beta_memory(BetaId, Tokens)),
            assertz(beta_memory(BetaId, [Token|Tokens]))
        ;   assertz(beta_memory(BetaId, [Token]))
        )
    ).

% Propagate token to next beta level or production nodes
propagate_beta_token(BetaId, Token) :-
    % Check if this feeds into another beta node
    forall(
        beta_node(NextBetaId, BetaId, RightParent, _),
        join_token_with_alpha(NextBetaId, Token, RightParent)
    ),
    forall(
        beta_node(NextBetaId, LeftParent, BetaId, _),
        join_alpha_with_token(NextBetaId, LeftParent, Token)
    ),
    % Check if this beta node connects to a production node
    forall(
        beta_to_production(BetaId, ProdId),
        (production_node(ProdId, RuleId, Action),
         activate_production(ProdId, RuleId, Action, Token))
    ).

% Join token with alpha memory
join_token_with_alpha(BetaId, Token, AlphaId) :-
    alpha_memory(AlphaId, Facts),
    forall(
        member(Fact, Facts),
        (create_extended_token(Token, Fact, NewToken),
         add_to_beta_memory(BetaId, NewToken))
    ).

% Join alpha with token
join_alpha_with_token(BetaId, AlphaId, Token) :-
    alpha_memory(AlphaId, Facts),
    forall(
        member(Fact, Facts),
        (create_extended_token(Fact, Token, NewToken),
         add_to_beta_memory(BetaId, NewToken))
    ).

% Create extended token
create_extended_token(token(Facts), NewFact, token([NewFact|Facts])) :- !.
create_extended_token(Fact, token(Facts), token([Fact|Facts])) :- !.
create_extended_token(Fact1, Fact2, token([Fact1, Fact2])).

% Activate production node
activate_production(ProdId, RuleId, Action, Token) :-
    format('Activating production ~w for rule ~w with token ~w~n', [ProdId, RuleId, Token]),
    % Use a simpler approach: re-evaluate the rule using the original system
    Token = token(Facts),
    Facts = [Event|_],
    rule(RuleId, EventPattern, Condition, _),
    % Use the original match_event and evaluate_condition from the base system
    (   match_event(Event, EventPattern),
        evaluate_condition(Condition, []) ->
        format('Rule ~w condition satisfied - executing action~n', [RuleId]),
        execute_action_rete(Action, RuleId, Token)
    ;   format('Rule ~w condition not satisfied~n', [RuleId]),
        fail
    ).

% Execute action with Rete context
execute_action_rete(Action, RuleId, Token) :-
    execute_action(Action, RuleId, Token).
    
% Simple consecutive event test
test_consecutive :-
    format('Testing consecutive events...~n'),
    clear_all,
    init_rete,
    add_rule(temp_rule, temperature(Temp), Temp > 25, turn_on_ac),
    build_rete_network,
    format('~n=== First event ===~n'),
    process_event_rete(temperature(30)),
    format('~n=== Second event (same) ===~n'),
    process_event_rete(temperature(30)),
    format('~n=== Third event (different) ===~n'),
    process_event_rete(temperature(35)),
    format('~n=== Fourth event (same as first) ===~n'),
    process_event_rete(temperature(30)).

% Clear working memory to allow consecutive identical events
clear_working_memory :-
    retractall(working_memory(_)),
    format('Working memory cleared.~n').

% Process event and clear working memory afterwards (for consecutive events)
process_event_rete_fresh(Event) :-
    clear_working_memory,
    process_event_rete(Event).

% Test consecutive events with memory clearing
test_consecutive_fresh :-
    format('Testing consecutive events with fresh processing...~n'),
    clear_all,
    init_rete,
    add_rule(temp_rule, temperature(Temp), Temp > 25, turn_on_ac),
    build_rete_network,
    format('~n=== First event ===~n'),
    process_event_rete_fresh(temperature(30)),
    format('~n=== Second event (same, but fresh) ===~n'),
    process_event_rete_fresh(temperature(30)),
    format('~n=== Third event (different) ===~n'),
    process_event_rete_fresh(temperature(35)),
    format('~n=== Fourth event (same as first, but fresh) ===~n'),
    process_event_rete_fresh(temperature(30)).

% Test with persistent memory (original behavior)
test_consecutive_persistent :-
    format('Testing consecutive events with persistent memory...~n'),
    clear_all,
    init_rete,
    add_rule(temp_rule, temperature(Temp), Temp > 25, turn_on_ac),
    build_rete_network,
    format('~n=== First event ===~n'),
    process_event_rete(temperature(30)),
    format('~n=== Second event (same, should be skipped) ===~n'),
    process_event_rete(temperature(30)),
    format('~n=== Third event (different) ===~n'),
    process_event_rete(temperature(35)),
    format('~n=== Fourth event (same as first, should be skipped) ===~n'),
    process_event_rete(temperature(30)).

% ===================================================================
% Rete Network Maintenance
% ===================================================================

% Retract fact from alpha nodes
retract_from_alpha_nodes(Fact) :-
    forall(
        alpha_memory(NodeId, Facts),
        (   member(Fact, Facts) ->
            select(Fact, Facts, NewFacts),
            retract(alpha_memory(NodeId, Facts)),
            assertz(alpha_memory(NodeId, NewFacts))
        ;   true
        )
    ).

% Retract from beta nodes (simplified)
retract_from_beta_nodes(Fact) :-
    forall(
        beta_memory(NodeId, Tokens),
        (   include(token_contains_fact(Fact), Tokens, ToRemove),
            ToRemove \= [] ->
            subtract(Tokens, ToRemove, NewTokens),
            retract(beta_memory(NodeId, Tokens)),
            assertz(beta_memory(NodeId, NewTokens))
        ;   true
        )
    ).

% Check if token contains a specific fact
token_contains_fact(Fact, token(Facts)) :-
    member(Fact, Facts).
token_contains_fact(Fact, token(Fact1, Fact2)) :-
    (Fact = Fact1 ; Fact = Fact2).

% Rebuild network when rules change
invalidate_rete_network :-
    retractall(rete_network(_)),
    assertz(rete_network(not_built)),
    format('Rete network invalidated - will rebuild on next event.~n').

% ===================================================================
% Enhanced Rule Management with Rete Integration
% ===================================================================

% Override add_rule to invalidate Rete network
add_rule_rete(RuleId, Event, Condition, Action) :-
    add_rule(RuleId, Event, Condition, Action),
    invalidate_rete_network.

% Override remove_rule to invalidate Rete network  
remove_rule_rete(RuleId) :-
    remove_rule(RuleId),
    invalidate_rete_network.

% Override update_rule to invalidate Rete network
update_rule_rete(RuleId, Event, Condition, Action) :-
    update_rule(RuleId, Event, Condition, Action),
    invalidate_rete_network.

% Enhanced fact management with Rete
add_fact_rete(Fact) :-
    add_fact(Fact),
    (   rete_network(built) ->
        process_event_rete(Fact)
    ;   true
    ).

remove_fact_rete(Fact) :-
    remove_fact(Fact),
    (   rete_network(built) ->
        remove_from_working_memory(Fact)
    ;   true
    ).

% ===================================================================
% Rete Performance Monitoring
% ===================================================================

% Show Rete network statistics
rete_stats :-
    findall(_, alpha_node(_, _, _), AlphaNodes),
    findall(_, beta_node(_, _, _, _), BetaNodes), 
    findall(_, production_node(_, _, _), ProdNodes),
    findall(_, working_memory(_), WorkingMem),
    length(AlphaNodes, AlphaCount),
    length(BetaNodes, BetaCount),
    length(ProdNodes, ProdCount),
    length(WorkingMem, MemCount),
    format('Rete Network Statistics:~n'),
    format('  Alpha nodes: ~w~n', [AlphaCount]),
    format('  Beta nodes: ~w~n', [BetaCount]),
    format('  Production nodes: ~w~n', [ProdCount]),
    format('  Working memory facts: ~w~n', [MemCount]),
    (   rete_network(built) ->
        format('  Network status: Built~n')
    ;   format('  Network status: Not built~n')
    ).

% Show detailed network structure
show_rete_network :-
    format('Alpha Nodes:~n'),
    forall(alpha_node(Id, Pattern, _),
           format('  Node ~w: ~w~n', [Id, Pattern])),
    format('Beta Nodes:~n'),
    forall(beta_node(Id, Left, Right, _),
           format('  Node ~w: ~w join ~w~n', [Id, Left, Right])),
    format('Production Nodes:~n'),
    forall(production_node(Id, RuleId, Action),
           format('  Node ~w: Rule ~w -> ~w~n', [Id, RuleId, Action])).

% Enhanced network display showing actual facts in memory
show_rete_network_detailed :-
    format('Alpha Nodes with Memory Contents:~n'),
    forall(alpha_node(Id, Pattern, _),
           (format('  Node ~w: Pattern ~w~n', [Id, Pattern]),
            (alpha_memory(Id, Facts) ->
                (Facts = [] ->
                    format('    Memory: empty~n')
                ;   forall(member(Fact, Facts),
                           format('    Memory: ~w~n', [Fact]))
                )
            ;   format('    Memory: not initialized~n')
            )
           )),
    format('Beta Nodes with Memory Contents:~n'),
    forall(beta_node(Id, Left, Right, _),
           (format('  Node ~w: ~w join ~w~n', [Id, Left, Right]),
            (beta_memory(Id, Tokens) ->
                (Tokens = [] ->
                    format('    Memory: empty~n')
                ;   forall(member(Token, Tokens),
                           format('    Memory: ~w~n', [Token]))
                )
            ;   format('    Memory: not initialized~n')
            )
           )),
    format('Production Nodes:~n'),
    forall(production_node(Id, RuleId, Action),
           format('  Node ~w: Rule ~w -> ~w~n', [Id, RuleId, Action])),
    format('Working Memory Contents:~n'),
    (working_memory(_) ->
        forall(working_memory(Fact),
               format('  ~w~n', [Fact]))
    ;   format('  empty~n')
    ).

% ===================================================================
% Updated API with Rete Support
% ===================================================================

% Keep original process_event as default, use process_event_rete explicitly for Rete
% process_event(Event) :-
%     process_event_rete(Event).

% ===================================================================
% COMPOUND CONDITIONS SYSTEM
% Advanced condition evaluation with logical operators
% ===================================================================


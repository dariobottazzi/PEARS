%
% PEARS Complete Example Application: Smart Home Automation System
% This demonstrates all basic PEARS features with actual rule firing
% 
% Load PEARS first: ?- [src/pears].
% Then run: ?- run_home_automation_demo.
%

% ===================================================================
% SMART HOME AUTOMATION SYSTEM
% Complete example showing PEARS basic features in action
% ===================================================================

% Main demo runner - demonstrates all features step by step
run_home_automation_demo :-
    format('~n========================================~n'),
    format('  PEARS Smart Home Automation Demo~n'),
    format('  Demonstrating Basic Features~n'),
    format('========================================~n'),
    
    % Initialize clean environment
    clear_all,
    setup_initial_facts,
    
    % Phase 1: Basic rule management
    demo_rule_management,
    
    % Phase 2: Event processing and rule firing
    demo_event_processing,
    
    % Phase 3: Fact management and working memory
    demo_fact_management,
    
    % Phase 4: Compound conditions
    demo_compound_conditions,
    
    % Phase 5: Rule utilities
    demo_rule_utilities,
    
    % Phase 6: Event history and logging
    demo_event_history,
    
    % Final summary
    demo_summary.

% ===================================================================
% Phase 1: Rule Management Demonstration
% ===================================================================

demo_rule_management :-
    format('~n=== Phase 1: Rule Management ===~n'),
    
    format('Adding home automation rules...~n'),
    
    % Temperature control rule
    add_rule(temp_control,
             temperature_reading(Room, Temp),
             (Temp > 25),
             adjust_temperature(Room, cool)),
    
    % Security system rule
    add_rule(security_alert,
             motion_detected(Location),
             (fact(security_armed), \+ fact(authorized_person_present)),
             trigger_security_alarm(Location)),
    
    % Lighting automation rule
    add_rule(auto_lighting,
             motion_detected(Room),
             (fact(light_level(Room, Level)), Level < 30),
             turn_on_lights(Room)),
    
    % Energy saving rule
    add_rule(energy_saver,
             all_rooms_empty,
             fact(energy_save_mode),
             reduce_power_consumption),
    
    format('Rules added successfully!~n'),
    
    % Show all rules
    format('~nCurrent rules in system:~n'),
    list_rules,
    
    % Demonstrate rule updating
    format('~nUpdating temperature threshold...~n'),
    update_rule(temp_control,
                temperature_reading(Room, Temp),
                (Temp > 23),  % Lower threshold
                adjust_temperature(Room, cool)),
    
    format('Temperature rule updated to trigger at 23°C~n'),
    
    % Demonstrate rule existence check
    (rule_exists(temp_control) ->
        format('✓ Temperature control rule exists~n')
    ;   format('✗ Temperature control rule missing~n')
    ).

% ===================================================================
% Phase 2: Event Processing and Rule Firing
% ===================================================================

demo_event_processing :-
    format('~n=== Phase 2: Event Processing & Rule Firing ===~n'),
    
    % Add some initial facts for rules to work with
    add_fact(light_level(living_room, 20)),
    add_fact(light_level(kitchen, 40)),
    add_fact(security_armed),
    
    format('Processing various home events...~n'),
    
    % Test 1: Temperature event that should trigger cooling
    format('~n--- Test 1: High temperature event ---~n'),
    process_event(temperature_reading(living_room, 26)),
    
    % Test 2: Motion in low-light room (should turn on lights)
    format('~n--- Test 2: Motion in dark room ---~n'),
    process_event(motion_detected(living_room)),
    
    % Test 3: Motion in well-lit room (should not trigger lights)
    format('~n--- Test 3: Motion in bright room ---~n'),
    process_event(motion_detected(kitchen)),
    
    % Test 4: Security event with no authorized person
    format('~n--- Test 4: Security breach ---~n'),
    process_event(motion_detected(garage)),
    
    % Test 5: Temperature below threshold (should not trigger)
    format('~n--- Test 5: Normal temperature ---~n'),
    process_event(temperature_reading(bedroom, 20)),
    
    % Test 6: Energy saving trigger
    format('~n--- Test 6: Energy saving activation ---~n'),
    add_fact(energy_save_mode),
    process_event(all_rooms_empty).

% ===================================================================
% Phase 3: Fact Management and Working Memory
% ===================================================================

demo_fact_management :-
    format('~n=== Phase 3: Fact Management ===~n'),
    
    format('Current facts in working memory:~n'),
    list_facts,
    
    % Add more facts dynamically
    format('~nAdding user presence facts...~n'),
    add_fact(person_present(alice, living_room)),
    add_fact(person_present(bob, kitchen)),
    add_fact(authorized_person_present),
    
    % Show updated facts
    format('~nUpdated facts:~n'),
    list_facts,
    
    % Test security with authorized person present
    format('~n--- Testing security with authorized person ---~n'),
    process_event(motion_detected(front_door)),
    
    % Remove authorization and test again
    format('~nRemoving authorization...~n'),
    remove_fact(authorized_person_present),
    
    format('--- Testing security without authorization ---~n'),
    process_event(motion_detected(back_door)),
    
    % Demonstrate fact-based rule conditions
    format('~nAdding weather-dependent rule...~n'),
    add_rule(weather_ac,
             temperature_reading(Room, Temp),
             (fact(weather(hot)), fact(person_present(_, Room)), Temp > 22),
             activate_air_conditioning(Room)),
    
    add_fact(weather(hot)),
    format('--- Testing weather-dependent cooling ---~n'),
    process_event(temperature_reading(living_room, 24)).

% ===================================================================
% Phase 4: Compound Conditions
% ===================================================================

demo_compound_conditions :-
    format('~n=== Phase 4: Compound Conditions ===~n'),
    
    % Complex rule with AND conditions
    add_rule(smart_ac,
             temperature_reading(Room, Temp),
             (Temp > 24, fact(person_present(_, Room)), \+ fact(windows_open(Room))),
             (close_blinds(Room), activate_ac(Room))),
    
    % Complex rule with OR conditions  
    add_rule(security_notification,
             motion_detected(Location),
             (fact(security_armed); fact(night_mode)),
             send_notification(motion_alert, Location)),
    
    % Rule with arithmetic calculations
    add_rule(energy_optimizer,
             power_consumption(Device, Watts),
             (CurrentHour is round(cputime), CurrentHour > 22, Watts > 100),
             reduce_device_power(Device, 50)),
    
    % Rule with if-then-else logic
    add_rule(smart_thermostat,
             temperature_reading(Room, Temp),
             (fact(person_present(_, Room)) -> 
                 (Temp < 18; Temp > 26) 
             ; 
                 (Temp < 15; Temp > 28)),
             adjust_thermostat(Room, auto)),
    
    format('Added complex rules with compound conditions~n'),
    
    % Test compound conditions
    format('~n--- Testing AND conditions (all must be true) ---~n'),
    add_fact(person_present(alice, bedroom)),
    process_event(temperature_reading(bedroom, 25)),
    
    format('~n--- Testing OR conditions (any can be true) ---~n'),
    add_fact(night_mode),
    process_event(motion_detected(hallway)),
    
    format('~n--- Testing arithmetic conditions ---~n'),
    process_event(power_consumption(heater, 150)),
    
    format('~n--- Testing if-then-else conditions ---~n'),
    process_event(temperature_reading(bedroom, 17)).

% ===================================================================
% Phase 5: Rule Utilities
% ===================================================================

demo_rule_utilities :-
    format('~n=== Phase 5: Rule Utilities ===~n'),
    
    % Test which rules would trigger without executing them
    format('Testing would_trigger utility:~n'),
    would_trigger(temperature_reading(office, 27), TriggeredRules),
    format('Rules that would trigger for office temp 27°C: ~w~n', [TriggeredRules]),
    
    would_trigger(motion_detected(garage), TriggeredRules2),
    format('Rules that would trigger for garage motion: ~w~n', [TriggeredRules2]),
    
    % Find rules by event pattern
    format('~nFinding rules for temperature events:~n'),
    rules_for_event(temperature_reading(_, _), TempRules),
    format('Temperature-related rules: '),
    forall(member(rule(Id, _, _, _), TempRules), format('~w ', [Id])),
    format('~n'),
    
    % Enable/disable rules demonstration
    format('~nTesting rule enable/disable:~n'),
    disable_rule(security_alert),
    format('Disabled security_alert rule~n'),
    
    % Test with disabled rule
    process_event(motion_detected(window)),
    
    % Re-enable the rule
    enable_rule(security_alert),
    format('Re-enabled security_alert rule~n'),
    
    % Add condition to check if rule is enabled
    add_rule(conditional_rule,
             test_event,
             fact(enabled(conditional_rule)),
             conditional_action),
    
    enable_rule(conditional_rule),
    process_event(test_event).

% ===================================================================
% Phase 6: Event History and Logging
% ===================================================================

demo_event_history :-
    format('~n=== Phase 6: Event History & Logging ===~n'),
    
    % Generate some events for history
    process_event(door_opened(front_door)),
    process_event(temperature_reading(living_room, 28)),
    process_event(motion_detected(kitchen)),
    
    % Show event history
    format('Recent event history:~n'),
    get_event_history(Events),
    forall(member(event(Event, Time), Events),
           format('  ~w at time ~w~n', [Event, Time])),
    
    % Clear history demonstration
    format('~nClearing event history...~n'),
    clear_event_history,
    
    format('Generating new events after clear...~n'),
    process_event(window_opened(bedroom)),
    process_event(motion_detected(bathroom)),
    
    format('New event history:~n'),
    get_event_history(NewEvents),
    forall(member(event(Event, Time), NewEvents),
           format('  ~w at time ~w~n', [Event, Time])).

% ===================================================================
% Demo Summary and System State
% ===================================================================

demo_summary :-
    format('~n=== Demo Summary ===~n'),
    
    % Show final system state
    format('Final system state:~n'),
    
    findall(R, rule(R, _, _, _), AllRules),
    length(AllRules, RuleCount),
    format('Total rules defined: ~w~n', [RuleCount]),
    
    findall(F, fact(F), AllFacts),
    length(AllFacts, FactCount),
    format('Total facts in memory: ~w~n', [FactCount]),
    
    get_event_history(FinalEvents),
    length(FinalEvents, EventCount),
    format('Events in history: ~w~n', [EventCount]),
    
    format('~nRule summary:~n'),
    forall(rule(RuleId, Event, _, Action),
           format('  ~w: ~w -> ~w~n', [RuleId, Event, Action])),
    
    format('~n========================================~n'),
    format('  Demo Complete! All features tested.~n'),
    format('  PEARS system is ready for use.~n'),
    format('========================================~n').

% ===================================================================
% Helper Predicates and Initial Setup
% ===================================================================

% Set up initial facts for the demo
setup_initial_facts :-
    format('Setting up initial home state...~n'),
    add_fact(house_mode(normal)),
    add_fact(time_of_day(evening)),
    add_fact(outdoor_temp(30)),
    format('Initial facts configured~n').

% Custom action predicates for demonstration
adjust_temperature(Room, Mode) :-
    format('🌡️  Adjusting temperature in ~w to ~w mode~n', [Room, Mode]).

trigger_security_alarm(Location) :-
    format('🚨 SECURITY ALERT: Motion detected at ~w!~n', [Location]).

turn_on_lights(Room) :-
    format('💡 Turning on lights in ~w~n', [Room]).

reduce_power_consumption :-
    format('⚡ Activating energy saving mode~n').

activate_air_conditioning(Room) :-
    format('❄️  Air conditioning activated in ~w~n', [Room]).

close_blinds(Room) :-
    format('🏠 Closing blinds in ~w~n', [Room]).

activate_ac(Room) :-
    format('❄️  AC system activated in ~w~n', [Room]).

send_notification(Type, Location) :-
    format('📱 Notification: ~w at ~w~n', [Type, Location]).

reduce_device_power(Device, Percent) :-
    format('⚡ Reducing ~w power by ~w%~n', [Device, Percent]).

adjust_thermostat(Room, Mode) :-
    format('🌡️  Smart thermostat: ~w set to ~w~n', [Room, Mode]).

conditional_action :-
    format('✅ Conditional rule executed successfully~n').

% ===================================================================
% Interactive Testing Commands
% ===================================================================

% Quick test command for users
test_basic_features :-
    format('~nRunning quick feature test...~n'),
    clear_all,
    
    % Add a simple rule
    add_rule(test_rule, test_event(X), X > 10, test_action(X)),
    
    % Test it
    process_event(test_event(15)),
    process_event(test_event(5)),
    
    format('Quick test complete!~n').

test_action(Value) :-
    format('Test action executed with value: ~w~n', [Value]).

% Reset demo environment
reset_demo :-
    clear_all,
    format('Demo environment reset~n').

% Show help for demo commands
demo_help :-
    format('Smart Home Demo Commands:~n'),
    format('  run_home_automation_demo - Run complete demo~n'),
    format('  test_basic_features - Quick feature test~n'),
    format('  reset_demo - Reset demo environment~n'),
    format('  demo_help - Show this help~n'),
    format('~nTo run individual phases:~n'),
    format('  demo_rule_management~n'),
    format('  demo_event_processing~n'),
    format('  demo_fact_management~n'),
    format('  demo_compound_conditions~n'),
    format('  demo_rule_utilities~n'),
    format('  demo_event_history~n').

% Auto-load message
:- format('~nSmart Home Automation Demo loaded!~n'),
   format('Run: ?- run_home_automation_demo.~n'),
   format('Help: ?- demo_help.~n~n').

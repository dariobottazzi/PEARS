% Simple ECA Rule Example for PEARS
% This example demonstrates basic Event-Condition-Action rules

% Load PEARS first: ?- [src/pears].

% Example 1: Temperature Monitoring
example_temperature_rules :-
    % Add rule for high temperature alert
    add_rule(temp_high,
             temperature_reading(Temp),
             (Temp > 30),
             send_alert(high_temperature, Temp)),
    
    % Add rule for low temperature alert  
    add_rule(temp_low,
             temperature_reading(Temp),
             (Temp < 10),
             send_alert(low_temperature, Temp)),
             
    % Add rule for normal temperature
    add_rule(temp_normal,
             temperature_reading(Temp),
             (Temp >= 10, Temp =< 30),
             log_message(normal_temperature, Temp)).

% Example 2: Security System
example_security_rules :-
    % Motion detected during night hours
    add_rule(night_motion,
             motion_detected(Location),
             (time_range(22, 6), \+ user_present),
             trigger_alarm(Location)),
    
    % Door opened without authorization
    add_rule(unauthorized_entry,
             door_opened(DoorId),
             (\+ authorized_access(DoorId)),
             security_breach(DoorId)).

% Example 3: Smart Home Automation
example_smart_home_rules :-
    % Turn on lights when motion detected and it's dark
    add_rule(auto_lights,
             motion_detected(room(Room)),
             (light_level(Room, Level), Level < 20),
             turn_on_lights(Room)),
    
    % Energy saving: turn off devices when no one is home
    add_rule(energy_save,
             all_users_left,
             energy_save_mode,
             turn_off_non_essential_devices).

% Demo function to run all examples
run_examples :-
    format('~n=== PEARS Example Demo ===~n'),
    
    % Load temperature rules
    format('~nLoading temperature monitoring rules...~n'),
    example_temperature_rules,
    
    % Test with some temperature readings
    format('~nTesting temperature readings:~n'),
    process_event(temperature_reading(35)),  % Should trigger high temp alert
    process_event(temperature_reading(5)),   % Should trigger low temp alert
    process_event(temperature_reading(22)),  % Should log normal temp
    
    % Load security rules
    format('~nLoading security rules...~n'),
    example_security_rules,
    
    % Add some facts to working memory
    add_fact(time_range(23, 6)),  % Night time
    add_fact(user_present :- false),
    
    % Test security events
    format('~nTesting security events:~n'),
    process_event(motion_detected(living_room)),
    process_event(door_opened(front_door)),
    
    % Load smart home rules
    format('~nLoading smart home rules...~n'),
    example_smart_home_rules,
    
    % Test smart home events
    add_fact(light_level(kitchen, 15)),
    add_fact(energy_save_mode),
    
    format('~nTesting smart home events:~n'),
    process_event(motion_detected(room(kitchen))),
    process_event(all_users_left),
    
    format('~n=== Demo Complete ===~n').

% Helper predicate to show current rules
show_demo_rules :-
    format('~nCurrent rules in system:~n'),
    list_rules.

% Helper to reset the demo
reset_demo :-
    % Remove all demo rules
    remove_rule(temp_high),
    remove_rule(temp_low), 
    remove_rule(temp_normal),
    remove_rule(night_motion),
    remove_rule(unauthorized_entry),
    remove_rule(auto_lights),
    remove_rule(energy_save),
    % Clear working memory
    retractall(event(_)),
    retractall(fact(_)),
    format('Demo environment reset.~n').

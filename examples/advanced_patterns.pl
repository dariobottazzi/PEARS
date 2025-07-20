% Advanced PEARS Example: Complex Event Processing
% Demonstrates compound conditions, event patterns, and Rete network usage

% Load PEARS first: ?- [src/pears].

% Example 1: Complex Financial Trading Rules
trading_rules :-
    % Rule 1: Buy signal when price crosses above moving average with volume
    add_rule(buy_signal,
             price_update(Symbol, Price),
             (Price > moving_average(Symbol, 20),
              volume(Symbol, Vol), Vol > 1000000,
              \+ position(Symbol, long)),
             execute_trade(buy, Symbol, Price)),
    
    % Rule 2: Sell signal for risk management
    add_rule(sell_signal,
             price_update(Symbol, Price),
             (position(Symbol, long, EntryPrice),
              (Price < EntryPrice * 0.95;  % Stop loss
               Price > EntryPrice * 1.15)), % Take profit
             execute_trade(sell, Symbol, Price)),
    
    % Rule 3: Market volatility alert
    add_rule(volatility_alert,
             market_data(Symbol, Price, Volume, Time),
             (price_change_percent(Symbol, Change),
              abs(Change) > 5),
             send_alert(high_volatility, Symbol, Change)).

% Example 2: Multi-sensor IoT System
iot_monitoring_rules :-
    % Rule 1: Fire detection with multiple sensors
    add_rule(fire_detection,
             sensor_reading(SensorId, Type, Value),
             (Type = temperature, Value > 60,
              smoke_detected(nearby(SensorId)),
              \+ maintenance_mode(SensorId)),
             emergency_alert(fire, SensorId)),
    
    % Rule 2: Equipment failure prediction
    add_rule(equipment_failure,
             equipment_status(DeviceId, Status),
             (Status = degraded,
              failure_history(DeviceId, Count), Count > 3,
              time_since_maintenance(DeviceId, Days), Days > 90),
             schedule_maintenance(DeviceId, urgent)),
    
    % Rule 3: Energy optimization
    add_rule(energy_optimize,
             power_consumption(Building, Watts),
             (Watts > threshold(Building),
              occupancy(Building, People), People < 10,
              \+ critical_systems_running(Building)),
             reduce_power(Building, 20)).

% Example 3: Network Security with Pattern Detection
security_monitoring_rules :-
    % Rule 1: DDoS attack detection
    add_rule(ddos_detection,
             connection_attempt(IP, Port, Time),
             (connection_count(IP, Count, last_minute),
              Count > 100),
             block_ip(IP, temporary)),
    
    % Rule 2: Suspicious login pattern
    add_rule(suspicious_login,
             login_attempt(User, IP, Success),
             (Success = false,
              failed_attempts(User, Attempts), Attempts > 5,
              geographic_distance(IP, last_successful_login(User)) > 1000),
             lock_account(User, suspicious_activity)),
    
    % Rule 3: Data exfiltration detection
    add_rule(data_exfiltration,
             file_access(User, File, Action),
             (Action = download,
              file_size(File, Size), Size > 100000000,
              access_time(unusual_hours),
              \+ authorized_bulk_download(User)),
             investigate_user(User, data_breach_risk)).

% Example 4: Healthcare Monitoring System  
healthcare_monitoring_rules :-
    % Rule 1: Critical vital signs
    add_rule(critical_vitals,
             vital_sign(PatientId, heart_rate, Rate),
             (Rate < 50; Rate > 120;
              (patient_age(PatientId, Age), Age > 70, Rate > 100)),
             alert_medical_staff(PatientId, critical_heart_rate)),
    
    % Rule 2: Medication interaction warning
    add_rule(drug_interaction,
             medication_prescribed(PatientId, Drug),
             (current_medications(PatientId, DrugList),
              dangerous_interaction(Drug, DrugList)),
             warn_physician(PatientId, drug_interaction, Drug)),
    
    % Rule 3: Fall risk assessment
    add_rule(fall_risk,
             mobility_assessment(PatientId, Score),
             (Score < 20,
              patient_age(PatientId, Age), Age > 75,
              \+ fall_prevention_active(PatientId)),
             activate_fall_prevention(PatientId)).

% Demo with Rete Network
demo_rete_performance :-
    format('~n=== Rete Network Performance Demo ===~n'),
    
    % Build the Rete network
    build_rete_network,
    
    % Add multiple complex rules
    trading_rules,
    iot_monitoring_rules,
    security_monitoring_rules,
    healthcare_monitoring_rules,
    
    % Process multiple events to show efficiency
    format('Processing events through Rete network...~n'),
    process_event_rete(price_update(aapl, 150.50)),
    process_event_rete(sensor_reading(temp_01, temperature, 65)),
    process_event_rete(connection_attempt('192.168.1.100', 80, 1640995200)),
    process_event_rete(vital_sign(patient_123, heart_rate, 130)),
    
    format('Rete network demonstration complete.~n').

% Advanced pattern matching examples
pattern_matching_demo :-
    format('~n=== Advanced Pattern Matching Demo ===~n'),
    
    % Rule with variable binding and unification
    add_rule(correlated_events,
             sensor_data(SensorId, Type, Value, Time),
             (Type = pressure, Value > 100,
              recent_event(temperature, SensorId, TempTime),
              abs(Time - TempTime) < 60),
             correlated_alert(pressure_temperature, SensorId)),
    
    % Rule with list processing
    add_rule(batch_processing,
             data_batch(BatchId, DataList),
             (length(DataList, Len), Len > 1000),
             process_large_batch(BatchId, Len)),
    
    % Test the patterns
    add_fact(recent_event(temperature, sensor_01, 1640995140)),
    process_event(sensor_data(sensor_01, pressure, 105, 1640995200)),
    process_event(data_batch(batch_001, [1,2,3,4,5|_])),  % Large list
    
    format('Pattern matching demonstration complete.~n').

% Comprehensive demo runner
run_advanced_demo :-
    format('~n=== PEARS Advanced Features Demo ===~n'),
    
    % Reset environment
    reset_advanced_demo,
    
    % Run individual demos
    demo_rete_performance,
    pattern_matching_demo,
    
    % Show final state
    format('~nFinal rule count: '),
    findall(RuleId, rule(RuleId, _, _, _), Rules),
    length(Rules, Count),
    format('~w rules loaded~n', [Count]),
    
    format('~n=== Advanced Demo Complete ===~n').

% Helper to reset advanced demo
reset_advanced_demo :-
    % Remove all rules
    retractall(rule(_, _, _, _)),
    retractall(rete_node(_, _)),
    retractall(event(_)),
    retractall(fact(_)),
    format('Advanced demo environment reset.~n').

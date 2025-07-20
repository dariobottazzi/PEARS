# PEARS Quick Start Guide

Get up and running with PEARS in minutes.

## Installation

1. **Prerequisites**: Ensure SWI-Prolog is installed
   ```bash
   # On macOS with Homebrew
   brew install swi-prolog
   
   # On Ubuntu/Debian
   sudo apt-get install swi-prolog
   ```

2. **Clone/Download PEARS**: Get the PEARS source code

3. **Load PEARS**: Start SWI-Prolog and load the system
   ```prolog
   ?- [src/pears].
   ```

## Your First Rule

Let's create a simple temperature monitoring rule:

```prolog
% Add a rule that triggers when temperature exceeds 30 degrees
?- add_rule(high_temp_alert,
            event(temperature_reading(Temp)),
            condition(Temp > 30),
            action(send_alert(high_temperature))).
true.

% Test the rule with an event
?- process_event(temperature_reading(35)).
Action executed: send_alert(high_temperature)
true.

% Test with a temperature that doesn't trigger the rule
?- process_event(temperature_reading(25)).
true.
```

## Core Operations

### 1. Rule Management
```prolog
% Add a rule
add_rule(rule_id, event_pattern, condition_logic, action_to_execute).

% Remove a rule
remove_rule(rule_id).

% List all rules
list_rules.

% Check if rule exists
rule_exists(rule_id).
```

### 2. Event Processing
```prolog
% Process a single event
process_event(some_event(data)).

% Process event with Rete algorithm
process_event_rete(some_event(data)).

% Test which rules would trigger
would_trigger(some_event(data), TriggeredRules).
```

### 3. Working Memory
```prolog
% Add a fact
add_fact(user_setting(john, theme, dark)).

% Remove a fact  
remove_fact(old_data(expired)).

% List all facts
list_facts.
```

### 4. Getting Help
```prolog
% Show comprehensive help with all commands
help.

% Show examples and test commands
load_examples.
```

The `help` predicate provides a complete overview of all available commands organized by:
- Basic Rule Management
- Fact Management  
- Examples and Testing
- Rete Algorithm Commands
- Compound Conditions Support
- System Commands

## Common Patterns

### Pattern 1: Simple Monitoring
```prolog
% Monitor system resources
add_rule(cpu_alert,
         event(cpu_usage(Percent)),
         condition(Percent > 80),
         action(notify_admin(high_cpu))).
```

### Pattern 2: Compound Conditions
```prolog
% Alert only during business hours and high load
add_rule(business_hour_alert,
         event(system_load(Load)),
         condition(and(Load > 5, business_hours)),
         action(escalate_alert)).
```

### Pattern 3: Pattern Matching
```prolog
% Different actions based on user type
add_rule(login_handling,
         event(user_login(User, Type)),
         condition(Type = admin),
         action(grant_admin_access(User))).
```

## Running Tests

Verify your installation works correctly:

```prolog
% Load and run tests
?- [tests/test_pears_fixed].
?- run_all_tests.

% Run specific test categories
?- run_core_tests.
?- run_integration_tests.
```

## Using VS Code Tasks (Optional)

If using VS Code, you can use the predefined tasks:

1. **Ctrl+Shift+P** → "Tasks: Run Task"
2. Select:
   - "Run All Tests" - Execute the full test suite
   - "Load PEARS" - Load the system
   - "Interactive PEARS REPL" - Start an interactive session

## Example Scenarios

### Home Automation
```prolog
% Load example
?- [examples/basic_eca_rules].

% Run smart home demo
?- example_smart_home_rules.

% Test motion detection
?- add_fact(light_level(kitchen, 15)),
   process_event(motion_detected(room(kitchen))).
```

### Security Monitoring
```prolog
% Set up security rules
?- example_security_rules.

% Simulate night time motion
?- add_fact(time_range(23, 6)),
   add_fact(user_present :- false),
   process_event(motion_detected(living_room)).
```

## Next Steps

1. **Read the [User Guide](user_guide.md)** for comprehensive documentation
2. **Explore [Examples](../examples/)** for real-world usage patterns  
3. **Try Advanced Features** like the Rete algorithm for performance
4. **Build Your Own Rules** for your specific use case

## Getting Help

- Check the test files for usage examples
- Read the source code comments for implementation details
- Experiment with the REPL to understand behavior

## Troubleshooting

### Common Issues

**Rule not firing?**
- Check that the event pattern matches exactly
- Verify the condition logic
- Use `list_rules` to see if the rule was added correctly

**Syntax errors?**
- Ensure proper Prolog syntax (periods, commas, parentheses)
- Check variable naming (start with uppercase)
- Verify predicate names are correct

**Performance issues?**
- Consider using the Rete algorithm for many rules
- Optimize condition ordering (most selective first)
- Clean up working memory regularly

---

You're now ready to start building powerful rule-based systems with PEARS!

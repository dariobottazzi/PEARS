# PEARS Documentation

## Overview

PEARS (Prolog Event-Action Rule System) is a sophisticated Event-Condition-Action (ECA) rule system implemented in SWI-Prolog. This document provides comprehensive information about the system's architecture, usage, and features.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [Architecture](#architecture)
3. [API Reference](#api-reference)
4. [Rule Definition Syntax](#rule-definition-syntax)
5. [Condition Language](#condition-language)
6. [Working Memory](#working-memory)
7. [Rete Algorithm](#rete-algorithm)
8. [Performance Considerations](#performance-considerations)
9. [Examples](#examples)

## Core Concepts

### Event-Condition-Action (ECA) Rules

PEARS implements the classic ECA paradigm:

- **Event**: A trigger that initiates rule evaluation
- **Condition**: A logical expression that must be satisfied
- **Action**: The operation to execute when the condition is met

### Dynamic Rule Management

Rules can be added, modified, and removed at runtime, allowing for flexible and adaptive behavior.

### Pattern Matching

PEARS uses Prolog's unification mechanism for powerful pattern matching capabilities.

## Architecture

```
PEARS System Architecture

┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Event Source  │───▶│  Event Processor │───▶│  Rule Engine    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                        │
                       ┌─────────────────┐              │
                       │ Working Memory  │◄─────────────┤
                       └─────────────────┘              │
                                                        ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Action Handler │◄───│  Rete Network   │◄───│ Condition Eval  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Components

1. **Event Processor**: Handles incoming events and triggers rule evaluation
2. **Rule Engine**: Manages rule storage and execution logic
3. **Condition Evaluator**: Processes complex conditional expressions
4. **Working Memory**: Maintains system state and facts
5. **Rete Network**: Provides efficient pattern matching (optional)
6. **Action Handler**: Executes actions when rules fire

## API Reference

### Rule Management

#### `add_rule(RuleId, Event, Condition, Action)`
Adds a new rule to the system.

**Parameters:**
- `RuleId`: Unique identifier for the rule
- `Event`: Event pattern to match
- `Condition`: Logical condition to evaluate
- `Action`: Action to execute when rule fires

**Example:**
```prolog
add_rule(temp_alert, 
         temperature(Temp), 
         (Temp > 30), 
         send_alert(high_temp)).
```

#### `remove_rule(RuleId)`
Removes a rule from the system.

#### `list_rules`
Displays all currently active rules.

#### `rule_exists(RuleId)`
Checks if a rule exists in the system.

### Event Processing

#### `process_event(Event)`
Processes a single event against all rules.

#### `process_event_rete(Event)`
Processes an event using the Rete algorithm for enhanced performance.

#### `would_trigger(Event, Rules)`
Tests which rules would be triggered by an event without executing actions.

### Working Memory Management

#### `add_fact(Fact)`
Adds a fact to the working memory.

#### `remove_fact(Fact)`
Removes a fact from working memory.

#### `list_facts`
Lists all facts in working memory.

### Rete Network Operations

#### `init_rete`
Initializes the Rete network system.

#### `build_rete_network`
Builds the Rete network from all current rules.

#### `rete_stats`
Shows statistics about the Rete network (node counts, memory usage).

#### `show_rete_network`
Displays the structure of the Rete network.

#### `show_rete_network_detailed`
Shows detailed network structure with memory contents.

## Rule Definition Syntax

### Basic Rule Structure

```prolog
add_rule(RuleId, Event, Condition, Action)
```

### Event Patterns

Events are Prolog terms that can contain variables for pattern matching:

```prolog
% Simple event
temperature_reading(25)

% Event with variables
sensor_data(SensorId, Value)

% Complex event structure  
transaction(UserId, Amount, Type, Timestamp)
```

### Action Specifications

Actions define what should happen when a rule fires:

```prolog
% Simple action
send_alert(fire_detected)

% Action with parameters from event/condition
log_transaction(UserId, Amount)

% Multiple actions (compound)
(send_email(admin), log_event(security_breach))
```

## Condition Language

PEARS supports a rich condition language with logical operators and built-in predicates.

### Logical Operators

- `and(Cond1, Cond2)`: Logical AND
- `or(Cond1, Cond2)`: Logical OR  
- `not(Condition)`: Logical NOT

### Comparison Operators

- `>(X, Y)`: Greater than
- `<(X, Y)`: Less than
- `>=(X, Y)`: Greater than or equal
- `=<(X, Y)`: Less than or equal
- `=(X, Y)`: Unification/equality
- `\\=(X, Y)`: Not equal

### Examples

```prolog
% Simple comparison
condition(Temperature > 30)

% Compound condition
condition(and(Temperature > 30, Humidity < 40))

% Complex nested condition
condition(or(and(Temperature > 35, not(AirConditionerOn)),
             and(Temperature < 10, not(HeaterOn))))

% Condition with variable binding
condition(and(sensor_reading(SensorId, Value), Value > threshold(SensorId)))
```

## Working Memory

Working memory stores facts and events that persist across rule evaluations.

### Fact Types

- **Static Facts**: Persistent information (e.g., configuration, constants)
- **Dynamic Facts**: Changing information (e.g., sensor readings, user states)
- **Event Facts**: Temporary events that may be consumed by rules

### Memory Management

```prolog
% Add persistent fact
add_fact(user_preference(john, temperature, 22)).

% Query with pattern matching
findall(X, fact(user_preference(john, X, _)), Preferences).

% Remove outdated fact
remove_fact(sensor_reading(sensor_01, _, old_timestamp)).
```

## Rete Algorithm

PEARS optionally supports the Rete algorithm for efficient pattern matching in systems with many rules.

### Benefits

- **Efficiency**: Avoids re-evaluating unchanged conditions
- **Scalability**: Performs well with large numbers of rules
- **Optimization**: Maintains partial match states

### Usage

```prolog
% Initialize Rete network
init_rete.
build_rete_network.

% Add rules (they automatically integrate with Rete when built)
add_rule(rule1, event_pattern1, condition1, action1).
add_rule(rule2, event_pattern2, condition2, action2).

% Process events efficiently
process_event_rete(new_event).
```

### When to Use Rete

- Systems with 100+ rules
- High event processing rates  
- Complex multi-condition rules
- Performance-critical applications

## Performance Considerations

### Rule Organization

1. **Specific Rules First**: Place more specific rules before general ones
2. **Condition Ordering**: Put most selective conditions first
3. **Index Usage**: Use indexed facts for frequent queries

### Memory Management

1. **Fact Cleanup**: Remove outdated facts regularly
2. **Event Lifecycle**: Define clear event expiration policies
3. **Working Memory Size**: Monitor and limit memory usage

### Optimization Techniques

```prolog
% Use cuts judiciously for performance
rule_with_cut(Event) :-
    specific_condition(Event),
    !,  % Cut to prevent backtracking
    expensive_action(Event).

% Index frequently queried facts
:- dynamic sensor_reading/3.
:- multifile sensor_reading/3.  
:- index(sensor_reading(1, 0, 0)).  % Index by sensor ID
```

## Examples

See the `examples/` directory for comprehensive usage examples:

- `basic_eca_rules.pl`: Simple rule patterns
- `advanced_patterns.pl`: Complex conditions and Rete usage

### Quick Start Example

```prolog
% Load PEARS
?- [src/pears].

% Define a simple monitoring rule
?- add_rule(temp_monitor,
            event(temperature(T)),
            condition(T > 25),
            action(turn_on(air_conditioner))).

% Process an event
?- process_event(temperature(30)).
% Output: Action executed: turn_on(air_conditioner)

% List active rules
?- list_rules.
```

This completes the core documentation for PEARS. For specific implementation details, refer to the source code and test files.

# PEARS Examples Directory

This directory contains comprehensive examples demonstrating all PEARS features.

## Available Examples

### 1. home_automation_demo.pl
**Complete tutorial application** - The best starting point!

- Demonstrates ALL basic PEARS features step by step
- Smart home automation system with real rule firing
- Interactive demo with visual feedback
- Shows rule management, event processing, facts, compound conditions, utilities, and logging

**Usage:**
```prolog
?- [src/pears].
?- [examples/home_automation_demo].
?- run_home_automation_demo.
```

**Features demonstrated:**
- ✅ Adding, updating, removing rules
- ✅ Event processing with actual rule execution
- ✅ Working memory and fact management  
- ✅ Compound conditions (AND, OR, NOT, arithmetic)
- ✅ Rule utilities (`would_trigger`, enable/disable)
- ✅ Event history and logging
- ✅ Custom action predicates with visual feedback

### 2. basic_eca_rules.pl
**Simple introduction** to ECA concepts

- Basic temperature monitoring rules
- Security system examples  
- Smart home automation basics
- Good for understanding core concepts

**Usage:**
```prolog
?- [examples/basic_eca_rules].
?- run_examples.
```

### 3. advanced_patterns.pl
**Complex scenarios** for experienced users

- Financial trading rules
- Multi-sensor IoT systems
- Network security monitoring
- Healthcare patient monitoring
- Rete algorithm performance demos
- Advanced pattern matching

**Usage:**
```prolog
?- [examples/advanced_patterns].
?- run_advanced_demo.
```

## Quick Start Recommendation

**New to PEARS?** Start here:
```prolog
?- [src/pears].
?- [examples/home_automation_demo].
?- run_home_automation_demo.
```

This will give you a complete tour of all PEARS capabilities with hands-on examples and clear explanations.

## Example Structure

Each example follows this pattern:
1. **Setup** - Clear environment and initial facts
2. **Rules** - Add relevant rules for the scenario
3. **Events** - Process events to trigger rules
4. **Results** - Show visual feedback of rule execution
5. **Summary** - Display final system state

## Getting Help

- Run `help.` in PEARS for complete command reference
- Run `demo_help.` in home_automation_demo for demo-specific commands
- Check the main README.md for installation and basic usage

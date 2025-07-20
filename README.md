# PEARS - Prolog Event-Action Rule System

PEARS is a comprehensive Event-Condition-Action (ECA) rule system implemented in Prolog. It provides dynamic rule management, efficient pattern matching using the Rete algorithm, and powerful event processing capabilities.

## Features

- **Dynamic Rule Management**: Add, remove, and modify rules at runtime
- **Event Processing**: Pattern matching and event handling
- **Compound Conditions**: Support for complex conditional logic with AND, OR, NOT operators
- **Rete Algorithm**: Efficient rule matching and working memory management
- **Comprehensive Testing**: Robust test suite with clear failure reporting
- **Working Memory**: Dynamic fact storage and retrieval

## Project Structure

```
├── src/                    # Source code
│   └── pears.pl           # Main PEARS implementation
├── tests/                 # Test suite
│   └── test_pears_fixed.pl # Comprehensive test cases
├── docs/                  # Documentation (to be added)
├── examples/              # Usage examples (to be added)
├── .github/               # GitHub configuration
│   └── copilot-instructions.md
├── .vscode/               # VS Code configuration
└── README.md              # This file
```

## Quick Start

1. **Load PEARS**:
   ```prolog
   ?- [src/pears].
   ```

2. **Add a simple rule**:
   ```prolog
   ?- add_rule(temperature_alert, 
               temperature_reading(Temp), 
               (Temp > 30), 
               send_alert(high_temperature)).
   ```

3. **Process an event**:
   ```prolog
   ?- process_event(temperature_reading(35)).
   ```

4. **Run tests**:
   ```prolog
   ?- [tests/test_pears_fixed].
   ?- test_all.
   ```

## Core Predicates

### Rule Management
- `add_rule(RuleId, Event, Condition, Action)` - Add a new rule
- `remove_rule(RuleId)` - Remove a rule
- `list_rules` - Display all active rules

### Event Processing
- `process_event(Event)` - Process a single event
- `process_event_rete(Event)` - Process event using Rete algorithm
- `would_trigger(Event, Rules)` - Test which rules an event would trigger

### Working Memory
- `add_fact(Fact)` - Add fact to working memory
- `remove_fact(Fact)` - Remove fact from working memory
- `list_facts` - List all facts

### Rete Network
- `init_rete` - Initialize the Rete network
- `build_rete_network` - Build Rete network from current rules
- `rete_stats` - Show Rete network statistics
- `show_rete_network` - Display network structure

## Testing

The test suite uses a custom framework that distinguishes between:
- **Expected logical negatives** (e.g., `10 > 10` is logically false)
- **True test failures** (unexpected behavior or errors)

Run tests with:
```prolog
?- [tests/test_pears_fixed].
?- test_all.
```

Individual test categories:
- `test_core` - Basic functionality
- `test_rete` - Rete algorithm tests
- `test_scenarios` - Integration scenarios

## Development

### Prerequisites
- SWI-Prolog 8.0 or later

### Code Style
- Use descriptive predicate names
- Include comprehensive comments
- Follow Prolog conventions (CamelCase variables)
- Add documentation for major predicates

### Contributing
1. Add tests for new features
2. Ensure all tests pass
3. Update documentation
4. Follow the existing code style

## License

This project is open source. See LICENSE file for details.

## Contact

For questions, issues, or contributions, please use the project's issue tracker.

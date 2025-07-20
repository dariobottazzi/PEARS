# Contributor Instructions

The PEARS (Prolog Event-Action Rule System) workspace in VS Code provides a comprehensive development environment with intelligent code assistance, automated testing, and project-specific optimizations.

## Workspace Features

### Intelligent Code Assistance
- **Context-aware suggestions**: Copilot understands Prolog syntax and PEARS-specific patterns
- **Rule completion**: Smart completion for ECA rule structures
- **Documentation generation**: Automatic comment and documentation suggestions
- **Best practices**: Suggestions follow established Prolog and PEARS conventions

### Automated Testing
- **VS Code Tasks**: Pre-configured tasks for running tests and building
- **Test categories**: Organized test suites (core, compound, integration, performance)
- **Failure reporting**: Clear distinction between logical negatives and true failures
- **Performance monitoring**: Built-in benchmarks and timing

### Project Structure
- **Organized directories**: Clean separation of source, tests, docs, and examples
- **Example-driven learning**: Comprehensive examples from basic to advanced patterns
- **Documentation**: User guides and API references
- **Configuration**: VS Code settings optimized for Prolog development

## Getting Started with the Workspace

1. **Open the workspace**: Load the entire PEARS directory in VS Code
2. **Install Prolog extension**: Ensure you have a Prolog language extension installed
3. **Run initial tests**: Use Ctrl+Shift+P → "Tasks: Run Task" → "Run All Tests"
4. **Explore examples**: Start with `examples/basic_eca_rules.pl`

## Development Workflow

### Adding New Features
1. **Write tests first**: Add tests in `tests/test_pears_fixed.pl`
2. **Implement feature**: Update `src/pears.pl`
3. **Run tests**: Use the "Run All Tests" task
4. **Update documentation**: Add to relevant docs
5. **Create examples**: Demonstrate usage in examples directory

### Code Style Guidelines
- Use descriptive predicate names that explain purpose
- Include comprehensive comments for complex logic
- Follow Prolog conventions (CamelCase variables, snake_case predicates)
- Add documentation strings for major predicates
- Use proper indentation for rule definitions

### Testing Guidelines
- All test predicates should return `true` for successful tests
- Use `test_case_expect_false` for tests that should logically return false
- Include both positive and negative test cases
- Test edge cases and error conditions
- Provide clear test descriptions

## VS Code Integration

### Available Tasks
- **Load PEARS**: Quick system loading for development
- **Run All Tests**: Execute complete test suite
- **Run Core Tests**: Basic functionality tests only
- **Interactive PEARS REPL**: Start interactive development session
- **Check Syntax**: Validate Prolog syntax

### Keyboard Shortcuts
- `Ctrl+Shift+P` → "Tasks: Run Task" for quick access to PEARS tasks
- Use VS Code's built-in terminal for interactive Prolog sessions
- Leverage Copilot's suggestions with `Ctrl+Space` (may vary by system)

### Copilot Integration
The workspace includes specific instructions for GitHub Copilot to:
- Understand PEARS architecture and patterns
- Suggest appropriate test structures
- Follow project coding conventions  
- Generate relevant documentation
- Provide context-aware rule examples

## Advanced Features

### Rete Algorithm Development
When working on Rete algorithm features:
- Focus on performance implications
- Add comprehensive benchmarks
- Test with large rule sets
- Document memory usage patterns

### Complex Pattern Matching
For advanced pattern matching features:
- Test unification behavior thoroughly
- Consider variable binding edge cases
- Validate with complex nested structures
- Benchmark performance impact

### Error Handling
When adding error handling:
- Use Prolog's catch/3 mechanism
- Provide meaningful error messages
- Test error conditions explicitly
- Document error behavior

## Best Practices

### Performance Considerations
- Profile code with large datasets
- Use indexing for frequently accessed predicates
- Consider cut placement carefully
- Monitor memory usage in long-running systems

### Documentation Standards
- Keep README.md current with new features
- Update API documentation for public predicates
- Provide examples for complex features
- Include performance characteristics

### Collaboration
- Use clear commit messages
- Reference issue numbers in commits
- Test on multiple Prolog implementations if possible
- Review code for adherence to style guidelines

## Troubleshooting Common Issues

### Copilot Not Working Optimally
- Ensure `.github/copilot-instructions.md` is present and current
- Restart VS Code if Copilot seems confused about context
- Use specific, descriptive variable names to help Copilot understand intent

### Test Failures
- Check if failures are logical negatives vs. true errors
- Use the detailed test output to understand failure causes
- Run individual test categories to isolate issues

### Performance Problems
- Profile using SWI-Prolog's profiling tools
- Consider Rete algorithm for rule-heavy scenarios
- Monitor working memory growth

This workspace is designed to provide an optimal development experience for the PEARS project. The combination of organized structure, comprehensive testing, and intelligent code assistance creates an environment where contributors can focus on building robust, efficient rule-based systems.

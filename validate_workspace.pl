#!/usr/bin/env swipl

% PEARS Workspace Validation Script
% Tests that the workspace is properly set up and functional

:- initialization(main, main).

main :-
    format('~n=== PEARS Workspace Validation ===~n'),
    
    % Test 1: Load main PEARS system
    format('Testing PEARS system loading... '),
    catch(
        ([src/pears], format('PASSED~n')),
        Error,
        (format('FAILED: ~w~n', [Error]), halt(1))
    ),
    
    % Test 2: Basic functionality
    format('Testing basic rule addition... '),
    catch(
        (add_rule(test_rule, 
                  event(temp(T)), 
                  condition(T > 25), 
                  action(alert)), 
         format('PASSED~n')),
        Error,
        (format('FAILED: ~w~n', [Error]), halt(1))
    ),
    
    % Test 3: Event processing
    format('Testing event processing... '),
    catch(
        (process_event(temp(30)), format('PASSED~n')),
        Error,
        (format('FAILED: ~w~n', [Error]), halt(1))
    ),
    
    % Test 4: Load examples
    format('Testing example loading... '),
    catch(
        ([examples/basic_eca_rules], format('PASSED~n')),
        Error,
        (format('FAILED: ~w~n', [Error]), halt(1))
    ),
    
    % Clean up
    remove_rule(test_rule),
    
    format('~n=== All validation tests PASSED ===~n'),
    format('PEARS workspace is ready for development!~n~n'),
    
    halt(0).

% Error handler
:- catch(main, Error, (format('FATAL ERROR: ~w~n', [Error]), halt(1))).

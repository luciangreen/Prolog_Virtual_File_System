:- module(vfs_tests, [run_all_tests/0]).
:- use_module(library(plunit)).

% Test setup
setup_test_env :-
    current_datetime('2025-03-18 14:23:53'),
    user_info('luciangreen').

cleanup_test_env :-
    true.

current_datetime('2025-03-18 14:23:53').
user_info('luciangreen').

% Test predicates that we'll use in our checks
greet_user(Name) :-
    format('Hello, ~w!~n', [Name]).

process_numbers :-
    read_line_safe(N1String),
    read_line_safe(N2String),
    number_string(N1, N1String),
    number_string(N2, N2String),
    Sum is N1 + N2,
    format('Sum: ~w~n', [Sum]).

log_operation(Operation) :-
    current_datetime(DateTime),
    user_info(User),
    format('[~w] User ~w performed: ~w~n', [DateTime, User, Operation]).

:- begin_tests(io_operations).

% Test 1: Multiple predicate calls in sequence
test(multiple_predicates) :-
    Input = 'John\n',
    Output = 'Enter name:\nHello, John!\nOperation logged.\n',
    check_io(Input, Output,
             (write('Enter name:\n'),
              read_line_safe(Name),
              greet_user(Name),
              log_operation(greeting),
              write('Operation logged.\n'))).

% Test 2: Complex interaction with multiple inputs
test(complex_interaction) :-
    Input = '5\n3\n',
    Output = 'Enter two numbers:\nSum: 8\n[2025-03-18 14:23:53] User luciangreen performed: addition\n',
    check_io(Input, Output,
             (write('Enter two numbers:\n'),
              process_numbers,
              log_operation(addition))).

% Test 3: Nested predicate calls
test(nested_calls) :-
    Input = 'Alice\nBob\n',
    Output = 'First user:\nHello, Alice!\nSecond user:\nHello, Bob!\n',
    check_io(Input, Output,
             (write('First user:\n'),
              read_line_safe(User1),
              greet_user(User1),
              write('Second user:\n'),
              read_line_safe(User2),
              greet_user(User2))).

% Test 4: Error handling in nested calls
test(error_handling, [throws(error(number_error,_))]) :-
    Input = 'not_a_number\n3\n',
    check_io(Input, _,
             (write('Enter numbers:\n'),
              process_numbers)).

% Test 5: Multiple operations with state
test(stateful_operations) :-
    Input = 'calculate\n10\n20\n',
    Output = 'Operation:\nEnter numbers:\nSum: 30\n[2025-03-18 14:23:53] Operation completed\n',
    check_io(Input, Output,
             (write('Operation:\n'),
              read_line_safe(_Operation),
              write('Enter numbers:\n'),
              process_numbers,
              current_datetime(DT),
              format('[~w] Operation completed~n', [DT]))).

% Test 6: Complex query with multiple predicates
test(complex_query) :-
    current_datetime(DateTime),
    user_info(User),
    Input = 'query\n123\n',
    format(string(ExpectedOutput),
           'Processing query...\nQuery: query\nID: 123\nTimestamp: ~w\nUser: ~w\nComplete.\n',
           [DateTime, User]),
    check_io(Input, ExpectedOutput,
             ((write('Processing query...\n'),
               read_line_safe(Query),
               read_line_safe(ID),
               format('Query: ~w\nID: ~w\n', [Query, ID]),
               format('Timestamp: ~w\n', [DateTime]),
               format('User: ~w\n', [User]),
               write('Complete.\n')))).

% Test 7: Multiple file operations with I/O
test(file_operations) :-
    setup_test_env,
    Input = 'test_content\nmore_content\n',
    Output = 'Writing to file...\nReading from file...\nContent: test_content\nAppending...\nFinal content: test_content\nmore_content\n',
    check_io(Input, Output,
             ((write('Writing to file...\n'),
               read_line_safe(Content1),
               open('/test.txt', write, S1),
               write(S1, Content1),
               close(S1),
               write('Reading from file...\n'),
               open('/test.txt', read, S2),
               read_line_safe(ReadContent),
               format('Content: ~w\n', [ReadContent]),
               close(S2),
               write('Appending...\n'),
               read_line_safe(Content2),
               open('/test.txt', append, S3),
               write(S3, '\n'),
               write(S3, Content2),
               close(S3),
               open('/test.txt', read, S4),
               read_string(S4, _, FinalContent),
               format('Final content: ~w\n', [FinalContent]),
               close(S4)))),
    cleanup_test_env.

:- end_tests(io_operations).

% Main test runner
run_all_tests :-
    format('Starting VFS I/O tests at ~w~n', ['2025-03-18 14:23:53']),
    format('Test user: ~w~n', ['luciangreen']),
    run_tests(io_operations),
    format('Tests completed at ~w~n', ['2025-03-18 14:23:53']).

% Helper for running specific test groups
run_io_tests :- run_tests(io_operations).
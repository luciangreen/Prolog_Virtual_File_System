:- module(vfs_tests, [run_all_tests/0]).
:- use_module(library(plunit)).
:- use_module(vfs).

current_datetime('2025-03-19 17:20:50').
current_user('luciangreen').

:- begin_tests(vfs_operations).

% Test system predicate detection
test(system_predicate_detection) :-
    is_system_predicate(member/2),
    is_system_predicate(format/2),
    \+ is_system_predicate(nonexistent_pred/1).

% Test initialization and state maintenance
test(init_and_state) :-
    InitialFiles = [
        vfs_file('/test1.txt', 'Hello'),
        vfs_file('/test2.txt', 'World')
    ],
    init_vfs(InitialFiles, State),
    State = InitialFiles.

% Test file operations with state preservation
test(file_operations_state) :-
    current_datetime(DateTime),
    current_user(User),
    InitialFiles = [
        vfs_file('/test.txt', 'Initial'),
        vfs_file('/log.txt', '')
    ],
    init_vfs(InitialFiles, _),
    
    % Write to test file
    open_vfs('/test.txt', write, Stream),
    format(string(Content), 'Modified by ~w at ~w', [User, DateTime]),
    write_vfs(Stream, Content),
    close_vfs(Stream),
    
    % Verify state
    get_vfs_state(State),
    member(vfs_file('/test.txt', Content), State).

% Test code conversion with system predicates
test(code_conversion_with_system_predicates) :-
    % Original code with system and non-system predicates
    Original = (
        format('Starting operation~n'),
        open(File, write, Stream),
        write(Stream, Content),
        member(X, List),
        close(Stream)
    ),
    
    % Convert code
    convert_to_vfs(Original, Converted),
    
    % Expected result (format and member should remain unchanged)
    Expected = (
        format('Starting operation~n'),
        open_vfs(File, write, Stream),
        write_vfs(Stream, Content),
        member(X, List),
        close_vfs(Stream)
    ),
    
    Converted = Expected.

% Test complete workflow with state tracking
test(complete_workflow) :-
    current_datetime(DateTime),
    current_user(User),
    
    % Initialize VFS
    InitialFiles = [
        vfs_file('/data.txt', 'Initial data'),
        vfs_file('/log.txt', '')
    ],
    init_vfs(InitialFiles, InitState),
    
    % Perform operations
    open_vfs('/log.txt', append, LogStream),
    format(string(LogEntry), '[~w] Operation started by ~w~n', [DateTime, User]),
    write_vfs(LogStream, LogEntry),
    close_vfs(LogStream),
    
    % Modify data file
    open_vfs('/data.txt', write, DataStream),
    format(string(NewData), 'Modified by ~w at ~w', [User, DateTime]),
    write_vfs(DataStream, NewData),
    close_vfs(DataStream),
    
    % Verify final state
    get_vfs_state(FinalState),
    member(vfs_file('/log.txt', LogEntry), FinalState),
    member(vfs_file('/data.txt', NewData), FinalState).

% Test error handling
test(error_handling, [fail]) :-
    open_vfs('/nonexistent.txt', read, _).

% Test preserved system predicates
test(preserved_predicates) :-
    Original = (
        member(X, [1,2,3]),
        is(Y, X + 1),
        format('Result: ~w~n', [Y])
    ),
    convert_to_vfs(Original, Converted),
    Original = Converted.

:- end_tests(vfs_operations).

% Main test runner
run_all_tests :-
    current_datetime(DateTime),
    current_user(User),
    format('Starting VFS tests at ~w~n', [DateTime]),
    format('Test user: ~w~n', [User]),
    run_tests(vfs_operations),
    get_vfs_state(FinalState),
    format('Final VFS State:~n'),
    maplist(print_file, FinalState),
    format('Tests completed at ~w~n', [DateTime]).

% Helper to print file state
print_file(vfs_file(Name, Contents)) :-
    format('File: ~w~n  Contents: ~w~n', [Name, Contents]).
:- module(vfs_tests, [run_all_tests/0]).
:- use_module(library(plunit)).
:- use_module(vfs).

current_datetime('2025-03-18 19:40:44').
current_user('luciangreen').

:- begin_tests(vfs_operations).

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
    % Initial state
    InitialFiles = [vfs_file('/test.txt', 'Initial')],
    init_vfs(InitialFiles, InitState),
    
    % Perform operations
    open_vfs('/test.txt', write, Stream),
    write_vfs(Stream, 'Modified'),
    close_vfs(Stream),
    
    % Check final state
    get_vfs_state(FinalState),
    FinalState = [vfs_file('/test.txt', 'Modified')].

% Test multiple files and operations
test(multiple_files) :-
    InitialFiles = [
        vfs_file('/file1.txt', 'Content1'),
        vfs_file('/file2.txt', 'Content2')
    ],
    init_vfs(InitialFiles, _),
    
    % Modify first file
    open_vfs('/file1.txt', write, S1),
    write_vfs(S1, 'NewContent1'),
    close_vfs(S1),
    
    % Modify second file
    open_vfs('/file2.txt', append, S2),
    write_vfs(S2, 'Content2Additional'),
    close_vfs(S2),
    
    % Check final state
    get_vfs_state(FinalState),
    member(vfs_file('/file1.txt', 'NewContent1'), FinalState),
    member(vfs_file('/file2.txt', 'Content2Additional'), FinalState).

% Test code conversion
test(code_conversion) :-
    % Original code
    Original = (open(File, write, Stream),
               write(Stream, Content),
               close(Stream)),
    
    % Expected conversion
    Expected = (open_vfs(File, write, Stream),
               write_vfs(Stream, Content),
               close_vfs(Stream)),
    
    convert_to_vfs(Original, Converted),
    Converted = Expected.

% Test system predicate detection
test(system_predicate_check) :-
    is_system_predicate(member/2),
    \+ is_system_predicate(nonexistent_pred/1).

% Test complete workflow with state tracking
test(complete_workflow) :-
    % Setup initial state
    current_datetime(DateTime),
    current_user(User),
    InitialFiles = [
        vfs_file('/log.txt', ''),
        vfs_file('/data.txt', 'Initial data')
    ],
    init_vfs(InitialFiles, InitState),
    
    % Perform operations
    open_vfs('/log.txt', append, LogStream),
    format(string(LogEntry), '[~w] User ~w started operation~n', [DateTime, User]),
    write_vfs(LogStream, LogEntry),
    close_vfs(LogStream),
    
    open_vfs('/data.txt', write, DataStream),
    write_vfs(DataStream, 'Modified data'),
    close_vfs(DataStream),
    
    % Verify final state
    get_vfs_state(FinalState),
    member(vfs_file('/log.txt', LogEntry), FinalState),
    member(vfs_file('/data.txt', 'Modified data'), FinalState).

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
    format('File: ~w, Contents: ~w~n', [Name, Contents]).
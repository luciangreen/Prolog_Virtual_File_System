:- module(vfs_tests, [run_all_tests/0]).
:- use_module(library(plunit)).

% Test utilities
setup_test_env :-
    get_time(Timestamp),
    format_time(string(TimeStr), '%Y-%m-%d %H:%M:%S', Timestamp),
    user_info('luciangreen'),
    init_vfs([], []).

cleanup_test_env :-
    retractall(vfs_file(_, _)),
    retractall(vfs_directory(_)),
    retractall(vfs_opened(_, _, _)),
    retractall(vfs_current_directory(_)).

user_info('luciangreen').
current_datetime('2025-03-18 13:52:32').

:- begin_tests(vfs_system).

test(init_vfs) :-
    current_datetime(DateTime),
    InitFiles = [
        vfs_file('/log.txt', DateTime),
        vfs_file('/docs/readme.txt', 'Documentation')
    ],
    InitDirs = [vfs_directory('/docs')],
    init_vfs(InitFiles, InitDirs),
    list_files(Files),
    list_directories(Dirs),
    member(vfs_file('/log.txt', DateTime), Files),
    member(vfs_directory('/docs'), Dirs).

test(file_operations) :-
    current_datetime(DateTime),
    user_info(User),
    setup_test_env,
    % Write test
    open('/test.txt', write, WStream),
    format(string(Content), 'Created on ~w by ~w', [DateTime, User]),
    write(WStream, Content),
    close(WStream),
    % Read test
    open('/test.txt', read, RStream),
    read(RStream, ReadContent),
    close(RStream),
    ReadContent = Content.

test(directory_operations) :-
    setup_test_env,
    mkdir('/projects'),
    cd('/projects'),
    pwd(CurrentDir),
    CurrentDir = '/projects'.

test(file_listing) :-
    setup_test_env,
    current_datetime(DateTime),
    user_info(User),
    % Create test files
    open('/test1.txt', write, S1),
    write(S1, 'Test 1'),
    close(S1),
    open('/test2.txt', write, S2),
    write(S2, 'Test 2'),
    close(S2),
    list_files(Files),
    length(Files, 2).

test(file_management) :-
    setup_test_env,
    % Create source file
    open('/source.txt', write, S),
    write(S, 'Test content'),
    close(S),
    % Test copy
    cp('/source.txt', '/copy.txt'),
    vfs_file('/copy.txt', Content1),
    Content1 = 'Test content',
    % Test move
    mv('/copy.txt', '/moved.txt'),
    \+ vfs_file('/copy.txt', _),
    vfs_file('/moved.txt', Content2),
    Content2 = 'Test content'.

test(run_prolog_program) :-
    setup_test_env,
    current_datetime(DateTime),
    % Create test program
    open('/test.pl', write, S),
    format(string(Code), 'test_time("~w").', [DateTime]),
    write(S, Code),
    close(S),
    run_prolog('/test.pl', Result),
    Result = DateTime.

test(error_handling) :-
    setup_test_env,
    % Test non-existent file
    \+ open('/nonexistent.txt', read, _),
    % Test duplicate directory
    mkdir('/test_dir'),
    \+ mkdir('/test_dir').

test(complete_workflow) :-
    setup_test_env,
    current_datetime(DateTime),
    user_info(User),
    run_with_vfs(
        (mkdir('/logs'),
         cd('/logs'),
         open('system.log', write, S),
         format(string(LogMsg), 'Log started by ~w at ~w', [User, DateTime]),
         write(S, LogMsg),
         close(S)),
        [], [], 
        FinalFiles,
        FinalDirs),
    member(vfs_file('/logs/system.log', _), FinalFiles),
    member(vfs_directory('/logs'), FinalDirs).

test(io_check) :-
    setup_test_env,
    current_datetime(DateTime),
    format(string(ExpectedOutput), 'Current time: ~w', [DateTime]),
    check_io('', ExpectedOutput,
             (write(ExpectedOutput))).

test(prolog_to_vfs_conversion) :-
    setup_test_env,
    current_datetime(DateTime),
    format(string(PrologCode), 
           'open("log.txt", write, S), write(S, "~w"), close(S)', 
           [DateTime]),
    convert_prolog_to_vfs(PrologCode, VFSCode),
    assertion(nonvar(VFSCode)).

:- end_tests(vfs_system).

% Main test runner
run_all_tests :-
    format('Starting VFS tests at ~w~n', ['2025-03-18 13:52:32']),
    format('Test user: ~w~n', ['luciangreen']),
    run_tests(vfs_system),
    format('Tests completed at ~w~n', ['2025-03-18 13:52:32']).

% Helper predicates for running individual test groups
run_init_tests :- run_tests(vfs_system:init_vfs).
run_file_op_tests :- run_tests(vfs_system:file_operations).
run_dir_op_tests :- run_tests(vfs_system:directory_operations).
run_listing_tests :- run_tests(vfs_system:file_listing).
run_management_tests :- run_tests(vfs_system:file_management).
run_prolog_tests :- run_tests(vfs_system:run_prolog_program).
run_error_tests :- run_tests(vfs_system:error_handling).
run_workflow_tests :- run_tests(vfs_system:complete_workflow).
run_io_tests :- run_tests(vfs_system:io_check).
run_conversion_tests :- run_tests(vfs_system:prolog_to_vfs_conversion).
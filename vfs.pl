:- dynamic vfs_file/2, vfs_directory/1, vfs_opened/3, vfs_current_directory/1.

% Initialize Virtual File System with a given file and directory list
init_vfs(FileList, DirList) :-
    retractall(vfs_file(_, _)),
    retractall(vfs_directory(_)),
    retractall(vfs_current_directory(_)),
    maplist(assertz, FileList),
    maplist(assertz, DirList),
    assertz(vfs_current_directory('/')).

% Open a file (simulate opening by asserting it as 'opened')
open(FileName, Mode, Stream) :-
    (   Mode = read
    ->  vfs_file(FileName, Contents),
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_opened(Stream, FileName, Contents))
    ;   Mode = write,
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_opened(Stream, FileName, ''))
    ;   Mode = append,
        vfs_file(FileName, ExistingContents),
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_opened(Stream, FileName, ExistingContents))
    ).

% Read from an open file
read(Stream, Contents) :-
    vfs_opened(Stream, _, Contents).

% Write to an open file
write(Stream, NewContents) :-
    retract(vfs_opened(Stream, FileName, _)),
    assertz(vfs_opened(Stream, FileName, NewContents)).

% Append to an open file
append(Stream, ExtraContents) :-
    vfs_opened(Stream, FileName, OldContents),
    atom_concat(OldContents, ExtraContents, NewContents),
    retract(vfs_opened(Stream, FileName, _)),
    assertz(vfs_opened(Stream, FileName, NewContents)).

% Close a file and persist changes
close(Stream) :-
    vfs_opened(Stream, FileName, Contents),
    retract(vfs_opened(Stream, FileName, Contents)),
    retractall(vfs_file(FileName, _)),
    assertz(vfs_file(FileName, Contents)).

% List all files in the virtual file system
list_files(FileList) :-
    findall(FileName-Contents, vfs_file(FileName, Contents), FileList).

% List all directories in the virtual file system
list_directories(DirList) :-
    findall(Dir, vfs_directory(Dir), DirList).

% Create a new directory
mkdir(DirName) :-
    \+ vfs_directory(DirName),
    assertz(vfs_directory(DirName)).

% Remove a directory (if empty)
rmdir(DirName) :-
    \+ (vfs_file(File, _), sub_atom(File, 0, _, _, DirName)),
    retract(vfs_directory(DirName)).

% Remove a file
rm(FileName) :-
    retractall(vfs_file(FileName, _)).

% Rename a file
mv(OldName, NewName) :-
    vfs_file(OldName, Contents),
    assertz(vfs_file(NewName, Contents)),
    retract(vfs_file(OldName, _)).

% Copy a file
cp(Source, Dest) :-
    vfs_file(Source, Contents),
    assertz(vfs_file(Dest, Contents)).

% Change the current directory
cd(DirName) :-
    vfs_directory(DirName),
    retractall(vfs_current_directory(_)),
    assertz(vfs_current_directory(DirName)).

% Get the current directory
pwd(CurrentDir) :-
    vfs_current_directory(CurrentDir).

% Execute the given algorithm while preserving VFS state
run_with_vfs(Algorithm, InitialFiles, InitialDirs, FinalFiles, FinalDirs) :-
    init_vfs(InitialFiles, InitialDirs),
    call(Algorithm),
    list_files(FinalFiles),
    list_directories(FinalDirs).

% Run a Prolog program stored in a virtual file
run_prolog(FileName, Result) :-
    vfs_file(FileName, Code),
    term_string(Term, Code),
    call(Term, Result).

% Convert a Prolog program to Virtual File System commands
convert_prolog_to_vfs(PrologCode, VFSCode) :-
    term_string(PrologTerm, PrologCode),
    convert_term(PrologTerm, VFSCode).

% Convert terms recursively to replace Prolog file operations with VFS equivalents
convert_term(open(File, Mode, Stream), open(File, Mode, Stream)).
convert_term(read(Stream, Content), read(Stream, Content)).
convert_term(write(Stream, Content), write(Stream, Content)).
convert_term(append(Stream, Content), append(Stream, Content)).
convert_term(close(Stream), close(Stream)).
convert_term(cd(Dir), cd(Dir)).
convert_term(pwd(Dir), pwd(Dir)).
convert_term(rm(File), rm(File)).
convert_term(mv(Old, New), mv(Old, New)).
convert_term(cp(Src, Dest), cp(Src, Dest)).
convert_term(mkdir(Dir), mkdir(Dir)).
convert_term(rmdir(Dir), rmdir(Dir)).
convert_term(list_files(Files), list_files(Files)).
convert_term(list_directories(Dirs), list_directories(Dirs)).
convert_term(run_prolog(File, Result), run_prolog(File, Result)).
convert_term(call(Term), call(ConvertedTerm)) :-
    convert_term(Term, ConvertedTerm).
convert_term(Term, Term).

% Check if running a term produces expected input/output
check_io(ExpectedInput, ExpectedOutput, Term) :-
    with_output_to(string(ActualOutput),
        with_input_from(string(ExpectedInput),
            call(Term)
        )
    ),
    ExpectedOutput = ActualOutput.

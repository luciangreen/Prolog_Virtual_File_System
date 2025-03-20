% Add this to the vfs module exports
/*:- module(vfs, [
    init_vfs/2,
    open_vfs/3,
    read_vfs/2,
    write_vfs/2,
    close_vfs/1,
    get_vfs_state/1,
    convert_to_vfs/2,
    convert_from_vfs/2  % Added new export
]).
*/

:- dynamic vfs_file/2, vfs_stream/3.

% System predicates that we need to preserve
preserved_predicates([
    (is)/2,
    (=)/2,
    (==)/2,
    member/2,
    append/3,
    format/2,
    format/3,
    write/1,
    nl/0,
    call/1,
    atom_concat/3
]).

% Check if predicate is a system predicate
is_system_predicate(Name/Arity) :-
    current_predicate_detour(%system:
    Name/Arity);
    preserved_predicates(List),
    member(Name/Arity, List),!.

current_predicate_detour(_):-!.

% Initialize VFS with a list of files
init_vfs(FileList, State) :-
    retractall(vfs_file(_, _)),
    retractall(vfs_stream(_, _, _)),
    maplist(assert_vfs_file, FileList),
    get_vfs_state(State).

% Assert a single file into VFS
assert_vfs_file(vfs_file(Name, Contents)) :-
    assertz(vfs_file(Name, Contents)).

% Open a file in the VFS
open_vfs(FileName, Mode, Stream) :-
    (Mode = read ->
        vfs_file(FileName, Contents) ->
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_stream(Stream, FileName, Contents))
    ; Mode = write ->
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_stream(Stream, FileName, ''))
    ; Mode = append,
      vfs_file(FileName, ExistingContents) ->
        atom_concat(FileName, '_stream', Stream),
        assertz(vfs_stream(Stream, FileName, ExistingContents))
    ).

% Read from a VFS stream
read_vfs(Stream, Contents) :-
    vfs_stream(Stream, _, Contents).

% Write to a VFS stream
write_vfs(Stream, NewContents) :-
    retract(vfs_stream(Stream, FileName, _)),
    assertz(vfs_stream(Stream, FileName, NewContents)).

% Close a VFS stream and update file contents
close_vfs(Stream) :-
    vfs_stream(Stream, FileName, Contents),
    retract(vfs_stream(Stream, FileName, Contents)),
    retractall(vfs_file(FileName, _)),
    assertz(vfs_file(FileName, Contents)).

% Get current VFS state as a list of vfs_file terms
get_vfs_state(State) :-
    findall(vfs_file(Name, Contents), vfs_file(Name, Contents), State).

% Convert regular Prolog code to VFS-compatible code
convert_to_vfs(Input, Output) :-
    convert_predicates(Input, Output).

% Convert predicates recursively
convert_predicates(Var, Var) :- var(Var), !.
convert_predicates([], []) :- !.
convert_predicates((A, B), (ConvA, ConvB)) :- !,
    convert_predicates(A, ConvA),
    convert_predicates(B, ConvB).
convert_predicates(Term, ConvertedTerm) :-
    Term =.. [Name|Args],
    (convert_predicate_name(Name, NewName) ->
        maplist(convert_predicates, Args, NewArgs),
        ConvertedTerm =.. [NewName|NewArgs]
    ; maplist(convert_predicates, Args, NewArgs),
        ConvertedTerm =.. [Name|NewArgs]
    ).

% Convert from VFS predicates back to regular predicates
convert_from_vfs(Input, Output) :-
    convert_predicates_from_vfs(Input, Output).

% Convert predicates recursively (from VFS to regular)
convert_predicates_from_vfs(Var, Var) :- var(Var), !.
convert_predicates_from_vfs([], []) :- !.
convert_predicates_from_vfs((A, B), (ConvA, ConvB)) :- !,
    convert_predicates_from_vfs(A, ConvA),
    convert_predicates_from_vfs(B, ConvB).
convert_predicates_from_vfs(Term, ConvertedTerm) :-
    Term =.. [Name|Args],
    (convert_from_vfs_name(Name, NewName) ->
        maplist(convert_predicates_from_vfs, Args, NewArgs),
        ConvertedTerm =.. [NewName|NewArgs]
    ; maplist(convert_predicates_from_vfs, Args, NewArgs),
        ConvertedTerm =.. [Name|NewArgs]
    ).

% Convert specific predicate names
convert_predicate_name(Name, NewName) :-
    member(Name-NewName, [
        open-open_vfs,
        read-read_vfs,
        write-write_vfs,
        close-close_vfs
    ]).
    
% Convert VFS predicate names back to regular names
convert_from_vfs_name(Name, NewName) :-
    member(NewName-Name, [
        open-open_vfs,
        read-read_vfs,
        write-write_vfs,
        close-close_vfs
    ]).
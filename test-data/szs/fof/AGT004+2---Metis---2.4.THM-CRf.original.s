% Problem    : AGT004+2 : TPTP v7.1.0. Bugfixed v3.1.0.
% Command    : metis --show proof --show saturation %s
% Computer   : n065.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-693.2.2.el7.x86_64
% CPULimit   : 300
% DateTime   : Tue Aug 28 09:30:41 CDT 2018
% CPUTime    : 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SZS status Theorem for /export/starexec/sandbox/benchmark/theBenchmark.p

% SZS output start CNFRefutation for /export/starexec/sandbox/benchmark/theBenchmark.p
fof(a1_1, axiom,
    (! [A, C, N, L] :
       (accept_team(A, L, C, N) <=>
        (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))))).

fof(deduced_13, axiom,
    (~ accept_city(countryamedicalorganization, coastvillage))).

fof(query_4, conjecture,
    (~
       accept_team(countryamedicalorganization,
         countryahumanitarianorganization, coastvillage, n5))).

fof(subgoal_0, plain,
    (~
       accept_team(countryamedicalorganization,
         countryahumanitarianorganization, coastvillage, n5)),
    inference(strip, [], [query_4])).

fof(negate_0_0, plain,
    (~ ~
       accept_team(countryamedicalorganization,
         countryahumanitarianorganization, coastvillage, n5)),
    inference(negate, [], [subgoal_0])).

fof(normalize_0_0, plain,
    (accept_team(countryamedicalorganization,
       countryahumanitarianorganization, coastvillage, n5)),
    inference(canonicalize, [], [negate_0_0])).

fof(normalize_0_1, plain,
    (! [A, C, L, N] :
       (~ accept_team(A, L, C, N) <=>
        (~ accept_city(A, C) | ~ accept_leader(A, L) |
         ~ accept_number(A, N)))), inference(canonicalize, [], [a1_1])).

fof(normalize_0_2, plain,
    (! [A, C, L, N] :
       (~ accept_team(A, L, C, N) <=>
        (~ accept_city(A, C) | ~ accept_leader(A, L) |
         ~ accept_number(A, N)))),
    inference(specialize, [], [normalize_0_1])).

fof(normalize_0_3, plain,
    (! [A, C, L, N] :
       ((~ accept_team(A, L, C, N) | accept_city(A, C)) &
        (~ accept_team(A, L, C, N) | accept_leader(A, L)) &
        (~ accept_team(A, L, C, N) | accept_number(A, N)) &
        (~ accept_city(A, C) | ~ accept_leader(A, L) |
         ~ accept_number(A, N) | accept_team(A, L, C, N)))),
    inference(clausify, [], [normalize_0_2])).

fof(normalize_0_4, plain,
    (! [A, C, L, N] : (~ accept_team(A, L, C, N) | accept_city(A, C))),
    inference(conjunct, [], [normalize_0_3])).

fof(normalize_0_5, plain,
    (~ accept_city(countryamedicalorganization, coastvillage)),
    inference(canonicalize, [], [deduced_13])).

cnf(refute_0_0, plain,
    (accept_team(countryamedicalorganization,
       countryahumanitarianorganization, coastvillage, n5)),
    inference(canonicalize, [], [normalize_0_0])).

cnf(refute_0_1, plain, (~ accept_team(A, L, C, N) | accept_city(A, C)),
    inference(canonicalize, [], [normalize_0_4])).

cnf(refute_0_2, plain,
    (~
       accept_team(countryamedicalorganization,
         countryahumanitarianorganization, coastvillage, n5) |
     accept_city(countryamedicalorganization, coastvillage)),
    inference(subst, [],
              [refute_0_1 :
               [bind(A, $fot(countryamedicalorganization)),
                bind(C, $fot(coastvillage)),
                bind(L, $fot(countryahumanitarianorganization)),
                bind(N, $fot(n5))]])).

cnf(refute_0_3, plain,
    (accept_city(countryamedicalorganization, coastvillage)),
    inference(resolve,
              [$cnf(accept_team(countryamedicalorganization,
                      countryahumanitarianorganization, coastvillage,
                      n5))], [refute_0_0, refute_0_2])).

cnf(refute_0_4, plain,
    (~ accept_city(countryamedicalorganization, coastvillage)),
    inference(canonicalize, [], [normalize_0_5])).

cnf(refute_0_5, plain, ($false),
    inference(resolve,
              [$cnf(accept_city(countryamedicalorganization,
                      coastvillage))], [refute_0_3, refute_0_4])).
% SZS output end CNFRefutation for /export/starexec/sandbox/benchmark/theBenchmark.p

%------------------------------------------------------------------------------
% File       : Metis---2.4
% Problem    : AGT004+2 : TPTP v7.1.0. Bugfixed v3.1.0.
% Transform  : none
% Format     : tptp:raw
% Command    : metis --show proof --show saturation %s

% Computer   : n065.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-693.2.2.el7.x86_64
% CPULimit   : 300s
% DateTime   : Wed Aug 29 14:06:30 EDT 2018

% Result     : Theorem 0.50s
% Output     : CNFRefutation 0.50s
% Verified   : 
% Statistics : Number of formulae       :   17 (  17 expanded)
%              Number of clauses        :    6 (   6 expanded)
%              Number of leaves         :    3 (   3 expanded)
%              Depth                    :    8
%              Number of atoms          :   38 (  38 expanded)
%              Number of equality atoms :    0 (   0 expanded)
%              Maximal formula depth    :   12 (   4 average)
%              Maximal clause size      :   10 (   1 average)
%              Maximal term depth       :    1 (   1 average)

% Comments   : 
%------------------------------------------------------------------------------
fof(a1_1,axiom,(
    ! [A,C,N,L] :
      ( accept_team(A,L,C,N)
    <=> ( accept_city(A,C)
        & accept_leader(A,L)
        & accept_number(A,N) ) ) )).

fof(deduced_13,axiom,(
    ~ accept_city(countryamedicalorganization,coastvillage) )).

fof(query_4,conjecture,(
    ~ accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) )).

fof(subgoal_0,plain,(
    ~ accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    inference(strip,[],[query_4])).

fof(negate_0_0,plain,(
    ~ ~ accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    inference(negate,[],[subgoal_0])).

fof(normalize_0_0,plain,(
    accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    inference(canonicalize,[],[negate_0_0])).

fof(normalize_0_1,plain,(
    ! [A,C,L,N] :
      ( ~ accept_team(A,L,C,N)
    <=> ( ~ accept_city(A,C)
        | ~ accept_leader(A,L)
        | ~ accept_number(A,N) ) ) ),
    inference(canonicalize,[],[a1_1])).

fof(normalize_0_2,plain,(
    ! [A,C,L,N] :
      ( ~ accept_team(A,L,C,N)
    <=> ( ~ accept_city(A,C)
        | ~ accept_leader(A,L)
        | ~ accept_number(A,N) ) ) ),
    inference(specialize,[],[normalize_0_1])).

fof(normalize_0_3,plain,(
    ! [A,C,L,N] :
      ( ( ~ accept_team(A,L,C,N)
        | accept_city(A,C) )
      & ( ~ accept_team(A,L,C,N)
        | accept_leader(A,L) )
      & ( ~ accept_team(A,L,C,N)
        | accept_number(A,N) )
      & ( ~ accept_city(A,C)
        | ~ accept_leader(A,L)
        | ~ accept_number(A,N)
        | accept_team(A,L,C,N) ) ) ),
    inference(clausify,[],[normalize_0_2])).

fof(normalize_0_4,plain,(
    ! [A,C,L,N] :
      ( ~ accept_team(A,L,C,N)
      | accept_city(A,C) ) ),
    inference(conjunct,[],[normalize_0_3])).

fof(normalize_0_5,plain,(
    ~ accept_city(countryamedicalorganization,coastvillage) ),
    inference(canonicalize,[],[deduced_13])).

cnf(refute_0_0,plain,
    ( accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    inference(canonicalize,[],[normalize_0_0])).

cnf(refute_0_1,plain,
    ( ~ accept_team(A,L,C,N)
    | accept_city(A,C) ),
    inference(canonicalize,[],[normalize_0_4])).

cnf(refute_0_2,plain,
    ( ~ accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5)
    | accept_city(countryamedicalorganization,coastvillage) ),
    inference(subst,[],[refute_0_1:[bind(A,$fot(countryamedicalorganization)),bind(C,$fot(coastvillage)),bind(L,$fot(countryahumanitarianorganization)),bind(N,$fot(n5))]])).

cnf(refute_0_3,plain,
    ( accept_city(countryamedicalorganization,coastvillage) ),
    inference(resolve,[$cnf(accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5))],[refute_0_0,refute_0_2])).

cnf(refute_0_4,plain,
    ( ~ accept_city(countryamedicalorganization,coastvillage) ),
    inference(canonicalize,[],[normalize_0_5])).

cnf(refute_0_5,plain,
    ( $false ),
    inference(resolve,[$cnf(accept_city(countryamedicalorganization,coastvillage))],[refute_0_3,refute_0_4])).
%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.04  % Problem    : AGT004+2 : TPTP v7.1.0. Bugfixed v3.1.0.
% 0.00/0.04  % Command    : metis --show proof --show saturation %s
% 0.03/0.23  % Computer   : n065.star.cs.uiowa.edu
% 0.03/0.23  % Model      : x86_64 x86_64
% 0.03/0.23  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.03/0.23  % Memory     : 32218.625MB
% 0.03/0.23  % OS         : Linux 3.10.0-693.2.2.el7.x86_64
% 0.03/0.23  % CPULimit   : 300
% 0.03/0.23  % DateTime   : Tue Aug 28 09:30:41 CDT 2018
% 0.03/0.23  % CPUTime    : 
% 0.03/0.24  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 0.50/0.68  % SZS status Theorem for /export/starexec/sandbox/benchmark/theBenchmark.p
% 0.50/0.68  
% 0.50/0.68  % SZS output start CNFRefutation for /export/starexec/sandbox/benchmark/theBenchmark.p
% 0.50/0.68  fof(a1_1, axiom,
% 0.50/0.68      (! [A, C, N, L] :
% 0.50/0.68         (accept_team(A, L, C, N) <=>
% 0.50/0.68          (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))))).
% 0.50/0.68  
% 0.50/0.68  fof(deduced_13, axiom,
% 0.50/0.68      (~ accept_city(countryamedicalorganization, coastvillage))).
% 0.50/0.68  
% 0.50/0.68  fof(query_4, conjecture,
% 0.50/0.68      (~
% 0.50/0.68         accept_team(countryamedicalorganization,
% 0.50/0.68           countryahumanitarianorganization, coastvillage, n5))).
% 0.50/0.68  
% 0.50/0.68  fof(subgoal_0, plain,
% 0.50/0.68      (~
% 0.50/0.68         accept_team(countryamedicalorganization,
% 0.50/0.68           countryahumanitarianorganization, coastvillage, n5)),
% 0.50/0.68      inference(strip, [], [query_4])).
% 0.50/0.68  
% 0.50/0.68  fof(negate_0_0, plain,
% 0.50/0.68      (~ ~
% 0.50/0.68         accept_team(countryamedicalorganization,
% 0.50/0.68           countryahumanitarianorganization, coastvillage, n5)),
% 0.50/0.68      inference(negate, [], [subgoal_0])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_0, plain,
% 0.50/0.68      (accept_team(countryamedicalorganization,
% 0.50/0.68         countryahumanitarianorganization, coastvillage, n5)),
% 0.50/0.68      inference(canonicalize, [], [negate_0_0])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_1, plain,
% 0.50/0.68      (! [A, C, L, N] :
% 0.50/0.68         (~ accept_team(A, L, C, N) <=>
% 0.50/0.68          (~ accept_city(A, C) | ~ accept_leader(A, L) |
% 0.50/0.68           ~ accept_number(A, N)))), inference(canonicalize, [], [a1_1])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_2, plain,
% 0.50/0.68      (! [A, C, L, N] :
% 0.50/0.68         (~ accept_team(A, L, C, N) <=>
% 0.50/0.68          (~ accept_city(A, C) | ~ accept_leader(A, L) |
% 0.50/0.68           ~ accept_number(A, N)))),
% 0.50/0.68      inference(specialize, [], [normalize_0_1])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_3, plain,
% 0.50/0.68      (! [A, C, L, N] :
% 0.50/0.68         ((~ accept_team(A, L, C, N) | accept_city(A, C)) &
% 0.50/0.68          (~ accept_team(A, L, C, N) | accept_leader(A, L)) &
% 0.50/0.68          (~ accept_team(A, L, C, N) | accept_number(A, N)) &
% 0.50/0.68          (~ accept_city(A, C) | ~ accept_leader(A, L) |
% 0.50/0.68           ~ accept_number(A, N) | accept_team(A, L, C, N)))),
% 0.50/0.68      inference(clausify, [], [normalize_0_2])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_4, plain,
% 0.50/0.68      (! [A, C, L, N] : (~ accept_team(A, L, C, N) | accept_city(A, C))),
% 0.50/0.68      inference(conjunct, [], [normalize_0_3])).
% 0.50/0.68  
% 0.50/0.68  fof(normalize_0_5, plain,
% 0.50/0.68      (~ accept_city(countryamedicalorganization, coastvillage)),
% 0.50/0.68      inference(canonicalize, [], [deduced_13])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_0, plain,
% 0.50/0.68      (accept_team(countryamedicalorganization,
% 0.50/0.68         countryahumanitarianorganization, coastvillage, n5)),
% 0.50/0.68      inference(canonicalize, [], [normalize_0_0])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_1, plain, (~ accept_team(A, L, C, N) | accept_city(A, C)),
% 0.50/0.68      inference(canonicalize, [], [normalize_0_4])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_2, plain,
% 0.50/0.68      (~
% 0.50/0.68         accept_team(countryamedicalorganization,
% 0.50/0.68           countryahumanitarianorganization, coastvillage, n5) |
% 0.50/0.68       accept_city(countryamedicalorganization, coastvillage)),
% 0.50/0.68      inference(subst, [],
% 0.50/0.68                [refute_0_1 :
% 0.50/0.68                 [bind(A, $fot(countryamedicalorganization)),
% 0.50/0.68                  bind(C, $fot(coastvillage)),
% 0.50/0.68                  bind(L, $fot(countryahumanitarianorganization)),
% 0.50/0.68                  bind(N, $fot(n5))]])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_3, plain,
% 0.50/0.68      (accept_city(countryamedicalorganization, coastvillage)),
% 0.50/0.68      inference(resolve,
% 0.50/0.68                [$cnf(accept_team(countryamedicalorganization,
% 0.50/0.68                        countryahumanitarianorganization, coastvillage,
% 0.50/0.68                        n5))], [refute_0_0, refute_0_2])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_4, plain,
% 0.50/0.68      (~ accept_city(countryamedicalorganization, coastvillage)),
% 0.50/0.68      inference(canonicalize, [], [normalize_0_5])).
% 0.50/0.68  
% 0.50/0.68  cnf(refute_0_5, plain, ($false),
% 0.50/0.68      inference(resolve,
% 0.50/0.68                [$cnf(accept_city(countryamedicalorganization,
% 0.50/0.68                        coastvillage))], [refute_0_3, refute_0_4])).
% 0.50/0.68  % SZS output end CNFRefutation for /export/starexec/sandbox/benchmark/theBenchmark.p
% 0.50/0.68  
%------------------------------------------------------------------------------

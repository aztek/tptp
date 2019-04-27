%------------------------------------------------------------------------------
% File       : SPASS---3.9
% Problem    : AGT004+2 : TPTP v6.4.0. Bugfixed v3.1.0.
% Transform  : none
% Format     : tptp
% Command    : run_spass %d %s

% Computer   : n017.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-514.6.1.el7.x86_64
% CPULimit   : 300s
% DateTime   : Fri Jul 14 14:11:04 EDT 2017

% Result     : Theorem 0.07s
% Output     : Refutation 0.07s
% Verified   : 
% Statistics : Number of clauses        :    5 (   5 expanded)
%              Number of leaves         :    3 (   3 expanded)
%              Depth                    :    2
%              Number of atoms          :    6 (   6 expanded)
%              Number of equality atoms :    0 (   0 expanded)
%              Maximal clause size      :    2 (   1 average)
%              Maximal term depth       :    1 (   1 average)

% Comments   : 
%------------------------------------------------------------------------------
cnf(305,axiom,
    ( ~ accept_city(countryamedicalorganization,coastvillage) ),
    file('AGT004+2.p',unknown),
    []).

cnf(474,axiom,
    ( accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    file('AGT004+2.p',unknown),
    []).

cnf(910,axiom,
    ( ~ accept_team(u,v,w,x)
    | accept_city(u,w) ),
    file('AGT004+2.p',unknown),
    []).

cnf(997,plain,
    ( accept_city(countryamedicalorganization,coastvillage) ),
    inference(res,[status(thm),theory(equality)],[474,910]),
    [iquote('0:Res:474.0,910.0')]).

cnf(1000,plain,
    ( $false ),
    inference(mrr,[status(thm)],[997,305]),
    [iquote('0:MRR:997.0,305.0')]).

%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.03  % Problem    : AGT004+2 : TPTP v6.4.0. Bugfixed v3.1.0.
% 0.00/0.04  % Command    : run_spass %d %s
% 0.02/0.23  % Computer   : n017.star.cs.uiowa.edu
% 0.02/0.23  % Model      : x86_64 x86_64
% 0.02/0.23  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.02/0.23  % Memory     : 32218.625MB
% 0.02/0.23  % OS         : Linux 3.10.0-514.6.1.el7.x86_64
% 0.02/0.23  % CPULimit   : 300
% 0.02/0.23  % DateTime   : Fri Jul 14 10:46:52 CDT 2017
% 0.02/0.23  % CPUTime    : 
% 0.07/0.50  
% 0.07/0.50  SPASS V 3.9 
% 0.07/0.50  SPASS beiseite: Proof found.
% 0.07/0.50  % SZS status Theorem
% 0.07/0.50  Problem: /export/starexec/sandbox/benchmark/theBenchmark.p 
% 0.07/0.50  SPASS derived 3 clauses, backtracked 0 clauses, performed 0 splits and kept 960 clauses.
% 0.07/0.50  SPASS allocated 99320 KBytes.
% 0.07/0.50  SPASS spent	0:00:00.24 on the problem.
% 0.07/0.50  		0:00:00.03 for the input.
% 0.07/0.50  		0:00:00.08 for the FLOTTER CNF translation.
% 0.07/0.50  		0:00:00.00 for inferences.
% 0.07/0.50  		0:00:00.00 for the backtracking.
% 0.07/0.50  		0:00:00.06 for the reduction.
% 0.07/0.50  
% 0.07/0.50  
% 0.07/0.50  Here is a proof with depth 1, length 5 :
% 0.07/0.50  % SZS output start Refutation
% 0.07/0.50  305[0:Inp] || accept_city(countryamedicalorganization,coastvillage)* -> .
% 0.07/0.50  474[0:Inp] ||  -> accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5)*.
% 0.07/0.50  910[0:Inp] || accept_team(u,v,w,x)* -> accept_city(u,w).
% 0.07/0.50  997[0:Res:474.0,910.0] ||  -> accept_city(countryamedicalorganization,coastvillage)*.
% 0.07/0.50  1000[0:MRR:997.0,305.0] ||  -> .
% 0.07/0.50  % SZS output end Refutation
% 0.07/0.50  Formulae used in the proof : deduced_13 query_4 a1_1
% 0.07/0.50  
%------------------------------------------------------------------------------

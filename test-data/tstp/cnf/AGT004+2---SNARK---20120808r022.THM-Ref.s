%------------------------------------------------------------------------------
% File       : SNARK---20120808r022
% Problem    : AGT004+2 : TPTP v6.4.0. Bugfixed v3.1.0.
% Transform  : none
% Format     : tptp:raw
% Command    : run-snark %s %d

% Computer   : n145.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.75MB
% OS         : Linux 3.10.0-327.10.1.el7.x86_64
% CPULimit   : 300s
% DateTime   : Mon Apr 25 10:31:20 EDT 2016

% Result     : Theorem 0.07s
% Output     : Refutation 0.07s
% Verified   : 
% Statistics : Number of clauses        :    4 (   4 expanded)
%              Number of leaves         :    3 (   3 expanded)
%              Depth                    :    1
%              Number of atoms          :    5 (   5 expanded)
%              Number of equality atoms :    0 (   0 expanded)
%              Maximal clause size      :    2 (   1 average)
%              Maximal term depth       :    1 (   1 average)

% Comments   : 
%------------------------------------------------------------------------------
cnf(1,axiom,
    ( ~ accept_team(X,Y,Z,U)
    | accept_city(X,Z) ),
    file('/export/starexec/sandbox/benchmark/Axioms/AGT001+0.ax',a1_1)).

cnf(350,axiom,
    ( ~ accept_city(countryamedicalorganization,coastvillage) ),
    file('/export/starexec/sandbox/benchmark/Axioms/AGT001+2.ax',deduced_13)).

cnf(957,negated_conjecture,
    ( accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5) ),
    file('/export/starexec/sandbox/benchmark/theBenchmark.p',query_4)).

cnf(964,plain,
    ( $false ),
    inference('UR-RESOLVE',[status(thm)],[1,957,350])).
%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.03  % Problem    : AGT004+2 : TPTP v6.4.0. Bugfixed v3.1.0.
% 0.00/0.04  % Command    : run-snark %s %d
% 0.03/0.23  % Computer   : n145.star.cs.uiowa.edu
% 0.03/0.23  % Model      : x86_64 x86_64
% 0.03/0.23  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.03/0.23  % Memory     : 32218.75MB
% 0.03/0.23  % OS         : Linux 3.10.0-327.10.1.el7.x86_64
% 0.03/0.23  % CPULimit   : 300
% 0.03/0.23  % DateTime   : Sun Apr 24 09:46:49 CDT 2016
% 0.03/0.23  % CPUTime    : 
% 0.03/0.26  /export/starexec/sandbox/benchmark/theBenchmark.p
% 0.03/0.27  * 
% 0.03/0.27  * 
% 0.03/0.27  #<PACKAGE "SNARK-USER">
% 0.03/0.27  * 
% 0.03/0.27  SNARK-TPTP-OPTIONS
% 0.03/0.27  * 
% 0.03/0.27  ((AGENDA-LENGTH-LIMIT NIL) (AGENDA-LENGTH-BEFORE-SIMPLIFICATION-LIMIT NIL)
% 0.03/0.27   (USE-HYPERRESOLUTION T) (USE-UR-RESOLUTION T) (USE-PARAMODULATION T)
% 0.03/0.27   (USE-FACTORING :POS)
% 0.03/0.27   (USE-LITERAL-ORDERING-WITH-HYPERRESOLUTION 'LITERAL-ORDERING-P)
% 0.03/0.27   (USE-LITERAL-ORDERING-WITH-PARAMODULATION 'LITERAL-ORDERING-P)
% 0.03/0.27   (ORDERING-FUNCTIONS>CONSTANTS T) (ASSERT-CONTEXT :CURRENT)
% 0.03/0.27   (RUN-TIME-LIMIT 300) (LISTEN-FOR-COMMANDS NIL)
% 0.03/0.27   (USE-CLOSURE-WHEN-SATISFIABLE T) (PRINT-ROWS-WHEN-GIVEN NIL)
% 0.03/0.27   (PRINT-ROWS-WHEN-DERIVED NIL) (PRINT-UNORIENTABLE-ROWS NIL)
% 0.03/0.27   (PRINT-ROW-WFFS-PRETTILY NIL) (PRINT-FINAL-ROWS :TPTP)
% 0.03/0.27   (PRINT-OPTIONS-WHEN-STARTING NIL) (USE-VARIABLE-NAME-SORTS NIL)
% 0.03/0.27   (USE-PURITY-TEST T) (USE-RELEVANCE-TEST T) (DECLARE-TPTP-SYMBOLS1)
% 0.03/0.27   (DECLARE-TPTP-SYMBOLS2))
% 0.03/0.27  * 
% 0.03/0.27  "."
% 0.03/0.27  * 
% 0.03/0.27  ; Begin refute-file /export/starexec/sandbox/benchmark/theBenchmark.p 2016-04-24T09:46:49
% 0.03/0.27  ; Running SNARK from /davis/home/graph/tptp/Systems/SNARK---20120808r022/Source/snark-system.lisp in SBCL 1.0.12 on n145.star.cs.uiowa.edu at 2016-04-24T09:46:49
% 0.07/0.36  WARNING:
% 0.07/0.36     |sum| is a 3-ary relation that occurs only negatively; disabling rows that contain it.
% 0.07/0.36  WARNING:
% 0.07/0.36     |min_number_of_proposed_agents| is a 2-ary relation that occurs only negatively; disabling rows that contain it.
% 0.07/0.45  
% 0.07/0.45  
% 0.07/0.45  #||
% 0.07/0.45  % SZS status Theorem for /export/starexec/sandbox/benchmark/theBenchmark.p
% 0.07/0.45  % SZS output start Refutation
% 0.07/0.45  cnf(1, axiom,
% 0.07/0.46      (~ accept_team(X,Y,Z,U) | accept_city(X,Z)),
% 0.07/0.46      file('/export/starexec/sandbox/benchmark/Axioms/AGT001+0.ax',a1_1)).
% 0.07/0.46  cnf(350, axiom,
% 0.07/0.46      ~ accept_city(countryamedicalorganization,coastvillage),
% 0.07/0.46      file('/export/starexec/sandbox/benchmark/Axioms/AGT001+2.ax',deduced_13)).
% 0.07/0.46  cnf(957, negated_conjecture,
% 0.07/0.46      accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n5),
% 0.07/0.46      file('/export/starexec/sandbox/benchmark/theBenchmark.p',query_4)).
% 0.07/0.46  cnf(964, plain,
% 0.07/0.46      $false,
% 0.07/0.46      inference('UR-RESOLVE',[status(thm)],[1,957,350])).
% 0.07/0.46  % SZS output end Refutation
% 0.07/0.46  ||#
% 0.07/0.46  
% 0.07/0.46  ; Summary of computation:
% 0.07/0.46  ;      1820 formulas have been input or derived (from 691 formulas).
% 0.07/0.46  ;       964 (53%) were retained.  Of these,
% 0.07/0.46  ;          964 (100%) are still being kept.
% 0.07/0.46  ; 
% 0.07/0.46  ; Run time in seconds:
% 0.07/0.46  ;     0.070  38%   Read assertion file          (1 call)
% 0.07/0.46  ;     0.008   4%   Assert                       (923 calls)
% 0.07/0.46  ;     0.045  25%   Process new row              (1,591 calls)
% 0.07/0.46  ;     0.019  10%   Resolution                   (1,380 calls)
% 0.07/0.46  ;     0.002   1%   Paramodulation               (690 calls)
% 0.07/0.46  ;     0.000   0%   Condensing                   (103 calls)
% 0.07/0.46  ;     0.000   0%   Forward subsumption          (103 calls)
% 0.07/0.46  ;     0.003   2%   Backward subsumption         (103 calls)
% 0.07/0.46  ;     0.000   0%   Clause clause subsumption    (1 call)
% 0.07/0.46  ;     0.006   3%   Forward simplification       (1,589 calls)
% 0.07/0.46  ;     0.001   1%   Backward simplification      (964 calls)
% 0.07/0.46  ;     0.000   0%   Ordering                     (3 calls)
% 0.07/0.46  ;     0.000   0%   Sortal reasoning             (32 calls)
% 0.07/0.46  ;     0.001   1%   Purity testing               (1 call)
% 0.07/0.46  ;     0.028  15%   Other
% 0.07/0.46  ;     0.183        Total
% 0.07/0.46  ;     0.183        Real time
% 0.07/0.46  ; 
% 0.07/0.46  ; Term-hash-array has 1,347 terms in all.
% 0.07/0.46  ; Feature-vector-row-index has 103 entries (103 at peak, 103 added, 0 deleted).
% 0.07/0.46  ; Feature-vector-row-index has 320 nodes (320 at peak, 320 added, 0 deleted).
% 0.07/0.46  ;  Retrieved 3 possibly forward subsuming rows in 103 calls.
% 0.07/0.46  ;  Retrieved 3 possibly backward subsumed rows in 103 calls.
% 0.07/0.46  ; Path-index has 1,633 entries (1,633 at peak, 1,633 added, 0 deleted).
% 0.07/0.46  ; Path-index has 1,094 nodes (1,094 at peak, 1,094 added, 0 deleted).
% 0.07/0.46  ; Trie-index has 1,633 entries (1,633 at peak, 1,633 added, 0 deleted).
% 0.07/0.46  ; Trie-index has 4,005 nodes (4,005 at peak, 4,005 added, 0 deleted).
% 0.07/0.46  ; Retrieved 625 generalization terms in 1,527 calls.
% 0.07/0.46  ; Retrieved 866 instance terms in 860 calls.
% 0.07/0.46  ; Retrieved 4,483 unifiable terms in 4,149 calls.
% 0.07/0.46  ; 
% 0.07/0.46  ; The agenda of rows to process has 164 entries:
% 0.07/0.46  ;   162 with value 5               2 with value 7
% 0.07/0.46  ; The agenda of input rows to give has 267 entries:
% 0.07/0.46  ;     3 with value 10              1 with value 14               1 with value 21
% 0.07/0.46  ;     1 with value 11             59 with value 16               1 with value 22
% 0.07/0.46  ;   183 with value 12              1 with value 18              13 with value 29
% 0.07/0.46  ;     2 with value 13              2 with value 20
% 0.07/0.46  ; The agenda of rows to give has 6 entries:
% 0.07/0.46  ;     2 with value (4 5)           4 with value (4 6)
% 0.07/0.46  Evaluation took:
% 0.07/0.46    0.184 seconds of real time
% 0.07/0.46    0.169975 seconds of user run time
% 0.07/0.46    0.014081 seconds of system run time
% 0.07/0.46    0 calls to %EVAL
% 0.07/0.46    0 page faults and
% 0.07/0.46    12,086,320 bytes consed.
% 0.07/0.46  :PROOF-FOUND
% 0.07/0.46  ; End refute-file /export/starexec/sandbox/benchmark/theBenchmark.p 2016-04-24T09:46:49
% 0.07/0.46  :PROOF-FOUND
% 0.07/0.46  * 
%------------------------------------------------------------------------------

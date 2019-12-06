% Problem    : AGT004+2 : TPTP v6.4.0. Bugfixed v3.1.0.
% Command    : z3_tptp -proof -model -t:%d -file:%s
% Computer   : n099.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% Memory     : 32218.75MB
% OS         : Linux 3.10.0-327.10.1.el7.x86_64
% CPULimit   : 300
% DateTime   : Thu Jul 21 10:50:24 CDT 2016
% CPUTime    : 
% SZS status Theorem
% SZS output start Proof
tff(accept_number_type, type, (
   accept_number: ( $i * $i ) > $o)).
tff(n5_type, type, (
   n5: $i)).
tff(countryamedicalorganization_type, type, (
   countryamedicalorganization: $i)).
tff(accept_leader_type, type, (
   accept_leader: ( $i * $i ) > $o)).
tff(countryahumanitarianorganization_type, type, (
   countryahumanitarianorganization: $i)).
tff(accept_city_type, type, (
   accept_city: ( $i * $i ) > $o)).
tff(coastvillage_type, type, (
   coastvillage: $i)).
tff(accept_team_type, type, (
   accept_team: ( $i * $i * $i * $i ) > $o)).
tff(1,axiom,((~accept_city(countryamedicalorganization, coastvillage))), file('/export/starexec/sandbox/benchmark/Axioms/AGT001+2.ax','deduced_13')).
tff(2,plain,
    ((((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5))) | accept_city(countryamedicalorganization, coastvillage))),
    inference(tautology,[status(thm)],[])).
tff(3,plain,
    (((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5)))),
    inference(unit_resolution,[status(thm)],[2, 1])).
tff(4,plain,
    (((~(~accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5))) <=> accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5))),
    inference(rewrite,[status(thm)],[])).
tff(5,axiom,((~(~accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5)))), file('/export/starexec/sandbox/benchmark/theBenchmark.p','query_4')).
tff(6,plain,
    (accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5)),
    inference(modus_ponens,[status(thm)],[5, 4])).
tff(7,plain,
    (((~(accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5) <=> (~((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5)))))) | (~accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5)) | (~((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5)))))),
    inference(tautology,[status(thm)],[])).
tff(8,plain,
    ((~(accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5) <=> (~((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5))))))),
    inference(unit_resolution,[status(thm)],[7, 6, 3])).
tff(9,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : ((accept_team(X4, X1, X3, X2) <=> (~((~accept_city(X4, X3)) | (~accept_leader(X4, X1)) | (~accept_number(X4, X2))))) <=> (accept_team(X4, X1, X3, X2) <=> (~((~accept_city(X4, X3)) | (~accept_leader(X4, X1)) | (~accept_number(X4, X2))))))),
    inference(reflexivity,[status(thm)],[])).
tff(10,plain,
    ((![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N))))) <=> ![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N))))))),
    inference(quant_intro,[status(thm)],[9])).
tff(11,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : ((accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2)) <=> (~((~accept_city(X4, X3)) | (~accept_leader(X4, X1)) | (~accept_number(X4, X2)))))),
    inference(rewrite,[status(thm)],[])).
tff(12,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : ((accept_team(X4, X1, X3, X2) <=> (accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2))) <=> (accept_team(X4, X1, X3, X2) <=> (~((~accept_city(X4, X3)) | (~accept_leader(X4, X1)) | (~accept_number(X4, X2))))))),
    inference(monotonicity,[status(thm)],[11])).
tff(13,plain,
    ((![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))) <=> ![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N))))))),
    inference(quant_intro,[status(thm)],[12])).
tff(14,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : ((accept_team(X4, X1, X3, X2) <=> (accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2))) <=> (accept_team(X4, X1, X3, X2) <=> (accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2))))),
    inference(rewrite,[status(thm)],[])).
tff(15,plain,
    ((![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))) <=> ![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))))),
    inference(quant_intro,[status(thm)],[14])).
tff(16,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : (((accept_city(X4, X3) & accept_leader(X4, X1)) & accept_number(X4, X2)) <=> (accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2)))),
    inference(rewrite,[status(thm)],[])).
tff(17,plain,
    (![X4: $i, X3: $i, X2: $i, X1: $i] : ((accept_team(X4, X1, X3, X2) <=> ((accept_city(X4, X3) & accept_leader(X4, X1)) & accept_number(X4, X2))) <=> (accept_team(X4, X1, X3, X2) <=> (accept_city(X4, X3) & accept_leader(X4, X1) & accept_number(X4, X2))))),
    inference(monotonicity,[status(thm)],[16])).
tff(18,plain,
    ((![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> ((accept_city(A, C) & accept_leader(A, L)) & accept_number(A, N))) <=> ![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N))))),
    inference(quant_intro,[status(thm)],[17])).
tff(19,axiom,(![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> ((accept_city(A, C) & accept_leader(A, L)) & accept_number(A, N)))), file('/export/starexec/sandbox/benchmark/Axioms/AGT001+0.ax','a1_1')).
tff(20,plain,
    (![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N)))),
    inference(modus_ponens,[status(thm)],[19, 18])).
tff(21,plain,
    (![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N)))),
    inference(modus_ponens,[status(thm)],[20, 15])).
tff(22,plain,(
    ![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (accept_city(A, C) & accept_leader(A, L) & accept_number(A, N)))),
    inference(nnf,[status(sab)],[21])).
tff(23,plain,
    (![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N)))))),
    inference(modus_ponens,[status(thm)],[22, 13])).
tff(24,plain,
    (![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N)))))),
    inference(modus_ponens,[status(thm)],[23, 10])).
tff(25,plain,
    (((~![A: $i, C: $i, N: $i, L: $i] : (accept_team(A, L, C, N) <=> (~((~accept_city(A, C)) | (~accept_leader(A, L)) | (~accept_number(A, N)))))) | (accept_team(countryamedicalorganization, countryahumanitarianorganization, coastvillage, n5) <=> (~((~accept_city(countryamedicalorganization, coastvillage)) | (~accept_leader(countryamedicalorganization, countryahumanitarianorganization)) | (~accept_number(countryamedicalorganization, n5))))))),
    inference(quant_inst,[status(thm)],[])).
tff(26,plain,
    ($false),
    inference(unit_resolution,[status(thm)],[25, 24, 8])).
% SZS output end Proof

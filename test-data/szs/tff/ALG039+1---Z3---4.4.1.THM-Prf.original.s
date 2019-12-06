% Problem    : ALG039+1 : TPTP v6.4.0. Released v2.7.0.
% Command    : z3_tptp -proof -model -t:%d -file:%s
% Computer   : n088.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% Memory     : 32218.75MB
% OS         : Linux 3.10.0-327.10.1.el7.x86_64
% CPULimit   : 300
% DateTime   : Thu Jul 21 10:50:10 CDT 2016
% CPUTime    : 
% SZS status Theorem
% SZS output start Proof
tff(e3_type, type, (
   e3: $i)).
tff(op_type, type, (
   op: ( $i * $i ) > $i)).
tff(e2_type, type, (
   e2: $i)).
tff(e1_type, type, (
   e1: $i)).
tff(e0_type, type, (
   e0: $i)).
tff(1,plain,
    (((~$true) <=> $false)),
    inference(rewrite,[status(thm)],[])).
tff(2,plain,
    (((~$false) <=> $true)),
    inference(rewrite,[status(thm)],[])).
tff(3,plain,
    ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))) & (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))) <=> $false)),
    inference(rewrite,[status(thm)],[])).
tff(4,plain,
    ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
    inference(rewrite,[status(thm)],[])).
tff(5,plain,
    (((((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))),
    inference(rewrite,[status(thm)],[])).
tff(6,plain,
    (((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)))),
    inference(rewrite,[status(thm)],[])).
tff(7,plain,
    ((((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> (((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))),
    inference(monotonicity,[status(thm)],[6])).
tff(8,plain,
    ((((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))),
    inference(transitivity,[status(thm)],[7, 5])).
tff(9,plain,
    ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
    inference(rewrite,[status(thm)],[])).
tff(10,plain,
    (((((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)))),
    inference(rewrite,[status(thm)],[])).
tff(11,plain,
    (((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)))),
    inference(rewrite,[status(thm)],[])).
tff(12,plain,
    ((((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> (((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)))),
    inference(monotonicity,[status(thm)],[11])).
tff(13,plain,
    ((((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)))),
    inference(transitivity,[status(thm)],[12, 10])).
tff(14,plain,
    (((((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)))),
    inference(rewrite,[status(thm)],[])).
tff(15,plain,
    (((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)))),
    inference(rewrite,[status(thm)],[])).
tff(16,plain,
    ((((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> (((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)))),
    inference(monotonicity,[status(thm)],[15])).
tff(17,plain,
    ((((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)))),
    inference(transitivity,[status(thm)],[16, 14])).
tff(18,plain,
    (((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)))),
    inference(rewrite,[status(thm)],[])).
tff(19,plain,
    (((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)))),
    inference(rewrite,[status(thm)],[])).
tff(20,plain,
    ((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)))),
    inference(monotonicity,[status(thm)],[19])).
tff(21,plain,
    ((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)))),
    inference(transitivity,[status(thm)],[20, 18])).
tff(22,plain,
    (((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))))),
    inference(monotonicity,[status(thm)],[21, 17])).
tff(23,plain,
    ((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
    inference(monotonicity,[status(thm)],[22, 13])).
tff(24,plain,
    ((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
    inference(transitivity,[status(thm)],[23, 9])).
tff(25,plain,
    (((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
    inference(monotonicity,[status(thm)],[24, 8])).
tff(26,plain,
    (((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
    inference(transitivity,[status(thm)],[25, 4])).
tff(27,plain,
    (((~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))) <=> (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))))),
    inference(monotonicity,[status(thm)],[26])).
tff(28,plain,
    ((((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))) & (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))))),
    inference(monotonicity,[status(thm)],[26, 27])).
tff(29,plain,
    ((((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))) <=> $false)),
    inference(transitivity,[status(thm)],[28, 3])).
tff(30,plain,
    (((~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))) <=> (~$false))),
    inference(monotonicity,[status(thm)],[29])).
tff(31,plain,
    (((~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))) <=> $true)),
    inference(transitivity,[status(thm)],[30, 2])).
tff(32,plain,
    (((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))))) <=> (~$true))),
    inference(monotonicity,[status(thm)],[31])).
tff(33,plain,
    (((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))))) <=> $false)),
    inference(transitivity,[status(thm)],[32, 1])).
tff(34,axiom,((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))))), file('/export/starexec/sandbox/benchmark/theBenchmark.p','co1')).
tff(35,plain,
    ($false),
    inference(modus_ponens,[status(thm)],[34, 33])).
% SZS output end Proof
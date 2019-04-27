%------------------------------------------------------------------------------
% File       : SPASS---3.9
% Problem    : ALG028+1 : TPTP v6.4.0. Released v2.7.0.
% Transform  : none
% Format     : tptp
% Command    : run_spass %d %s

% Computer   : n018.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-514.6.1.el7.x86_64
% CPULimit   : 300s
% DateTime   : Fri Jul 14 14:11:14 EDT 2017

% Result     : Theorem 0.06s
% Output     : Refutation 0.06s
% Verified   : 
% Statistics : Number of clauses        :  126 (9526 expanded)
%              Number of leaves         :   42 (6040 expanded)
%              Depth                    :   18
%              Number of atoms          :  184 (9754 expanded)
%              Number of equality atoms :  183 (9753 expanded)
%              Maximal clause size      :   21 (   1 average)
%              Maximal term depth       :    5 (   2 average)

% Comments   : 
%------------------------------------------------------------------------------
cnf(35,axiom,
    ( equal(op(e4,e4),e3) ),
    file('ALG028+1.p',unknown),
    []).

cnf(64,axiom,
    ( equal(op(e1,e0),op(e0,e1)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(65,axiom,
    ( equal(op(e2,e0),op(e0,e2)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(66,axiom,
    ( equal(op(e3,e0),op(e0,e3)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(67,axiom,
    ( equal(op(e4,e0),op(e0,e4)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(68,axiom,
    ( equal(op(e5,e0),op(e0,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(70,axiom,
    ( equal(op(e2,e1),op(e1,e2)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(71,axiom,
    ( equal(op(e3,e1),op(e1,e3)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(72,axiom,
    ( equal(op(e4,e1),op(e1,e4)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(73,axiom,
    ( equal(op(e5,e1),op(e1,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(75,axiom,
    ( equal(op(e3,e2),op(e2,e3)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(76,axiom,
    ( equal(op(e4,e2),op(e2,e4)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(77,axiom,
    ( equal(op(e5,e2),op(e2,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(79,axiom,
    ( equal(op(e4,e3),op(e3,e4)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(80,axiom,
    ( equal(op(e5,e3),op(e3,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(82,axiom,
    ( equal(op(e5,e4),op(e4,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(84,axiom,
    ( equal(op(op(e4,e4),e4),e1) ),
    file('ALG028+1.p',unknown),
    []).

cnf(235,axiom,
    ( ~ equal(op(e4,e1),op(e4,e0)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(236,axiom,
    ( ~ equal(op(e4,e2),op(e4,e0)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(237,axiom,
    ( ~ equal(op(e4,e3),op(e4,e0)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(238,axiom,
    ( ~ equal(op(e4,e4),op(e4,e0)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(239,axiom,
    ( ~ equal(op(e4,e5),op(e4,e0)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(295,axiom,
    ( equal(op(op(op(e4,e4),e4),e4),e2) ),
    file('ALG028+1.p',unknown),
    []).

cnf(441,axiom,
    ( equal(op(op(e4,e0),e1),op(e4,op(e0,e1))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(442,axiom,
    ( equal(op(op(e4,e0),e2),op(e4,op(e0,e2))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(443,axiom,
    ( equal(op(op(e4,e0),e3),op(e4,op(e0,e3))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(446,axiom,
    ( equal(op(op(e4,e1),e0),op(e4,op(e1,e0))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(452,axiom,
    ( equal(op(op(e4,e2),e0),op(e4,op(e2,e0))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(458,axiom,
    ( equal(op(op(e4,e3),e0),op(e4,op(e3,e0))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(459,axiom,
    ( equal(op(op(e4,e3),e1),op(e4,op(e3,e1))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(460,axiom,
    ( equal(op(op(e4,e3),e2),op(e4,op(e3,e2))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(464,axiom,
    ( equal(op(op(e4,e4),e0),op(e4,op(e4,e0))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(465,axiom,
    ( equal(op(op(e4,e4),e1),op(e4,op(e4,e1))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(466,axiom,
    ( equal(op(op(e4,e4),e2),op(e4,op(e4,e2))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(474,axiom,
    ( equal(op(op(e4,e5),e4),op(e4,op(e5,e4))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(501,axiom,
    ( equal(op(op(e5,e4),e1),op(e5,op(e4,e1))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(503,axiom,
    ( equal(op(op(e5,e4),e3),op(e5,op(e4,e3))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(504,axiom,
    ( equal(op(op(e5,e4),e4),op(e5,op(e4,e4))) ),
    file('ALG028+1.p',unknown),
    []).

cnf(512,axiom,
    ( equal(op(op(op(op(e4,e4),e4),e4),e4),e5) ),
    file('ALG028+1.p',unknown),
    []).

cnf(513,axiom,
    ( equal(op(op(op(op(e4,e4),e4),e4),op(e4,e4)),e0) ),
    file('ALG028+1.p',unknown),
    []).

cnf(523,axiom,
    ( equal(op(e5,e3),e0)
    | equal(op(e5,e3),e1)
    | equal(op(e5,e3),e2)
    | equal(op(e5,e3),e3)
    | equal(op(e5,e3),e4)
    | equal(op(e5,e3),e5) ),
    file('ALG028+1.p',unknown),
    []).

cnf(557,axiom,
    ( ~ equal(op(e0,e0),op(e0,e0))
    | ~ equal(op(e1,e0),op(e0,e1))
    | ~ equal(op(e2,e0),op(e0,e2))
    | ~ equal(op(e3,e0),op(e0,e3))
    | ~ equal(op(e4,e0),op(e0,e4))
    | ~ equal(op(e5,e0),op(e0,e5))
    | ~ equal(op(e1,e1),op(e1,e1))
    | ~ equal(op(e2,e1),op(e1,e2))
    | ~ equal(op(e3,e1),op(e1,e3))
    | ~ equal(op(e4,e1),op(e1,e4))
    | ~ equal(op(e5,e1),op(e1,e5))
    | ~ equal(op(e2,e2),op(e2,e2))
    | ~ equal(op(e3,e2),op(e2,e3))
    | ~ equal(op(e4,e2),op(e2,e4))
    | ~ equal(op(e5,e2),op(e2,e5))
    | ~ equal(op(e3,e3),op(e3,e3))
    | ~ equal(op(e4,e3),op(e3,e4))
    | ~ equal(op(e5,e3),op(e3,e5))
    | ~ equal(op(e4,e4),op(e4,e4))
    | ~ equal(op(e5,e4),op(e4,e5))
    | ~ equal(op(e5,e5),op(e5,e5)) ),
    file('ALG028+1.p',unknown),
    []).

cnf(558,plain,
    ( equal(op(e3,e4),e1) ),
    inference(rew,[status(thm),theory(equality)],[35,84]),
    [iquote('0:Rew:35.0,84.0')]).

cnf(559,plain,
    ( equal(op(e4,e3),e1) ),
    inference(rew,[status(thm),theory(equality)],[558,79]),
    [iquote('0:Rew:558.0,79.0')]).

cnf(585,plain,
    ( ~ equal(op(e4,e5),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[67,239]),
    [iquote('0:Rew:67.0,239.0')]).

cnf(586,plain,
    ( ~ equal(op(e0,e4),e3) ),
    inference(rew,[status(thm),theory(equality)],[35,238,67]),
    [iquote('0:Rew:35.0,238.0,67.0,238.0')]).

cnf(587,plain,
    ( ~ equal(op(e0,e4),e1) ),
    inference(rew,[status(thm),theory(equality)],[559,237,67]),
    [iquote('0:Rew:559.0,237.0,67.0,237.0')]).

cnf(588,plain,
    ( ~ equal(op(e2,e4),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[76,236,67]),
    [iquote('0:Rew:76.0,236.0,67.0,236.0')]).

cnf(589,plain,
    ( ~ equal(op(e1,e4),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[72,235,67]),
    [iquote('0:Rew:72.0,235.0,67.0,235.0')]).

cnf(680,plain,
    ( equal(op(e1,e4),e2) ),
    inference(rew,[status(thm),theory(equality)],[558,295,35]),
    [iquote('0:Rew:558.0,295.0,35.0,295.0')]).

cnf(681,plain,
    ( equal(op(e4,e1),e2) ),
    inference(rew,[status(thm),theory(equality)],[680,72]),
    [iquote('0:Rew:680.0,72.0')]).

cnf(686,plain,
    ( ~ equal(op(e0,e4),e2) ),
    inference(rew,[status(thm),theory(equality)],[680,589]),
    [iquote('0:Rew:680.0,589.0')]).

cnf(692,plain,
    ( equal(op(e2,e4),e5) ),
    inference(rew,[status(thm),theory(equality)],[680,512,558,35]),
    [iquote('0:Rew:680.0,512.0,558.0,512.0,35.0,512.0')]).

cnf(693,plain,
    ( equal(op(e4,e2),e5) ),
    inference(rew,[status(thm),theory(equality)],[692,76]),
    [iquote('0:Rew:692.0,76.0')]).

cnf(697,plain,
    ( ~ equal(op(e0,e4),e5) ),
    inference(rew,[status(thm),theory(equality)],[692,588]),
    [iquote('0:Rew:692.0,588.0')]).

cnf(710,plain,
    ( equal(op(op(e4,e5),e4),op(e3,e5)) ),
    inference(rew,[status(thm),theory(equality)],[82,504,80,35]),
    [iquote('0:Rew:82.0,504.0,80.0,504.0,35.0,504.0')]).

cnf(711,plain,
    ( equal(op(op(e4,e5),e3),op(e1,e5)) ),
    inference(rew,[status(thm),theory(equality)],[82,503,73,559]),
    [iquote('0:Rew:82.0,503.0,73.0,503.0,559.0,503.0')]).

cnf(713,plain,
    ( equal(op(op(e4,e5),e1),op(e2,e5)) ),
    inference(rew,[status(thm),theory(equality)],[82,501,77,681]),
    [iquote('0:Rew:82.0,501.0,77.0,501.0,681.0,501.0')]).

cnf(742,plain,
    ( equal(op(e4,op(e4,e5)),op(e3,e5)) ),
    inference(rew,[status(thm),theory(equality)],[710,474,82]),
    [iquote('0:Rew:710.0,474.0,82.0,474.0')]).

cnf(759,plain,
    ( equal(op(e4,e5),op(e2,e3)) ),
    inference(rew,[status(thm),theory(equality)],[75,466,35,693]),
    [iquote('0:Rew:75.0,466.0,35.0,466.0,693.0,466.0')]).

cnf(760,plain,
    ( equal(op(e5,e4),op(e2,e3)) ),
    inference(rew,[status(thm),theory(equality)],[759,82]),
    [iquote('0:Rew:759.0,82.0')]).

cnf(768,plain,
    ( ~ equal(op(e2,e3),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[759,585]),
    [iquote('0:Rew:759.0,585.0')]).

cnf(771,plain,
    ( equal(op(op(e2,e3),e4),op(e3,e5)) ),
    inference(rew,[status(thm),theory(equality)],[759,710]),
    [iquote('0:Rew:759.0,710.0')]).

cnf(772,plain,
    ( equal(op(op(e2,e3),e3),op(e1,e5)) ),
    inference(rew,[status(thm),theory(equality)],[759,711]),
    [iquote('0:Rew:759.0,711.0')]).

cnf(774,plain,
    ( equal(op(op(e2,e3),e1),op(e2,e5)) ),
    inference(rew,[status(thm),theory(equality)],[759,713]),
    [iquote('0:Rew:759.0,713.0')]).

cnf(776,plain,
    ( equal(op(e4,op(e2,e3)),op(e3,e5)) ),
    inference(rew,[status(thm),theory(equality)],[759,742]),
    [iquote('0:Rew:759.0,742.0')]).

cnf(777,plain,
    ( equal(op(e1,e3),e5) ),
    inference(rew,[status(thm),theory(equality)],[71,465,35,693,681]),
    [iquote('0:Rew:71.0,465.0,35.0,465.0,693.0,465.0,681.0,465.0')]).

cnf(778,plain,
    ( equal(op(e3,e1),e5) ),
    inference(rew,[status(thm),theory(equality)],[777,71]),
    [iquote('0:Rew:777.0,71.0')]).

cnf(790,plain,
    ( equal(op(e4,op(e0,e4)),op(e0,e3)) ),
    inference(rew,[status(thm),theory(equality)],[66,464,35,67]),
    [iquote('0:Rew:66.0,464.0,35.0,464.0,67.0,464.0')]).

cnf(794,plain,
    ( equal(op(e3,e5),op(e1,e2)) ),
    inference(rew,[status(thm),theory(equality)],[559,460,776,75]),
    [iquote('0:Rew:559.0,460.0,776.0,460.0,75.0,460.0')]).

cnf(795,plain,
    ( equal(op(e5,e3),op(e1,e2)) ),
    inference(rew,[status(thm),theory(equality)],[794,80]),
    [iquote('0:Rew:794.0,80.0')]).

cnf(811,plain,
    ( equal(op(op(e2,e3),e4),op(e1,e2)) ),
    inference(rew,[status(thm),theory(equality)],[794,771]),
    [iquote('0:Rew:794.0,771.0')]).

cnf(815,plain,
    ( equal(op(e2,e3),op(e1,e1)) ),
    inference(rew,[status(thm),theory(equality)],[559,459,759,778]),
    [iquote('0:Rew:559.0,459.0,759.0,459.0,778.0,459.0')]).

cnf(816,plain,
    ( equal(op(e3,e2),op(e1,e1)) ),
    inference(rew,[status(thm),theory(equality)],[815,75]),
    [iquote('0:Rew:815.0,75.0')]).

cnf(826,plain,
    ( equal(op(e4,e5),op(e1,e1)) ),
    inference(rew,[status(thm),theory(equality)],[815,759]),
    [iquote('0:Rew:815.0,759.0')]).

cnf(828,plain,
    ( equal(op(e5,e4),op(e1,e1)) ),
    inference(rew,[status(thm),theory(equality)],[815,760]),
    [iquote('0:Rew:815.0,760.0')]).

cnf(832,plain,
    ( ~ equal(op(e1,e1),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[815,768]),
    [iquote('0:Rew:815.0,768.0')]).

cnf(833,plain,
    ( equal(op(op(e1,e1),e3),op(e1,e5)) ),
    inference(rew,[status(thm),theory(equality)],[815,772]),
    [iquote('0:Rew:815.0,772.0')]).

cnf(835,plain,
    ( equal(op(op(e1,e1),e1),op(e2,e5)) ),
    inference(rew,[status(thm),theory(equality)],[815,774]),
    [iquote('0:Rew:815.0,774.0')]).

cnf(836,plain,
    ( equal(op(op(e1,e1),e4),op(e1,e2)) ),
    inference(rew,[status(thm),theory(equality)],[815,811]),
    [iquote('0:Rew:815.0,811.0')]).

cnf(838,plain,
    ( equal(op(e4,op(e0,e3)),op(e0,e1)) ),
    inference(rew,[status(thm),theory(equality)],[64,458,559,66]),
    [iquote('0:Rew:64.0,458.0,559.0,458.0,66.0,458.0')]).

cnf(844,plain,
    ( equal(op(e4,op(e0,e2)),op(e0,e5)) ),
    inference(rew,[status(thm),theory(equality)],[68,452,693,65]),
    [iquote('0:Rew:68.0,452.0,693.0,452.0,65.0,452.0')]).

cnf(857,plain,
    ( equal(op(e4,op(e0,e1)),op(e0,e2)) ),
    inference(rew,[status(thm),theory(equality)],[65,446,681,64]),
    [iquote('0:Rew:65.0,446.0,681.0,446.0,64.0,446.0')]).

cnf(860,plain,
    ( equal(op(op(e0,e4),e3),op(e0,e1)) ),
    inference(rew,[status(thm),theory(equality)],[67,443,838]),
    [iquote('0:Rew:67.0,443.0,838.0,443.0')]).

cnf(861,plain,
    ( equal(op(op(e0,e4),e2),op(e0,e5)) ),
    inference(rew,[status(thm),theory(equality)],[67,442,844]),
    [iquote('0:Rew:67.0,442.0,844.0,442.0')]).

cnf(862,plain,
    ( equal(op(op(e0,e4),e1),op(e0,e2)) ),
    inference(rew,[status(thm),theory(equality)],[67,441,857]),
    [iquote('0:Rew:67.0,441.0,857.0,441.0')]).

cnf(1077,plain,
    ( equal(op(e1,e1),e0) ),
    inference(rew,[status(thm),theory(equality)],[815,513,680,558,35]),
    [iquote('0:Rew:815.0,513.0,680.0,513.0,558.0,513.0,35.0,513.0')]).

cnf(1083,plain,
    ( equal(op(e2,e3),e0) ),
    inference(rew,[status(thm),theory(equality)],[1077,815]),
    [iquote('0:Rew:1077.0,815.0')]).

cnf(1086,plain,
    ( equal(op(e3,e2),e0) ),
    inference(rew,[status(thm),theory(equality)],[1077,816]),
    [iquote('0:Rew:1077.0,816.0')]).

cnf(1087,plain,
    ( equal(op(e4,e5),e0) ),
    inference(rew,[status(thm),theory(equality)],[1077,826]),
    [iquote('0:Rew:1077.0,826.0')]).

cnf(1088,plain,
    ( equal(op(e5,e4),e0) ),
    inference(rew,[status(thm),theory(equality)],[1077,828]),
    [iquote('0:Rew:1077.0,828.0')]).

cnf(1094,plain,
    ( ~ equal(op(e0,e4),e0) ),
    inference(rew,[status(thm),theory(equality)],[1077,832]),
    [iquote('0:Rew:1077.0,832.0')]).

cnf(1095,plain,
    ( equal(op(e1,e5),op(e0,e3)) ),
    inference(rew,[status(thm),theory(equality)],[1077,833]),
    [iquote('0:Rew:1077.0,833.0')]).

cnf(1097,plain,
    ( equal(op(e2,e5),op(e0,e1)) ),
    inference(rew,[status(thm),theory(equality)],[1077,835]),
    [iquote('0:Rew:1077.0,835.0')]).

cnf(1098,plain,
    ( equal(op(e1,e2),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[1077,836]),
    [iquote('0:Rew:1077.0,836.0')]).

cnf(1116,plain,
    ( equal(op(e5,e1),op(e0,e3)) ),
    inference(rew,[status(thm),theory(equality)],[1095,73]),
    [iquote('0:Rew:1095.0,73.0')]).

cnf(1171,plain,
    ( equal(op(e5,e2),op(e0,e1)) ),
    inference(rew,[status(thm),theory(equality)],[1097,77]),
    [iquote('0:Rew:1097.0,77.0')]).

cnf(1187,plain,
    ( equal(op(e2,e1),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[1098,70]),
    [iquote('0:Rew:1098.0,70.0')]).

cnf(1192,plain,
    ( equal(op(e3,e5),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[1098,794]),
    [iquote('0:Rew:1098.0,794.0')]).

cnf(1194,plain,
    ( equal(op(e5,e3),op(e0,e4)) ),
    inference(rew,[status(thm),theory(equality)],[1098,795]),
    [iquote('0:Rew:1098.0,795.0')]).

cnf(1261,plain,
    ( equal(op(e0,e4),e0)
    | equal(op(e0,e4),e1)
    | equal(op(e0,e4),e2)
    | equal(op(e0,e4),e3)
    | equal(op(e0,e4),e4)
    | equal(op(e0,e4),e5) ),
    inference(rew,[status(thm),theory(equality)],[1194,523]),
    [iquote('0:Rew:1194.0,523.5,1194.0,523.4,1194.0,523.3,1194.0,523.2,1194.0,523.1,1194.0,523.0')]).

cnf(1262,plain,
    ( equal(op(e0,e4),e4) ),
    inference(mrr,[status(thm)],[1261,1094,587,686,586,697]),
    [iquote('0:MRR:1261.0,1261.1,1261.2,1261.3,1261.5,1094.0,587.0,686.0,586.0,697.0')]).

cnf(1263,plain,
    ( equal(op(e4,e0),e4) ),
    inference(rew,[status(thm),theory(equality)],[1262,67]),
    [iquote('0:Rew:1262.0,67.0')]).

cnf(1273,plain,
    ( equal(op(e4,e4),op(e0,e3)) ),
    inference(rew,[status(thm),theory(equality)],[1262,790]),
    [iquote('0:Rew:1262.0,790.0')]).

cnf(1275,plain,
    ( equal(op(e4,e3),op(e0,e1)) ),
    inference(rew,[status(thm),theory(equality)],[1262,860]),
    [iquote('0:Rew:1262.0,860.0')]).

cnf(1276,plain,
    ( equal(op(e4,e2),op(e0,e5)) ),
    inference(rew,[status(thm),theory(equality)],[1262,861]),
    [iquote('0:Rew:1262.0,861.0')]).

cnf(1277,plain,
    ( equal(op(e4,e1),op(e0,e2)) ),
    inference(rew,[status(thm),theory(equality)],[1262,862]),
    [iquote('0:Rew:1262.0,862.0')]).

cnf(1293,plain,
    ( equal(op(e1,e2),e4) ),
    inference(rew,[status(thm),theory(equality)],[1262,1098]),
    [iquote('0:Rew:1262.0,1098.0')]).

cnf(1294,plain,
    ( equal(op(e2,e1),e4) ),
    inference(rew,[status(thm),theory(equality)],[1262,1187]),
    [iquote('0:Rew:1262.0,1187.0')]).

cnf(1295,plain,
    ( equal(op(e3,e5),e4) ),
    inference(rew,[status(thm),theory(equality)],[1262,1192]),
    [iquote('0:Rew:1262.0,1192.0')]).

cnf(1296,plain,
    ( equal(op(e5,e3),e4) ),
    inference(rew,[status(thm),theory(equality)],[1262,1194]),
    [iquote('0:Rew:1262.0,1194.0')]).

cnf(1300,plain,
    ( equal(op(e0,e3),e3) ),
    inference(rew,[status(thm),theory(equality)],[35,1273]),
    [iquote('0:Rew:35.0,1273.0')]).

cnf(1301,plain,
    ( equal(op(e3,e0),e3) ),
    inference(rew,[status(thm),theory(equality)],[1300,66]),
    [iquote('0:Rew:1300.0,66.0')]).

cnf(1325,plain,
    ( equal(op(e1,e5),e3) ),
    inference(rew,[status(thm),theory(equality)],[1300,1095]),
    [iquote('0:Rew:1300.0,1095.0')]).

cnf(1326,plain,
    ( equal(op(e5,e1),e3) ),
    inference(rew,[status(thm),theory(equality)],[1300,1116]),
    [iquote('0:Rew:1300.0,1116.0')]).

cnf(1332,plain,
    ( equal(op(e0,e1),e1) ),
    inference(rew,[status(thm),theory(equality)],[559,1275]),
    [iquote('0:Rew:559.0,1275.0')]).

cnf(1333,plain,
    ( equal(op(e1,e0),e1) ),
    inference(rew,[status(thm),theory(equality)],[1332,64]),
    [iquote('0:Rew:1332.0,64.0')]).

cnf(1351,plain,
    ( equal(op(e2,e5),e1) ),
    inference(rew,[status(thm),theory(equality)],[1332,1097]),
    [iquote('0:Rew:1332.0,1097.0')]).

cnf(1352,plain,
    ( equal(op(e5,e2),e1) ),
    inference(rew,[status(thm),theory(equality)],[1332,1171]),
    [iquote('0:Rew:1332.0,1171.0')]).

cnf(1357,plain,
    ( equal(op(e0,e5),e5) ),
    inference(rew,[status(thm),theory(equality)],[693,1276]),
    [iquote('0:Rew:693.0,1276.0')]).

cnf(1358,plain,
    ( equal(op(e5,e0),e5) ),
    inference(rew,[status(thm),theory(equality)],[1357,68]),
    [iquote('0:Rew:1357.0,68.0')]).

cnf(1374,plain,
    ( equal(op(e0,e2),e2) ),
    inference(rew,[status(thm),theory(equality)],[681,1277]),
    [iquote('0:Rew:681.0,1277.0')]).

cnf(1375,plain,
    ( equal(op(e2,e0),e2) ),
    inference(rew,[status(thm),theory(equality)],[1374,65]),
    [iquote('0:Rew:1374.0,65.0')]).

cnf(1454,plain,
    ( ~ equal(op(e1,e0),op(e0,e1))
    | ~ equal(op(e2,e0),op(e0,e2))
    | ~ equal(op(e3,e0),op(e0,e3))
    | ~ equal(op(e4,e0),op(e0,e4))
    | ~ equal(op(e5,e0),op(e0,e5))
    | ~ equal(op(e2,e1),op(e1,e2))
    | ~ equal(op(e3,e1),op(e1,e3))
    | ~ equal(op(e4,e1),op(e1,e4))
    | ~ equal(op(e5,e1),op(e1,e5))
    | ~ equal(op(e3,e2),op(e2,e3))
    | ~ equal(op(e4,e2),op(e2,e4))
    | ~ equal(op(e5,e2),op(e2,e5))
    | ~ equal(op(e4,e3),op(e3,e4))
    | ~ equal(op(e5,e3),op(e3,e5))
    | ~ equal(op(e5,e4),op(e4,e5)) ),
    inference(obv,[status(thm),theory(equality)],[557]),
    [iquote('0:Obv:557.20')]).

cnf(1455,plain,
    ( ~ equal(e1,e1)
    | ~ equal(e2,e2)
    | ~ equal(e3,e3)
    | ~ equal(e4,e4)
    | ~ equal(e5,e5)
    | ~ equal(e4,e4)
    | ~ equal(e5,e5)
    | ~ equal(e2,e2)
    | ~ equal(e3,e3)
    | ~ equal(e0,e0)
    | ~ equal(e5,e5)
    | ~ equal(e1,e1)
    | ~ equal(e1,e1)
    | ~ equal(e4,e4)
    | ~ equal(e0,e0) ),
    inference(rew,[status(thm),theory(equality)],[1088,1454,1087,1296,1295,559,558,1352,1351,693,692,1086,1083,1326,1325,681,680,778,777,1294,1293,1358,1357,1263,1262,1301,1300,1375,1374,1333,1332]),
    [iquote('0:Rew:1088.0,1454.14,1087.0,1454.14,1296.0,1454.13,1295.0,1454.13,559.0,1454.12,558.0,1454.12,1352.0,1454.11,1351.0,1454.11,693.0,1454.10,692.0,1454.10,1086.0,1454.9,1083.0,1454.9,1326.0,1454.8,1325.0,1454.8,681.0,1454.7,680.0,1454.7,778.0,1454.6,777.0,1454.6,1294.0,1454.5,1293.0,1454.5,1358.0,1454.4,1357.0,1454.4,1263.0,1454.3,1262.0,1454.3,1301.0,1454.2,1300.0,1454.2,1375.0,1454.1,1374.0,1454.1,1333.0,1454.0,1332.0,1454.0')]).

cnf(1456,plain,
    ( $false ),
    inference(obv,[status(thm),theory(equality)],[1455]),
    [iquote('0:Obv:1455.14')]).
%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.03  % Problem    : ALG028+1 : TPTP v6.4.0. Released v2.7.0.
% 0.00/0.04  % Command    : run_spass %d %s
% 0.03/0.23  % Computer   : n018.star.cs.uiowa.edu
% 0.03/0.23  % Model      : x86_64 x86_64
% 0.03/0.23  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.03/0.23  % Memory     : 32218.625MB
% 0.03/0.23  % OS         : Linux 3.10.0-514.6.1.el7.x86_64
% 0.03/0.23  % CPULimit   : 300
% 0.03/0.23  % DateTime   : Fri Jul 14 10:53:22 CDT 2017
% 0.03/0.23  % CPUTime    : 
% 0.06/0.41  
% 0.06/0.41  SPASS V 3.9 
% 0.06/0.41  SPASS beiseite: Proof found.
% 0.06/0.41  % SZS status Theorem
% 0.06/0.41  Problem: /export/starexec/sandbox2/benchmark/theBenchmark.p 
% 0.06/0.41  SPASS derived 425 clauses, backtracked 0 clauses, performed 0 splits and kept 560 clauses.
% 0.06/0.41  SPASS allocated 87017 KBytes.
% 0.06/0.41  SPASS spent	0:00:00.17 on the problem.
% 0.06/0.41  		0:00:00.03 for the input.
% 0.06/0.41  		0:00:00.09 for the FLOTTER CNF translation.
% 0.06/0.41  		0:00:00.00 for inferences.
% 0.06/0.41  		0:00:00.00 for the backtracking.
% 0.06/0.41  		0:00:00.04 for the reduction.
% 0.06/0.41  
% 0.06/0.41  
% 0.06/0.41  Here is a proof with depth 0, length 126 :
% 0.06/0.41  % SZS output start Refutation
% 0.06/0.41  35[0:Inp] ||  -> equal(op(e4,e4),e3)**.
% 0.06/0.41  64[0:Inp] ||  -> equal(op(e1,e0),op(e0,e1))**.
% 0.06/0.41  65[0:Inp] ||  -> equal(op(e2,e0),op(e0,e2))**.
% 0.06/0.41  66[0:Inp] ||  -> equal(op(e3,e0),op(e0,e3))**.
% 0.06/0.41  67[0:Inp] ||  -> equal(op(e4,e0),op(e0,e4))**.
% 0.06/0.41  68[0:Inp] ||  -> equal(op(e5,e0),op(e0,e5))**.
% 0.06/0.41  70[0:Inp] ||  -> equal(op(e2,e1),op(e1,e2))**.
% 0.06/0.41  71[0:Inp] ||  -> equal(op(e3,e1),op(e1,e3))**.
% 0.06/0.41  72[0:Inp] ||  -> equal(op(e4,e1),op(e1,e4))**.
% 0.06/0.41  73[0:Inp] ||  -> equal(op(e5,e1),op(e1,e5))**.
% 0.06/0.41  75[0:Inp] ||  -> equal(op(e3,e2),op(e2,e3))**.
% 0.06/0.41  76[0:Inp] ||  -> equal(op(e4,e2),op(e2,e4))**.
% 0.06/0.41  77[0:Inp] ||  -> equal(op(e5,e2),op(e2,e5))**.
% 0.06/0.41  79[0:Inp] ||  -> equal(op(e4,e3),op(e3,e4))**.
% 0.06/0.41  80[0:Inp] ||  -> equal(op(e5,e3),op(e3,e5))**.
% 0.06/0.41  82[0:Inp] ||  -> equal(op(e5,e4),op(e4,e5))**.
% 0.06/0.41  84[0:Inp] ||  -> equal(op(op(e4,e4),e4),e1)**.
% 0.06/0.41  235[0:Inp] || equal(op(e4,e1),op(e4,e0))** -> .
% 0.06/0.41  236[0:Inp] || equal(op(e4,e2),op(e4,e0))** -> .
% 0.06/0.41  237[0:Inp] || equal(op(e4,e3),op(e4,e0))** -> .
% 0.06/0.41  238[0:Inp] || equal(op(e4,e4),op(e4,e0))** -> .
% 0.06/0.41  239[0:Inp] || equal(op(e4,e5),op(e4,e0))** -> .
% 0.06/0.41  295[0:Inp] ||  -> equal(op(op(op(e4,e4),e4),e4),e2)**.
% 0.06/0.41  441[0:Inp] ||  -> equal(op(op(e4,e0),e1),op(e4,op(e0,e1)))**.
% 0.06/0.41  442[0:Inp] ||  -> equal(op(op(e4,e0),e2),op(e4,op(e0,e2)))**.
% 0.06/0.41  443[0:Inp] ||  -> equal(op(op(e4,e0),e3),op(e4,op(e0,e3)))**.
% 0.06/0.41  446[0:Inp] ||  -> equal(op(op(e4,e1),e0),op(e4,op(e1,e0)))**.
% 0.06/0.41  452[0:Inp] ||  -> equal(op(op(e4,e2),e0),op(e4,op(e2,e0)))**.
% 0.06/0.41  458[0:Inp] ||  -> equal(op(op(e4,e3),e0),op(e4,op(e3,e0)))**.
% 0.06/0.41  459[0:Inp] ||  -> equal(op(op(e4,e3),e1),op(e4,op(e3,e1)))**.
% 0.06/0.41  460[0:Inp] ||  -> equal(op(op(e4,e3),e2),op(e4,op(e3,e2)))**.
% 0.06/0.41  464[0:Inp] ||  -> equal(op(op(e4,e4),e0),op(e4,op(e4,e0)))**.
% 0.06/0.41  465[0:Inp] ||  -> equal(op(op(e4,e4),e1),op(e4,op(e4,e1)))**.
% 0.06/0.41  466[0:Inp] ||  -> equal(op(op(e4,e4),e2),op(e4,op(e4,e2)))**.
% 0.06/0.41  474[0:Inp] ||  -> equal(op(op(e4,e5),e4),op(e4,op(e5,e4)))**.
% 0.06/0.41  501[0:Inp] ||  -> equal(op(op(e5,e4),e1),op(e5,op(e4,e1)))**.
% 0.06/0.41  503[0:Inp] ||  -> equal(op(op(e5,e4),e3),op(e5,op(e4,e3)))**.
% 0.06/0.41  504[0:Inp] ||  -> equal(op(op(e5,e4),e4),op(e5,op(e4,e4)))**.
% 0.06/0.41  512[0:Inp] ||  -> equal(op(op(op(op(e4,e4),e4),e4),e4),e5)**.
% 0.06/0.41  513[0:Inp] ||  -> equal(op(op(op(op(e4,e4),e4),e4),op(e4,e4)),e0)**.
% 0.06/0.41  523[0:Inp] ||  -> equal(op(e5,e3),e0) equal(op(e5,e3),e1) equal(op(e5,e3),e2) equal(op(e5,e3),e3) equal(op(e5,e3),e4) equal(op(e5,e3),e5)**.
% 0.06/0.41  557[0:Inp] || equal(op(e0,e0),op(e0,e0)) equal(op(e1,e0),op(e0,e1)) equal(op(e2,e0),op(e0,e2)) equal(op(e3,e0),op(e0,e3)) equal(op(e4,e0),op(e0,e4)) equal(op(e5,e0),op(e0,e5)) equal(op(e1,e1),op(e1,e1)) equal(op(e2,e1),op(e1,e2)) equal(op(e3,e1),op(e1,e3)) equal(op(e4,e1),op(e1,e4)) equal(op(e5,e1),op(e1,e5)) equal(op(e2,e2),op(e2,e2)) equal(op(e3,e2),op(e2,e3)) equal(op(e4,e2),op(e2,e4)) equal(op(e5,e2),op(e2,e5)) equal(op(e3,e3),op(e3,e3)) equal(op(e4,e3),op(e3,e4)) equal(op(e5,e3),op(e3,e5)) equal(op(e4,e4),op(e4,e4)) equal(op(e5,e4),op(e4,e5)) equal(op(e5,e5),op(e5,e5))* -> .
% 0.06/0.41  558[0:Rew:35.0,84.0] ||  -> equal(op(e3,e4),e1)**.
% 0.06/0.41  559[0:Rew:558.0,79.0] ||  -> equal(op(e4,e3),e1)**.
% 0.06/0.41  585[0:Rew:67.0,239.0] || equal(op(e4,e5),op(e0,e4))** -> .
% 0.06/0.41  586[0:Rew:35.0,238.0,67.0,238.0] || equal(op(e0,e4),e3)** -> .
% 0.06/0.41  587[0:Rew:559.0,237.0,67.0,237.0] || equal(op(e0,e4),e1)** -> .
% 0.06/0.41  588[0:Rew:76.0,236.0,67.0,236.0] || equal(op(e2,e4),op(e0,e4))** -> .
% 0.06/0.41  589[0:Rew:72.0,235.0,67.0,235.0] || equal(op(e1,e4),op(e0,e4))** -> .
% 0.06/0.41  680[0:Rew:558.0,295.0,35.0,295.0] ||  -> equal(op(e1,e4),e2)**.
% 0.06/0.41  681[0:Rew:680.0,72.0] ||  -> equal(op(e4,e1),e2)**.
% 0.06/0.41  686[0:Rew:680.0,589.0] || equal(op(e0,e4),e2)** -> .
% 0.06/0.41  692[0:Rew:680.0,512.0,558.0,512.0,35.0,512.0] ||  -> equal(op(e2,e4),e5)**.
% 0.06/0.41  693[0:Rew:692.0,76.0] ||  -> equal(op(e4,e2),e5)**.
% 0.06/0.41  697[0:Rew:692.0,588.0] || equal(op(e0,e4),e5)** -> .
% 0.06/0.41  710[0:Rew:82.0,504.0,80.0,504.0,35.0,504.0] ||  -> equal(op(op(e4,e5),e4),op(e3,e5))**.
% 0.06/0.41  711[0:Rew:82.0,503.0,73.0,503.0,559.0,503.0] ||  -> equal(op(op(e4,e5),e3),op(e1,e5))**.
% 0.06/0.41  713[0:Rew:82.0,501.0,77.0,501.0,681.0,501.0] ||  -> equal(op(op(e4,e5),e1),op(e2,e5))**.
% 0.06/0.41  742[0:Rew:710.0,474.0,82.0,474.0] ||  -> equal(op(e4,op(e4,e5)),op(e3,e5))**.
% 0.06/0.41  759[0:Rew:75.0,466.0,35.0,466.0,693.0,466.0] ||  -> equal(op(e4,e5),op(e2,e3))**.
% 0.06/0.41  760[0:Rew:759.0,82.0] ||  -> equal(op(e5,e4),op(e2,e3))**.
% 0.06/0.41  768[0:Rew:759.0,585.0] || equal(op(e2,e3),op(e0,e4))** -> .
% 0.06/0.41  771[0:Rew:759.0,710.0] ||  -> equal(op(op(e2,e3),e4),op(e3,e5))**.
% 0.06/0.41  772[0:Rew:759.0,711.0] ||  -> equal(op(op(e2,e3),e3),op(e1,e5))**.
% 0.06/0.41  774[0:Rew:759.0,713.0] ||  -> equal(op(op(e2,e3),e1),op(e2,e5))**.
% 0.06/0.41  776[0:Rew:759.0,742.0] ||  -> equal(op(e4,op(e2,e3)),op(e3,e5))**.
% 0.06/0.41  777[0:Rew:71.0,465.0,35.0,465.0,693.0,465.0,681.0,465.0] ||  -> equal(op(e1,e3),e5)**.
% 0.06/0.41  778[0:Rew:777.0,71.0] ||  -> equal(op(e3,e1),e5)**.
% 0.06/0.41  790[0:Rew:66.0,464.0,35.0,464.0,67.0,464.0] ||  -> equal(op(e4,op(e0,e4)),op(e0,e3))**.
% 0.06/0.41  794[0:Rew:559.0,460.0,776.0,460.0,75.0,460.0] ||  -> equal(op(e3,e5),op(e1,e2))**.
% 0.06/0.41  795[0:Rew:794.0,80.0] ||  -> equal(op(e5,e3),op(e1,e2))**.
% 0.06/0.41  811[0:Rew:794.0,771.0] ||  -> equal(op(op(e2,e3),e4),op(e1,e2))**.
% 0.06/0.41  815[0:Rew:559.0,459.0,759.0,459.0,778.0,459.0] ||  -> equal(op(e2,e3),op(e1,e1))**.
% 0.06/0.41  816[0:Rew:815.0,75.0] ||  -> equal(op(e3,e2),op(e1,e1))**.
% 0.06/0.41  826[0:Rew:815.0,759.0] ||  -> equal(op(e4,e5),op(e1,e1))**.
% 0.06/0.41  828[0:Rew:815.0,760.0] ||  -> equal(op(e5,e4),op(e1,e1))**.
% 0.06/0.41  832[0:Rew:815.0,768.0] || equal(op(e1,e1),op(e0,e4))** -> .
% 0.06/0.41  833[0:Rew:815.0,772.0] ||  -> equal(op(op(e1,e1),e3),op(e1,e5))**.
% 0.06/0.41  835[0:Rew:815.0,774.0] ||  -> equal(op(op(e1,e1),e1),op(e2,e5))**.
% 0.06/0.41  836[0:Rew:815.0,811.0] ||  -> equal(op(op(e1,e1),e4),op(e1,e2))**.
% 0.06/0.41  838[0:Rew:64.0,458.0,559.0,458.0,66.0,458.0] ||  -> equal(op(e4,op(e0,e3)),op(e0,e1))**.
% 0.06/0.41  844[0:Rew:68.0,452.0,693.0,452.0,65.0,452.0] ||  -> equal(op(e4,op(e0,e2)),op(e0,e5))**.
% 0.06/0.41  857[0:Rew:65.0,446.0,681.0,446.0,64.0,446.0] ||  -> equal(op(e4,op(e0,e1)),op(e0,e2))**.
% 0.06/0.41  860[0:Rew:67.0,443.0,838.0,443.0] ||  -> equal(op(op(e0,e4),e3),op(e0,e1))**.
% 0.06/0.41  861[0:Rew:67.0,442.0,844.0,442.0] ||  -> equal(op(op(e0,e4),e2),op(e0,e5))**.
% 0.06/0.41  862[0:Rew:67.0,441.0,857.0,441.0] ||  -> equal(op(op(e0,e4),e1),op(e0,e2))**.
% 0.06/0.41  1077[0:Rew:815.0,513.0,680.0,513.0,558.0,513.0,35.0,513.0] ||  -> equal(op(e1,e1),e0)**.
% 0.06/0.41  1083[0:Rew:1077.0,815.0] ||  -> equal(op(e2,e3),e0)**.
% 0.06/0.41  1086[0:Rew:1077.0,816.0] ||  -> equal(op(e3,e2),e0)**.
% 0.06/0.41  1087[0:Rew:1077.0,826.0] ||  -> equal(op(e4,e5),e0)**.
% 0.06/0.41  1088[0:Rew:1077.0,828.0] ||  -> equal(op(e5,e4),e0)**.
% 0.06/0.41  1094[0:Rew:1077.0,832.0] || equal(op(e0,e4),e0)** -> .
% 0.06/0.41  1095[0:Rew:1077.0,833.0] ||  -> equal(op(e1,e5),op(e0,e3))**.
% 0.06/0.41  1097[0:Rew:1077.0,835.0] ||  -> equal(op(e2,e5),op(e0,e1))**.
% 0.06/0.41  1098[0:Rew:1077.0,836.0] ||  -> equal(op(e1,e2),op(e0,e4))**.
% 0.06/0.41  1116[0:Rew:1095.0,73.0] ||  -> equal(op(e5,e1),op(e0,e3))**.
% 0.06/0.41  1171[0:Rew:1097.0,77.0] ||  -> equal(op(e5,e2),op(e0,e1))**.
% 0.06/0.41  1187[0:Rew:1098.0,70.0] ||  -> equal(op(e2,e1),op(e0,e4))**.
% 0.06/0.41  1192[0:Rew:1098.0,794.0] ||  -> equal(op(e3,e5),op(e0,e4))**.
% 0.06/0.41  1194[0:Rew:1098.0,795.0] ||  -> equal(op(e5,e3),op(e0,e4))**.
% 0.06/0.41  1261[0:Rew:1194.0,523.5,1194.0,523.4,1194.0,523.3,1194.0,523.2,1194.0,523.1,1194.0,523.0] ||  -> equal(op(e0,e4),e0) equal(op(e0,e4),e1) equal(op(e0,e4),e2) equal(op(e0,e4),e3) equal(op(e0,e4),e4) equal(op(e0,e4),e5)**.
% 0.06/0.41  1262[0:MRR:1261.0,1261.1,1261.2,1261.3,1261.5,1094.0,587.0,686.0,586.0,697.0] ||  -> equal(op(e0,e4),e4)**.
% 0.06/0.41  1263[0:Rew:1262.0,67.0] ||  -> equal(op(e4,e0),e4)**.
% 0.06/0.41  1273[0:Rew:1262.0,790.0] ||  -> equal(op(e4,e4),op(e0,e3))**.
% 0.06/0.41  1275[0:Rew:1262.0,860.0] ||  -> equal(op(e4,e3),op(e0,e1))**.
% 0.06/0.41  1276[0:Rew:1262.0,861.0] ||  -> equal(op(e4,e2),op(e0,e5))**.
% 0.06/0.41  1277[0:Rew:1262.0,862.0] ||  -> equal(op(e4,e1),op(e0,e2))**.
% 0.06/0.41  1293[0:Rew:1262.0,1098.0] ||  -> equal(op(e1,e2),e4)**.
% 0.06/0.41  1294[0:Rew:1262.0,1187.0] ||  -> equal(op(e2,e1),e4)**.
% 0.06/0.41  1295[0:Rew:1262.0,1192.0] ||  -> equal(op(e3,e5),e4)**.
% 0.06/0.41  1296[0:Rew:1262.0,1194.0] ||  -> equal(op(e5,e3),e4)**.
% 0.06/0.41  1300[0:Rew:35.0,1273.0] ||  -> equal(op(e0,e3),e3)**.
% 0.06/0.42  1301[0:Rew:1300.0,66.0] ||  -> equal(op(e3,e0),e3)**.
% 0.06/0.42  1325[0:Rew:1300.0,1095.0] ||  -> equal(op(e1,e5),e3)**.
% 0.06/0.42  1326[0:Rew:1300.0,1116.0] ||  -> equal(op(e5,e1),e3)**.
% 0.06/0.42  1332[0:Rew:559.0,1275.0] ||  -> equal(op(e0,e1),e1)**.
% 0.06/0.42  1333[0:Rew:1332.0,64.0] ||  -> equal(op(e1,e0),e1)**.
% 0.06/0.42  1351[0:Rew:1332.0,1097.0] ||  -> equal(op(e2,e5),e1)**.
% 0.06/0.42  1352[0:Rew:1332.0,1171.0] ||  -> equal(op(e5,e2),e1)**.
% 0.06/0.42  1357[0:Rew:693.0,1276.0] ||  -> equal(op(e0,e5),e5)**.
% 0.06/0.42  1358[0:Rew:1357.0,68.0] ||  -> equal(op(e5,e0),e5)**.
% 0.06/0.42  1374[0:Rew:681.0,1277.0] ||  -> equal(op(e0,e2),e2)**.
% 0.06/0.42  1375[0:Rew:1374.0,65.0] ||  -> equal(op(e2,e0),e2)**.
% 0.06/0.42  1454[0:Obv:557.20] || equal(op(e1,e0),op(e0,e1)) equal(op(e2,e0),op(e0,e2)) equal(op(e3,e0),op(e0,e3)) equal(op(e4,e0),op(e0,e4)) equal(op(e5,e0),op(e0,e5)) equal(op(e2,e1),op(e1,e2)) equal(op(e3,e1),op(e1,e3)) equal(op(e4,e1),op(e1,e4)) equal(op(e5,e1),op(e1,e5)) equal(op(e3,e2),op(e2,e3)) equal(op(e4,e2),op(e2,e4)) equal(op(e5,e2),op(e2,e5)) equal(op(e4,e3),op(e3,e4)) equal(op(e5,e3),op(e3,e5)) equal(op(e5,e4),op(e4,e5))** -> .
% 0.06/0.42  1455[0:Rew:1088.0,1454.14,1087.0,1454.14,1296.0,1454.13,1295.0,1454.13,559.0,1454.12,558.0,1454.12,1352.0,1454.11,1351.0,1454.11,693.0,1454.10,692.0,1454.10,1086.0,1454.9,1083.0,1454.9,1326.0,1454.8,1325.0,1454.8,681.0,1454.7,680.0,1454.7,778.0,1454.6,777.0,1454.6,1294.0,1454.5,1293.0,1454.5,1358.0,1454.4,1357.0,1454.4,1263.0,1454.3,1262.0,1454.3,1301.0,1454.2,1300.0,1454.2,1375.0,1454.1,1374.0,1454.1,1333.0,1454.0,1332.0,1454.0] || equal(e1,e1) equal(e2,e2) equal(e3,e3) equal(e4,e4) equal(e5,e5)* equal(e4,e4) equal(e5,e5)* equal(e2,e2) equal(e3,e3) equal(e0,e0) equal(e5,e5)* equal(e1,e1) equal(e1,e1) equal(e4,e4) equal(e0,e0) -> .
% 0.06/0.42  1456[0:Obv:1455.14] ||  -> .
% 0.06/0.42  % SZS output end Refutation
% 0.06/0.42  Formulae used in the proof : ax11 co1 ax9 ax2 ax1
% 0.06/0.42  
%------------------------------------------------------------------------------

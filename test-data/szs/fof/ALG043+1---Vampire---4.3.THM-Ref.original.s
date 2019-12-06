% Problem    : ALG043+1 : TPTP v7.1.0. Released v2.7.0.
% Command    : vampire --mode casc -t %d %s
% Computer   : n157.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-693.2.2.el7.x86_64
% CPULimit   : 300
% DateTime   : Wed Aug 29 18:25:56 CDT 2018
% CPUTime    : 
% ott+1002_2_av=off:bd=preordered:irw=on:lma=on:nm=64:nwc=10:sp=reverse_arity:updr=off_2 on theBenchmark
% Refutation found. Thanks to Tanya!
% SZS status Theorem for theBenchmark
% SZS output start Proof for theBenchmark
fof(f2,axiom,(
  e0 = op(e3,e3) & e1 = op(e3,e2) & e2 = op(e3,e1) & e3 = op(e3,e0) & e1 = op(e2,e3) & e0 = op(e2,e2) & e3 = op(e2,e1) & e2 = op(e2,e0) & e2 = op(e1,e3) & e3 = op(e1,e2) & e0 = op(e1,e1) & e1 = op(e1,e0) & e3 = op(e0,e3) & e2 = op(e0,e2) & e1 = op(e0,e1) & e0 = op(e0,e0)),
  file('/export/starexec/sandbox2/benchmark/theBenchmark.p',ax2)).
fof(f3,axiom,(
  e0 = unit),
  file('/export/starexec/sandbox2/benchmark/theBenchmark.p',ax3)).
fof(f4,conjecture,(
  (e3 = op(e3,e3) | e3 = op(e2,e3) | e3 = op(e1,e3) | e3 = op(e0,e3)) & (e3 = op(e3,e3) | e3 = op(e3,e2) | e3 = op(e3,e1) | e3 = op(e3,e0)) & (e2 = op(e3,e3) | e2 = op(e2,e3) | e2 = op(e1,e3) | e2 = op(e0,e3)) & (e2 = op(e3,e3) | e2 = op(e3,e2) | e2 = op(e3,e1) | e2 = op(e3,e0)) & (e1 = op(e3,e3) | e1 = op(e2,e3) | e1 = op(e1,e3) | e1 = op(e0,e3)) & (e1 = op(e3,e3) | e1 = op(e3,e2) | e1 = op(e3,e1) | e1 = op(e3,e0)) & (e0 = op(e3,e3) | e0 = op(e2,e3) | e0 = op(e1,e3) | e0 = op(e0,e3)) & (e0 = op(e3,e3) | e0 = op(e3,e2) | e0 = op(e3,e1) | e0 = op(e3,e0)) & (e3 = op(e3,e2) | e3 = op(e2,e2) | e3 = op(e1,e2) | e3 = op(e0,e2)) & (e3 = op(e2,e3) | e3 = op(e2,e2) | e3 = op(e2,e1) | e3 = op(e2,e0)) & (e2 = op(e3,e2) | e2 = op(e2,e2) | e2 = op(e1,e2) | e2 = op(e0,e2)) & (e2 = op(e2,e3) | e2 = op(e2,e2) | e2 = op(e2,e1) | e2 = op(e2,e0)) & (e1 = op(e3,e2) | e1 = op(e2,e2) | e1 = op(e1,e2) | e1 = op(e0,e2)) & (e1 = op(e2,e3) | e1 = op(e2,e2) | e1 = op(e2,e1) | e1 = op(e2,e0)) & (e0 = op(e3,e2) | e0 = op(e2,e2) | e0 = op(e1,e2) | e0 = op(e0,e2)) & (e0 = op(e2,e3) | e0 = op(e2,e2) | e0 = op(e2,e1) | e0 = op(e2,e0)) & (e3 = op(e3,e1) | e3 = op(e2,e1) | e3 = op(e1,e1) | e3 = op(e0,e1)) & (e3 = op(e1,e3) | e3 = op(e1,e2) | e3 = op(e1,e1) | e3 = op(e1,e0)) & (e2 = op(e3,e1) | e2 = op(e2,e1) | e2 = op(e1,e1) | e2 = op(e0,e1)) & (e2 = op(e1,e3) | e2 = op(e1,e2) | e2 = op(e1,e1) | e2 = op(e1,e0)) & (e1 = op(e3,e1) | e1 = op(e2,e1) | e1 = op(e1,e1) | e1 = op(e0,e1)) & (e1 = op(e1,e3) | e1 = op(e1,e2) | e1 = op(e1,e1) | e1 = op(e1,e0)) & (e0 = op(e3,e1) | e0 = op(e2,e1) | e0 = op(e1,e1) | e0 = op(e0,e1)) & (e0 = op(e1,e3) | e0 = op(e1,e2) | e0 = op(e1,e1) | e0 = op(e1,e0)) & (e3 = op(e3,e0) | e3 = op(e2,e0) | e3 = op(e1,e0) | e3 = op(e0,e0)) & (e3 = op(e0,e3) | e3 = op(e0,e2) | e3 = op(e0,e1) | e3 = op(e0,e0)) & (e2 = op(e3,e0) | e2 = op(e2,e0) | e2 = op(e1,e0) | e2 = op(e0,e0)) & (e2 = op(e0,e3) | e2 = op(e0,e2) | e2 = op(e0,e1) | e2 = op(e0,e0)) & (e1 = op(e3,e0) | e1 = op(e2,e0) | e1 = op(e1,e0) | e1 = op(e0,e0)) & (e1 = op(e0,e3) | e1 = op(e0,e2) | e1 = op(e0,e1) | e1 = op(e0,e0)) & (e0 = op(e3,e0) | e0 = op(e2,e0) | e0 = op(e1,e0) | e0 = op(e0,e0)) & (e0 = op(e0,e3) | e0 = op(e0,e2) | e0 = op(e0,e1) | e0 = op(e0,e0)) & (e3 = unit | e2 = unit | e1 = unit | e0 = unit) & e3 = op(e3,unit) & e3 = op(unit,e3) & e2 = op(e2,unit) & e2 = op(unit,e2) & e1 = op(e1,unit) & e1 = op(unit,e1) & e0 = op(e0,unit) & e0 = op(unit,e0) & (e3 = op(e3,e3) | e2 = op(e3,e3) | e1 = op(e3,e3) | e0 = op(e3,e3)) & (e3 = op(e3,e2) | e2 = op(e3,e2) | e1 = op(e3,e2) | e0 = op(e3,e2)) & (e3 = op(e3,e1) | e2 = op(e3,e1) | e1 = op(e3,e1) | e0 = op(e3,e1)) & (e3 = op(e3,e0) | e2 = op(e3,e0) | e1 = op(e3,e0) | e0 = op(e3,e0)) & (e3 = op(e2,e3) | e2 = op(e2,e3) | e1 = op(e2,e3) | e0 = op(e2,e3)) & (e3 = op(e2,e2) | e2 = op(e2,e2) | e1 = op(e2,e2) | e0 = op(e2,e2)) & (e3 = op(e2,e1) | e2 = op(e2,e1) | e1 = op(e2,e1) | e0 = op(e2,e1)) & (e3 = op(e2,e0) | e2 = op(e2,e0) | e1 = op(e2,e0) | e0 = op(e2,e0)) & (e3 = op(e1,e3) | e2 = op(e1,e3) | e1 = op(e1,e3) | e0 = op(e1,e3)) & (e3 = op(e1,e2) | e2 = op(e1,e2) | e1 = op(e1,e2) | e0 = op(e1,e2)) & (e3 = op(e1,e1) | e2 = op(e1,e1) | e1 = op(e1,e1) | e0 = op(e1,e1)) & (e3 = op(e1,e0) | e2 = op(e1,e0) | e1 = op(e1,e0) | e0 = op(e1,e0)) & (e3 = op(e0,e3) | e2 = op(e0,e3) | e1 = op(e0,e3) | e0 = op(e0,e3)) & (e3 = op(e0,e2) | e2 = op(e0,e2) | e1 = op(e0,e2) | e0 = op(e0,e2)) & (e3 = op(e0,e1) | e2 = op(e0,e1) | e1 = op(e0,e1) | e0 = op(e0,e1)) & (e3 = op(e0,e0) | e2 = op(e0,e0) | e1 = op(e0,e0) | e0 = op(e0,e0)) & ((e3 = op(e3,e3) & e3 = op(e2,e2) & e3 = op(e1,e1) & e3 = op(e0,e0)) | (e2 = op(e3,e3) & e2 = op(e2,e2) & e2 = op(e1,e1) & e2 = op(e0,e0)) | (e1 = op(e3,e3) & e1 = op(e2,e2) & e1 = op(e1,e1) & e1 = op(e0,e0)) | (e0 = op(e3,e3) & e0 = op(e2,e2) & e0 = op(e1,e1) & e0 = op(e0,e0)))),
  file('/export/starexec/sandbox2/benchmark/theBenchmark.p',co1)).
fof(f5,negated_conjecture,(
  ~((e3 = op(e3,e3) | e3 = op(e2,e3) | e3 = op(e1,e3) | e3 = op(e0,e3)) & (e3 = op(e3,e3) | e3 = op(e3,e2) | e3 = op(e3,e1) | e3 = op(e3,e0)) & (e2 = op(e3,e3) | e2 = op(e2,e3) | e2 = op(e1,e3) | e2 = op(e0,e3)) & (e2 = op(e3,e3) | e2 = op(e3,e2) | e2 = op(e3,e1) | e2 = op(e3,e0)) & (e1 = op(e3,e3) | e1 = op(e2,e3) | e1 = op(e1,e3) | e1 = op(e0,e3)) & (e1 = op(e3,e3) | e1 = op(e3,e2) | e1 = op(e3,e1) | e1 = op(e3,e0)) & (e0 = op(e3,e3) | e0 = op(e2,e3) | e0 = op(e1,e3) | e0 = op(e0,e3)) & (e0 = op(e3,e3) | e0 = op(e3,e2) | e0 = op(e3,e1) | e0 = op(e3,e0)) & (e3 = op(e3,e2) | e3 = op(e2,e2) | e3 = op(e1,e2) | e3 = op(e0,e2)) & (e3 = op(e2,e3) | e3 = op(e2,e2) | e3 = op(e2,e1) | e3 = op(e2,e0)) & (e2 = op(e3,e2) | e2 = op(e2,e2) | e2 = op(e1,e2) | e2 = op(e0,e2)) & (e2 = op(e2,e3) | e2 = op(e2,e2) | e2 = op(e2,e1) | e2 = op(e2,e0)) & (e1 = op(e3,e2) | e1 = op(e2,e2) | e1 = op(e1,e2) | e1 = op(e0,e2)) & (e1 = op(e2,e3) | e1 = op(e2,e2) | e1 = op(e2,e1) | e1 = op(e2,e0)) & (e0 = op(e3,e2) | e0 = op(e2,e2) | e0 = op(e1,e2) | e0 = op(e0,e2)) & (e0 = op(e2,e3) | e0 = op(e2,e2) | e0 = op(e2,e1) | e0 = op(e2,e0)) & (e3 = op(e3,e1) | e3 = op(e2,e1) | e3 = op(e1,e1) | e3 = op(e0,e1)) & (e3 = op(e1,e3) | e3 = op(e1,e2) | e3 = op(e1,e1) | e3 = op(e1,e0)) & (e2 = op(e3,e1) | e2 = op(e2,e1) | e2 = op(e1,e1) | e2 = op(e0,e1)) & (e2 = op(e1,e3) | e2 = op(e1,e2) | e2 = op(e1,e1) | e2 = op(e1,e0)) & (e1 = op(e3,e1) | e1 = op(e2,e1) | e1 = op(e1,e1) | e1 = op(e0,e1)) & (e1 = op(e1,e3) | e1 = op(e1,e2) | e1 = op(e1,e1) | e1 = op(e1,e0)) & (e0 = op(e3,e1) | e0 = op(e2,e1) | e0 = op(e1,e1) | e0 = op(e0,e1)) & (e0 = op(e1,e3) | e0 = op(e1,e2) | e0 = op(e1,e1) | e0 = op(e1,e0)) & (e3 = op(e3,e0) | e3 = op(e2,e0) | e3 = op(e1,e0) | e3 = op(e0,e0)) & (e3 = op(e0,e3) | e3 = op(e0,e2) | e3 = op(e0,e1) | e3 = op(e0,e0)) & (e2 = op(e3,e0) | e2 = op(e2,e0) | e2 = op(e1,e0) | e2 = op(e0,e0)) & (e2 = op(e0,e3) | e2 = op(e0,e2) | e2 = op(e0,e1) | e2 = op(e0,e0)) & (e1 = op(e3,e0) | e1 = op(e2,e0) | e1 = op(e1,e0) | e1 = op(e0,e0)) & (e1 = op(e0,e3) | e1 = op(e0,e2) | e1 = op(e0,e1) | e1 = op(e0,e0)) & (e0 = op(e3,e0) | e0 = op(e2,e0) | e0 = op(e1,e0) | e0 = op(e0,e0)) & (e0 = op(e0,e3) | e0 = op(e0,e2) | e0 = op(e0,e1) | e0 = op(e0,e0)) & (e3 = unit | e2 = unit | e1 = unit | e0 = unit) & e3 = op(e3,unit) & e3 = op(unit,e3) & e2 = op(e2,unit) & e2 = op(unit,e2) & e1 = op(e1,unit) & e1 = op(unit,e1) & e0 = op(e0,unit) & e0 = op(unit,e0) & (e3 = op(e3,e3) | e2 = op(e3,e3) | e1 = op(e3,e3) | e0 = op(e3,e3)) & (e3 = op(e3,e2) | e2 = op(e3,e2) | e1 = op(e3,e2) | e0 = op(e3,e2)) & (e3 = op(e3,e1) | e2 = op(e3,e1) | e1 = op(e3,e1) | e0 = op(e3,e1)) & (e3 = op(e3,e0) | e2 = op(e3,e0) | e1 = op(e3,e0) | e0 = op(e3,e0)) & (e3 = op(e2,e3) | e2 = op(e2,e3) | e1 = op(e2,e3) | e0 = op(e2,e3)) & (e3 = op(e2,e2) | e2 = op(e2,e2) | e1 = op(e2,e2) | e0 = op(e2,e2)) & (e3 = op(e2,e1) | e2 = op(e2,e1) | e1 = op(e2,e1) | e0 = op(e2,e1)) & (e3 = op(e2,e0) | e2 = op(e2,e0) | e1 = op(e2,e0) | e0 = op(e2,e0)) & (e3 = op(e1,e3) | e2 = op(e1,e3) | e1 = op(e1,e3) | e0 = op(e1,e3)) & (e3 = op(e1,e2) | e2 = op(e1,e2) | e1 = op(e1,e2) | e0 = op(e1,e2)) & (e3 = op(e1,e1) | e2 = op(e1,e1) | e1 = op(e1,e1) | e0 = op(e1,e1)) & (e3 = op(e1,e0) | e2 = op(e1,e0) | e1 = op(e1,e0) | e0 = op(e1,e0)) & (e3 = op(e0,e3) | e2 = op(e0,e3) | e1 = op(e0,e3) | e0 = op(e0,e3)) & (e3 = op(e0,e2) | e2 = op(e0,e2) | e1 = op(e0,e2) | e0 = op(e0,e2)) & (e3 = op(e0,e1) | e2 = op(e0,e1) | e1 = op(e0,e1) | e0 = op(e0,e1)) & (e3 = op(e0,e0) | e2 = op(e0,e0) | e1 = op(e0,e0) | e0 = op(e0,e0)) & ((e3 = op(e3,e3) & e3 = op(e2,e2) & e3 = op(e1,e1) & e3 = op(e0,e0)) | (e2 = op(e3,e3) & e2 = op(e2,e2) & e2 = op(e1,e1) & e2 = op(e0,e0)) | (e1 = op(e3,e3) & e1 = op(e2,e2) & e1 = op(e1,e1) & e1 = op(e0,e0)) | (e0 = op(e3,e3) & e0 = op(e2,e2) & e0 = op(e1,e1) & e0 = op(e0,e0))))),
  inference(negated_conjecture,[],[f4])).
fof(f6,plain,(
  (e3 != op(e3,e3) & e3 != op(e2,e3) & e3 != op(e1,e3) & e3 != op(e0,e3)) | (e3 != op(e3,e3) & e3 != op(e3,e2) & e3 != op(e3,e1) & e3 != op(e3,e0)) | (e2 != op(e3,e3) & e2 != op(e2,e3) & e2 != op(e1,e3) & e2 != op(e0,e3)) | (e2 != op(e3,e3) & e2 != op(e3,e2) & e2 != op(e3,e1) & e2 != op(e3,e0)) | (e1 != op(e3,e3) & e1 != op(e2,e3) & e1 != op(e1,e3) & e1 != op(e0,e3)) | (e1 != op(e3,e3) & e1 != op(e3,e2) & e1 != op(e3,e1) & e1 != op(e3,e0)) | (e0 != op(e3,e3) & e0 != op(e2,e3) & e0 != op(e1,e3) & e0 != op(e0,e3)) | (e0 != op(e3,e3) & e0 != op(e3,e2) & e0 != op(e3,e1) & e0 != op(e3,e0)) | (e3 != op(e3,e2) & e3 != op(e2,e2) & e3 != op(e1,e2) & e3 != op(e0,e2)) | (e3 != op(e2,e3) & e3 != op(e2,e2) & e3 != op(e2,e1) & e3 != op(e2,e0)) | (e2 != op(e3,e2) & e2 != op(e2,e2) & e2 != op(e1,e2) & e2 != op(e0,e2)) | (e2 != op(e2,e3) & e2 != op(e2,e2) & e2 != op(e2,e1) & e2 != op(e2,e0)) | (e1 != op(e3,e2) & e1 != op(e2,e2) & e1 != op(e1,e2) & e1 != op(e0,e2)) | (e1 != op(e2,e3) & e1 != op(e2,e2) & e1 != op(e2,e1) & e1 != op(e2,e0)) | (e0 != op(e3,e2) & e0 != op(e2,e2) & e0 != op(e1,e2) & e0 != op(e0,e2)) | (e0 != op(e2,e3) & e0 != op(e2,e2) & e0 != op(e2,e1) & e0 != op(e2,e0)) | (e3 != op(e3,e1) & e3 != op(e2,e1) & e3 != op(e1,e1) & e3 != op(e0,e1)) | (e3 != op(e1,e3) & e3 != op(e1,e2) & e3 != op(e1,e1) & e3 != op(e1,e0)) | (e2 != op(e3,e1) & e2 != op(e2,e1) & e2 != op(e1,e1) & e2 != op(e0,e1)) | (e2 != op(e1,e3) & e2 != op(e1,e2) & e2 != op(e1,e1) & e2 != op(e1,e0)) | (e1 != op(e3,e1) & e1 != op(e2,e1) & e1 != op(e1,e1) & e1 != op(e0,e1)) | (e1 != op(e1,e3) & e1 != op(e1,e2) & e1 != op(e1,e1) & e1 != op(e1,e0)) | (e0 != op(e3,e1) & e0 != op(e2,e1) & e0 != op(e1,e1) & e0 != op(e0,e1)) | (e0 != op(e1,e3) & e0 != op(e1,e2) & e0 != op(e1,e1) & e0 != op(e1,e0)) | (e3 != op(e3,e0) & e3 != op(e2,e0) & e3 != op(e1,e0) & e3 != op(e0,e0)) | (e3 != op(e0,e3) & e3 != op(e0,e2) & e3 != op(e0,e1) & e3 != op(e0,e0)) | (e2 != op(e3,e0) & e2 != op(e2,e0) & e2 != op(e1,e0) & e2 != op(e0,e0)) | (e2 != op(e0,e3) & e2 != op(e0,e2) & e2 != op(e0,e1) & e2 != op(e0,e0)) | (e1 != op(e3,e0) & e1 != op(e2,e0) & e1 != op(e1,e0) & e1 != op(e0,e0)) | (e1 != op(e0,e3) & e1 != op(e0,e2) & e1 != op(e0,e1) & e1 != op(e0,e0)) | (e0 != op(e3,e0) & e0 != op(e2,e0) & e0 != op(e1,e0) & e0 != op(e0,e0)) | (e0 != op(e0,e3) & e0 != op(e0,e2) & e0 != op(e0,e1) & e0 != op(e0,e0)) | (e3 != unit & e2 != unit & e1 != unit & e0 != unit) | e3 != op(e3,unit) | e3 != op(unit,e3) | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | e0 != op(e0,unit) | e0 != op(unit,e0) | (e3 != op(e3,e3) & e2 != op(e3,e3) & e1 != op(e3,e3) & e0 != op(e3,e3)) | (e3 != op(e3,e2) & e2 != op(e3,e2) & e1 != op(e3,e2) & e0 != op(e3,e2)) | (e3 != op(e3,e1) & e2 != op(e3,e1) & e1 != op(e3,e1) & e0 != op(e3,e1)) | (e3 != op(e3,e0) & e2 != op(e3,e0) & e1 != op(e3,e0) & e0 != op(e3,e0)) | (e3 != op(e2,e3) & e2 != op(e2,e3) & e1 != op(e2,e3) & e0 != op(e2,e3)) | (e3 != op(e2,e2) & e2 != op(e2,e2) & e1 != op(e2,e2) & e0 != op(e2,e2)) | (e3 != op(e2,e1) & e2 != op(e2,e1) & e1 != op(e2,e1) & e0 != op(e2,e1)) | (e3 != op(e2,e0) & e2 != op(e2,e0) & e1 != op(e2,e0) & e0 != op(e2,e0)) | (e3 != op(e1,e3) & e2 != op(e1,e3) & e1 != op(e1,e3) & e0 != op(e1,e3)) | (e3 != op(e1,e2) & e2 != op(e1,e2) & e1 != op(e1,e2) & e0 != op(e1,e2)) | (e3 != op(e1,e1) & e2 != op(e1,e1) & e1 != op(e1,e1) & e0 != op(e1,e1)) | (e3 != op(e1,e0) & e2 != op(e1,e0) & e1 != op(e1,e0) & e0 != op(e1,e0)) | (e3 != op(e0,e3) & e2 != op(e0,e3) & e1 != op(e0,e3) & e0 != op(e0,e3)) | (e3 != op(e0,e2) & e2 != op(e0,e2) & e1 != op(e0,e2) & e0 != op(e0,e2)) | (e3 != op(e0,e1) & e2 != op(e0,e1) & e1 != op(e0,e1) & e0 != op(e0,e1)) | (e3 != op(e0,e0) & e2 != op(e0,e0) & e1 != op(e0,e0) & e0 != op(e0,e0)) | ((e3 != op(e3,e3) | e3 != op(e2,e2) | e3 != op(e1,e1) | e3 != op(e0,e0)) & (e2 != op(e3,e3) | e2 != op(e2,e2) | e2 != op(e1,e1) | e2 != op(e0,e0)) & (e1 != op(e3,e3) | e1 != op(e2,e2) | e1 != op(e1,e1) | e1 != op(e0,e0)) & (e0 != op(e3,e3) | e0 != op(e2,e2) | e0 != op(e1,e1) | e0 != op(e0,e0)))),
  inference(ennf_transformation,[],[f5])).
fof(f7,plain,(
  ((e3 != op(e3,e3) | e3 != op(e2,e2) | e3 != op(e1,e1) | e3 != op(e0,e0)) & (e2 != op(e3,e3) | e2 != op(e2,e2) | e2 != op(e1,e1) | e2 != op(e0,e0)) & (e1 != op(e3,e3) | e1 != op(e2,e2) | e1 != op(e1,e1) | e1 != op(e0,e0)) & (e0 != op(e3,e3) | e0 != op(e2,e2) | e0 != op(e1,e1) | e0 != op(e0,e0))) | ~sP0),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP0])])).
fof(f8,plain,(
  (e3 != op(e0,e0) & e2 != op(e0,e0) & e1 != op(e0,e0) & e0 != op(e0,e0)) | ~sP1),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP1])])).
fof(f9,plain,(
  (e3 != op(e0,e1) & e2 != op(e0,e1) & e1 != op(e0,e1) & e0 != op(e0,e1)) | ~sP2),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP2])])).
fof(f10,plain,(
  (e3 != op(e0,e2) & e2 != op(e0,e2) & e1 != op(e0,e2) & e0 != op(e0,e2)) | ~sP3),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP3])])).
fof(f11,plain,(
  (e3 != op(e0,e3) & e2 != op(e0,e3) & e1 != op(e0,e3) & e0 != op(e0,e3)) | ~sP4),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP4])])).
fof(f12,plain,(
  (e3 != op(e1,e0) & e2 != op(e1,e0) & e1 != op(e1,e0) & e0 != op(e1,e0)) | ~sP5),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP5])])).
fof(f13,plain,(
  (e3 != op(e1,e1) & e2 != op(e1,e1) & e1 != op(e1,e1) & e0 != op(e1,e1)) | ~sP6),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP6])])).
fof(f14,plain,(
  (e3 != op(e1,e2) & e2 != op(e1,e2) & e1 != op(e1,e2) & e0 != op(e1,e2)) | ~sP7),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP7])])).
fof(f15,plain,(
  (e3 != op(e1,e3) & e2 != op(e1,e3) & e1 != op(e1,e3) & e0 != op(e1,e3)) | ~sP8),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP8])])).
fof(f16,plain,(
  (e3 != op(e2,e0) & e2 != op(e2,e0) & e1 != op(e2,e0) & e0 != op(e2,e0)) | ~sP9),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP9])])).
fof(f17,plain,(
  (e3 != op(e2,e1) & e2 != op(e2,e1) & e1 != op(e2,e1) & e0 != op(e2,e1)) | ~sP10),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP10])])).
fof(f18,plain,(
  (e3 != op(e2,e2) & e2 != op(e2,e2) & e1 != op(e2,e2) & e0 != op(e2,e2)) | ~sP11),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP11])])).
fof(f19,plain,(
  (e3 != op(e2,e3) & e2 != op(e2,e3) & e1 != op(e2,e3) & e0 != op(e2,e3)) | ~sP12),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP12])])).
fof(f20,plain,(
  (e3 != op(e3,e0) & e2 != op(e3,e0) & e1 != op(e3,e0) & e0 != op(e3,e0)) | ~sP13),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP13])])).
fof(f21,plain,(
  (e3 != op(e3,e1) & e2 != op(e3,e1) & e1 != op(e3,e1) & e0 != op(e3,e1)) | ~sP14),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP14])])).
fof(f22,plain,(
  (e3 != op(e3,e2) & e2 != op(e3,e2) & e1 != op(e3,e2) & e0 != op(e3,e2)) | ~sP15),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP15])])).
fof(f23,plain,(
  (e3 != op(e3,e3) & e2 != op(e3,e3) & e1 != op(e3,e3) & e0 != op(e3,e3)) | ~sP16),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP16])])).
fof(f24,plain,(
  (e3 != unit & e2 != unit & e1 != unit & e0 != unit) | ~sP17),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP17])])).
fof(f25,plain,(
  (e0 != op(e0,e3) & e0 != op(e0,e2) & e0 != op(e0,e1) & e0 != op(e0,e0)) | ~sP18),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP18])])).
fof(f26,plain,(
  (e0 != op(e3,e0) & e0 != op(e2,e0) & e0 != op(e1,e0) & e0 != op(e0,e0)) | ~sP19),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP19])])).
fof(f27,plain,(
  (e1 != op(e0,e3) & e1 != op(e0,e2) & e1 != op(e0,e1) & e1 != op(e0,e0)) | ~sP20),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP20])])).
fof(f28,plain,(
  (e1 != op(e3,e0) & e1 != op(e2,e0) & e1 != op(e1,e0) & e1 != op(e0,e0)) | ~sP21),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP21])])).
fof(f29,plain,(
  (e2 != op(e0,e3) & e2 != op(e0,e2) & e2 != op(e0,e1) & e2 != op(e0,e0)) | ~sP22),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP22])])).
fof(f30,plain,(
  (e2 != op(e3,e0) & e2 != op(e2,e0) & e2 != op(e1,e0) & e2 != op(e0,e0)) | ~sP23),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP23])])).
fof(f31,plain,(
  (e3 != op(e0,e3) & e3 != op(e0,e2) & e3 != op(e0,e1) & e3 != op(e0,e0)) | ~sP24),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP24])])).
fof(f32,plain,(
  (e3 != op(e3,e0) & e3 != op(e2,e0) & e3 != op(e1,e0) & e3 != op(e0,e0)) | ~sP25),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP25])])).
fof(f33,plain,(
  (e0 != op(e1,e3) & e0 != op(e1,e2) & e0 != op(e1,e1) & e0 != op(e1,e0)) | ~sP26),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP26])])).
fof(f34,plain,(
  (e0 != op(e3,e1) & e0 != op(e2,e1) & e0 != op(e1,e1) & e0 != op(e0,e1)) | ~sP27),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP27])])).
fof(f35,plain,(
  (e1 != op(e1,e3) & e1 != op(e1,e2) & e1 != op(e1,e1) & e1 != op(e1,e0)) | ~sP28),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP28])])).
fof(f36,plain,(
  (e1 != op(e3,e1) & e1 != op(e2,e1) & e1 != op(e1,e1) & e1 != op(e0,e1)) | ~sP29),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP29])])).
fof(f37,plain,(
  (e2 != op(e1,e3) & e2 != op(e1,e2) & e2 != op(e1,e1) & e2 != op(e1,e0)) | ~sP30),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP30])])).
fof(f38,plain,(
  (e2 != op(e3,e1) & e2 != op(e2,e1) & e2 != op(e1,e1) & e2 != op(e0,e1)) | ~sP31),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP31])])).
fof(f39,plain,(
  (e3 != op(e1,e3) & e3 != op(e1,e2) & e3 != op(e1,e1) & e3 != op(e1,e0)) | ~sP32),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP32])])).
fof(f40,plain,(
  (e3 != op(e3,e1) & e3 != op(e2,e1) & e3 != op(e1,e1) & e3 != op(e0,e1)) | ~sP33),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP33])])).
fof(f41,plain,(
  (e0 != op(e2,e3) & e0 != op(e2,e2) & e0 != op(e2,e1) & e0 != op(e2,e0)) | ~sP34),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP34])])).
fof(f42,plain,(
  (e0 != op(e3,e2) & e0 != op(e2,e2) & e0 != op(e1,e2) & e0 != op(e0,e2)) | ~sP35),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP35])])).
fof(f43,plain,(
  (e1 != op(e2,e3) & e1 != op(e2,e2) & e1 != op(e2,e1) & e1 != op(e2,e0)) | ~sP36),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP36])])).
fof(f44,plain,(
  (e1 != op(e3,e2) & e1 != op(e2,e2) & e1 != op(e1,e2) & e1 != op(e0,e2)) | ~sP37),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP37])])).
fof(f45,plain,(
  (e2 != op(e2,e3) & e2 != op(e2,e2) & e2 != op(e2,e1) & e2 != op(e2,e0)) | ~sP38),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP38])])).
fof(f46,plain,(
  (e2 != op(e3,e2) & e2 != op(e2,e2) & e2 != op(e1,e2) & e2 != op(e0,e2)) | ~sP39),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP39])])).
fof(f47,plain,(
  (e3 != op(e2,e3) & e3 != op(e2,e2) & e3 != op(e2,e1) & e3 != op(e2,e0)) | ~sP40),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP40])])).
fof(f48,plain,(
  (e3 != op(e3,e2) & e3 != op(e2,e2) & e3 != op(e1,e2) & e3 != op(e0,e2)) | ~sP41),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP41])])).
fof(f49,plain,(
  (e0 != op(e3,e3) & e0 != op(e3,e2) & e0 != op(e3,e1) & e0 != op(e3,e0)) | ~sP42),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP42])])).
fof(f50,plain,(
  (e0 != op(e3,e3) & e0 != op(e2,e3) & e0 != op(e1,e3) & e0 != op(e0,e3)) | ~sP43),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP43])])).
fof(f51,plain,(
  (e1 != op(e3,e3) & e1 != op(e3,e2) & e1 != op(e3,e1) & e1 != op(e3,e0)) | ~sP44),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP44])])).
fof(f52,plain,(
  (e1 != op(e3,e3) & e1 != op(e2,e3) & e1 != op(e1,e3) & e1 != op(e0,e3)) | ~sP45),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP45])])).
fof(f53,plain,(
  (e2 != op(e3,e3) & e2 != op(e3,e2) & e2 != op(e3,e1) & e2 != op(e3,e0)) | ~sP46),
  introduced(predicate_definition_introduction,[new_symbols(naming,[sP46])])).
fof(f54,plain,(
  (e3 != op(e3,e3) & e3 != op(e2,e3) & e3 != op(e1,e3) & e3 != op(e0,e3)) | (e3 != op(e3,e3) & e3 != op(e3,e2) & e3 != op(e3,e1) & e3 != op(e3,e0)) | (e2 != op(e3,e3) & e2 != op(e2,e3) & e2 != op(e1,e3) & e2 != op(e0,e3)) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | e3 != op(e3,unit) | e3 != op(unit,e3) | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | e0 != op(e0,unit) | e0 != op(unit,e0) | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0),
  inference(definition_folding,[],[f6,f53,f52,f51,f50,f49,f48,f47,f46,f45,f44,f43,f42,f41,f40,f39,f38,f37,f36,f35,f34,f33,f32,f31,f30,f29,f28,f27,f26,f25,f24,f23,f22,f21,f20,f19,f18,f17,f16,f15,f14,f13,f12,f11,f10,f9,f8,f7])).
fof(f55,plain,(
  (e2 != op(e3,e3) & e2 != op(e3,e2) & e2 != op(e3,e1) & e2 != op(e3,e0)) | ~sP46),
  inference(nnf_transformation,[],[f53])).
fof(f56,plain,(
  (e1 != op(e3,e3) & e1 != op(e2,e3) & e1 != op(e1,e3) & e1 != op(e0,e3)) | ~sP45),
  inference(nnf_transformation,[],[f52])).
fof(f57,plain,(
  (e1 != op(e3,e3) & e1 != op(e3,e2) & e1 != op(e3,e1) & e1 != op(e3,e0)) | ~sP44),
  inference(nnf_transformation,[],[f51])).
fof(f58,plain,(
  (e0 != op(e3,e3) & e0 != op(e2,e3) & e0 != op(e1,e3) & e0 != op(e0,e3)) | ~sP43),
  inference(nnf_transformation,[],[f50])).
fof(f59,plain,(
  (e0 != op(e3,e3) & e0 != op(e3,e2) & e0 != op(e3,e1) & e0 != op(e3,e0)) | ~sP42),
  inference(nnf_transformation,[],[f49])).
fof(f60,plain,(
  (e3 != op(e3,e2) & e3 != op(e2,e2) & e3 != op(e1,e2) & e3 != op(e0,e2)) | ~sP41),
  inference(nnf_transformation,[],[f48])).
fof(f61,plain,(
  (e3 != op(e2,e3) & e3 != op(e2,e2) & e3 != op(e2,e1) & e3 != op(e2,e0)) | ~sP40),
  inference(nnf_transformation,[],[f47])).
fof(f62,plain,(
  (e2 != op(e3,e2) & e2 != op(e2,e2) & e2 != op(e1,e2) & e2 != op(e0,e2)) | ~sP39),
  inference(nnf_transformation,[],[f46])).
fof(f63,plain,(
  (e2 != op(e2,e3) & e2 != op(e2,e2) & e2 != op(e2,e1) & e2 != op(e2,e0)) | ~sP38),
  inference(nnf_transformation,[],[f45])).
fof(f64,plain,(
  (e1 != op(e3,e2) & e1 != op(e2,e2) & e1 != op(e1,e2) & e1 != op(e0,e2)) | ~sP37),
  inference(nnf_transformation,[],[f44])).
fof(f65,plain,(
  (e1 != op(e2,e3) & e1 != op(e2,e2) & e1 != op(e2,e1) & e1 != op(e2,e0)) | ~sP36),
  inference(nnf_transformation,[],[f43])).
fof(f66,plain,(
  (e0 != op(e3,e2) & e0 != op(e2,e2) & e0 != op(e1,e2) & e0 != op(e0,e2)) | ~sP35),
  inference(nnf_transformation,[],[f42])).
fof(f67,plain,(
  (e0 != op(e2,e3) & e0 != op(e2,e2) & e0 != op(e2,e1) & e0 != op(e2,e0)) | ~sP34),
  inference(nnf_transformation,[],[f41])).
fof(f68,plain,(
  (e3 != op(e3,e1) & e3 != op(e2,e1) & e3 != op(e1,e1) & e3 != op(e0,e1)) | ~sP33),
  inference(nnf_transformation,[],[f40])).
fof(f69,plain,(
  (e3 != op(e1,e3) & e3 != op(e1,e2) & e3 != op(e1,e1) & e3 != op(e1,e0)) | ~sP32),
  inference(nnf_transformation,[],[f39])).
fof(f70,plain,(
  (e2 != op(e3,e1) & e2 != op(e2,e1) & e2 != op(e1,e1) & e2 != op(e0,e1)) | ~sP31),
  inference(nnf_transformation,[],[f38])).
fof(f71,plain,(
  (e2 != op(e1,e3) & e2 != op(e1,e2) & e2 != op(e1,e1) & e2 != op(e1,e0)) | ~sP30),
  inference(nnf_transformation,[],[f37])).
fof(f72,plain,(
  (e1 != op(e3,e1) & e1 != op(e2,e1) & e1 != op(e1,e1) & e1 != op(e0,e1)) | ~sP29),
  inference(nnf_transformation,[],[f36])).
fof(f73,plain,(
  (e1 != op(e1,e3) & e1 != op(e1,e2) & e1 != op(e1,e1) & e1 != op(e1,e0)) | ~sP28),
  inference(nnf_transformation,[],[f35])).
fof(f74,plain,(
  (e0 != op(e3,e1) & e0 != op(e2,e1) & e0 != op(e1,e1) & e0 != op(e0,e1)) | ~sP27),
  inference(nnf_transformation,[],[f34])).
fof(f75,plain,(
  (e0 != op(e1,e3) & e0 != op(e1,e2) & e0 != op(e1,e1) & e0 != op(e1,e0)) | ~sP26),
  inference(nnf_transformation,[],[f33])).
fof(f76,plain,(
  (e3 != op(e3,e0) & e3 != op(e2,e0) & e3 != op(e1,e0) & e3 != op(e0,e0)) | ~sP25),
  inference(nnf_transformation,[],[f32])).
fof(f77,plain,(
  (e3 != op(e0,e3) & e3 != op(e0,e2) & e3 != op(e0,e1) & e3 != op(e0,e0)) | ~sP24),
  inference(nnf_transformation,[],[f31])).
fof(f78,plain,(
  (e2 != op(e3,e0) & e2 != op(e2,e0) & e2 != op(e1,e0) & e2 != op(e0,e0)) | ~sP23),
  inference(nnf_transformation,[],[f30])).
fof(f79,plain,(
  (e2 != op(e0,e3) & e2 != op(e0,e2) & e2 != op(e0,e1) & e2 != op(e0,e0)) | ~sP22),
  inference(nnf_transformation,[],[f29])).
fof(f80,plain,(
  (e1 != op(e3,e0) & e1 != op(e2,e0) & e1 != op(e1,e0) & e1 != op(e0,e0)) | ~sP21),
  inference(nnf_transformation,[],[f28])).
fof(f81,plain,(
  (e1 != op(e0,e3) & e1 != op(e0,e2) & e1 != op(e0,e1) & e1 != op(e0,e0)) | ~sP20),
  inference(nnf_transformation,[],[f27])).
fof(f82,plain,(
  (e0 != op(e3,e0) & e0 != op(e2,e0) & e0 != op(e1,e0) & e0 != op(e0,e0)) | ~sP19),
  inference(nnf_transformation,[],[f26])).
fof(f83,plain,(
  (e0 != op(e0,e3) & e0 != op(e0,e2) & e0 != op(e0,e1) & e0 != op(e0,e0)) | ~sP18),
  inference(nnf_transformation,[],[f25])).
fof(f84,plain,(
  (e3 != unit & e2 != unit & e1 != unit & e0 != unit) | ~sP17),
  inference(nnf_transformation,[],[f24])).
fof(f85,plain,(
  (e3 != op(e3,e3) & e2 != op(e3,e3) & e1 != op(e3,e3) & e0 != op(e3,e3)) | ~sP16),
  inference(nnf_transformation,[],[f23])).
fof(f86,plain,(
  (e3 != op(e3,e2) & e2 != op(e3,e2) & e1 != op(e3,e2) & e0 != op(e3,e2)) | ~sP15),
  inference(nnf_transformation,[],[f22])).
fof(f87,plain,(
  (e3 != op(e3,e1) & e2 != op(e3,e1) & e1 != op(e3,e1) & e0 != op(e3,e1)) | ~sP14),
  inference(nnf_transformation,[],[f21])).
fof(f88,plain,(
  (e3 != op(e3,e0) & e2 != op(e3,e0) & e1 != op(e3,e0) & e0 != op(e3,e0)) | ~sP13),
  inference(nnf_transformation,[],[f20])).
fof(f89,plain,(
  (e3 != op(e2,e3) & e2 != op(e2,e3) & e1 != op(e2,e3) & e0 != op(e2,e3)) | ~sP12),
  inference(nnf_transformation,[],[f19])).
fof(f90,plain,(
  (e3 != op(e2,e2) & e2 != op(e2,e2) & e1 != op(e2,e2) & e0 != op(e2,e2)) | ~sP11),
  inference(nnf_transformation,[],[f18])).
fof(f91,plain,(
  (e3 != op(e2,e1) & e2 != op(e2,e1) & e1 != op(e2,e1) & e0 != op(e2,e1)) | ~sP10),
  inference(nnf_transformation,[],[f17])).
fof(f92,plain,(
  (e3 != op(e2,e0) & e2 != op(e2,e0) & e1 != op(e2,e0) & e0 != op(e2,e0)) | ~sP9),
  inference(nnf_transformation,[],[f16])).
fof(f93,plain,(
  (e3 != op(e1,e3) & e2 != op(e1,e3) & e1 != op(e1,e3) & e0 != op(e1,e3)) | ~sP8),
  inference(nnf_transformation,[],[f15])).
fof(f94,plain,(
  (e3 != op(e1,e2) & e2 != op(e1,e2) & e1 != op(e1,e2) & e0 != op(e1,e2)) | ~sP7),
  inference(nnf_transformation,[],[f14])).
fof(f95,plain,(
  (e3 != op(e1,e1) & e2 != op(e1,e1) & e1 != op(e1,e1) & e0 != op(e1,e1)) | ~sP6),
  inference(nnf_transformation,[],[f13])).
fof(f96,plain,(
  (e3 != op(e1,e0) & e2 != op(e1,e0) & e1 != op(e1,e0) & e0 != op(e1,e0)) | ~sP5),
  inference(nnf_transformation,[],[f12])).
fof(f97,plain,(
  (e3 != op(e0,e3) & e2 != op(e0,e3) & e1 != op(e0,e3) & e0 != op(e0,e3)) | ~sP4),
  inference(nnf_transformation,[],[f11])).
fof(f98,plain,(
  (e3 != op(e0,e2) & e2 != op(e0,e2) & e1 != op(e0,e2) & e0 != op(e0,e2)) | ~sP3),
  inference(nnf_transformation,[],[f10])).
fof(f99,plain,(
  (e3 != op(e0,e1) & e2 != op(e0,e1) & e1 != op(e0,e1) & e0 != op(e0,e1)) | ~sP2),
  inference(nnf_transformation,[],[f9])).
fof(f100,plain,(
  (e3 != op(e0,e0) & e2 != op(e0,e0) & e1 != op(e0,e0) & e0 != op(e0,e0)) | ~sP1),
  inference(nnf_transformation,[],[f8])).
fof(f101,plain,(
  ((e3 != op(e3,e3) | e3 != op(e2,e2) | e3 != op(e1,e1) | e3 != op(e0,e0)) & (e2 != op(e3,e3) | e2 != op(e2,e2) | e2 != op(e1,e1) | e2 != op(e0,e0)) & (e1 != op(e3,e3) | e1 != op(e2,e2) | e1 != op(e1,e1) | e1 != op(e0,e0)) & (e0 != op(e3,e3) | e0 != op(e2,e2) | e0 != op(e1,e1) | e0 != op(e0,e0))) | ~sP0),
  inference(nnf_transformation,[],[f7])).
fof(f103,plain,(
  e2 != op(e3,e1) | ~sP46),
  inference(cnf_transformation,[],[f55])).
fof(f108,plain,(
  e1 != op(e2,e3) | ~sP45),
  inference(cnf_transformation,[],[f56])).
fof(f112,plain,(
  e1 != op(e3,e2) | ~sP44),
  inference(cnf_transformation,[],[f57])).
fof(f117,plain,(
  e0 != op(e3,e3) | ~sP43),
  inference(cnf_transformation,[],[f58])).
fof(f121,plain,(
  e0 != op(e3,e3) | ~sP42),
  inference(cnf_transformation,[],[f59])).
fof(f123,plain,(
  e3 != op(e1,e2) | ~sP41),
  inference(cnf_transformation,[],[f60])).
fof(f127,plain,(
  e3 != op(e2,e1) | ~sP40),
  inference(cnf_transformation,[],[f61])).
fof(f130,plain,(
  e2 != op(e0,e2) | ~sP39),
  inference(cnf_transformation,[],[f62])).
fof(f134,plain,(
  e2 != op(e2,e0) | ~sP38),
  inference(cnf_transformation,[],[f63])).
fof(f141,plain,(
  e1 != op(e3,e2) | ~sP37),
  inference(cnf_transformation,[],[f64])).
fof(f145,plain,(
  e1 != op(e2,e3) | ~sP36),
  inference(cnf_transformation,[],[f65])).
fof(f148,plain,(
  e0 != op(e2,e2) | ~sP35),
  inference(cnf_transformation,[],[f66])).
fof(f152,plain,(
  e0 != op(e2,e2) | ~sP34),
  inference(cnf_transformation,[],[f67])).
fof(f156,plain,(
  e3 != op(e2,e1) | ~sP33),
  inference(cnf_transformation,[],[f68])).
fof(f160,plain,(
  e3 != op(e1,e2) | ~sP32),
  inference(cnf_transformation,[],[f69])).
fof(f165,plain,(
  e2 != op(e3,e1) | ~sP31),
  inference(cnf_transformation,[],[f70])).
fof(f169,plain,(
  e2 != op(e1,e3) | ~sP30),
  inference(cnf_transformation,[],[f71])).
fof(f170,plain,(
  e1 != op(e0,e1) | ~sP29),
  inference(cnf_transformation,[],[f72])).
fof(f174,plain,(
  e1 != op(e1,e0) | ~sP28),
  inference(cnf_transformation,[],[f73])).
fof(f179,plain,(
  e0 != op(e1,e1) | ~sP27),
  inference(cnf_transformation,[],[f74])).
fof(f183,plain,(
  e0 != op(e1,e1) | ~sP26),
  inference(cnf_transformation,[],[f75])).
fof(f189,plain,(
  e3 != op(e3,e0) | ~sP25),
  inference(cnf_transformation,[],[f76])).
fof(f193,plain,(
  e3 != op(e0,e3) | ~sP24),
  inference(cnf_transformation,[],[f77])).
fof(f196,plain,(
  e2 != op(e2,e0) | ~sP23),
  inference(cnf_transformation,[],[f78])).
fof(f200,plain,(
  e2 != op(e0,e2) | ~sP22),
  inference(cnf_transformation,[],[f79])).
fof(f203,plain,(
  e1 != op(e1,e0) | ~sP21),
  inference(cnf_transformation,[],[f80])).
fof(f207,plain,(
  e1 != op(e0,e1) | ~sP20),
  inference(cnf_transformation,[],[f81])).
fof(f210,plain,(
  e0 != op(e0,e0) | ~sP19),
  inference(cnf_transformation,[],[f82])).
fof(f214,plain,(
  e0 != op(e0,e0) | ~sP18),
  inference(cnf_transformation,[],[f83])).
fof(f218,plain,(
  e0 != unit | ~sP17),
  inference(cnf_transformation,[],[f84])).
fof(f222,plain,(
  e0 != op(e3,e3) | ~sP16),
  inference(cnf_transformation,[],[f85])).
fof(f227,plain,(
  e1 != op(e3,e2) | ~sP15),
  inference(cnf_transformation,[],[f86])).
fof(f232,plain,(
  e2 != op(e3,e1) | ~sP14),
  inference(cnf_transformation,[],[f87])).
fof(f237,plain,(
  e3 != op(e3,e0) | ~sP13),
  inference(cnf_transformation,[],[f88])).
fof(f239,plain,(
  e1 != op(e2,e3) | ~sP12),
  inference(cnf_transformation,[],[f89])).
fof(f242,plain,(
  e0 != op(e2,e2) | ~sP11),
  inference(cnf_transformation,[],[f90])).
fof(f249,plain,(
  e3 != op(e2,e1) | ~sP10),
  inference(cnf_transformation,[],[f91])).
fof(f252,plain,(
  e2 != op(e2,e0) | ~sP9),
  inference(cnf_transformation,[],[f92])).
fof(f256,plain,(
  e2 != op(e1,e3) | ~sP8),
  inference(cnf_transformation,[],[f93])).
fof(f261,plain,(
  e3 != op(e1,e2) | ~sP7),
  inference(cnf_transformation,[],[f94])).
fof(f262,plain,(
  e0 != op(e1,e1) | ~sP6),
  inference(cnf_transformation,[],[f95])).
fof(f267,plain,(
  e1 != op(e1,e0) | ~sP5),
  inference(cnf_transformation,[],[f96])).
fof(f273,plain,(
  e3 != op(e0,e3) | ~sP4),
  inference(cnf_transformation,[],[f97])).
fof(f276,plain,(
  e2 != op(e0,e2) | ~sP3),
  inference(cnf_transformation,[],[f98])).
fof(f279,plain,(
  e1 != op(e0,e1) | ~sP2),
  inference(cnf_transformation,[],[f99])).
fof(f282,plain,(
  e0 != op(e0,e0) | ~sP1),
  inference(cnf_transformation,[],[f100])).
fof(f286,plain,(
  e0 != op(e3,e3) | e0 != op(e2,e2) | e0 != op(e1,e1) | e0 != op(e0,e0) | ~sP0),
  inference(cnf_transformation,[],[f101])).
fof(f291,plain,(
  e3 != op(e0,e3) | e3 != op(e3,e0) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | e3 != op(e3,unit) | e3 != op(unit,e3) | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | e0 != op(e0,unit) | e0 != op(unit,e0) | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0),
  inference(cnf_transformation,[],[f54])).
fof(f354,plain,(
  e0 = unit),
  inference(cnf_transformation,[],[f3])).
fof(f361,plain,(
  e0 = op(e0,e0)),
  inference(cnf_transformation,[],[f2])).
fof(f362,plain,(
  e1 = op(e0,e1)),
  inference(cnf_transformation,[],[f2])).
fof(f363,plain,(
  e2 = op(e0,e2)),
  inference(cnf_transformation,[],[f2])).
fof(f364,plain,(
  e3 = op(e0,e3)),
  inference(cnf_transformation,[],[f2])).
fof(f365,plain,(
  e1 = op(e1,e0)),
  inference(cnf_transformation,[],[f2])).
fof(f366,plain,(
  e0 = op(e1,e1)),
  inference(cnf_transformation,[],[f2])).
fof(f367,plain,(
  e3 = op(e1,e2)),
  inference(cnf_transformation,[],[f2])).
fof(f368,plain,(
  e2 = op(e1,e3)),
  inference(cnf_transformation,[],[f2])).
fof(f369,plain,(
  e2 = op(e2,e0)),
  inference(cnf_transformation,[],[f2])).
fof(f370,plain,(
  e3 = op(e2,e1)),
  inference(cnf_transformation,[],[f2])).
fof(f371,plain,(
  e0 = op(e2,e2)),
  inference(cnf_transformation,[],[f2])).
fof(f372,plain,(
  e1 = op(e2,e3)),
  inference(cnf_transformation,[],[f2])).
fof(f373,plain,(
  e3 = op(e3,e0)),
  inference(cnf_transformation,[],[f2])).
fof(f374,plain,(
  e2 = op(e3,e1)),
  inference(cnf_transformation,[],[f2])).
fof(f375,plain,(
  e1 = op(e3,e2)),
  inference(cnf_transformation,[],[f2])).
fof(f376,plain,(
  e0 = op(e3,e3)),
  inference(cnf_transformation,[],[f2])).
fof(f380,plain,(
  op(e3,e3) != unit | ~sP43),
  inference(definition_unfolding,[],[f117,f354])).
fof(f384,plain,(
  op(e3,e3) != unit | ~sP42),
  inference(definition_unfolding,[],[f121,f354])).
fof(f390,plain,(
  e2 != op(unit,e2) | ~sP39),
  inference(definition_unfolding,[],[f130,f354])).
fof(f391,plain,(
  e2 != op(e2,unit) | ~sP38),
  inference(definition_unfolding,[],[f134,f354])).
fof(f395,plain,(
  op(e2,e2) != unit | ~sP35),
  inference(definition_unfolding,[],[f148,f354])).
fof(f399,plain,(
  op(e2,e2) != unit | ~sP34),
  inference(definition_unfolding,[],[f152,f354])).
fof(f406,plain,(
  e1 != op(unit,e1) | ~sP29),
  inference(definition_unfolding,[],[f170,f354])).
fof(f407,plain,(
  e1 != op(e1,unit) | ~sP28),
  inference(definition_unfolding,[],[f174,f354])).
fof(f410,plain,(
  op(e1,e1) != unit | ~sP27),
  inference(definition_unfolding,[],[f179,f354])).
fof(f414,plain,(
  op(e1,e1) != unit | ~sP26),
  inference(definition_unfolding,[],[f183,f354])).
fof(f416,plain,(
  e3 != op(e3,unit) | ~sP25),
  inference(definition_unfolding,[],[f189,f354])).
fof(f420,plain,(
  e3 != op(unit,e3) | ~sP24),
  inference(definition_unfolding,[],[f193,f354])).
fof(f425,plain,(
  e2 != op(e2,unit) | ~sP23),
  inference(definition_unfolding,[],[f196,f354])).
fof(f429,plain,(
  e2 != op(unit,e2) | ~sP22),
  inference(definition_unfolding,[],[f200,f354])).
fof(f434,plain,(
  e1 != op(e1,unit) | ~sP21),
  inference(definition_unfolding,[],[f203,f354])).
fof(f438,plain,(
  e1 != op(unit,e1) | ~sP20),
  inference(definition_unfolding,[],[f207,f354])).
fof(f443,plain,(
  op(unit,unit) != unit | ~sP19),
  inference(definition_unfolding,[],[f210,f354,f354,f354])).
fof(f447,plain,(
  op(unit,unit) != unit | ~sP18),
  inference(definition_unfolding,[],[f214,f354,f354,f354])).
fof(f448,plain,(
  unit != unit | ~sP17),
  inference(definition_unfolding,[],[f218,f354])).
fof(f449,plain,(
  op(e3,e3) != unit | ~sP16),
  inference(definition_unfolding,[],[f222,f354])).
fof(f452,plain,(
  e3 != op(e3,unit) | ~sP13),
  inference(definition_unfolding,[],[f237,f354])).
fof(f457,plain,(
  op(e2,e2) != unit | ~sP11),
  inference(definition_unfolding,[],[f242,f354])).
fof(f460,plain,(
  e2 != op(e2,unit) | ~sP9),
  inference(definition_unfolding,[],[f252,f354])).
fof(f465,plain,(
  op(e1,e1) != unit | ~sP6),
  inference(definition_unfolding,[],[f262,f354])).
fof(f468,plain,(
  e1 != op(e1,unit) | ~sP5),
  inference(definition_unfolding,[],[f267,f354])).
fof(f470,plain,(
  e3 != op(unit,e3) | ~sP4),
  inference(definition_unfolding,[],[f273,f354])).
fof(f475,plain,(
  e2 != op(unit,e2) | ~sP3),
  inference(definition_unfolding,[],[f276,f354])).
fof(f480,plain,(
  e1 != op(unit,e1) | ~sP2),
  inference(definition_unfolding,[],[f279,f354])).
fof(f485,plain,(
  op(unit,unit) != unit | ~sP1),
  inference(definition_unfolding,[],[f282,f354,f354,f354])).
fof(f489,plain,(
  op(e3,e3) != unit | op(e2,e2) != unit | op(e1,e1) != unit | op(unit,unit) != unit | ~sP0),
  inference(definition_unfolding,[],[f286,f354,f354,f354,f354,f354,f354])).
fof(f552,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | e3 != op(e3,unit) | e3 != op(unit,e3) | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0),
  inference(definition_unfolding,[],[f291,f354,f354,f354,f354,f354,f354])).
fof(f557,plain,(
  op(e3,e3) = unit),
  inference(definition_unfolding,[],[f376,f354])).
fof(f558,plain,(
  e3 = op(e3,unit)),
  inference(definition_unfolding,[],[f373,f354])).
fof(f559,plain,(
  op(e2,e2) = unit),
  inference(definition_unfolding,[],[f371,f354])).
fof(f560,plain,(
  e2 = op(e2,unit)),
  inference(definition_unfolding,[],[f369,f354])).
fof(f561,plain,(
  op(e1,e1) = unit),
  inference(definition_unfolding,[],[f366,f354])).
fof(f562,plain,(
  e1 = op(e1,unit)),
  inference(definition_unfolding,[],[f365,f354])).
fof(f563,plain,(
  e3 = op(unit,e3)),
  inference(definition_unfolding,[],[f364,f354])).
fof(f564,plain,(
  e2 = op(unit,e2)),
  inference(definition_unfolding,[],[f363,f354])).
fof(f565,plain,(
  e1 = op(unit,e1)),
  inference(definition_unfolding,[],[f362,f354])).
fof(f566,plain,(
  op(unit,unit) = unit),
  inference(definition_unfolding,[],[f361,f354,f354,f354])).
fof(f568,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0),
  inference(duplicate_literal_removal,[],[f552])).
fof(f631,plain,(
  ~sP17),
  inference(trivial_inequality_removal,[],[f448])).
fof(f1912,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0),
  inference(subsumption_resolution,[],[f568,f631])).
fof(f1913,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP0),
  inference(subsumption_resolution,[],[f1912,f485])).
fof(f1914,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP0),
  inference(subsumption_resolution,[],[f1913,f447])).
fof(f1915,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP0),
  inference(subsumption_resolution,[],[f1914,f443])).
fof(f1916,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP0),
  inference(subsumption_resolution,[],[f1915,f406])).
fof(f1917,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP0),
  inference(subsumption_resolution,[],[f1916,f438])).
fof(f1918,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1917,f480])).
fof(f1919,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1918,f407])).
fof(f1920,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1919,f434])).
fof(f1921,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1920,f468])).
fof(f1922,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1921,f390])).
fof(f1923,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP3 | sP0),
  inference(subsumption_resolution,[],[f1922,f429])).
fof(f1924,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1923,f475])).
fof(f1925,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | sP23 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1924,f391])).
fof(f1926,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1925,f425])).
fof(f1927,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP27 | sP26 | sP25 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP8 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1926,f460])).
fof(f1928,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | sP25 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP8 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1927,f169])).
fof(f1929,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | sP25 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1928,f256])).
fof(f1930,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1929,f416])).
fof(f1931,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | sP24 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1930,f452])).
fof(f1932,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP4 | sP0),
  inference(subsumption_resolution,[],[f1931,f420])).
fof(f1933,plain,(
  e3 != op(unit,e3) | e3 != op(e3,unit) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f1932,f470])).
fof(f1963,plain,(
  unit != unit | ~sP43),
  inference(backward_demodulation,[],[f557,f380])).
fof(f1964,plain,(
  unit != unit | ~sP42),
  inference(backward_demodulation,[],[f557,f384])).
fof(f1965,plain,(
  unit != unit | ~sP16),
  inference(backward_demodulation,[],[f557,f449])).
fof(f1969,plain,(
  unit != unit | op(e2,e2) != unit | op(e1,e1) != unit | op(unit,unit) != unit | ~sP0),
  inference(backward_demodulation,[],[f557,f489])).
fof(f1981,plain,(
  op(e2,e2) != unit | op(e1,e1) != unit | op(unit,unit) != unit | ~sP0),
  inference(trivial_inequality_removal,[],[f1969])).
fof(f1982,plain,(
  ~sP16),
  inference(trivial_inequality_removal,[],[f1965])).
fof(f1983,plain,(
  ~sP42),
  inference(trivial_inequality_removal,[],[f1964])).
fof(f1984,plain,(
  ~sP43),
  inference(trivial_inequality_removal,[],[f1963])).
fof(f1986,plain,(
  e1 != e1 | ~sP44),
  inference(backward_demodulation,[],[f375,f112])).
fof(f1989,plain,(
  e1 != e1 | ~sP37),
  inference(backward_demodulation,[],[f375,f141])).
fof(f1990,plain,(
  e1 != e1 | ~sP15),
  inference(backward_demodulation,[],[f375,f227])).
fof(f2000,plain,(
  ~sP15),
  inference(trivial_inequality_removal,[],[f1990])).
fof(f2001,plain,(
  ~sP37),
  inference(trivial_inequality_removal,[],[f1989])).
fof(f2002,plain,(
  ~sP44),
  inference(trivial_inequality_removal,[],[f1986])).
fof(f2003,plain,(
  e2 != e2 | ~sP46),
  inference(backward_demodulation,[],[f374,f103])).
fof(f2006,plain,(
  e2 != e2 | ~sP31),
  inference(backward_demodulation,[],[f374,f165])).
fof(f2009,plain,(
  e2 != e2 | ~sP14),
  inference(backward_demodulation,[],[f374,f232])).
fof(f2018,plain,(
  ~sP14),
  inference(trivial_inequality_removal,[],[f2009])).
fof(f2019,plain,(
  ~sP31),
  inference(trivial_inequality_removal,[],[f2006])).
fof(f2020,plain,(
  ~sP46),
  inference(trivial_inequality_removal,[],[f2003])).
fof(f2035,plain,(
  e3 != e3 | e3 != op(unit,e3) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(backward_demodulation,[],[f558,f1933])).
fof(f2036,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(trivial_inequality_removal,[],[f2035])).
fof(f2069,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2036,f2020])).
fof(f2070,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP43 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2069,f2002])).
fof(f2071,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP42 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2070,f1984])).
fof(f2072,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2071,f1983])).
fof(f2073,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2072,f2001])).
fof(f2074,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP16 | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2073,f2019])).
fof(f2075,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP15 | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2074,f1982])).
fof(f2076,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP14 | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2075,f2000])).
fof(f2077,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(e2,unit) | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2076,f2018])).
fof(f2078,plain,(
  e1 != e1 | ~sP45),
  inference(backward_demodulation,[],[f372,f108])).
fof(f2081,plain,(
  e1 != e1 | ~sP36),
  inference(backward_demodulation,[],[f372,f145])).
fof(f2082,plain,(
  e1 != e1 | ~sP12),
  inference(backward_demodulation,[],[f372,f239])).
fof(f2091,plain,(
  ~sP12),
  inference(trivial_inequality_removal,[],[f2082])).
fof(f2092,plain,(
  ~sP36),
  inference(trivial_inequality_removal,[],[f2081])).
fof(f2093,plain,(
  ~sP45),
  inference(trivial_inequality_removal,[],[f2078])).
fof(f2103,plain,(
  unit != unit | ~sP35),
  inference(backward_demodulation,[],[f559,f395])).
fof(f2104,plain,(
  unit != unit | ~sP34),
  inference(backward_demodulation,[],[f559,f399])).
fof(f2105,plain,(
  unit != unit | ~sP11),
  inference(backward_demodulation,[],[f559,f457])).
fof(f2114,plain,(
  unit != unit | op(e1,e1) != unit | op(unit,unit) != unit | ~sP0),
  inference(backward_demodulation,[],[f559,f1981])).
fof(f2115,plain,(
  op(e1,e1) != unit | op(unit,unit) != unit | ~sP0),
  inference(trivial_inequality_removal,[],[f2114])).
fof(f2116,plain,(
  ~sP11),
  inference(trivial_inequality_removal,[],[f2105])).
fof(f2117,plain,(
  ~sP34),
  inference(trivial_inequality_removal,[],[f2104])).
fof(f2118,plain,(
  ~sP35),
  inference(trivial_inequality_removal,[],[f2103])).
fof(f2119,plain,(
  e3 != e3 | ~sP40),
  inference(backward_demodulation,[],[f370,f127])).
fof(f2122,plain,(
  e3 != e3 | ~sP33),
  inference(backward_demodulation,[],[f370,f156])).
fof(f2127,plain,(
  e3 != e3 | ~sP10),
  inference(backward_demodulation,[],[f370,f249])).
fof(f2135,plain,(
  ~sP10),
  inference(trivial_inequality_removal,[],[f2127])).
fof(f2136,plain,(
  ~sP33),
  inference(trivial_inequality_removal,[],[f2122])).
fof(f2137,plain,(
  ~sP40),
  inference(trivial_inequality_removal,[],[f2119])).
fof(f2152,plain,(
  e2 != e2 | e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(backward_demodulation,[],[f560,f2077])).
fof(f2153,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP45 | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(trivial_inequality_removal,[],[f2152])).
fof(f2159,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP40 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2153,f2093])).
fof(f2160,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP36 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2159,f2137])).
fof(f2161,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP35 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2160,f2092])).
fof(f2162,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP34 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2161,f2118])).
fof(f2163,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP33 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2162,f2117])).
fof(f2164,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP12 | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2163,f2136])).
fof(f2165,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP11 | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2164,f2091])).
fof(f2166,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP10 | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2165,f2116])).
fof(f2167,plain,(
  e3 != op(unit,e3) | e2 != op(e1,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2166,f2135])).
fof(f2178,plain,(
  e2 != e2 | e3 != op(unit,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(backward_demodulation,[],[f368,f2167])).
fof(f2179,plain,(
  e3 != op(unit,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(e1,unit) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(trivial_inequality_removal,[],[f2178])).
fof(f2182,plain,(
  e3 != e3 | ~sP41),
  inference(backward_demodulation,[],[f367,f123])).
fof(f2185,plain,(
  e3 != e3 | ~sP32),
  inference(backward_demodulation,[],[f367,f160])).
fof(f2190,plain,(
  e3 != e3 | ~sP7),
  inference(backward_demodulation,[],[f367,f261])).
fof(f2194,plain,(
  ~sP7),
  inference(trivial_inequality_removal,[],[f2190])).
fof(f2195,plain,(
  ~sP32),
  inference(trivial_inequality_removal,[],[f2185])).
fof(f2196,plain,(
  ~sP41),
  inference(trivial_inequality_removal,[],[f2182])).
fof(f2206,plain,(
  unit != unit | ~sP27),
  inference(backward_demodulation,[],[f561,f410])).
fof(f2207,plain,(
  unit != unit | ~sP26),
  inference(backward_demodulation,[],[f561,f414])).
fof(f2208,plain,(
  unit != unit | ~sP6),
  inference(backward_demodulation,[],[f561,f465])).
fof(f2209,plain,(
  unit != unit | op(unit,unit) != unit | ~sP0),
  inference(backward_demodulation,[],[f561,f2115])).
fof(f2210,plain,(
  op(unit,unit) != unit | ~sP0),
  inference(trivial_inequality_removal,[],[f2209])).
fof(f2211,plain,(
  ~sP6),
  inference(trivial_inequality_removal,[],[f2208])).
fof(f2212,plain,(
  ~sP26),
  inference(trivial_inequality_removal,[],[f2207])).
fof(f2213,plain,(
  ~sP27),
  inference(trivial_inequality_removal,[],[f2206])).
fof(f2226,plain,(
  e1 != e1 | e3 != op(unit,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(backward_demodulation,[],[f562,f2179])).
fof(f2227,plain,(
  e3 != op(unit,e3) | sP41 | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(trivial_inequality_removal,[],[f2226])).
fof(f2231,plain,(
  e3 != op(unit,e3) | sP32 | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2227,f2196])).
fof(f2232,plain,(
  e3 != op(unit,e3) | sP27 | sP26 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2231,f2195])).
fof(f2233,plain,(
  e3 != op(unit,e3) | sP26 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2232,f2213])).
fof(f2234,plain,(
  e3 != op(unit,e3) | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP7 | sP6 | sP0),
  inference(subsumption_resolution,[],[f2233,f2212])).
fof(f2235,plain,(
  e3 != op(unit,e3) | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP6 | sP0),
  inference(subsumption_resolution,[],[f2234,f2194])).
fof(f2236,plain,(
  e3 != op(unit,e3) | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit | sP0),
  inference(subsumption_resolution,[],[f2235,f2211])).
fof(f2237,plain,(
  e3 != op(unit,e3) | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit),
  inference(subsumption_resolution,[],[f2236,f2210])).
fof(f2248,plain,(
  e3 != e3 | e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit),
  inference(backward_demodulation,[],[f563,f2237])).
fof(f2249,plain,(
  e2 != op(unit,e2) | e1 != op(unit,e1) | op(unit,unit) != unit),
  inference(trivial_inequality_removal,[],[f2248])).
fof(f2264,plain,(
  e2 != e2 | e1 != op(unit,e1) | op(unit,unit) != unit),
  inference(backward_demodulation,[],[f564,f2249])).
fof(f2265,plain,(
  e1 != op(unit,e1) | op(unit,unit) != unit),
  inference(trivial_inequality_removal,[],[f2264])).
fof(f2281,plain,(
  e1 != e1 | op(unit,unit) != unit),
  inference(backward_demodulation,[],[f565,f2265])).
fof(f2282,plain,(
  op(unit,unit) != unit),
  inference(trivial_inequality_removal,[],[f2281])).
fof(f2286,plain,(
  $false),
  inference(subsumption_resolution,[],[f566,f2282])).
% SZS output end Proof for theBenchmark
% ------------------------------
% Version: Vampire 4.2.2 (commit 552c234 on 2018-07-02 14:53:33 +0100)
% Termination reason: Refutation

% Memory used [KB]: 1535
% Time elapsed: 0.178 s
% ------------------------------
% ------------------------------
% Success in time 0.214 s

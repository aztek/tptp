% Refutation found. Thanks to Tanya!
% SZS status Theorem for 
% SZS output start Proof for 
fof(f276,plain,(
  $false),
  inference(trivial_inequality_removal,[],[f275])).
fof(f275,plain,(
  mult(sK0,sK1) != mult(sK0,sK1)),
  inference(superposition,[],[f16,f131])).
fof(f131,plain,(
  ( ! [X2,X3] : (mult(X2,X3) = mult(X3,X2)) )),
  inference(superposition,[],[f27,f96])).
fof(f96,plain,(
  ( ! [X4,X3] : (mult(X4,mult(X3,X4)) = X3) )),
  inference(forward_demodulation,[],[f81,f33])).
fof(f33,plain,(
  ( ! [X2] : (mult(inverse(X2),e) = X2) )),
  inference(superposition,[],[f27,f18])).
fof(f18,plain,(
  ( ! [X0] : (e = mult(inverse(X0),X0)) )),
  inference(cnf_transformation,[],[f9])).
fof(f9,plain,(
  ! [X0] : e = mult(inverse(X0),X0)),
  inference(rectify,[],[f4])).
fof(f4,axiom,(
  ! [X1] : e = mult(inverse(X1),X1)),
  file(unknown,unknown)).
fof(f81,plain,(
  ( ! [X4,X3] : (mult(X4,mult(X3,X4)) = mult(inverse(X3),e)) )),
  inference(superposition,[],[f28,f25])).
fof(f25,plain,(
  ( ! [X0,X1] : (e = mult(X0,mult(X1,mult(X0,X1)))) )),
  inference(superposition,[],[f19,f20])).
fof(f20,plain,(
  ( ! [X0] : (e = mult(X0,X0)) )),
  inference(cnf_transformation,[],[f12])).
fof(f12,plain,(
  ! [X0] : e = mult(X0,X0)),
  inference(rectify,[],[f6])).
fof(f6,axiom,(
  ! [X1] : e = mult(X1,X1)),
  file(unknown,unknown)).
fof(f19,plain,(
  ( ! [X2,X0,X1] : (mult(mult(X0,X1),X2) = mult(X0,mult(X1,X2))) )),
  inference(cnf_transformation,[],[f11])).
fof(f11,plain,(
  ! [X0,X1,X2] : mult(mult(X0,X1),X2) = mult(X0,mult(X1,X2))),
  inference(flattening,[],[f10])).
fof(f10,plain,(
  ! [X0] : ! [X1] : ! [X2] : mult(mult(X0,X1),X2) = mult(X0,mult(X1,X2))),
  inference(rectify,[],[f5])).
fof(f5,axiom,(
  ! [X2] : ! [X0] : ! [X1] : mult(mult(X2,X0),X1) = mult(X2,mult(X0,X1))),
  file(unknown,unknown)).
fof(f28,plain,(
  ( ! [X4,X5] : (mult(inverse(X4),mult(X4,X5)) = X5) )),
  inference(forward_demodulation,[],[f23,f17])).
fof(f17,plain,(
  ( ! [X0] : (mult(e,X0) = X0) )),
  inference(cnf_transformation,[],[f8])).
fof(f8,plain,(
  ! [X0] : mult(e,X0) = X0),
  inference(rectify,[],[f3])).
fof(f3,axiom,(
  ! [X1] : mult(e,X1) = X1),
  file(unknown,unknown)).
fof(f23,plain,(
  ( ! [X4,X5] : (mult(inverse(X4),mult(X4,X5)) = mult(e,X5)) )),
  inference(superposition,[],[f19,f18])).
fof(f27,plain,(
  ( ! [X2,X3] : (mult(X2,mult(X2,X3)) = X3) )),
  inference(forward_demodulation,[],[f22,f17])).
fof(f22,plain,(
  ( ! [X2,X3] : (mult(X2,mult(X2,X3)) = mult(e,X3)) )),
  inference(superposition,[],[f19,f20])).
fof(f16,plain,(
  mult(sK0,sK1) != mult(sK1,sK0)),
  inference(cnf_transformation,[],[f15])).
fof(f15,plain,(
  mult(sK0,sK1) != mult(sK1,sK0)),
  inference(skolemisation,[status(esa),new_symbols(skolem,[sK0,sK1])],[f13,f14])).
fof(f14,plain,(
  ? [X0,X1] : mult(X0,X1) != mult(X1,X0) => mult(sK0,sK1) != mult(sK1,sK0)),
  introduced(choice_axiom,[])).
fof(f13,plain,(
  ? [X0,X1] : mult(X0,X1) != mult(X1,X0)),
  inference(ennf_transformation,[],[f7])).
fof(f7,plain,(
  ~! [X0,X1] : mult(X0,X1) = mult(X1,X0)),
  inference(flattening,[],[f2])).
fof(f2,negated_conjecture,(
  ~! [X0] : ! [X1] : mult(X0,X1) = mult(X1,X0)),
  inference(negated_conjecture,[],[f1])).
fof(f1,conjecture,(
  ! [X0] : ! [X1] : mult(X0,X1) = mult(X1,X0)),
  file(unknown,unknown)).
% SZS output end Proof for 

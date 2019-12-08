% Refutation found. Thanks to Tanya!
% SZS status Theorem for 
% SZS output start Proof for 
fof(f11,plain,(
  $false),
  inference(subsumption_resolution,[],[f10,f7])).
fof(f7,plain,(
  ~mortal(socrates)),
  inference(cnf_transformation,[],[f5])).
fof(f5,plain,(
  ~mortal(socrates)),
  inference(flattening,[],[f2])).
fof(f2,negated_conjecture,(
  ~mortal(socrates)),
  inference(negated_conjecture,[],[f1])).
fof(f1,conjecture,(
  mortal(socrates)),
  file(unknown,unknown)).
fof(f10,plain,(
  mortal(socrates)),
  inference(resolution,[],[f8,f9])).
fof(f9,plain,(
  human(socrates)),
  inference(cnf_transformation,[],[f4])).
fof(f4,axiom,(
  human(socrates)),
  file(unknown,unknown)).
fof(f8,plain,(
  ( ! [X0] : (~human(X0) | mortal(X0)) )),
  inference(cnf_transformation,[],[f6])).
fof(f6,plain,(
  ! [X0] : (mortal(X0) | ~human(X0))),
  inference(ennf_transformation,[],[f3])).
fof(f3,axiom,(
  ! [X0] : (human(X0) => mortal(X0))),
  file(unknown,unknown)).
% SZS output end Proof for 

% SZS status CounterSatisfiable for 
% # SZS output start Saturation.
tff(u47,axiom,
    trace(s,nil,bot)).

tff(u46,axiom,
    trace(i,nil,bot)).

tff(u45,axiom,
    (![X0] : (trace(ap(k,X0),nil,bot)))).

tff(u44,axiom,
    (![X0] : (trace(ap(s,X0),nil,bot)))).

tff(u43,axiom,
    (![X1, X0] : (trace(ap(ap(s,X0),X1),nil,bot)))).

tff(u42,axiom,
    trace(k,nil,bot)).

tff(u41,axiom,
    (![X0] : (trace(ap(i,X0),nil,X0)))).

tff(u40,axiom,
    (![X1, X0] : (trace(ap(ap(k,X0),X1),nil,X0)))).

tff(u39,axiom,
    (![X1, X0, X2] : (trace(ap(ap(ap(s,X0),X1),X2),nil,ap(ap(X0,X2),ap(X1,X2)))))).

tff(u38,negated_conjecture,
    trace(ap(ap(ap(s,i),i),k),sK0,bot)).

tff(u37,axiom,
    (![X1, X3, X0, X2] : ((trace(X0,cons(X1,X3),X2) | ~trace(X1,nil,X2) | ~trace(X0,X3,X1))))).

% # SZS output end Saturation.


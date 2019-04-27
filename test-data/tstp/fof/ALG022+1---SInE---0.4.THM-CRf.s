
cnf(4290,plain,
    ( epred7_0
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | $false
    | e2 != op(e1,op(e2,e2))
    | op(op(e1,e2),e3) != op(e1,op(e2,e3))
    | op(op(e1,e3),e0) != op(e1,op(e3,e0))
    | op(op(e1,e3),e1) != op(e1,op(e3,e1))
    | op(op(e1,e3),e2) != op(e1,op(e3,e2))
    | op(op(e1,e3),e3) != op(e1,op(e3,e3))
    | op(op(e2,e0),e0) != op(e2,op(e0,e0))
    | op(op(e2,e0),e1) != op(e2,op(e0,e1))
    | op(op(e2,e0),e2) != op(e2,op(e0,e2))
    | op(op(e2,e0),e3) != op(e2,op(e0,e3))
    | op(op(e2,e1),e0) != op(e2,op(e1,e0))
    | op(op(e2,e1),e1) != op(e2,op(e1,e1))
    | op(op(e2,e1),e2) != op(e2,op(e1,e2))
    | op(op(e2,e1),e3) != op(e2,op(e1,e3))
    | op(op(e2,e2),e0) != op(e2,op(e2,e0))
    | op(op(e2,e2),e1) != op(e2,op(e2,e1))
    | op(op(e2,e2),e2) != op(e2,op(e2,e2))
    | op(op(e2,e2),e3) != op(e2,op(e2,e3))
    | op(op(e2,e3),e0) != op(e2,op(e3,e0))
    | op(op(e2,e3),e1) != op(e2,op(e3,e1))
    | op(op(e2,e3),e2) != op(e2,op(e3,e2))
    | op(op(e2,e3),e3) != op(e2,op(e3,e3))
    | op(op(e3,e0),e0) != op(e3,op(e0,e0))
    | op(op(e3,e0),e1) != op(e3,op(e0,e1))
    | op(op(e3,e0),e2) != op(e3,op(e0,e2))
    | op(op(e3,e0),e3) != op(e3,op(e0,e3))
    | op(op(e3,e1),e0) != op(e3,op(e1,e0))
    | op(op(e3,e1),e1) != op(e3,op(e1,e1))
    | op(op(e3,e1),e2) != op(e3,op(e1,e2))
    | op(op(e3,e1),e3) != op(e3,op(e1,e3))
    | op(op(e3,e2),e0) != op(e3,op(e2,e0))
    | op(op(e3,e2),e1) != op(e3,op(e2,e1))
    | op(op(e3,e2),e2) != op(e3,op(e2,e2))
    | op(op(e3,e2),e3) != op(e3,op(e2,e3))
    | op(op(e3,e3),e0) != op(e3,op(e3,e0))
    | op(op(e3,e3),e1) != op(e3,op(e3,e1))
    | op(op(e3,e3),e2) != op(e3,op(e3,e2))
    | op(op(e3,e3),e3) != op(e3,op(e3,e3))
    | ~ epred6_0 ),
    inference(rw,[status(thm)],[inference(rw,[status(thm)],[4289,601,theory(equality)]),595,theory(equality)])).

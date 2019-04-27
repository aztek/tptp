%------------------------------------------------------------------------------
% File       : Z3---4.4.1
% Problem    : ALG039+1 : TPTP v6.4.0. Released v2.7.0.
% Transform  : none
% Format     : tptp
% Command    : z3_tptp -proof -model -t:%d -file:%s

% Computer   : n088.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.75MB
% OS         : Linux 3.10.0-327.10.1.el7.x86_64
% CPULimit   : 300s
% DateTime   : Tue Jul 26 10:30:50 EDT 2016

% Result     : Theorem 0.02s
% Output     : Proof 0.07s
% Verified   : 
% Statistics : Number of formulae       :   40 (  63 expanded)
%              Number of leaves         :   19 (  29 expanded)
%              Depth                    :   15
%              Number of atoms          :  635 ( 939 expanded)
%              Number of equality atoms :  624 ( 928 expanded)
%              Maximal formula depth    :   12 (   6 average)
%              Maximal term depth       :    2 (   2 average)

% Comments   : 
%------------------------------------------------------------------------------
%----WARNING: Z3---4.4.1 format not known, defaulting to TPTP
tff(e3_type,type,(
    e3: $i )).

tff(op_type,type,(
    op: ( $i * $i ) > $i )).

tff(e2_type,type,(
    e2: $i )).

tff(e1_type,type,(
    e1: $i )).

tff(e0_type,type,(
    e0: $i )).

tff(1,plain,
    ( ~ $true
  <=> $false ),
    inference(rewrite,[status(thm)],[])).

tff(2,plain,
    ( ~ $false
  <=> $true ),
    inference(rewrite,[status(thm)],[])).

tff(3,plain,
    ( ( ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) )
      & ~ ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) ) )
  <=> $false ),
    inference(rewrite,[status(thm)],[])).

tff(4,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) ) ),
    inference(rewrite,[status(thm)],[])).

tff(5,plain,
    ( ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 )
  <=> ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 ) ),
    inference(rewrite,[status(thm)],[])).

tff(6,plain,
    ( ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3 )
  <=> ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3 ) ),
    inference(rewrite,[status(thm)],[])).

tff(7,plain,
    ( ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 )
  <=> ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 ) ),
    inference(monotonicity,[status(thm)],[6])).

tff(8,plain,
    ( ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 )
  <=> ( op(e0,e0) = e3
      & op(e1,e1) = e3
      & op(e2,e2) = e3
      & op(e3,e3) = e3 ) ),
    inference(transitivity,[status(thm)],[7,5])).

tff(9,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) ) ),
    inference(rewrite,[status(thm)],[])).

tff(10,plain,
    ( ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 )
  <=> ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 ) ),
    inference(rewrite,[status(thm)],[])).

tff(11,plain,
    ( ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2 )
  <=> ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2 ) ),
    inference(rewrite,[status(thm)],[])).

tff(12,plain,
    ( ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 )
  <=> ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 ) ),
    inference(monotonicity,[status(thm)],[11])).

tff(13,plain,
    ( ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 )
  <=> ( op(e0,e0) = e2
      & op(e1,e1) = e2
      & op(e2,e2) = e2
      & op(e3,e3) = e2 ) ),
    inference(transitivity,[status(thm)],[12,10])).

tff(14,plain,
    ( ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 )
  <=> ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 ) ),
    inference(rewrite,[status(thm)],[])).

tff(15,plain,
    ( ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1 )
  <=> ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1 ) ),
    inference(rewrite,[status(thm)],[])).

tff(16,plain,
    ( ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 )
  <=> ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 ) ),
    inference(monotonicity,[status(thm)],[15])).

tff(17,plain,
    ( ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 )
  <=> ( op(e0,e0) = e1
      & op(e1,e1) = e1
      & op(e2,e2) = e1
      & op(e3,e3) = e1 ) ),
    inference(transitivity,[status(thm)],[16,14])).

tff(18,plain,
    ( ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 )
  <=> ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 ) ),
    inference(rewrite,[status(thm)],[])).

tff(19,plain,
    ( ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0 )
  <=> ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0 ) ),
    inference(rewrite,[status(thm)],[])).

tff(20,plain,
    ( ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 )
  <=> ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 ) ),
    inference(monotonicity,[status(thm)],[19])).

tff(21,plain,
    ( ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 )
  <=> ( op(e0,e0) = e0
      & op(e1,e1) = e0
      & op(e2,e2) = e0
      & op(e3,e3) = e0 ) ),
    inference(transitivity,[status(thm)],[20,18])).

tff(22,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 ) ) ),
    inference(monotonicity,[status(thm)],[21,17])).

tff(23,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) ) ),
    inference(monotonicity,[status(thm)],[22,13])).

tff(24,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 ) ) ),
    inference(transitivity,[status(thm)],[23,9])).

tff(25,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) ) ),
    inference(monotonicity,[status(thm)],[24,8])).

tff(26,plain,
    ( ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) )
  <=> ( ( op(e0,e0) = e0
        & op(e1,e1) = e0
        & op(e2,e2) = e0
        & op(e3,e3) = e0 )
      | ( op(e0,e0) = e1
        & op(e1,e1) = e1
        & op(e2,e2) = e1
        & op(e3,e3) = e1 )
      | ( op(e0,e0) = e2
        & op(e1,e1) = e2
        & op(e2,e2) = e2
        & op(e3,e3) = e2 )
      | ( op(e0,e0) = e3
        & op(e1,e1) = e3
        & op(e2,e2) = e3
        & op(e3,e3) = e3 ) ) ),
    inference(transitivity,[status(thm)],[25,4])).

tff(27,plain,
    ( ~ ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) )
  <=> ~ ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) ) ),
    inference(monotonicity,[status(thm)],[26])).

tff(28,plain,
    ( ( ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) )
      & ~ ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) ) )
  <=> ( ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) )
      & ~ ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) ) ) ),
    inference(monotonicity,[status(thm)],[26,27])).

tff(29,plain,
    ( ( ( ( op(e0,e0) = e0
          & op(e1,e1) = e0
          & op(e2,e2) = e0
          & op(e3,e3) = e0 )
        | ( op(e0,e0) = e1
          & op(e1,e1) = e1
          & op(e2,e2) = e1
          & op(e3,e3) = e1 )
        | ( op(e0,e0) = e2
          & op(e1,e1) = e2
          & op(e2,e2) = e2
          & op(e3,e3) = e2 )
        | ( op(e0,e0) = e3
          & op(e1,e1) = e3
          & op(e2,e2) = e3
          & op(e3,e3) = e3 ) )
      & ~ ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) ) )
  <=> $false ),
    inference(transitivity,[status(thm)],[28,3])).

tff(30,plain,
    ( ~ ( ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) )
        & ~ ( ( op(e0,e0) = e0
              & op(e1,e1) = e0
              & op(e2,e2) = e0
              & op(e3,e3) = e0 )
            | ( op(e0,e0) = e1
              & op(e1,e1) = e1
              & op(e2,e2) = e1
              & op(e3,e3) = e1 )
            | ( op(e0,e0) = e2
              & op(e1,e1) = e2
              & op(e2,e2) = e2
              & op(e3,e3) = e2 )
            | ( op(e0,e0) = e3
              & op(e1,e1) = e3
              & op(e2,e2) = e3
              & op(e3,e3) = e3 ) ) )
  <=> ~ $false ),
    inference(monotonicity,[status(thm)],[29])).

tff(31,plain,
    ( ~ ( ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) )
        & ~ ( ( op(e0,e0) = e0
              & op(e1,e1) = e0
              & op(e2,e2) = e0
              & op(e3,e3) = e0 )
            | ( op(e0,e0) = e1
              & op(e1,e1) = e1
              & op(e2,e2) = e1
              & op(e3,e3) = e1 )
            | ( op(e0,e0) = e2
              & op(e1,e1) = e2
              & op(e2,e2) = e2
              & op(e3,e3) = e2 )
            | ( op(e0,e0) = e3
              & op(e1,e1) = e3
              & op(e2,e2) = e3
              & op(e3,e3) = e3 ) ) )
  <=> $true ),
    inference(transitivity,[status(thm)],[30,2])).

tff(32,plain,
    ( ~ ~ ( ( ( op(e0,e0) = e0
              & op(e1,e1) = e0
              & op(e2,e2) = e0
              & op(e3,e3) = e0 )
            | ( op(e0,e0) = e1
              & op(e1,e1) = e1
              & op(e2,e2) = e1
              & op(e3,e3) = e1 )
            | ( op(e0,e0) = e2
              & op(e1,e1) = e2
              & op(e2,e2) = e2
              & op(e3,e3) = e2 )
            | ( op(e0,e0) = e3
              & op(e1,e1) = e3
              & op(e2,e2) = e3
              & op(e3,e3) = e3 ) )
          & ~ ( ( op(e0,e0) = e0
                & op(e1,e1) = e0
                & op(e2,e2) = e0
                & op(e3,e3) = e0 )
              | ( op(e0,e0) = e1
                & op(e1,e1) = e1
                & op(e2,e2) = e1
                & op(e3,e3) = e1 )
              | ( op(e0,e0) = e2
                & op(e1,e1) = e2
                & op(e2,e2) = e2
                & op(e3,e3) = e2 )
              | ( op(e0,e0) = e3
                & op(e1,e1) = e3
                & op(e2,e2) = e3
                & op(e3,e3) = e3 ) ) )
  <=> ~ $true ),
    inference(monotonicity,[status(thm)],[31])).

tff(33,plain,
    ( ~ ~ ( ( ( op(e0,e0) = e0
              & op(e1,e1) = e0
              & op(e2,e2) = e0
              & op(e3,e3) = e0 )
            | ( op(e0,e0) = e1
              & op(e1,e1) = e1
              & op(e2,e2) = e1
              & op(e3,e3) = e1 )
            | ( op(e0,e0) = e2
              & op(e1,e1) = e2
              & op(e2,e2) = e2
              & op(e3,e3) = e2 )
            | ( op(e0,e0) = e3
              & op(e1,e1) = e3
              & op(e2,e2) = e3
              & op(e3,e3) = e3 ) )
          & ~ ( ( op(e0,e0) = e0
                & op(e1,e1) = e0
                & op(e2,e2) = e0
                & op(e3,e3) = e0 )
              | ( op(e0,e0) = e1
                & op(e1,e1) = e1
                & op(e2,e2) = e1
                & op(e3,e3) = e1 )
              | ( op(e0,e0) = e2
                & op(e1,e1) = e2
                & op(e2,e2) = e2
                & op(e3,e3) = e2 )
              | ( op(e0,e0) = e3
                & op(e1,e1) = e3
                & op(e2,e2) = e3
                & op(e3,e3) = e3 ) ) )
  <=> $false ),
    inference(transitivity,[status(thm)],[32,1])).

tff(34,axiom,(
    ~ ~ ( ( ( op(e0,e0) = e0
            & op(e1,e1) = e0
            & op(e2,e2) = e0
            & op(e3,e3) = e0 )
          | ( op(e0,e0) = e1
            & op(e1,e1) = e1
            & op(e2,e2) = e1
            & op(e3,e3) = e1 )
          | ( op(e0,e0) = e2
            & op(e1,e1) = e2
            & op(e2,e2) = e2
            & op(e3,e3) = e2 )
          | ( op(e0,e0) = e3
            & op(e1,e1) = e3
            & op(e2,e2) = e3
            & op(e3,e3) = e3 ) )
        & ~ ( ( op(e0,e0) = e0
              & op(e1,e1) = e0
              & op(e2,e2) = e0
              & op(e3,e3) = e0 )
            | ( op(e0,e0) = e1
              & op(e1,e1) = e1
              & op(e2,e2) = e1
              & op(e3,e3) = e1 )
            | ( op(e0,e0) = e2
              & op(e1,e1) = e2
              & op(e2,e2) = e2
              & op(e3,e3) = e2 )
            | ( op(e0,e0) = e3
              & op(e1,e1) = e3
              & op(e2,e2) = e3
              & op(e3,e3) = e3 ) ) ) ),
    file('/export/starexec/sandbox/benchmark/theBenchmark.p',co1)).

tff(35,plain,(
    $false ),
    inference(modus_ponens,[status(thm)],[34,33])).
%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.04  % Problem    : ALG039+1 : TPTP v6.4.0. Released v2.7.0.
% 0.00/0.04  % Command    : z3_tptp -proof -model -t:%d -file:%s
% 0.02/0.24  % Computer   : n088.star.cs.uiowa.edu
% 0.02/0.24  % Model      : x86_64 x86_64
% 0.02/0.24  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.02/0.24  % Memory     : 32218.75MB
% 0.02/0.24  % OS         : Linux 3.10.0-327.10.1.el7.x86_64
% 0.02/0.24  % CPULimit   : 300
% 0.02/0.24  % DateTime   : Thu Jul 21 10:50:10 CDT 2016
% 0.02/0.24  % CPUTime    : 
% 0.02/0.27  % SZS status Theorem
% 0.02/0.27  % SZS output start Proof
% 0.02/0.27  tff(e3_type, type, (
% 0.02/0.27     e3: $i)).
% 0.02/0.27  tff(op_type, type, (
% 0.02/0.27     op: ( $i * $i ) > $i)).
% 0.02/0.27  tff(e2_type, type, (
% 0.02/0.27     e2: $i)).
% 0.02/0.27  tff(e1_type, type, (
% 0.02/0.27     e1: $i)).
% 0.02/0.27  tff(e0_type, type, (
% 0.02/0.27     e0: $i)).
% 0.02/0.27  tff(1,plain,
% 0.02/0.27      (((~$true) <=> $false)),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(2,plain,
% 0.02/0.27      (((~$false) <=> $true)),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(3,plain,
% 0.02/0.27      ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))) & (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))) <=> $false)),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(4,plain,
% 0.02/0.27      ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(5,plain,
% 0.02/0.27      (((((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(6,plain,
% 0.02/0.27      (((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(7,plain,
% 0.02/0.27      ((((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> (((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[6])).
% 0.02/0.27  tff(8,plain,
% 0.02/0.27      ((((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)) <=> ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))),
% 0.02/0.27      inference(transitivity,[status(thm)],[7, 5])).
% 0.02/0.27  tff(9,plain,
% 0.02/0.27      ((((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(10,plain,
% 0.02/0.27      (((((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(11,plain,
% 0.02/0.27      (((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(12,plain,
% 0.02/0.27      ((((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> (((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[11])).
% 0.02/0.27  tff(13,plain,
% 0.02/0.27      ((((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2)) <=> ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)))),
% 0.02/0.27      inference(transitivity,[status(thm)],[12, 10])).
% 0.02/0.27  tff(14,plain,
% 0.02/0.27      (((((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(15,plain,
% 0.02/0.27      (((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(16,plain,
% 0.02/0.27      ((((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> (((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[15])).
% 0.02/0.27  tff(17,plain,
% 0.02/0.27      ((((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1)) <=> ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)))),
% 0.02/0.27      inference(transitivity,[status(thm)],[16, 14])).
% 0.02/0.27  tff(18,plain,
% 0.02/0.27      (((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(19,plain,
% 0.02/0.27      (((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)))),
% 0.02/0.27      inference(rewrite,[status(thm)],[])).
% 0.02/0.27  tff(20,plain,
% 0.02/0.27      ((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[19])).
% 0.02/0.27  tff(21,plain,
% 0.02/0.27      ((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) <=> ((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)))),
% 0.02/0.27      inference(transitivity,[status(thm)],[20, 18])).
% 0.02/0.27  tff(22,plain,
% 0.02/0.27      (((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[21, 17])).
% 0.02/0.27  tff(23,plain,
% 0.02/0.27      ((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1))) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
% 0.02/0.27      inference(monotonicity,[status(thm)],[22, 13])).
% 0.02/0.27  tff(24,plain,
% 0.02/0.27      ((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))))),
% 0.02/0.27      inference(transitivity,[status(thm)],[23, 9])).
% 0.02/0.27  tff(25,plain,
% 0.02/0.27      (((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2))) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
% 0.02/0.28      inference(monotonicity,[status(thm)],[24, 8])).
% 0.02/0.28  tff(26,plain,
% 0.02/0.28      (((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) <=> (((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))),
% 0.02/0.28      inference(transitivity,[status(thm)],[25, 4])).
% 0.02/0.28  tff(27,plain,
% 0.02/0.28      (((~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))) <=> (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3)))))),
% 0.02/0.28      inference(monotonicity,[status(thm)],[26])).
% 0.02/0.28  tff(28,plain,
% 0.02/0.28      ((((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))) <=> ((((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & 
% (op(e2, e2) = e3) & (op(e3, e3) = e3))) & (~(((op(e0, e0) = e0) & (op(e1, e1) = e0) & (op(e2, e2) = e0) & (op(e3, e3) = e0)) | ((op(e0, e0) = e1) & (op(e1, e1) = e1) & (op(e2, e2) = e1) & (op(e3, e3) = e1)) | ((op(e0, e0) = e2) & (op(e1, e1) = e2) & (op(e2, e2) = e2) & (op(e3, e3) = e2)) | ((op(e0, e0) = e3) & (op(e1, e1) = e3) & (op(e2, e2) = e3) & (op(e3, e3) = e3))))))),
% 0.02/0.28      inference(monotonicity,[status(thm)],[26, 27])).
% 0.02/0.28  tff(29,plain,
% 0.02/0.28      ((((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))) <=> $false)),
% 0.02/0.28      inference(transitivity,[status(thm)],[28, 3])).
% 0.02/0.28  tff(30,plain,
% 0.02/0.28      (((~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))) <=> (~$false))),
% 0.07/0.29      inference(monotonicity,[status(thm)],[29])).
% 0.07/0.29  tff(31,plain,
% 0.07/0.29      (((~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))) <=> $true)),
% 0.07/0.29      inference(transitivity,[status(thm)],[30, 2])).
% 0.07/0.29  tff(32,plain,
% 0.07/0.29      (((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))))) <=> (~$true))),
% 0.07/0.29      inference(monotonicity,[status(thm)],[31])).
% 0.07/0.29  tff(33,plain,
% 0.07/0.29      (((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))))))) <=> $false)),
% 0.07/0.29      inference(transitivity,[status(thm)],[32, 1])).
% 0.07/0.29  tff(34,axiom,((~(~((((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3))) & (~(((((((op(e0, e0) = e0) & (op(e1, e1) = e0)) & (op(e2, e2) = e0)) & (op(e3, e3) = e0)) | ((((op(e0, e0) = e1) & (op(e1, e1) = e1)) & (op(e2, e2) = e1)) & (op(e3, e3) = e1))) | ((((op(e0, e0) = e2) & (op(e1, e1) = e2)) & (op(e2, e2) = e2)) & (op(e3, e3) = e2))) | ((((op(e0, e0) = e3) & (op(e1, e1) = e3)) & (op(e2, e2) = e3)) & (op(e3, e3) = e3)))))))), file('/export/starexec/sandbox/benchmark/theBenchmark.p','co1')).
% 0.07/0.29  tff(35,plain,
% 0.07/0.29      ($false),
% 0.07/0.29      inference(modus_ponens,[status(thm)],[34, 33])).
% 0.07/0.29  % SZS output end Proof
%------------------------------------------------------------------------------

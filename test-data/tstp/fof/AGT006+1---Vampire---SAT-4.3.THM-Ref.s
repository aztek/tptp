%------------------------------------------------------------------------------
% File       : Vampire---SAT-4.3
% Problem    : AGT006+1 : TPTP v7.1.0. Bugfixed v3.1.0.
% Transform  : none
% Format     : tptp:raw
% Command    : vampire --mode casc_sat -t %d %s

% Computer   : n017.star.cs.uiowa.edu
% Model      : x86_64 x86_64
% CPU        : Intel(R) Xeon(R) CPU E5-2609 0 2.40GHz
% Memory     : 32218.625MB
% OS         : Linux 3.10.0-693.2.2.el7.x86_64
% CPULimit   : 300s
% DateTime   : Tue Sep  4 10:07:08 EDT 2018

% Result     : Theorem 65.97s
% Output     : Refutation 65.97s
% Verified   : 
% Statistics : Number of formulae       :  144 ( 273 expanded)
%              Number of leaves         :   34 (  74 expanded)
%              Depth                    :   13
%              Number of atoms          :  372 ( 800 expanded)
%              Number of equality atoms :    0 (   0 expanded)
%              Maximal formula depth    :   10 (   4 average)
%              Maximal term depth       :    4 (   1 average)

% Comments   : 
%------------------------------------------------------------------------------
%----WARNING: Vampire---SAT-4.3 format not known, defaulting to TPTP
fof(f1,axiom,(
    ! [X0,X1,X2,X3] :
      ( accept_team(X0,X3,X1,X2)
    <=> ( accept_number(X0,X2)
        & accept_leader(X0,X3)
        & accept_city(X0,X1) ) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_1)).

fof(f3,axiom,(
    ! [X0,X2,X4,X5] :
      ( ( less(X4,X2)
        & accept_population(X0,X5,X2) )
     => accept_population(X0,X5,X4) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_3)).

fof(f4,axiom,(
    ! [X0,X3,X1] :
      ( the_agent_in_all_proposed_teams(X0,X3,X1)
     => ( accept_city(X0,X1)
        & accept_leader(X0,X3) ) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_4)).

fof(f8,axiom,(
    ! [X0] :
      ( ( accept_population(X0,other,n4)
        & accept_population(X0,native,n4)
        & accept_population(X0,muslim,n7)
        & accept_population(X0,christian,n20)
        & accept_population(X0,atheist,n65) )
    <=> accept_city(X0,suffertown) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_1)).

fof(f14,axiom,(
    ! [X0] :
      ( ( accept_population(X0,other,n0)
        & accept_population(X0,native,n85)
        & accept_population(X0,muslim,n0)
        & accept_population(X0,christian,n3)
        & accept_population(X0,atheist,n12) )
    <=> accept_city(X0,coastvillage) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_7)).

fof(f16,axiom,(
    ! [X0] :
      ( ( accept_population(X0,other,n0)
        & accept_population(X0,native,n0)
        & accept_population(X0,muslim,n1)
        & accept_population(X0,christian,n24)
        & accept_population(X0,atheist,n75) )
    <=> accept_city(X0,towna) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_9)).

fof(f19,axiom,(
    ! [X0] :
      ( ( accept_population(X0,other,n1)
        & accept_population(X0,native,n0)
        & accept_population(X0,muslim,n1)
        & accept_population(X0,christian,n20)
        & accept_population(X0,atheist,n78) )
    <=> accept_city(X0,cityb) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_12)).

fof(f20,axiom,(
    ! [X0] :
      ( ( accept_population(X0,other,n5)
        & accept_population(X0,native,n0)
        & accept_population(X0,muslim,n65)
        & accept_population(X0,christian,n0)
        & accept_population(X0,atheist,n30) )
    <=> accept_city(X0,townc) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_13)).

fof(f38,axiom,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n4) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_18)).

fof(f94,axiom,(
    accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n5) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_74)).

fof(f97,axiom,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,townc,n6) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_77)).

fof(f100,axiom,(
    the_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_80)).

fof(f181,axiom,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n6) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_161)).

fof(f229,axiom,(
    the_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_209)).

fof(f274,axiom,(
    rdn_translate(n4,rdn_pos(rdnn(n4))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn4)).

fof(f275,axiom,(
    rdn_translate(n5,rdn_pos(rdnn(n5))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn5)).

fof(f277,axiom,(
    rdn_translate(n7,rdn_pos(rdnn(n7))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn7)).

fof(f335,axiom,(
    rdn_translate(n65,rdn_pos(rdn(rdnn(n5),rdnn(n6)))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn65)).

fof(f340,axiom,(
    rdn_translate(n70,rdn_pos(rdn(rdnn(n0),rdnn(n7)))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn70)).

fof(f345,axiom,(
    rdn_translate(n75,rdn_pos(rdn(rdnn(n5),rdnn(n7)))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn75)).

fof(f355,axiom,(
    rdn_translate(n85,rdn_pos(rdn(rdnn(n5),rdnn(n8)))) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn85)).

fof(f531,axiom,(
    rdn_non_zero_digit(rdnn(n6)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_digit6)).

fof(f532,axiom,(
    rdn_non_zero_digit(rdnn(n7)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_digit7)).

fof(f539,axiom,(
    rdn_positive_less(rdnn(n4),rdnn(n5)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less45)).

fof(f540,axiom,(
    rdn_positive_less(rdnn(n5),rdnn(n6)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less56)).

fof(f541,axiom,(
    rdn_positive_less(rdnn(n6),rdnn(n7)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less67)).

fof(f542,axiom,(
    rdn_positive_less(rdnn(n7),rdnn(n8)) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less78)).

fof(f544,axiom,(
    ! [X6,X7,X8] :
      ( ( rdn_positive_less(rdnn(X7),rdnn(X8))
        & rdn_positive_less(rdnn(X6),rdnn(X7)) )
     => rdn_positive_less(rdnn(X6),rdnn(X8)) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less_transitivity)).

fof(f545,axiom,(
    ! [X9,X10,X11,X12] :
      ( rdn_positive_less(X10,X12)
     => rdn_positive_less(rdn(rdnn(X9),X10),rdn(rdnn(X11),X12)) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less_multi_digit_high)).

fof(f547,axiom,(
    ! [X14,X11,X12] :
      ( rdn_non_zero(X12)
     => rdn_positive_less(rdnn(X14),rdn(rdnn(X11),X12)) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_extra_digits_positive_less)).

fof(f548,axiom,(
    ! [X6] :
      ( rdn_non_zero_digit(rdnn(X6))
     => rdn_non_zero(rdnn(X6)) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_non_zero_by_digit)).

fof(f550,axiom,(
    ! [X6,X7,X15,X16] :
      ( ( rdn_positive_less(X15,X16)
        & rdn_translate(X7,rdn_pos(X16))
        & rdn_translate(X6,rdn_pos(X15)) )
     => less(X6,X7) ) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',less_entry_point_pos_pos)).

fof(f556,conjecture,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4) ),
    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',query_6)).

fof(f557,negated_conjecture,(
    ~ accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4) ),
    inference(negated_conjecture,[],[f556])).

fof(f558,plain,(
    ~ accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4) ),
    inference(flattening,[],[f557])).

fof(f559,plain,(
    ! [X0] :
      ( rdn_non_zero_digit(rdnn(X0))
     => rdn_non_zero(rdnn(X0)) ) ),
    inference(rectify,[],[f548])).

fof(f564,plain,(
    ! [X0,X1,X2] :
      ( rdn_non_zero(X2)
     => rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2)) ) ),
    inference(rectify,[],[f547])).

fof(f566,plain,(
    ! [X0,X1,X2] :
      ( the_agent_in_all_proposed_teams(X0,X1,X2)
     => ( accept_city(X0,X2)
        & accept_leader(X0,X1) ) ) ),
    inference(rectify,[],[f4])).

fof(f571,plain,(
    ! [X0,X1,X2] :
      ( ( rdn_positive_less(rdnn(X1),rdnn(X2))
        & rdn_positive_less(rdnn(X0),rdnn(X1)) )
     => rdn_positive_less(rdnn(X0),rdnn(X2)) ) ),
    inference(rectify,[],[f544])).

fof(f572,plain,(
    ! [X0,X1,X2,X3] :
      ( rdn_positive_less(X1,X3)
     => rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3)) ) ),
    inference(rectify,[],[f545])).

fof(f574,plain,(
    ! [X0,X1,X2,X3] :
      ( ( rdn_positive_less(X2,X3)
        & rdn_translate(X1,rdn_pos(X3))
        & rdn_translate(X0,rdn_pos(X2)) )
     => less(X0,X1) ) ),
    inference(rectify,[],[f550])).

fof(f576,plain,(
    ! [X0,X1,X2,X3] :
      ( ( less(X2,X1)
        & accept_population(X0,X3,X1) )
     => accept_population(X0,X3,X2) ) ),
    inference(rectify,[],[f3])).

fof(f577,plain,(
    ! [X0] :
      ( rdn_non_zero(rdnn(X0))
      | ~ rdn_non_zero_digit(rdnn(X0)) ) ),
    inference(ennf_transformation,[],[f559])).

fof(f580,plain,(
    ! [X0,X1,X2] :
      ( rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2))
      | ~ rdn_non_zero(X2) ) ),
    inference(ennf_transformation,[],[f564])).

fof(f583,plain,(
    ! [X0,X1,X2] :
      ( ( accept_city(X0,X2)
        & accept_leader(X0,X1) )
      | ~ the_agent_in_all_proposed_teams(X0,X1,X2) ) ),
    inference(ennf_transformation,[],[f566])).

fof(f591,plain,(
    ! [X0,X1,X2] :
      ( rdn_positive_less(rdnn(X0),rdnn(X2))
      | ~ rdn_positive_less(rdnn(X1),rdnn(X2))
      | ~ rdn_positive_less(rdnn(X0),rdnn(X1)) ) ),
    inference(ennf_transformation,[],[f571])).

fof(f592,plain,(
    ! [X0,X1,X2] :
      ( rdn_positive_less(rdnn(X0),rdnn(X2))
      | ~ rdn_positive_less(rdnn(X1),rdnn(X2))
      | ~ rdn_positive_less(rdnn(X0),rdnn(X1)) ) ),
    inference(flattening,[],[f591])).

fof(f593,plain,(
    ! [X0,X1,X2,X3] :
      ( rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3))
      | ~ rdn_positive_less(X1,X3) ) ),
    inference(ennf_transformation,[],[f572])).

fof(f596,plain,(
    ! [X0,X1,X2,X3] :
      ( less(X0,X1)
      | ~ rdn_positive_less(X2,X3)
      | ~ rdn_translate(X1,rdn_pos(X3))
      | ~ rdn_translate(X0,rdn_pos(X2)) ) ),
    inference(ennf_transformation,[],[f574])).

fof(f597,plain,(
    ! [X0,X1,X2,X3] :
      ( less(X0,X1)
      | ~ rdn_positive_less(X2,X3)
      | ~ rdn_translate(X1,rdn_pos(X3))
      | ~ rdn_translate(X0,rdn_pos(X2)) ) ),
    inference(flattening,[],[f596])).

fof(f600,plain,(
    ! [X0,X1,X2,X3] :
      ( accept_population(X0,X3,X2)
      | ~ less(X2,X1)
      | ~ accept_population(X0,X3,X1) ) ),
    inference(ennf_transformation,[],[f576])).

fof(f601,plain,(
    ! [X0,X1,X2,X3] :
      ( accept_population(X0,X3,X2)
      | ~ less(X2,X1)
      | ~ accept_population(X0,X3,X1) ) ),
    inference(flattening,[],[f600])).

fof(f602,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n5)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n65)
          & accept_population(X0,christian,n0)
          & accept_population(X0,atheist,n30) )
        | ~ accept_city(X0,townc) )
      & ( accept_city(X0,townc)
        | ~ accept_population(X0,other,n5)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n65)
        | ~ accept_population(X0,christian,n0)
        | ~ accept_population(X0,atheist,n30) ) ) ),
    inference(nnf_transformation,[],[f20])).

fof(f603,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n5)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n65)
          & accept_population(X0,christian,n0)
          & accept_population(X0,atheist,n30) )
        | ~ accept_city(X0,townc) )
      & ( accept_city(X0,townc)
        | ~ accept_population(X0,other,n5)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n65)
        | ~ accept_population(X0,christian,n0)
        | ~ accept_population(X0,atheist,n30) ) ) ),
    inference(flattening,[],[f602])).

fof(f604,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n1)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n1)
          & accept_population(X0,christian,n20)
          & accept_population(X0,atheist,n78) )
        | ~ accept_city(X0,cityb) )
      & ( accept_city(X0,cityb)
        | ~ accept_population(X0,other,n1)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n1)
        | ~ accept_population(X0,christian,n20)
        | ~ accept_population(X0,atheist,n78) ) ) ),
    inference(nnf_transformation,[],[f19])).

fof(f605,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n1)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n1)
          & accept_population(X0,christian,n20)
          & accept_population(X0,atheist,n78) )
        | ~ accept_city(X0,cityb) )
      & ( accept_city(X0,cityb)
        | ~ accept_population(X0,other,n1)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n1)
        | ~ accept_population(X0,christian,n20)
        | ~ accept_population(X0,atheist,n78) ) ) ),
    inference(flattening,[],[f604])).

fof(f610,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n0)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n1)
          & accept_population(X0,christian,n24)
          & accept_population(X0,atheist,n75) )
        | ~ accept_city(X0,towna) )
      & ( accept_city(X0,towna)
        | ~ accept_population(X0,other,n0)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n1)
        | ~ accept_population(X0,christian,n24)
        | ~ accept_population(X0,atheist,n75) ) ) ),
    inference(nnf_transformation,[],[f16])).

fof(f611,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n0)
          & accept_population(X0,native,n0)
          & accept_population(X0,muslim,n1)
          & accept_population(X0,christian,n24)
          & accept_population(X0,atheist,n75) )
        | ~ accept_city(X0,towna) )
      & ( accept_city(X0,towna)
        | ~ accept_population(X0,other,n0)
        | ~ accept_population(X0,native,n0)
        | ~ accept_population(X0,muslim,n1)
        | ~ accept_population(X0,christian,n24)
        | ~ accept_population(X0,atheist,n75) ) ) ),
    inference(flattening,[],[f610])).

fof(f620,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n0)
          & accept_population(X0,native,n85)
          & accept_population(X0,muslim,n0)
          & accept_population(X0,christian,n3)
          & accept_population(X0,atheist,n12) )
        | ~ accept_city(X0,coastvillage) )
      & ( accept_city(X0,coastvillage)
        | ~ accept_population(X0,other,n0)
        | ~ accept_population(X0,native,n85)
        | ~ accept_population(X0,muslim,n0)
        | ~ accept_population(X0,christian,n3)
        | ~ accept_population(X0,atheist,n12) ) ) ),
    inference(nnf_transformation,[],[f14])).

fof(f621,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n0)
          & accept_population(X0,native,n85)
          & accept_population(X0,muslim,n0)
          & accept_population(X0,christian,n3)
          & accept_population(X0,atheist,n12) )
        | ~ accept_city(X0,coastvillage) )
      & ( accept_city(X0,coastvillage)
        | ~ accept_population(X0,other,n0)
        | ~ accept_population(X0,native,n85)
        | ~ accept_population(X0,muslim,n0)
        | ~ accept_population(X0,christian,n3)
        | ~ accept_population(X0,atheist,n12) ) ) ),
    inference(flattening,[],[f620])).

fof(f626,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n4)
          & accept_population(X0,native,n4)
          & accept_population(X0,muslim,n7)
          & accept_population(X0,christian,n20)
          & accept_population(X0,atheist,n65) )
        | ~ accept_city(X0,suffertown) )
      & ( accept_city(X0,suffertown)
        | ~ accept_population(X0,other,n4)
        | ~ accept_population(X0,native,n4)
        | ~ accept_population(X0,muslim,n7)
        | ~ accept_population(X0,christian,n20)
        | ~ accept_population(X0,atheist,n65) ) ) ),
    inference(nnf_transformation,[],[f8])).

fof(f627,plain,(
    ! [X0] :
      ( ( ( accept_population(X0,other,n4)
          & accept_population(X0,native,n4)
          & accept_population(X0,muslim,n7)
          & accept_population(X0,christian,n20)
          & accept_population(X0,atheist,n65) )
        | ~ accept_city(X0,suffertown) )
      & ( accept_city(X0,suffertown)
        | ~ accept_population(X0,other,n4)
        | ~ accept_population(X0,native,n4)
        | ~ accept_population(X0,muslim,n7)
        | ~ accept_population(X0,christian,n20)
        | ~ accept_population(X0,atheist,n65) ) ) ),
    inference(flattening,[],[f626])).

fof(f632,plain,(
    ! [X0,X1,X2,X3] :
      ( ( accept_team(X0,X3,X1,X2)
        | ~ accept_number(X0,X2)
        | ~ accept_leader(X0,X3)
        | ~ accept_city(X0,X1) )
      & ( ( accept_number(X0,X2)
          & accept_leader(X0,X3)
          & accept_city(X0,X1) )
        | ~ accept_team(X0,X3,X1,X2) ) ) ),
    inference(nnf_transformation,[],[f1])).

fof(f633,plain,(
    ! [X0,X1,X2,X3] :
      ( ( accept_team(X0,X3,X1,X2)
        | ~ accept_number(X0,X2)
        | ~ accept_leader(X0,X3)
        | ~ accept_city(X0,X1) )
      & ( ( accept_number(X0,X2)
          & accept_leader(X0,X3)
          & accept_city(X0,X1) )
        | ~ accept_team(X0,X3,X1,X2) ) ) ),
    inference(flattening,[],[f632])).

fof(f634,plain,(
    ~ accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4) ),
    inference(cnf_transformation,[],[f558])).

fof(f639,plain,(
    rdn_non_zero_digit(rdnn(n6)) ),
    inference(cnf_transformation,[],[f531])).

fof(f640,plain,(
    rdn_non_zero_digit(rdnn(n7)) ),
    inference(cnf_transformation,[],[f532])).

fof(f698,plain,(
    the_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc) ),
    inference(cnf_transformation,[],[f100])).

fof(f710,plain,(
    the_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna) ),
    inference(cnf_transformation,[],[f229])).

fof(f781,plain,(
    rdn_positive_less(rdnn(n5),rdnn(n6)) ),
    inference(cnf_transformation,[],[f540])).

fof(f782,plain,(
    rdn_positive_less(rdnn(n6),rdnn(n7)) ),
    inference(cnf_transformation,[],[f541])).

fof(f787,plain,(
    rdn_positive_less(rdnn(n4),rdnn(n5)) ),
    inference(cnf_transformation,[],[f539])).

fof(f789,plain,(
    rdn_positive_less(rdnn(n7),rdnn(n8)) ),
    inference(cnf_transformation,[],[f542])).

fof(f791,plain,(
    rdn_translate(n4,rdn_pos(rdnn(n4))) ),
    inference(cnf_transformation,[],[f274])).

fof(f792,plain,(
    rdn_translate(n5,rdn_pos(rdnn(n5))) ),
    inference(cnf_transformation,[],[f275])).

fof(f793,plain,(
    rdn_translate(n7,rdn_pos(rdnn(n7))) ),
    inference(cnf_transformation,[],[f277])).

fof(f814,plain,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n6) ),
    inference(cnf_transformation,[],[f181])).

fof(f877,plain,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,townc,n6) ),
    inference(cnf_transformation,[],[f97])).

fof(f885,plain,(
    accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n5) ),
    inference(cnf_transformation,[],[f94])).

fof(f910,plain,(
    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n4) ),
    inference(cnf_transformation,[],[f38])).

fof(f972,plain,(
    rdn_translate(n70,rdn_pos(rdn(rdnn(n0),rdnn(n7)))) ),
    inference(cnf_transformation,[],[f340])).

fof(f977,plain,(
    rdn_translate(n75,rdn_pos(rdn(rdnn(n5),rdnn(n7)))) ),
    inference(cnf_transformation,[],[f345])).

fof(f987,plain,(
    rdn_translate(n85,rdn_pos(rdn(rdnn(n5),rdnn(n8)))) ),
    inference(cnf_transformation,[],[f355])).

fof(f999,plain,(
    rdn_translate(n65,rdn_pos(rdn(rdnn(n5),rdnn(n6)))) ),
    inference(cnf_transformation,[],[f335])).

fof(f1158,plain,(
    ! [X0] :
      ( ~ rdn_non_zero_digit(rdnn(X0))
      | rdn_non_zero(rdnn(X0)) ) ),
    inference(cnf_transformation,[],[f577])).

fof(f1162,plain,(
    ! [X0] :
      ( accept_population(X0,muslim,n65)
      | ~ accept_city(X0,townc) ) ),
    inference(cnf_transformation,[],[f603])).

fof(f1164,plain,(
    ! [X0] :
      ( accept_population(X0,other,n5)
      | ~ accept_city(X0,townc) ) ),
    inference(cnf_transformation,[],[f603])).

fof(f1167,plain,(
    ! [X0] :
      ( accept_population(X0,christian,n20)
      | ~ accept_city(X0,cityb) ) ),
    inference(cnf_transformation,[],[f605])).

fof(f1184,plain,(
    ! [X0] :
      ( accept_population(X0,atheist,n75)
      | ~ accept_city(X0,towna) ) ),
    inference(cnf_transformation,[],[f611])).

fof(f1217,plain,(
    ! [X0] :
      ( accept_population(X0,native,n85)
      | ~ accept_city(X0,coastvillage) ) ),
    inference(cnf_transformation,[],[f621])).

fof(f1231,plain,(
    ! [X0] :
      ( ~ accept_population(X0,other,n4)
      | accept_city(X0,suffertown)
      | ~ accept_population(X0,native,n4)
      | ~ accept_population(X0,muslim,n7)
      | ~ accept_population(X0,christian,n20)
      | ~ accept_population(X0,atheist,n65) ) ),
    inference(cnf_transformation,[],[f627])).

fof(f1245,plain,(
    ! [X2,X0,X1] :
      ( ~ rdn_non_zero(X2)
      | rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2)) ) ),
    inference(cnf_transformation,[],[f580])).

fof(f1248,plain,(
    ! [X2,X0,X1] :
      ( ~ the_agent_in_all_proposed_teams(X0,X1,X2)
      | accept_city(X0,X2) ) ),
    inference(cnf_transformation,[],[f583])).

fof(f1253,plain,(
    ! [X2,X0,X1] :
      ( ~ rdn_positive_less(rdnn(X1),rdnn(X2))
      | rdn_positive_less(rdnn(X0),rdnn(X2))
      | ~ rdn_positive_less(rdnn(X0),rdnn(X1)) ) ),
    inference(cnf_transformation,[],[f592])).

fof(f1254,plain,(
    ! [X2,X0,X3,X1] :
      ( rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3))
      | ~ rdn_positive_less(X1,X3) ) ),
    inference(cnf_transformation,[],[f593])).

fof(f1256,plain,(
    ! [X2,X0,X3,X1] :
      ( less(X0,X1)
      | ~ rdn_positive_less(X2,X3)
      | ~ rdn_translate(X1,rdn_pos(X3))
      | ~ rdn_translate(X0,rdn_pos(X2)) ) ),
    inference(cnf_transformation,[],[f597])).

fof(f1258,plain,(
    ! [X2,X0,X3,X1] :
      ( ~ accept_population(X0,X3,X1)
      | ~ less(X2,X1)
      | accept_population(X0,X3,X2) ) ),
    inference(cnf_transformation,[],[f601])).

fof(f1259,plain,(
    ! [X2,X0,X3,X1] :
      ( ~ accept_team(X0,X3,X1,X2)
      | accept_city(X0,X1) ) ),
    inference(cnf_transformation,[],[f633])).

fof(f1260,plain,(
    ! [X2,X0,X3,X1] :
      ( ~ accept_team(X0,X3,X1,X2)
      | accept_leader(X0,X3) ) ),
    inference(cnf_transformation,[],[f633])).

fof(f1261,plain,(
    ! [X2,X0,X3,X1] :
      ( ~ accept_team(X0,X3,X1,X2)
      | accept_number(X0,X2) ) ),
    inference(cnf_transformation,[],[f633])).

fof(f1262,plain,(
    ! [X2,X0,X3,X1] :
      ( ~ accept_number(X0,X2)
      | accept_team(X0,X3,X1,X2)
      | ~ accept_leader(X0,X3)
      | ~ accept_city(X0,X1) ) ),
    inference(cnf_transformation,[],[f633])).

fof(f1269,plain,(
    ! [X2,X0,X3] :
      ( ~ rdn_translate(X0,rdn_pos(X2))
      | ~ rdn_positive_less(X2,X3)
      | sP2(X3,X0) ) ),
    inference(cnf_transformation,[],[f1269_D])).

fof(f1269_D,plain,(
    ! [X0,X3] :
      ( ! [X2] :
          ( ~ rdn_translate(X0,rdn_pos(X2))
          | ~ rdn_positive_less(X2,X3) )
    <=> ~ sP2(X3,X0) ) ),
    introduced(general_splitting_component_introduction,[new_symbols(naming,[sP2])])).

fof(f1270,plain,(
    ! [X0,X3,X1] :
      ( ~ rdn_translate(X1,rdn_pos(X3))
      | less(X0,X1)
      | ~ sP2(X3,X0) ) ),
    inference(general_splitting,[],[f1256,f1269_D])).

fof(f1273,plain,(
    rdn_non_zero(rdnn(n7)) ),
    inference(unit_resulting_resolution,[],[f640,f1158])).

fof(f1281,plain,(
    rdn_non_zero(rdnn(n6)) ),
    inference(unit_resulting_resolution,[],[f639,f1158])).

fof(f1421,plain,(
    accept_city(countrybcivilorganization,townc) ),
    inference(unit_resulting_resolution,[],[f698,f1248])).

fof(f1423,plain,(
    accept_city(countrybcivilorganization,towna) ),
    inference(unit_resulting_resolution,[],[f710,f1248])).

fof(f1535,plain,(
    accept_population(countrybcivilorganization,other,n5) ),
    inference(unit_resulting_resolution,[],[f1421,f1164])).

fof(f1537,plain,(
    accept_population(countrybcivilorganization,muslim,n65) ),
    inference(unit_resulting_resolution,[],[f1421,f1162])).

fof(f1544,plain,(
    accept_population(countrybcivilorganization,atheist,n75) ),
    inference(unit_resulting_resolution,[],[f1423,f1184])).

fof(f63624,plain,(
    accept_city(countrybcivilorganization,coastvillage) ),
    inference(unit_resulting_resolution,[],[f885,f1259])).

fof(f63679,plain,(
    accept_city(countrybcivilorganization,cityb) ),
    inference(unit_resulting_resolution,[],[f814,f1259])).

fof(f63841,plain,(
    accept_population(countrybcivilorganization,native,n85) ),
    inference(unit_resulting_resolution,[],[f63624,f1217])).

fof(f63898,plain,(
    accept_population(countrybcivilorganization,christian,n20) ),
    inference(unit_resulting_resolution,[],[f63679,f1167])).

fof(f64028,plain,(
    accept_leader(countrybcivilorganization,countrybhumanitarianorganization) ),
    inference(unit_resulting_resolution,[],[f877,f1260])).

fof(f64210,plain,(
    accept_number(countrybcivilorganization,n4) ),
    inference(unit_resulting_resolution,[],[f910,f1261])).

fof(f71586,plain,(
    ! [X0,X1] : rdn_positive_less(rdnn(X0),rdn(rdnn(X1),rdnn(n7))) ),
    inference(unit_resulting_resolution,[],[f1273,f1245])).

fof(f71594,plain,(
    ! [X0,X1] : rdn_positive_less(rdnn(X0),rdn(rdnn(X1),rdnn(n6))) ),
    inference(unit_resulting_resolution,[],[f1281,f1245])).

fof(f87098,plain,(
    sP2(rdnn(n5),n4) ),
    inference(unit_resulting_resolution,[],[f787,f791,f1269])).

fof(f153300,plain,(
    ! [X0,X1] : rdn_positive_less(rdn(rdnn(X0),rdnn(n7)),rdn(rdnn(X1),rdnn(n8))) ),
    inference(unit_resulting_resolution,[],[f789,f1254])).

fof(f153308,plain,(
    ! [X0,X1] : rdn_positive_less(rdn(rdnn(X0),rdnn(n6)),rdn(rdnn(X1),rdnn(n7))) ),
    inference(unit_resulting_resolution,[],[f782,f1254])).

fof(f154388,plain,(
    ~ accept_city(countrybcivilorganization,suffertown) ),
    inference(unit_resulting_resolution,[],[f64028,f634,f64210,f1262])).

fof(f154954,plain,(
    rdn_positive_less(rdnn(n4),rdnn(n6)) ),
    inference(unit_resulting_resolution,[],[f787,f781,f1253])).

fof(f159585,plain,(
    less(n4,n5) ),
    inference(unit_resulting_resolution,[],[f792,f87098,f1270])).

fof(f159591,plain,(
    accept_population(countrybcivilorganization,other,n4) ),
    inference(unit_resulting_resolution,[],[f1535,f159585,f1258])).

fof(f171287,plain,(
    rdn_positive_less(rdnn(n4),rdnn(n7)) ),
    inference(unit_resulting_resolution,[],[f782,f154954,f1253])).

fof(f320337,plain,(
    sP2(rdnn(n7),n4) ),
    inference(unit_resulting_resolution,[],[f791,f171287,f1269])).

fof(f320571,plain,(
    less(n4,n7) ),
    inference(unit_resulting_resolution,[],[f793,f320337,f1270])).

fof(f618193,plain,(
    ! [X0] : sP2(rdn(rdnn(X0),rdnn(n8)),n70) ),
    inference(unit_resulting_resolution,[],[f972,f153300,f1269])).

fof(f632875,plain,(
    less(n70,n85) ),
    inference(unit_resulting_resolution,[],[f987,f618193,f1270])).

fof(f632919,plain,(
    accept_population(countrybcivilorganization,native,n70) ),
    inference(unit_resulting_resolution,[],[f63841,f632875,f1258])).

fof(f654968,plain,(
    ! [X0] : sP2(rdn(rdnn(X0),rdnn(n7)),n65) ),
    inference(unit_resulting_resolution,[],[f999,f153308,f1269])).

fof(f870608,plain,(
    less(n65,n75) ),
    inference(unit_resulting_resolution,[],[f977,f654968,f1270])).

fof(f870678,plain,(
    accept_population(countrybcivilorganization,atheist,n65) ),
    inference(unit_resulting_resolution,[],[f1544,f870608,f1258])).

fof(f1179609,plain,(
    ! [X0] : sP2(rdn(rdnn(X0),rdnn(n7)),n7) ),
    inference(unit_resulting_resolution,[],[f793,f71586,f1269])).

fof(f1232370,plain,(
    ! [X0] : sP2(rdn(rdnn(X0),rdnn(n6)),n7) ),
    inference(unit_resulting_resolution,[],[f793,f71594,f1269])).

fof(f1313139,plain,(
    less(n7,n70) ),
    inference(unit_resulting_resolution,[],[f972,f1179609,f1270])).

fof(f1313165,plain,(
    accept_population(countrybcivilorganization,native,n7) ),
    inference(unit_resulting_resolution,[],[f632919,f1313139,f1258])).

fof(f1319571,plain,(
    accept_population(countrybcivilorganization,native,n4) ),
    inference(unit_resulting_resolution,[],[f320571,f1313165,f1258])).

fof(f1326519,plain,(
    ~ accept_population(countrybcivilorganization,muslim,n7) ),
    inference(unit_resulting_resolution,[],[f154388,f870678,f63898,f159591,f1319571,f1231])).

fof(f1340164,plain,(
    ~ less(n7,n65) ),
    inference(unit_resulting_resolution,[],[f1537,f1326519,f1258])).

fof(f1340497,plain,(
    ~ sP2(rdn(rdnn(n5),rdnn(n6)),n7) ),
    inference(unit_resulting_resolution,[],[f999,f1340164,f1270])).

fof(f1340500,plain,(
    $false ),
    inference(subsumption_resolution,[],[f1340497,f1232370])).
%------------------------------------------------------------------------------
%----ORIGINAL SYSTEM OUTPUT
% 0.00/0.04  % Problem    : AGT006+1 : TPTP v7.1.0. Bugfixed v3.1.0.
% 0.00/0.04  % Command    : vampire --mode casc_sat -t %d %s
% 0.03/0.23  % Computer   : n017.star.cs.uiowa.edu
% 0.03/0.23  % Model      : x86_64 x86_64
% 0.03/0.23  % CPU        : Intel(R) Xeon(R) CPU E5-2609 0 @ 2.40GHz
% 0.03/0.23  % Memory     : 32218.625MB
% 0.03/0.23  % OS         : Linux 3.10.0-693.2.2.el7.x86_64
% 0.03/0.23  % CPULimit   : 300
% 0.03/0.23  % DateTime   : Wed Aug 29 17:15:27 CDT 2018
% 0.03/0.23  % CPUTime    : 
% 0.03/0.28  % ott+11_3_aac=none:afr=on:afp=4000:afq=1.4:amm=off:anc=all:bs=unit_only:bsr=on:bce=on:fde=unused:irw=on:nm=64:newcnf=on:nwc=1:nicw=on:sac=on:sp=reverse_arity:uhcvi=on_31 on theBenchmark
% 4.24/4.48  % Time limit reached!
% 4.24/4.48  % ------------------------------
% 4.24/4.48  % Version: Vampire 4.2.2 (commit 552c234 on 2018-07-02 14:53:33 +0100)
% 4.24/4.48  % Termination reason: Time limit
% 4.24/4.48  % Termination phase: Saturation
% 4.24/4.48  
% 4.24/4.48  % Memory used [KB]: 71640
% 4.24/4.48  % Time elapsed: 4.200 s
% 4.24/4.48  % ------------------------------
% 4.24/4.48  % ------------------------------
% 4.31/4.52  % fmb+10_1_av=off:fmbsr=1.1:newcnf=on_266 on theBenchmark
% 4.31/4.53  TRYING [1]
% 4.31/4.54  TRYING [2]
% 4.39/4.57  TRYING [3]
% 4.45/4.68  TRYING [4]
% 4.74/4.92  TRYING [5]
% 5.20/5.45  TRYING [6]
% 6.03/6.28  TRYING [7]
% 7.25/7.47  TRYING [8]
% 9.05/9.22  TRYING [9]
% 11.76/11.95  TRYING [10]
% 15.77/15.97  TRYING [11]
% 20.54/20.63  TRYING [12]
% 28.15/28.19  TRYING [13]
% 37.95/37.93  TRYING [14]
% 39.25/39.22  % Time limit reached!
% 39.25/39.22  % ------------------------------
% 39.25/39.22  % Version: Vampire 4.2.2 (commit 552c234 on 2018-07-02 14:53:33 +0100)
% 39.25/39.22  % Termination reason: Time limit
% 39.25/39.22  % Termination phase: Finite model building constraint generation
% 39.25/39.22  
% 39.25/39.22  % Memory used [KB]: 792780
% 39.25/39.22  % Time elapsed: 34.700 s
% 39.25/39.22  % ------------------------------
% 39.25/39.22  % ------------------------------
% 39.25/39.28  % ott+11_3:1_afp=4000:afq=2.0:amm=off:anc=none:fsr=off:gs=on:gsem=off:lma=on:nm=64:newcnf=on:nwc=1:updr=off_83 on theBenchmark
% 50.19/50.18  % Time limit reached!
% 50.19/50.18  % ------------------------------
% 50.19/50.18  % Version: Vampire 4.2.2 (commit 552c234 on 2018-07-02 14:53:33 +0100)
% 50.19/50.18  % Termination reason: Time limit
% 50.19/50.18  % Termination phase: Saturation
% 50.19/50.18  
% 50.19/50.18  % Memory used [KB]: 91341
% 50.19/50.18  % Time elapsed: 10.900 s
% 50.19/50.18  % ------------------------------
% 50.19/50.18  % ------------------------------
% 50.28/50.22  % ott+10_128_av=off:bs=on:gsp=input_only:irw=on:lcm=predicate:lma=on:nm=0:nwc=1:sp=occurrence:urr=on:updr=off:uhcvi=on_231 on theBenchmark
% 65.97/65.84  % Refutation found. Thanks to Tanya!
% 65.97/65.84  % SZS status Theorem for theBenchmark
% 65.97/65.84  % SZS output start Proof for theBenchmark
% 65.97/65.84  fof(f1,axiom,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (accept_team(X0,X3,X1,X2) <=> (accept_number(X0,X2) & accept_leader(X0,X3) & accept_city(X0,X1)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_1)).
% 65.97/65.84  fof(f3,axiom,(
% 65.97/65.84    ! [X0,X2,X4,X5] : ((less(X4,X2) & accept_population(X0,X5,X2)) => accept_population(X0,X5,X4))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_3)).
% 65.97/65.84  fof(f4,axiom,(
% 65.97/65.84    ! [X0,X3,X1] : (the_agent_in_all_proposed_teams(X0,X3,X1) => (accept_city(X0,X1) & accept_leader(X0,X3)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a1_4)).
% 65.97/65.84  fof(f8,axiom,(
% 65.97/65.84    ! [X0] : ((accept_population(X0,other,n4) & accept_population(X0,native,n4) & accept_population(X0,muslim,n7) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n65)) <=> accept_city(X0,suffertown))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_1)).
% 65.97/65.84  fof(f14,axiom,(
% 65.97/65.84    ! [X0] : ((accept_population(X0,other,n0) & accept_population(X0,native,n85) & accept_population(X0,muslim,n0) & accept_population(X0,christian,n3) & accept_population(X0,atheist,n12)) <=> accept_city(X0,coastvillage))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_7)).
% 65.97/65.84  fof(f16,axiom,(
% 65.97/65.84    ! [X0] : ((accept_population(X0,other,n0) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n24) & accept_population(X0,atheist,n75)) <=> accept_city(X0,towna))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_9)).
% 65.97/65.84  fof(f19,axiom,(
% 65.97/65.84    ! [X0] : ((accept_population(X0,other,n1) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n78)) <=> accept_city(X0,cityb))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_12)).
% 65.97/65.84  fof(f20,axiom,(
% 65.97/65.84    ! [X0] : ((accept_population(X0,other,n5) & accept_population(X0,native,n0) & accept_population(X0,muslim,n65) & accept_population(X0,christian,n0) & accept_population(X0,atheist,n30)) <=> accept_city(X0,townc))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',a2_13)).
% 65.97/65.84  fof(f38,axiom,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n4)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_18)).
% 65.97/65.84  fof(f94,axiom,(
% 65.97/65.84    accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n5)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_74)).
% 65.97/65.84  fof(f97,axiom,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,townc,n6)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_77)).
% 65.97/65.84  fof(f100,axiom,(
% 65.97/65.84    the_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_80)).
% 65.97/65.84  fof(f181,axiom,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n6)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_161)).
% 65.97/65.84  fof(f229,axiom,(
% 65.97/65.84    the_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',event_209)).
% 65.97/65.84  fof(f274,axiom,(
% 65.97/65.84    rdn_translate(n4,rdn_pos(rdnn(n4)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn4)).
% 65.97/65.84  fof(f275,axiom,(
% 65.97/65.84    rdn_translate(n5,rdn_pos(rdnn(n5)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn5)).
% 65.97/65.84  fof(f277,axiom,(
% 65.97/65.84    rdn_translate(n7,rdn_pos(rdnn(n7)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn7)).
% 65.97/65.84  fof(f335,axiom,(
% 65.97/65.84    rdn_translate(n65,rdn_pos(rdn(rdnn(n5),rdnn(n6))))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn65)).
% 65.97/65.84  fof(f340,axiom,(
% 65.97/65.84    rdn_translate(n70,rdn_pos(rdn(rdnn(n0),rdnn(n7))))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn70)).
% 65.97/65.84  fof(f345,axiom,(
% 65.97/65.84    rdn_translate(n75,rdn_pos(rdn(rdnn(n5),rdnn(n7))))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn75)).
% 65.97/65.84  fof(f355,axiom,(
% 65.97/65.84    rdn_translate(n85,rdn_pos(rdn(rdnn(n5),rdnn(n8))))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn85)).
% 65.97/65.84  fof(f531,axiom,(
% 65.97/65.84    rdn_non_zero_digit(rdnn(n6))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_digit6)).
% 65.97/65.84  fof(f532,axiom,(
% 65.97/65.84    rdn_non_zero_digit(rdnn(n7))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_digit7)).
% 65.97/65.84  fof(f539,axiom,(
% 65.97/65.84    rdn_positive_less(rdnn(n4),rdnn(n5))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less45)).
% 65.97/65.84  fof(f540,axiom,(
% 65.97/65.84    rdn_positive_less(rdnn(n5),rdnn(n6))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less56)).
% 65.97/65.84  fof(f541,axiom,(
% 65.97/65.84    rdn_positive_less(rdnn(n6),rdnn(n7))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less67)).
% 65.97/65.84  fof(f542,axiom,(
% 65.97/65.84    rdn_positive_less(rdnn(n7),rdnn(n8))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less78)).
% 65.97/65.84  fof(f544,axiom,(
% 65.97/65.84    ! [X6,X7,X8] : ((rdn_positive_less(rdnn(X7),rdnn(X8)) & rdn_positive_less(rdnn(X6),rdnn(X7))) => rdn_positive_less(rdnn(X6),rdnn(X8)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less_transitivity)).
% 65.97/65.84  fof(f545,axiom,(
% 65.97/65.84    ! [X9,X10,X11,X12] : (rdn_positive_less(X10,X12) => rdn_positive_less(rdn(rdnn(X9),X10),rdn(rdnn(X11),X12)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_positive_less_multi_digit_high)).
% 65.97/65.84  fof(f547,axiom,(
% 65.97/65.84    ! [X14,X11,X12] : (rdn_non_zero(X12) => rdn_positive_less(rdnn(X14),rdn(rdnn(X11),X12)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_extra_digits_positive_less)).
% 65.97/65.84  fof(f548,axiom,(
% 65.97/65.84    ! [X6] : (rdn_non_zero_digit(rdnn(X6)) => rdn_non_zero(rdnn(X6)))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',rdn_non_zero_by_digit)).
% 65.97/65.84  fof(f550,axiom,(
% 65.97/65.84    ! [X6,X7,X15,X16] : ((rdn_positive_less(X15,X16) & rdn_translate(X7,rdn_pos(X16)) & rdn_translate(X6,rdn_pos(X15))) => less(X6,X7))),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',less_entry_point_pos_pos)).
% 65.97/65.84  fof(f556,conjecture,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4)),
% 65.97/65.84    file('/export/starexec/sandbox2/benchmark/theBenchmark.p',query_6)).
% 65.97/65.84  fof(f557,negated_conjecture,(
% 65.97/65.84    ~accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4)),
% 65.97/65.84    inference(negated_conjecture,[],[f556])).
% 65.97/65.84  fof(f558,plain,(
% 65.97/65.84    ~accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4)),
% 65.97/65.84    inference(flattening,[],[f557])).
% 65.97/65.84  fof(f559,plain,(
% 65.97/65.84    ! [X0] : (rdn_non_zero_digit(rdnn(X0)) => rdn_non_zero(rdnn(X0)))),
% 65.97/65.84    inference(rectify,[],[f548])).
% 65.97/65.84  fof(f564,plain,(
% 65.97/65.84    ! [X0,X1,X2] : (rdn_non_zero(X2) => rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2)))),
% 65.97/65.84    inference(rectify,[],[f547])).
% 65.97/65.84  fof(f566,plain,(
% 65.97/65.84    ! [X0,X1,X2] : (the_agent_in_all_proposed_teams(X0,X1,X2) => (accept_city(X0,X2) & accept_leader(X0,X1)))),
% 65.97/65.84    inference(rectify,[],[f4])).
% 65.97/65.84  fof(f571,plain,(
% 65.97/65.84    ! [X0,X1,X2] : ((rdn_positive_less(rdnn(X1),rdnn(X2)) & rdn_positive_less(rdnn(X0),rdnn(X1))) => rdn_positive_less(rdnn(X0),rdnn(X2)))),
% 65.97/65.84    inference(rectify,[],[f544])).
% 65.97/65.84  fof(f572,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (rdn_positive_less(X1,X3) => rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3)))),
% 65.97/65.84    inference(rectify,[],[f545])).
% 65.97/65.84  fof(f574,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : ((rdn_positive_less(X2,X3) & rdn_translate(X1,rdn_pos(X3)) & rdn_translate(X0,rdn_pos(X2))) => less(X0,X1))),
% 65.97/65.84    inference(rectify,[],[f550])).
% 65.97/65.84  fof(f576,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : ((less(X2,X1) & accept_population(X0,X3,X1)) => accept_population(X0,X3,X2))),
% 65.97/65.84    inference(rectify,[],[f3])).
% 65.97/65.84  fof(f577,plain,(
% 65.97/65.84    ! [X0] : (rdn_non_zero(rdnn(X0)) | ~rdn_non_zero_digit(rdnn(X0)))),
% 65.97/65.84    inference(ennf_transformation,[],[f559])).
% 65.97/65.84  fof(f580,plain,(
% 65.97/65.84    ! [X0,X1,X2] : (rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2)) | ~rdn_non_zero(X2))),
% 65.97/65.84    inference(ennf_transformation,[],[f564])).
% 65.97/65.84  fof(f583,plain,(
% 65.97/65.84    ! [X0,X1,X2] : ((accept_city(X0,X2) & accept_leader(X0,X1)) | ~the_agent_in_all_proposed_teams(X0,X1,X2))),
% 65.97/65.84    inference(ennf_transformation,[],[f566])).
% 65.97/65.84  fof(f591,plain,(
% 65.97/65.84    ! [X0,X1,X2] : (rdn_positive_less(rdnn(X0),rdnn(X2)) | (~rdn_positive_less(rdnn(X1),rdnn(X2)) | ~rdn_positive_less(rdnn(X0),rdnn(X1))))),
% 65.97/65.84    inference(ennf_transformation,[],[f571])).
% 65.97/65.84  fof(f592,plain,(
% 65.97/65.84    ! [X0,X1,X2] : (rdn_positive_less(rdnn(X0),rdnn(X2)) | ~rdn_positive_less(rdnn(X1),rdnn(X2)) | ~rdn_positive_less(rdnn(X0),rdnn(X1)))),
% 65.97/65.84    inference(flattening,[],[f591])).
% 65.97/65.84  fof(f593,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3)) | ~rdn_positive_less(X1,X3))),
% 65.97/65.84    inference(ennf_transformation,[],[f572])).
% 65.97/65.84  fof(f596,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (less(X0,X1) | (~rdn_positive_less(X2,X3) | ~rdn_translate(X1,rdn_pos(X3)) | ~rdn_translate(X0,rdn_pos(X2))))),
% 65.97/65.84    inference(ennf_transformation,[],[f574])).
% 65.97/65.84  fof(f597,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (less(X0,X1) | ~rdn_positive_less(X2,X3) | ~rdn_translate(X1,rdn_pos(X3)) | ~rdn_translate(X0,rdn_pos(X2)))),
% 65.97/65.84    inference(flattening,[],[f596])).
% 65.97/65.84  fof(f600,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (accept_population(X0,X3,X2) | (~less(X2,X1) | ~accept_population(X0,X3,X1)))),
% 65.97/65.84    inference(ennf_transformation,[],[f576])).
% 65.97/65.84  fof(f601,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : (accept_population(X0,X3,X2) | ~less(X2,X1) | ~accept_population(X0,X3,X1))),
% 65.97/65.84    inference(flattening,[],[f600])).
% 65.97/65.84  fof(f602,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n5) & accept_population(X0,native,n0) & accept_population(X0,muslim,n65) & accept_population(X0,christian,n0) & accept_population(X0,atheist,n30)) | ~accept_city(X0,townc)) & (accept_city(X0,townc) | (~accept_population(X0,other,n5) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n65) | ~accept_population(X0,christian,n0) | ~accept_population(X0,atheist,n30))))),
% 65.97/65.84    inference(nnf_transformation,[],[f20])).
% 65.97/65.84  fof(f603,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n5) & accept_population(X0,native,n0) & accept_population(X0,muslim,n65) & accept_population(X0,christian,n0) & accept_population(X0,atheist,n30)) | ~accept_city(X0,townc)) & (accept_city(X0,townc) | ~accept_population(X0,other,n5) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n65) | ~accept_population(X0,christian,n0) | ~accept_population(X0,atheist,n30)))),
% 65.97/65.84    inference(flattening,[],[f602])).
% 65.97/65.84  fof(f604,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n1) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n78)) | ~accept_city(X0,cityb)) & (accept_city(X0,cityb) | (~accept_population(X0,other,n1) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n1) | ~accept_population(X0,christian,n20) | ~accept_population(X0,atheist,n78))))),
% 65.97/65.84    inference(nnf_transformation,[],[f19])).
% 65.97/65.84  fof(f605,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n1) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n78)) | ~accept_city(X0,cityb)) & (accept_city(X0,cityb) | ~accept_population(X0,other,n1) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n1) | ~accept_population(X0,christian,n20) | ~accept_population(X0,atheist,n78)))),
% 65.97/65.84    inference(flattening,[],[f604])).
% 65.97/65.84  fof(f610,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n0) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n24) & accept_population(X0,atheist,n75)) | ~accept_city(X0,towna)) & (accept_city(X0,towna) | (~accept_population(X0,other,n0) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n1) | ~accept_population(X0,christian,n24) | ~accept_population(X0,atheist,n75))))),
% 65.97/65.84    inference(nnf_transformation,[],[f16])).
% 65.97/65.84  fof(f611,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n0) & accept_population(X0,native,n0) & accept_population(X0,muslim,n1) & accept_population(X0,christian,n24) & accept_population(X0,atheist,n75)) | ~accept_city(X0,towna)) & (accept_city(X0,towna) | ~accept_population(X0,other,n0) | ~accept_population(X0,native,n0) | ~accept_population(X0,muslim,n1) | ~accept_population(X0,christian,n24) | ~accept_population(X0,atheist,n75)))),
% 65.97/65.84    inference(flattening,[],[f610])).
% 65.97/65.84  fof(f620,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n0) & accept_population(X0,native,n85) & accept_population(X0,muslim,n0) & accept_population(X0,christian,n3) & accept_population(X0,atheist,n12)) | ~accept_city(X0,coastvillage)) & (accept_city(X0,coastvillage) | (~accept_population(X0,other,n0) | ~accept_population(X0,native,n85) | ~accept_population(X0,muslim,n0) | ~accept_population(X0,christian,n3) | ~accept_population(X0,atheist,n12))))),
% 65.97/65.84    inference(nnf_transformation,[],[f14])).
% 65.97/65.84  fof(f621,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n0) & accept_population(X0,native,n85) & accept_population(X0,muslim,n0) & accept_population(X0,christian,n3) & accept_population(X0,atheist,n12)) | ~accept_city(X0,coastvillage)) & (accept_city(X0,coastvillage) | ~accept_population(X0,other,n0) | ~accept_population(X0,native,n85) | ~accept_population(X0,muslim,n0) | ~accept_population(X0,christian,n3) | ~accept_population(X0,atheist,n12)))),
% 65.97/65.84    inference(flattening,[],[f620])).
% 65.97/65.84  fof(f626,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n4) & accept_population(X0,native,n4) & accept_population(X0,muslim,n7) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n65)) | ~accept_city(X0,suffertown)) & (accept_city(X0,suffertown) | (~accept_population(X0,other,n4) | ~accept_population(X0,native,n4) | ~accept_population(X0,muslim,n7) | ~accept_population(X0,christian,n20) | ~accept_population(X0,atheist,n65))))),
% 65.97/65.84    inference(nnf_transformation,[],[f8])).
% 65.97/65.84  fof(f627,plain,(
% 65.97/65.84    ! [X0] : (((accept_population(X0,other,n4) & accept_population(X0,native,n4) & accept_population(X0,muslim,n7) & accept_population(X0,christian,n20) & accept_population(X0,atheist,n65)) | ~accept_city(X0,suffertown)) & (accept_city(X0,suffertown) | ~accept_population(X0,other,n4) | ~accept_population(X0,native,n4) | ~accept_population(X0,muslim,n7) | ~accept_population(X0,christian,n20) | ~accept_population(X0,atheist,n65)))),
% 65.97/65.84    inference(flattening,[],[f626])).
% 65.97/65.84  fof(f632,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : ((accept_team(X0,X3,X1,X2) | (~accept_number(X0,X2) | ~accept_leader(X0,X3) | ~accept_city(X0,X1))) & ((accept_number(X0,X2) & accept_leader(X0,X3) & accept_city(X0,X1)) | ~accept_team(X0,X3,X1,X2)))),
% 65.97/65.84    inference(nnf_transformation,[],[f1])).
% 65.97/65.84  fof(f633,plain,(
% 65.97/65.84    ! [X0,X1,X2,X3] : ((accept_team(X0,X3,X1,X2) | ~accept_number(X0,X2) | ~accept_leader(X0,X3) | ~accept_city(X0,X1)) & ((accept_number(X0,X2) & accept_leader(X0,X3) & accept_city(X0,X1)) | ~accept_team(X0,X3,X1,X2)))),
% 65.97/65.84    inference(flattening,[],[f632])).
% 65.97/65.84  fof(f634,plain,(
% 65.97/65.84    ~accept_team(countrybcivilorganization,countrybhumanitarianorganization,suffertown,n4)),
% 65.97/65.84    inference(cnf_transformation,[],[f558])).
% 65.97/65.84  fof(f639,plain,(
% 65.97/65.84    rdn_non_zero_digit(rdnn(n6))),
% 65.97/65.84    inference(cnf_transformation,[],[f531])).
% 65.97/65.84  fof(f640,plain,(
% 65.97/65.84    rdn_non_zero_digit(rdnn(n7))),
% 65.97/65.84    inference(cnf_transformation,[],[f532])).
% 65.97/65.84  fof(f698,plain,(
% 65.97/65.84    the_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc)),
% 65.97/65.84    inference(cnf_transformation,[],[f100])).
% 65.97/65.84  fof(f710,plain,(
% 65.97/65.84    the_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna)),
% 65.97/65.84    inference(cnf_transformation,[],[f229])).
% 65.97/65.84  fof(f781,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n5),rdnn(n6))),
% 65.97/65.84    inference(cnf_transformation,[],[f540])).
% 65.97/65.84  fof(f782,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n6),rdnn(n7))),
% 65.97/65.84    inference(cnf_transformation,[],[f541])).
% 65.97/65.84  fof(f787,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n4),rdnn(n5))),
% 65.97/65.84    inference(cnf_transformation,[],[f539])).
% 65.97/65.84  fof(f789,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n7),rdnn(n8))),
% 65.97/65.84    inference(cnf_transformation,[],[f542])).
% 65.97/65.84  fof(f791,plain,(
% 65.97/65.84    rdn_translate(n4,rdn_pos(rdnn(n4)))),
% 65.97/65.84    inference(cnf_transformation,[],[f274])).
% 65.97/65.84  fof(f792,plain,(
% 65.97/65.84    rdn_translate(n5,rdn_pos(rdnn(n5)))),
% 65.97/65.84    inference(cnf_transformation,[],[f275])).
% 65.97/65.84  fof(f793,plain,(
% 65.97/65.84    rdn_translate(n7,rdn_pos(rdnn(n7)))),
% 65.97/65.84    inference(cnf_transformation,[],[f277])).
% 65.97/65.84  fof(f814,plain,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n6)),
% 65.97/65.84    inference(cnf_transformation,[],[f181])).
% 65.97/65.84  fof(f877,plain,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,townc,n6)),
% 65.97/65.84    inference(cnf_transformation,[],[f97])).
% 65.97/65.84  fof(f885,plain,(
% 65.97/65.84    accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n5)),
% 65.97/65.84    inference(cnf_transformation,[],[f94])).
% 65.97/65.84  fof(f910,plain,(
% 65.97/65.84    accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n4)),
% 65.97/65.84    inference(cnf_transformation,[],[f38])).
% 65.97/65.84  fof(f972,plain,(
% 65.97/65.84    rdn_translate(n70,rdn_pos(rdn(rdnn(n0),rdnn(n7))))),
% 65.97/65.84    inference(cnf_transformation,[],[f340])).
% 65.97/65.84  fof(f977,plain,(
% 65.97/65.84    rdn_translate(n75,rdn_pos(rdn(rdnn(n5),rdnn(n7))))),
% 65.97/65.84    inference(cnf_transformation,[],[f345])).
% 65.97/65.84  fof(f987,plain,(
% 65.97/65.84    rdn_translate(n85,rdn_pos(rdn(rdnn(n5),rdnn(n8))))),
% 65.97/65.84    inference(cnf_transformation,[],[f355])).
% 65.97/65.84  fof(f999,plain,(
% 65.97/65.84    rdn_translate(n65,rdn_pos(rdn(rdnn(n5),rdnn(n6))))),
% 65.97/65.84    inference(cnf_transformation,[],[f335])).
% 65.97/65.84  fof(f1158,plain,(
% 65.97/65.84    ( ! [X0] : (~rdn_non_zero_digit(rdnn(X0)) | rdn_non_zero(rdnn(X0))) )),
% 65.97/65.84    inference(cnf_transformation,[],[f577])).
% 65.97/65.84  fof(f1162,plain,(
% 65.97/65.84    ( ! [X0] : (accept_population(X0,muslim,n65) | ~accept_city(X0,townc)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f603])).
% 65.97/65.84  fof(f1164,plain,(
% 65.97/65.84    ( ! [X0] : (accept_population(X0,other,n5) | ~accept_city(X0,townc)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f603])).
% 65.97/65.84  fof(f1167,plain,(
% 65.97/65.84    ( ! [X0] : (accept_population(X0,christian,n20) | ~accept_city(X0,cityb)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f605])).
% 65.97/65.84  fof(f1184,plain,(
% 65.97/65.84    ( ! [X0] : (accept_population(X0,atheist,n75) | ~accept_city(X0,towna)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f611])).
% 65.97/65.84  fof(f1217,plain,(
% 65.97/65.84    ( ! [X0] : (accept_population(X0,native,n85) | ~accept_city(X0,coastvillage)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f621])).
% 65.97/65.84  fof(f1231,plain,(
% 65.97/65.84    ( ! [X0] : (~accept_population(X0,other,n4) | accept_city(X0,suffertown) | ~accept_population(X0,native,n4) | ~accept_population(X0,muslim,n7) | ~accept_population(X0,christian,n20) | ~accept_population(X0,atheist,n65)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f627])).
% 65.97/65.84  fof(f1245,plain,(
% 65.97/65.84    ( ! [X2,X0,X1] : (~rdn_non_zero(X2) | rdn_positive_less(rdnn(X0),rdn(rdnn(X1),X2))) )),
% 65.97/65.84    inference(cnf_transformation,[],[f580])).
% 65.97/65.84  fof(f1248,plain,(
% 65.97/65.84    ( ! [X2,X0,X1] : (~the_agent_in_all_proposed_teams(X0,X1,X2) | accept_city(X0,X2)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f583])).
% 65.97/65.84  fof(f1253,plain,(
% 65.97/65.84    ( ! [X2,X0,X1] : (~rdn_positive_less(rdnn(X1),rdnn(X2)) | rdn_positive_less(rdnn(X0),rdnn(X2)) | ~rdn_positive_less(rdnn(X0),rdnn(X1))) )),
% 65.97/65.84    inference(cnf_transformation,[],[f592])).
% 65.97/65.84  fof(f1254,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (rdn_positive_less(rdn(rdnn(X0),X1),rdn(rdnn(X2),X3)) | ~rdn_positive_less(X1,X3)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f593])).
% 65.97/65.84  fof(f1256,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (less(X0,X1) | ~rdn_positive_less(X2,X3) | ~rdn_translate(X1,rdn_pos(X3)) | ~rdn_translate(X0,rdn_pos(X2))) )),
% 65.97/65.84    inference(cnf_transformation,[],[f597])).
% 65.97/65.84  fof(f1258,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (~accept_population(X0,X3,X1) | ~less(X2,X1) | accept_population(X0,X3,X2)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f601])).
% 65.97/65.84  fof(f1259,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (~accept_team(X0,X3,X1,X2) | accept_city(X0,X1)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f633])).
% 65.97/65.84  fof(f1260,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (~accept_team(X0,X3,X1,X2) | accept_leader(X0,X3)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f633])).
% 65.97/65.84  fof(f1261,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (~accept_team(X0,X3,X1,X2) | accept_number(X0,X2)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f633])).
% 65.97/65.84  fof(f1262,plain,(
% 65.97/65.84    ( ! [X2,X0,X3,X1] : (~accept_number(X0,X2) | accept_team(X0,X3,X1,X2) | ~accept_leader(X0,X3) | ~accept_city(X0,X1)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f633])).
% 65.97/65.84  fof(f1269,plain,(
% 65.97/65.84    ( ! [X2,X0,X3] : (~rdn_translate(X0,rdn_pos(X2)) | ~rdn_positive_less(X2,X3) | sP2(X3,X0)) )),
% 65.97/65.84    inference(cnf_transformation,[],[f1269_D])).
% 65.97/65.84  fof(f1269_D,plain,(
% 65.97/65.84    ( ! [X0,X3] : (( ! [X2] : (~rdn_translate(X0,rdn_pos(X2)) | ~rdn_positive_less(X2,X3)) ) <=> ~sP2(X3,X0)) )),
% 65.97/65.84    introduced(general_splitting_component_introduction,[new_symbols(naming,[sP2])])).
% 65.97/65.84  fof(f1270,plain,(
% 65.97/65.84    ( ! [X0,X3,X1] : (~rdn_translate(X1,rdn_pos(X3)) | less(X0,X1) | ~sP2(X3,X0)) )),
% 65.97/65.84    inference(general_splitting,[],[f1256,f1269_D])).
% 65.97/65.84  fof(f1273,plain,(
% 65.97/65.84    rdn_non_zero(rdnn(n7))),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f640,f1158])).
% 65.97/65.84  fof(f1281,plain,(
% 65.97/65.84    rdn_non_zero(rdnn(n6))),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f639,f1158])).
% 65.97/65.84  fof(f1421,plain,(
% 65.97/65.84    accept_city(countrybcivilorganization,townc)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f698,f1248])).
% 65.97/65.84  fof(f1423,plain,(
% 65.97/65.84    accept_city(countrybcivilorganization,towna)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f710,f1248])).
% 65.97/65.84  fof(f1535,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,other,n5)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1421,f1164])).
% 65.97/65.84  fof(f1537,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,muslim,n65)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1421,f1162])).
% 65.97/65.84  fof(f1544,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,atheist,n75)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1423,f1184])).
% 65.97/65.84  fof(f63624,plain,(
% 65.97/65.84    accept_city(countrybcivilorganization,coastvillage)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f885,f1259])).
% 65.97/65.84  fof(f63679,plain,(
% 65.97/65.84    accept_city(countrybcivilorganization,cityb)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f814,f1259])).
% 65.97/65.84  fof(f63841,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,native,n85)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f63624,f1217])).
% 65.97/65.84  fof(f63898,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,christian,n20)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f63679,f1167])).
% 65.97/65.84  fof(f64028,plain,(
% 65.97/65.84    accept_leader(countrybcivilorganization,countrybhumanitarianorganization)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f877,f1260])).
% 65.97/65.84  fof(f64210,plain,(
% 65.97/65.84    accept_number(countrybcivilorganization,n4)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f910,f1261])).
% 65.97/65.84  fof(f71586,plain,(
% 65.97/65.84    ( ! [X0,X1] : (rdn_positive_less(rdnn(X0),rdn(rdnn(X1),rdnn(n7)))) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1273,f1245])).
% 65.97/65.84  fof(f71594,plain,(
% 65.97/65.84    ( ! [X0,X1] : (rdn_positive_less(rdnn(X0),rdn(rdnn(X1),rdnn(n6)))) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1281,f1245])).
% 65.97/65.84  fof(f87098,plain,(
% 65.97/65.84    sP2(rdnn(n5),n4)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f787,f791,f1269])).
% 65.97/65.84  fof(f153300,plain,(
% 65.97/65.84    ( ! [X0,X1] : (rdn_positive_less(rdn(rdnn(X0),rdnn(n7)),rdn(rdnn(X1),rdnn(n8)))) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f789,f1254])).
% 65.97/65.84  fof(f153308,plain,(
% 65.97/65.84    ( ! [X0,X1] : (rdn_positive_less(rdn(rdnn(X0),rdnn(n6)),rdn(rdnn(X1),rdnn(n7)))) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f782,f1254])).
% 65.97/65.84  fof(f154388,plain,(
% 65.97/65.84    ~accept_city(countrybcivilorganization,suffertown)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f64028,f634,f64210,f1262])).
% 65.97/65.84  fof(f154954,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n4),rdnn(n6))),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f787,f781,f1253])).
% 65.97/65.84  fof(f159585,plain,(
% 65.97/65.84    less(n4,n5)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f792,f87098,f1270])).
% 65.97/65.84  fof(f159591,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,other,n4)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1535,f159585,f1258])).
% 65.97/65.84  fof(f171287,plain,(
% 65.97/65.84    rdn_positive_less(rdnn(n4),rdnn(n7))),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f782,f154954,f1253])).
% 65.97/65.84  fof(f320337,plain,(
% 65.97/65.84    sP2(rdnn(n7),n4)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f791,f171287,f1269])).
% 65.97/65.84  fof(f320571,plain,(
% 65.97/65.84    less(n4,n7)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f793,f320337,f1270])).
% 65.97/65.84  fof(f618193,plain,(
% 65.97/65.84    ( ! [X0] : (sP2(rdn(rdnn(X0),rdnn(n8)),n70)) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f972,f153300,f1269])).
% 65.97/65.84  fof(f632875,plain,(
% 65.97/65.84    less(n70,n85)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f987,f618193,f1270])).
% 65.97/65.84  fof(f632919,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,native,n70)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f63841,f632875,f1258])).
% 65.97/65.84  fof(f654968,plain,(
% 65.97/65.84    ( ! [X0] : (sP2(rdn(rdnn(X0),rdnn(n7)),n65)) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f999,f153308,f1269])).
% 65.97/65.84  fof(f870608,plain,(
% 65.97/65.84    less(n65,n75)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f977,f654968,f1270])).
% 65.97/65.84  fof(f870678,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,atheist,n65)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1544,f870608,f1258])).
% 65.97/65.84  fof(f1179609,plain,(
% 65.97/65.84    ( ! [X0] : (sP2(rdn(rdnn(X0),rdnn(n7)),n7)) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f793,f71586,f1269])).
% 65.97/65.84  fof(f1232370,plain,(
% 65.97/65.84    ( ! [X0] : (sP2(rdn(rdnn(X0),rdnn(n6)),n7)) )),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f793,f71594,f1269])).
% 65.97/65.84  fof(f1313139,plain,(
% 65.97/65.84    less(n7,n70)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f972,f1179609,f1270])).
% 65.97/65.84  fof(f1313165,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,native,n7)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f632919,f1313139,f1258])).
% 65.97/65.84  fof(f1319571,plain,(
% 65.97/65.84    accept_population(countrybcivilorganization,native,n4)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f320571,f1313165,f1258])).
% 65.97/65.84  fof(f1326519,plain,(
% 65.97/65.84    ~accept_population(countrybcivilorganization,muslim,n7)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f154388,f870678,f63898,f159591,f1319571,f1231])).
% 65.97/65.84  fof(f1340164,plain,(
% 65.97/65.84    ~less(n7,n65)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f1537,f1326519,f1258])).
% 65.97/65.84  fof(f1340497,plain,(
% 65.97/65.84    ~sP2(rdn(rdnn(n5),rdnn(n6)),n7)),
% 65.97/65.84    inference(unit_resulting_resolution,[],[f999,f1340164,f1270])).
% 65.97/65.84  fof(f1340500,plain,(
% 65.97/65.84    $false),
% 65.97/65.84    inference(subsumption_resolution,[],[f1340497,f1232370])).
% 65.97/65.84  % SZS output end Proof for theBenchmark
% 65.97/65.84  % ------------------------------
% 65.97/65.84  % Version: Vampire 4.2.2 (commit 552c234 on 2018-07-02 14:53:33 +0100)
% 65.97/65.84  % Termination reason: Refutation
% 65.97/65.84  
% 65.97/65.84  % Memory used [KB]: 173600
% 65.97/65.84  % Time elapsed: 15.616 s
% 65.97/65.84  % ------------------------------
% 65.97/65.84  % ------------------------------
% 65.97/65.85  % Success in time 65.618 s
%------------------------------------------------------------------------------

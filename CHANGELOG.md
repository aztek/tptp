# Revision history for tptp

## 0.1.2.0 -- 2020-04-03

* Consume leading whitespace in the TSTP parser.

* Do not pretty print parenthesis in negated equality literals.

* Parse superfluous parenthesis in types, terms and clauses.

* Add a helper function 'unitClause'.

* Add more tests and improve documentation.

* Set more accurate lower bounds of the base and scientific dependencies.

* Support compilation with GHC 8.8.3 and 8.10.1.

## 0.1.1.0 -- 2019-12-07

* Parse SZS ontology information in the TSTP input.

* Parse single line comments starting with #.

* Increase the upper bound of the prettyprinter dependency.

* Support compilation with GHC 8.8.1.

## 0.1.0.3 -- 2019-06-11

* Support compilation with GHC 7.8.

## 0.1.0.0 -- 2019-05-07

* First version. Released on an unsuspecting world.

* Supported TPTP languages: CNF, FOF, TFF0, TFF1.

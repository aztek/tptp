
# Proof found!
# SZS status Theorem
# SZS output start CNFRefutation
fof(0, conjecture, mortal(socrates), file('<stdin>', 0)).
fof(1, axiom, ![X1]:(human(X1)=>mortal(X1)), file('<stdin>', 1)).
fof(2, axiom, human(socrates), file('<stdin>', 2)).
fof(c_0_3, negated_conjecture, ~(mortal(socrates)), inference(assume_negation,[status(cth)],[0])).
fof(c_0_4, plain, ![X2]:(~human(X2)|mortal(X2)), inference(variable_rename,[status(thm)],[inference(fof_nnf,[status(thm)],[1])])).
fof(c_0_5, negated_conjecture, ~mortal(socrates), inference(fof_simplification,[status(thm)],[c_0_3])).
cnf(c_0_6, plain, (mortal(X1)|~human(X1)), inference(split_conjunct,[status(thm)],[c_0_4])).
cnf(c_0_7, plain, (human(socrates)), inference(split_conjunct,[status(thm)],[2])).
cnf(c_0_8, negated_conjecture, (~mortal(socrates)), inference(split_conjunct,[status(thm)],[c_0_5])).
cnf(c_0_9, plain, (mortal(socrates)), inference(pm,[status(thm)],[c_0_6, c_0_7])).
cnf(c_0_10, negated_conjecture, ($false), inference(cn,[status(thm)],[inference(rw,[status(thm)],[c_0_8, c_0_9])]), ['proof']).
# SZS output end CNFRefutation
# Training examples: 0 positive, 0 negative


# Proof found!
# SZS status Theorem
# SZS output start CNFRefutation
fof(3, axiom, ![X3, X1, X2]:mult(mult(X3,X1),X2)=mult(X3,mult(X1,X2)), file('<stdin>', 3)).
fof(4, axiom, ![X2]:mult(X2,X2)=e, file('<stdin>', 4)).
fof(1, axiom, ![X2]:mult(e,X2)=X2, file('<stdin>', 1)).
fof(0, conjecture, ![X1, X2]:mult(X1,X2)=mult(X2,X1), file('<stdin>', 0)).
fof(c_0_4, plain, ![X8, X9, X10]:mult(mult(X8,X9),X10)=mult(X8,mult(X9,X10)), inference(variable_rename,[status(thm)],[3])).
fof(c_0_5, plain, ![X11]:mult(X11,X11)=e, inference(variable_rename,[status(thm)],[4])).
fof(c_0_6, plain, ![X6]:mult(e,X6)=X6, inference(variable_rename,[status(thm)],[1])).
cnf(c_0_7, plain, (mult(mult(X1,X2),X3)=mult(X1,mult(X2,X3))), inference(split_conjunct,[status(thm)],[c_0_4])).
cnf(c_0_8, plain, (mult(X1,X1)=e), inference(split_conjunct,[status(thm)],[c_0_5])).
cnf(c_0_9, plain, (mult(e,X1)=X1), inference(split_conjunct,[status(thm)],[c_0_6])).
cnf(c_0_10, plain, (mult(X1,mult(X1,X2))=X2), inference(rw,[status(thm)],[inference(pm,[status(thm)],[c_0_7, c_0_8]), c_0_9])).
fof(c_0_11, negated_conjecture, ~(![X1, X2]:mult(X1,X2)=mult(X2,X1)), inference(assume_negation,[status(cth)],[0])).
cnf(c_0_12, plain, (mult(X1,mult(X2,mult(X1,X2)))=e), inference(pm,[status(thm)],[c_0_8, c_0_7])).
cnf(c_0_13, plain, (mult(X1,e)=X1), inference(pm,[status(thm)],[c_0_10, c_0_8])).
fof(c_0_14, negated_conjecture, mult(esk1_0,esk2_0)!=mult(esk2_0,esk1_0), inference(skolemize,[status(esa)],[inference(variable_rename,[status(thm)],[inference(fof_nnf,[status(thm)],[c_0_11])])])).
cnf(c_0_15, plain, (mult(X1,mult(X2,X1))=X2), inference(rw,[status(thm)],[inference(pm,[status(thm)],[c_0_10, c_0_12]), c_0_13])).
cnf(c_0_16, negated_conjecture, (mult(esk1_0,esk2_0)!=mult(esk2_0,esk1_0)), inference(split_conjunct,[status(thm)],[c_0_14])).
cnf(c_0_17, plain, (mult(X1,X2)=mult(X2,X1)), inference(pm,[status(thm)],[c_0_10, c_0_15])).
cnf(c_0_18, negated_conjecture, ($false), inference(cn,[status(thm)],[inference(rw,[status(thm)],[c_0_16, c_0_17])]), ['proof']).
# SZS output end CNFRefutation
# Training examples: 0 positive, 0 negative

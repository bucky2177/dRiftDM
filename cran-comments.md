## R CMD check results

0 errors | 0 warnings | 1 note

* unable to verify current time.


## Resubmission

In this version (0.2.2) I have made small changes to address reviewer 
requests of JSS:

* written print() and summary() methods for several object types.

* deprecated unpack_traces() and introduced the more general S3 method 
unpack_obj(). 

* introduced the S3 method pdfs().

* added an argument to coef.drift_dm().

* renamed the class list_stats_dm to stats_dm_list for name consistency.

* added a couple of attributes to traces_dm that I required for appropriate 
print and summary methods.

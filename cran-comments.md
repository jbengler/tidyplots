## Resubmission
This is a resubmission. Regarding the issues raised:

* There are no methods-related references to include.

* I added \value to grapes-greater-than-grapes.Rd

* I removed if(FALSE) and adapted the examples in save_plot.Rd

* I wrapped some save_plot.Rd examples in \donttest{} because they were taking slightly over 5 seconds.

## R CMD check results

0 errors | 0 warnings | 1 note

❯ checking for future file timestamps ... NOTE
  unable to verify current time
  
This appears to be an issue with the clock server, and not with this package.
https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time

* This is a new release.

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a minor release.

Minor updates, +addresses previous CRAN check issues (see Note 2.)

Note 1.: 'unable to verify current time' is likely a bug in 'check':  https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time

Note 2.: 'new submission, package was archived on CRAN' (archived: https://www.r-project.org/nosvn/R.check/r-release-linux-x86_64/rineq-00check.html  due to problems in documentaiton syntax and breaking change in the 'survival' package) 

Additional comment: RMD check fails with legacy versions of the 'survival' package (e.g. with github actions only the ubuntu-devel build passes, but not the release builds)

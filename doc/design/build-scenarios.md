build scenarios
===============

This document describes a bunch of _tricky_ build scenarios that we want to test using scoutess.



build from darcs
----------------

Build the lastest version of package X from darcs.



test lower bounds
-----------------

Build package X, but select the lower bounds for all build dependencies.



build against latest versions
-----------------------------

Test that build succeeds if only the most recent versions of every package are available.



rebuild only affected packages
------------------------------

When rebuilding a large list of packages, ensure that packages are only rebuilt if their build-dependencies have changed since the last build.


haskell platform
----------------

Test that package(s) build against several stock versions of the Haskell platform.

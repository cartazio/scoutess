build scenarios
===============

This document describes a bunch of normal and _tricky_ build scenarios that we want to test using scoutess.



build package X from darcs against hackage
------------------------------------------

Build the lastest version of package X from darcs.


 1. find the location of the darcs package.
 2. get out or update the source darcs source code (we may have a previous version checked out), recording any changes
 3. assign/calculate the augmented .cabal version number (cabal + extra stuff to distinguish between different darcs revisions)
 4. check if version is in local hackage already
 5. if not, run cabal sdist on package and add .tar.gz to local hackage
 6. update the hackage package list (we probably want to only do this once per run, but only if the run requires this information)
 7. calculate the transitive closure of build-dependencies that would be used to build the package
 8. compare that list to the previous run (if any) and make a build decision (yes if changes, no if none)
 9. build the package and create a build report

Notes:

The process of updating all our source things (getting things from darcs, updating the hackage package list), should probably only be done once per run, even if there are a bunch of different packages being built? How would that affect parallel builds?

build packages X and Y from darcs against hackage where X and Y do not depend on each other
-------------------------------------------------------------------------------------------


build packages X and Y from darcs against hackage where X depends on Y
----------------------------------------------------------------------


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


incremental building
--------------------

Test building the project after *every single commit*, and not only after all the commits pushed since the last update. if one makes local commits A,B,C and push them to the repo, we want individual builds for {A}, {A,B}, {A,B,C}.

did we break packages that rely on our project
----------------------------------------------

We may want to try to build (all / some of the) reverse deps of our project to check that we haven't broken them with the changes we made to our project.
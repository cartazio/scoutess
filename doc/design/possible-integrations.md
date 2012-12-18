Possible integrations
=====================

This document describes things which would go well with scoutress and which can enhance the workflow and usefulness of scoutress, but which will not be implemented in scoutress.

Which features belong here
--------------------------

There are several reasons to not implement features in scoutress. Some of them are:
* keeping scoutress lean. concerns which are not central to scoutress do not belong in scoutress.
* giving features a life of their own. A feature can grow and progress better if people can commit to it without having to commit to the whole of scoutress. This means things like adhering to coding standards and team policy, but it also means that it's much easier to commit to your own repository than it is to pull-merge-commit-push to someone else's project - the administrative overhead is much lower with small projects.
* making the features more healthy, by letting them grow and learn from their mistakes on their own, rather than piggyback on top of a central project and become an indistinguishable speck on the map. If a feature is successful, others can use it too. If a feature dies off, it will not create dead code in scoutress.

How to identify a feature which does not belong in scoutress, list (partial):
* can the feature make sense on its own, outside of scoutress? If yes, then making it part of scoutress you are doing yourself and the users a disservice. Examples: a web front-end to managing your source code and viewing its documentation.
* can the feature be implemented completely separate of scoutress? If yes, then there's no reason for it to be part of scoutress. Example: source code linting (HSLint, ghc-mod), building itself (cabal, cabal-dev)
* does the feature operate only moderately on some data internal to scoutress? You probably want to implement your feature as a stand-alone with arguments or command-line switches. Example: if you want scoutress to deploy your built binaries, you can just make it run a deployment script after build.
* does the feature operate mostly on data internal to scoutress? Consider asking scoutress for an API, or document export, which you can parse. Example: parsing scoutress logs to get email notifications of build errors.

But other build servers do X!
-----------------------------

A lot of build servers were made in the IDE Boom era. They have been made into huge, kludgy mudslides of code that try to check every box possible. Computational power increase in general-purpose systems that happened in the 90s gave developers too much power, and so they ended up building Huge Things. Since the 90s, a lot has changed in the popular mindset about how to develop software. Modularity and interchangeability, lightweight apis and composability are some lessons that were hard to learn. If you look at your favourite build server, chances are that features that are internal to it belong in here - as separate tools that are external to scoutress. Use conventions like file system structures rather than explicit passing around of parameters to make the interaction layer between scoutress and your tool easier. Use loose-coupling communication like socket files, message passing (rabbitmq comes to mind), document databases, rather than things which rely on scoutress being installed in the first place. Example: don't make your tool depend on some SQL table which is installed and written to by scoutress only; make it optional.


List of ideas
=============

Figure out qualified imports
----------------------------

13:18 < neutrino> you mention that scoutess can do automatic bounds discovery. it would be nice if it could also automatically turn unqualified imports into qualified ones. that is probably a completely separateble tool.
13:18 < neutrino> "can do" when it's done that is
13:22 < alpounet> you're maybe pushing it a bit too far, that's a bit out of scoutess' scope :P
13:22 < alpounet> stylish-haskell and whatnot could do these stuffs
13:27 < donri> hm i thought so too at first, but there may be a point to having scoutess do it before releasing
13:27 < donri> like, it could use -ddump-minimal-imports when testing builds, and then for a working build it rewrites open imports to use known-working explicit imports
13:28 < donri> that way we get to write open imports in the code, but for hackage installations people will be guaranteed to get the same imports scoutess got (which worked)
13:29 < alpounet> hm.
13:30 < neutrino> yeah, that's why i mention it's probably a completely separate tool
13:30 < alpounet> if someone does write a tool that does exactly that, we certainly can offer an option in the config to do that, that would just call out to that program
13:30 < neutrino> so: it is something that could integrate into scoutess, but not something that has to be provided by scoutess
13:30 < donri> yea

Test builds on preconfigured virtual machines
---------------------------------------------

Have several images predefined, and have scoutress run inside that vm and see if things build. This way you can always see if your production servers will be able to build from scratch and/or build from their current checkpoint.

Have scoutress deploy code to production via continuous deployment
------------------------------------------------------------------

Scoutress builds the program, and then runs a script which queues up the deployment with some sort of continuous deployment daemon.


Have scoutress build debs (or rpms or msi's) of my program
----------------------------------------------------------

Scoutress builds a package, but builds it under a chroot or checkinstall or something similar, in order to obtain a binary package that can be installed on any other computer.

Have scoutress report build failures via email, sms (phone texting), twitter, or a bot on a channel
---------------------------------------------------------------------------------------------------

Scoutress builds a package, then sends the log and build status to a bot which does the rest.


Have scoutress bisect builds to figure out when they started breaking
---------------------------------------------------------------------

Scoutress builds a package and it fails. It notifies an external daemon about this. This daemon looks at the build parameters, and looks at the known-good builds of a package, and then gives scoutress commands to build the same package with different build parameters (including revision id of the source being built). Scoutress records the validity of every build in some database anyways, and the daemon uses this information to perform further bisection steps. Once the daemon has found the breakage boundary, it is done bisecting. The database is retained by scoutress and now it knows when something started breaking.

Have scoutress cache Hackage
----------------------------

Hackage is down every now and then, and when it's down it is gone for a day or so. If I need to do an urgent rollout, and can't access the packages, that's disastrous for my business. Scoutress should include a cache of all the packages which are required. Actually not sure if this one belongs in a separate tool, or if it's something that should make it into scoutress "after the first release".

Have scoutress cache Hackage Haddocks
-------------------------------------

Hackage is down every now and then, and when it's down it is gone for a day or so. If I have to make an urgent bug fix, and can't access the documentation, that's terrible. Scoutress could very well be able to notify a daemon of what packages I am currently using, and in what versions, and this daemon could, among others, make sure I have a server somewhere with the haddocks available.

# Online ATPs [![Build Status](https://travis-ci.org/jonaprieto/online-atps.svg?branch=master)](https://travis-ci.org/jonaprieto/online-atps)

  Online ATPs is a client for
  [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP)
  that allows us to take advantage of using
  [ATPs](http://www.cs.miami.edu/~tptp/OverviewOfATP.html) without install any of them.

#### Requirement

Install the last version of `cabal` tool.

````bash
$ cabal update
$ cabal install cabal-install
````

#### Installation

````bash
$ git clone https://github.com/jonaprieto/online-atps.git
$ cd online-atps
$ cabal install
````

#### Usage

* See all ATPs available running the command

````bash
$ online-atps --list-atps
````

* Check the `help` command to see all options of `online-atps`:

````bash
$ online-atps --help
Usage: online-atps [OPTIONS] FILE

    --atp=NAME          Set the ATP (online-e, online-vampire, online-z3, ...)
    --fof               Only use ATP for FOF
    --help              Show this help
    --list-atps         Consult all ATPs available in TPTP World
    --only-check        Only checks the output looking for a theorem.
    --time=NUM          Set timeout for the ATPs in seconds (default: 300)
    --version           Show version number
    --version-atp=NAME  Show version of the atp NAME
    --
````

* Prove a small conjecture (in TSTP format) like this:

````bash
$ cat basic.tptp
fof(a1,axiom,a).
fof(a2,axiom,b).
fof(a3,axiom, (a & b) => z).
fof(a4,conjecture,z).

````


Using the ATP [Vampire](http://www.vprover.org):

```
$ online-atps basic.tptp --atp=vampire
% SZS start RequiredInformation
% Congratulations - you have become a registered power user of SystemOnTPTP,
at IP address 138.121.12.14.
% Please consider donating to the TPTP project - see www.tptp.org for
details.
% When you donate this message will disappear.
% If you do not donate a random delay might be added to your processing time.
% SZS end RequiredInformation
Vampire---4.1   system information being retrieved
Vampire---4.1's non-default parameters being retrieved
    -t none
    -f tptp:raw
    -x vampire --mode casc -m 90000 -t %d %s
Vampire---4.1   being checked for execution
Vampire---4.1   checking time limit 240
Vampire---4.1   checking problem name /tmp/SystemOnTPTPFormReply38743/
SOT_Xry401
...
% ------------------------------
% Version: Vampire 4.1 for CASC J8 Entry
% Termination reason: Refutation

% Memory used [KB]: 511
% Time elapsed: 0.043 s
% ------------------------------
% ------------------------------
% Success in time 0.045 s

% END OF SYSTEM OUTPUT
RESULT: SOT_Xry401 - Vampire---4.1 says Theorem - CPU = 0.00 WC = 0.04
OUTPUT: SOT_Xry401 - Vampire---4.1 says Refutation - CPU = 0.00 WC = 0.04

```

* Online ATPs accepts a name for a ATP using the prefix "online-" or not (e.g "vampire" or "online-vampire").

#### Contributions

Check the issues. Thanks.

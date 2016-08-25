# OnlineATPs

  OnlineATPs connects to
  [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP)
  and allows us to take advantage of using ATPs without install any of them.

#### Installation

  Use `cabal` to install OnlineATPs.

  ```bash
    $ git clone https://github.com/jonaprieto/onlineatps.git
    $ cd onlineatps
    $ cabal install
  ```

#### Usage

  We should start checking the help:

  ```
  $  onlineatps --help
  Usage: onlineatps [OPTIONS] FILE

      --atp=NAME          Set the ATP (online-e, online-vampire, online-z3, ...)
      --fof               Only use ATP for FOF
      --help              Show this help
      --list-atps         Consult all ATPs available in TPTP World
      --only-check        Only checks the output looking for a theorem.
      --time=NUM          Set timeout for the ATPs in seconds (default: 300)
      --version           Show version number
      --version-atp=NAME  Show version of the ATP with name equals NAME
  ```

  Then, we can prove something like:

  ```
  $ cat basic.tptp
  fof(a1,axiom,a).
  fof(a2,axiom,b).
  fof(a3,axiom, (a & b) => z).
  fof(a4,conjecture,z).
  ```
  with an ATP like [Vampire](http://www.vprover.org):

  ```
  $ onlineatps examples/basic.tptp --atp=vampire
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

  Note that we can specify the ATP with both names, "vampire" or just
  "online-vampire".

#### YAML Configuration

  For use a YAML file configuration, use the name `. onlineatps`.
  Check the first example in `examples/.onlineatps`. Nowadays, only the form
  data that the program sends to SystemOnTPTP can change using this file. It is
  missing the parameters of the command line.

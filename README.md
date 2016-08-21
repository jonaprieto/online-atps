# OnlineATPs

#### A web interface for the ATPs.

This haskell package aim to be a new feature for the [Apia](https://github.com/asr/apia)
program.

> The TPTP World offers an excellent web interface for many ATPs.
This interface is called [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP).
[This module will provide the integration of this service with the Apia program,
which would avoid the user needs to install the ATPs used in your proofs.]
(http://www1.eafit.edu.co/asr/research-proposals.html#apia-web-interface-for-the-atps)

#### Goals:

  - [x] Parsing GET request of [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP)
  - [x] Get the list of all online ATPs available on SystemOnTPTP
  - [x] Filter the list of online ATPs: gather only ones that focus on FOF
  - [x] Provide a full data type for the form on SystemOnTPTP
  - [x] Send POST resquest to the TPTP World.
  - [x] Check the result of a response. Issue [#4](https://github.com/jonaprieto/OnlineATPs/issues/4)
  - [x] Test all atps against a problem
  - [ ] Get the first *positive* result if exists fromt the test all atps with a problem
  - [x] Test a fileproblem in TPTP formal. Issue [#5](https://github.com/jonaprieto/OnlineATPs/issues/5)
  - [x] Make a first version of the excutable version. Issue [#7](https://github.com/jonaprieto/OnlineATPs/issues/5)


#### Installation

As a normal haskell package, we use `cabal` to install OnlineATPs.

```bash
  $ git clone https://github.com/jonaprieto/onlineatps.git
  $ cd onlineatps
  $ cabal install
```

#### Usage

A basic example of usage should be like:

```bash
  $ cd onlineatps/examples
  $ onlineatps basic.tptp --atp=vampire
  Theorem
```
Please check the help command `onlineatps --help`.


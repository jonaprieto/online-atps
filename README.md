# OnlineATPs

#### A web interface for the ATPs.

A client to connect to [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP).

#### Installation

Use `cabal` to install OnlineATPs.

```bash
  $ git clone https://github.com/jonaprieto/onlineatps.git
  $ cd onlineatps
  $ cabal install
```

#### Usage

We should start for check the help information:

```bash
$  onlineatps --help
Usage: onlineatps [OPTIONS] FILE

    --atp=NAME          Set the ATP (online-e, online-vampire, online-z3, ...)
    --fof               Only use ATP for FOF
    --help              Show this help
    --list-atps         Consult all ATPs available in TPTP World
    --only-check        Only checks the output looking for a theorem.
    --time=NUM          Set timeout for the ATPs in seconds (default: 300)
    --version           Show version number
    --version-atp=NAME  Show version of the atp NAME
```

A basic example of usage should be like:

```bash
  $ cd onlineatps/examples
  $ onlineatps basic.tptp --atp=vampire
  Theorem
```
Note that we can specify the ATP with both names, "vampire" or just "online-vampire".

#### YAML Configuration

For use yaml file configurations, use the name `.onlineatps`.
Check the first example in `examples/.onlineatps`. Nowadays, only the form data
that the program sends to SystemOnTPTP can change using this file. It is
missing the parameters of the command line.

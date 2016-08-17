# OnlineATPs
#### A web interface for the ATPs

This module is the plan for a new feature for the [Apia](https://github.com/asr/apia) program.

> The TPTP World offers an excellent web interface for many ATPs. This interface is called [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP). [This module will provide the integration of this service with the Apia program, which would avoid the user needs to install the ATPs used in your proofs.](http://www1.eafit.edu.co/asr/research-proposals.html#apia-web-interface-for-the-atps)

#### Goals:

  - [x] Parsing GET request to [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP)
  - [x] Get the list of all online ATPs available on SystemOnTPTP
  - [x] Filter the list of online ATPs: gather only ones that focus on FOF
  - [x] Provide a full data type for the form on SystemOnTPTP
  - [x] Send POST resquest to [SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP)
  - [ ] Check the result of a response. Issue [#4](https://github.com/jonaprieto/OnlineATPs/issues/4)
  - [ ] Test all atps against a problem
  - [ ] Use SystemOnTPTP with file uploads. Issue [#5](https://github.com/jonaprieto/OnlineATPs/issues/5)

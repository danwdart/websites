title: "Building a POS: the development process"
author: "Dan Dart"
date: 2018/08/11

I've been interested in building a POS for a while. It seems to me that POS systems crash or break a lot, and I've wanted to see how well such a system I could make. I also thought I could take an opportunity to go through some of the thought process I would go through if I were making such a system, for those who have not got into programming or systems design just yet.

But before any kinds of programming, as we don't just jump straight into something where we don't know what we're doing (what a colossal waste of time that would be), we first go through a process that helps us decide what we want:

* What does it do?
- Perhaps the most obvious one, but if the functionality of a system is not thought through to completion, then fragmentation, non-obvious bugs, irritation or ending up with a system not fit for the purpose of its intention will all get in the way.
* How does it do it?
- Surprisingly, not necessarily obvious for people with not much experience in development. If you aren't sure what the client wants, and just start juggling around with ideas or functionality, or delve into the wrong thing first, this is a big time-waster, and there is more likelihood that more of the work will end up being thrown away. Know what you want to do before you do it, but it might be useful to provide several designs for an indecisive client. Even go so far as to produce a small amount of pseudocode (interpretable by the client) to explain how your designs will function in the real application.
* Who's it for?
- Perhaps a strange question at first, but very relevant once you consider the ways of working of different people, as it will help you design your systems for the right person. After all, you wouldn't want a programmer's interface to a system you'll show a shop assistant, nor would you want funny lights and sounds unless you were designing for babies.
* Do they like your designs?
- Before you do any development, consult with your target audience and market to find the right fit. If people are used to working in a certain way, they will be confused if presented with a foreign way of doing things, which will markedly hurt their productivity.
* How long will it last?
- I've barely ever heard this question answered, but if a development timescale, even if vague, is not decided upon, proper time will not be allocated to technical debt, such as redesigning flow, integrating client ideas, discussing improvements, bugs, upgrades and even rewrites to avoid maintaining old code, allowing more developers from the next generation of paradigm shifts to be happy working on your project in five years, avoiding undue stress to all parties, and all such plans should be out in the open to ensure everybody who's involved knows about any potential delays,


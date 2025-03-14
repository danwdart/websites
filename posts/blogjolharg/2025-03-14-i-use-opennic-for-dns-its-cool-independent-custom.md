---
title: "I use OpenNIC for DNS, it's cool, independent and custom"
date: 2025-03-14T18:30:00Z
draft: false
aliases: [ "/2025/03/i-use-opennic-for-dns-its-cool-independent-custom" ]
tags: [opennic,networking,dns,cool,custom,independent]
---

Ever wish you could register domain names for free, with cool TLDs? Well... you can, in a private, democratic community  namespace.

> So what is it?

It's a set of community-maintained DNS servers, some of which run with DoH, DoT, DNSSEC and claim to not log, etc.

> What's so good about that? Doesn't everybody do that? What's the niche, then?

The niche here is a set of custom, OpenNIC-only TLDs you can register and maintain for free, which is kind of neat.

> What about TLS, though? How do you get certificates for it? Surely if other DNS providers don't understand 

Well, what you can do is that you can use custom certificates. Of course, this means you'll need to do one of two things:

1. Create and self-sign your own certificate
    This means that everybody will have to trust you and pass through the yellow screens when visiting your website.
2. Create and run your own certificate authority on your own computer
    This means that you, and only you, will not have to face the yellow screens, and it's kind of useful. It still protects against man-in-the-middle attacks, because you'll have to click through otherwise signed certificates.
3. [Import a "special" root certificate from the wiki](https://wiki.opennic.org/opennic/tls), then run ACME against their server, specifically.
    This means you can share the authority between users of the authority, and that you'll have to trust the authority, but otherwise no yellow screens.

> Nice. Anything else you can do with it?

There's always volunteering to run a DNS server or providing translations if you're multilingual.

> That's pretty cool. How do I get started?

Simply visit [their website](https://opennic.org/). You'll find information about what DNS servers to add that are closest to you, and you can decide what kind of domains you wish to register. There are some pretty gnarly ones available.

Ta-ta!
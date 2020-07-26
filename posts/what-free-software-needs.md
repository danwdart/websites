---
title: 'What Free Software needs'
date: 2009-07-05T14:21:00.000+01:00
draft: false
aliases: [ "/2009/07/what-free-software-needs.html" ]
tags : [standardisation, bsd. competition, windows, free, linux, packages]
---

When I was young, I remember wanting SUSE 9.2 Professional. It seemed like a good stable system with many good reviews. Afterwards (luckily) the distribution switched to GPL and I managed to acquire a copy of 9.3. It was very good for its time, its acheivements vastly outstepping anything I had previously seen. With instant search, good photo management, and all the rest, it seemed to be a good stepping stone onto which further development could be put upon.  
  
A while later, I find that "cool" features seem to be getting less and less common. With the advent of Compiz a few years ago, coolness in the desktop rose a little, but with less common other features, and small incremental updates in most distributions, computing was getting a little more boring, with little to wow about. We now need a good jump up, or proprietary software will catch up. KDE 4 recently has been a downfall, mainly because people disliked it from being so very different to the very stable and mature 3.5 series, which I was in fact excited about at the time. When the "broken" Microsoft Vista followed after KDE 4's (premature) release, people managed to give Vista bad hype for being so out of step with current needs that it would not run software that ran on previous releases flawlessly. Between now and then, KDE 4 and Vista have largely sorted out their concerns. Ever since Vista SP1 and KDE 4.2, I think a lot more people are happy with either release. But many people still dislike this new "dark" theme over the previous light theme, and as such prefer to stick to the "dead" XP or the less-supported KDE 3.5.  
  
Other problems we in the Free Software community face are:  
  
Lack of standardisation. Yes, as much as I hate to say it, standard ways of doing things are waning. Especially in the Linux community, 500 different distributions are not a good way of doing things. Factoring out the "useless" distributions, based on whether they have been done before, how useful a distribution is, whether the same effect could be copied painlessly in another distribution, I think maybe 50 might remain.  
  
Lack of standard package formats. As much as I still hate to say it, all Linux distros need one defining package format. Right now it is considered too difficult to develop for Linux, as there are so many formats to develop for. DEB, RPM, RUN, Autopackage, TAR.GZ, TGZ, and others make it difficult to develop for. I think we need a standardised package format and standard repositories that all distros can pull from. Having different ones means that it is currently difficult and long-winded to document how to install software on all current distributions. Here is what I think we need:  
  
One format. One format, one download link for Linux, one way of packaging. Easy, simple.  
  
One repository. One website serving download links for every conceivable package, in an installable static format (including every library it requires) or dynamic (for short downloads).  
  
Every application. There are far too many repositories. There are in excess of 50 for Ubuntu and openSUSE. Why can't they just all be in the same place? Of course, to keep freedom-lovers happy, split it into free and non-free but essentially it's easier to get what you want now.  
  
Every proprietary game or application maker can now package their game or application into ONE single format, upload it to ONE server (if necessary) or ONE CD/DVD/USB, and allow use or play to EVERY free software user.  
  
Standard Libraries. GNOME and KDE are in pretty much fair competition. I cannot dispute or argue against it. Choice is paramount, but applications that don't work are unacceptable. If there were a library that could be used to create desktop applications that would run fairly on each, and not look foreign on one or the other, then it should be developed upon by everyone trying to provide a fair experience. People don't always have the libraries that are needed by some obscure piece of software, so they should be readily available, or the application should use something more common.  
  
Backward/Forward Compatibility.  
Proprietary module or "driver" creation is impossible in Linux. If a hardware manufacturer wishes to hide the functionality of their driver, they cannot release binary-only drivers in Linux currently. I know that manufacturers should be encouraged to develop freely, but if there is no chance of this, there is no chance of that hardware working on Linux. The license makes it difficult but I believe that if a manufacturer provides one of those "Driver CDs" in that standard package format above with Module Versioning support on in the kernel, drivers do not have to wear out.  
  
The kernel seems to not like modules from a past or future kernel, mainly because it is not at all stable, but also because Module Versioning does not work by default in most distros by default. Looking at Windows, applications and drivers from a number of years ago will work in today's release, and (in general) releases before the release of the application or driver. We need this back-forward compatibility for proprietary software vendors (who can't be convinced to switch to free) not to have to either release their code or keep compiling their code for each kernel or new release.  
  
A hopeful fix  
I'm in the process of creating a browser-based desktop environment that will hopefully overcome all that, and allow for major cool features as well as ultra compatibility and ease of use for new users. It isn't Linux, or anything to do with current free software but it can lie on top of Linux/Solaris/BSD/Windows/Mac/whatever if the user so wishes.  
[Xenon](http://xenon.kevinghadyani.com)
---
title: 'BSD for Linux Users: An Introduction'
date: 2015-03-29T15:05:00.003+01:00
draft: false
aliases: [ "/2015/03/bsd-for-linux-users-introduction.html" ]
tags: [dragonfly, netbsd, openbsd, zfs, cddl, freebsd, bsd, open, dragonflybsd, free, linux, gnu, debian, pcbsd, sco, unix, licence]
---

  
BSD means a few things in the Open Source / Operating System world:  

1.  The **Berkeley System Distribution**, a variant of UNIX that stemmed from the original AT&T UNIX, originally developed by Computer Systems Research Group (CSRG) of the University of California, Berkeley [\[1\]](http://en.wikipedia.org/wiki/Berkeley_Software_Distribution)
2.  One or many of a number of **BSD distributions** - a "flavour" of the original, modified by the vendor to suit the purpose of the distribution in question. Examples might be **FreeBSD**, **NetBSD**, **OpenBSD** and **Dragonfly BSD**. Note that these are not "Unix-like" as Linux is, but actually based from and including code from the original BSD - minus the code from AT&T, hence the version they are based upon is known as "4.4BSD-Lite".
3.  The actual **kernel** of one of these distributions, in the same way as Linux is the main kernel of distributions such as Ubuntu, Fedora or Mint, although there are other choices of kernel for some distributions.
4.  One or many **BSD communities** around the world to support and help develop these distributions, such as forums, help desks, etc.
5.  A set of **permissive non-copyleft licences** which said distributions and kernels are distributed under [\[2\]](http://opensource.org/licenses/BSD-2-Clause)[\[3\]](http://opensource.org/licenses/BSD-3-Clause) which allow redistribution provided that the original copyright notices are left in the associated media, and pose no other restrictions other than an optional "no endorsement" clause.

Following is a summary of some of the main distributions.  
  
**FreeBSD**: the most popular BSD distribution, recognised by its support for running servers and famously run on [\[4\]](http://www.freebsd.org/)  
  
**PC-BSD**: touted as "user-friendly", offering an easy install process and simple package installations from self-contained packages. [\[5\]](http://www.pcbsd.org/)  
  
**OpenBSD**: supposedly the most secure operating system, boasting "Only two remote holes in the default install, in a heck of a long time!" [\[6\]](http://openbsd.org/)  
  
**NetBSD**: a distribution with a small install size, a popular base and excelling at portability with "formal releases for 53 architectures [\[7\]](http://www.netbsd.org/about/portability.html), and has integrated ports for four others", celebrating 20 years since its foundation this year.  
  
**Dragonfly BSD**: a 10-year-old (so still relatively young) distribution famous for its extremely speedy filesystems. [\[8\]](http://www.dragonflybsd.org/features/)  
  
**Debian GNU/kFreeBSD** [\[9\]](http://www.debian.org/ports/kfreebsd-gnu/), the Debian distribution compiled to work on top of a BSD-type kernel rather than a Linux one, which has the upshots of being able to be used on top of BSD-supported filesystems, those compatible with BSD licences (but not the GPLv2 [\[10\]](http://www.gnu.org/licenses/gpl-2.0.html) used by Linux [\[11\]](https://www.kernel.org/pub/linux/kernel/COPYING)) such as ZFS [\[12\]](https://www.freebsd.org/doc/handbook/filesystems-zfs.html) (licenced under the CDDL [\[13\]](http://opensource.org/licenses/CDDL-1.0)), and all the while using the familiar GNU tools common to the Debian GNU/Linux distribution.  
  
Next time: BSD Licences and why they are good, bad and/or certainly not ugly.  
  
  
References  
  
\[1\] [http://en.wikipedia.org/wiki/Berkeley\_Software\_Distribution](http://en.wikipedia.org/wiki/Berkeley_Software_Distribution)  
\[2\] [http://opensource.org/licenses/BSD-2-Clause](http://opensource.org/licenses/BSD-2-Clause)  
\[3\] [http://opensource.org/licenses/BSD-3-Clause](http://opensource.org/licenses/BSD-2-Clause)  
\[4\] [http://www.freebsd.org/](http://www.freebsd.org/)  
\[5\] [http://www.pcbsd.org/](http://www.pcbsd.org/)  
\[6\] [http://openbsd.org/](http://openbsd.org/)  
\[7\] [http://www.netbsd.org/about/portability.html](http://www.netbsd.org/about/portability.html)  
\[8\] [http://www.dragonflybsd.org/features/](http://www.dragonflybsd.org/features/)  
\[9\] [http://www.debian.org/ports/kfreebsd-gnu/](http://www.debian.org/ports/kfreebsd-gnu/)  
\[10\] [http://www.gnu.org/licenses/gpl-2.0.html](http://www.gnu.org/licenses/gpl-2.0.html)  
\[11\] [https://www.kernel.org/pub/linux/kernel/COPYING](https://www.kernel.org/pub/linux/kernel/COPYING)  
\[12\] [https://www.freebsd.org/doc/handbook/filesystems-zfs.html](https://www.freebsd.org/doc/handbook/filesystems-zfs.html)  
\[13\] [http://opensource.org/licenses/CDDL-1.0](http://opensource.org/licenses/CDDL-1.0)
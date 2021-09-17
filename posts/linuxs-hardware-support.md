---
title: 'Linux''s Hardware Support'
date: 2010-01-24T14:52:00.000Z
draft: false
aliases: [ "/2010/01/linuxs-hardware-support.html" ]
tags: [ubuntu, 64 bit, hardware, intel, windows, linux, core, usb, i7, opensuse, support]
---

Lately, I've been hearing a lot about "Linux needs to master .... to beat Windows". I'll now show you how that's completely false, and how it already has beaten it, by talking about hardware support.  
  
Linux has been proved to have the best hardware support around - see [this interview](http://howsoftwareisbuilt.com/2009/11/18/interview-with-greg-kroah-hartman-linux-kernel-devmaintainer/#hardware) with Greg KH who's a kernel dev to see in-depth information. Linux had most support for hardware first, including:  
\* 64 bit  
\* USB 3.0  
\* Core i7  
  
And many more.  
Conversely, it's easy to install the hardware on Linux. In windows for instance, half the time your hardware doesn't work because you downloaded a dodgy driver, or you have to install it off a CD, or it could even be the case that it bluescreens because the driver hasn't been verified by whoever. Fair enough, that hardly ever happens anymore.  
  
The misconception that a lot of hardware doesn't work on Linux isn't because it doesn't, it does, but because quite often your distribution of choice doesn't ship with the correct userspace tools - e.g. a webcam viewer, a scanning program, an iPod syncer. It's not the actual Linux kernel that's at fault here, it's that the distribution vendors don't include software to manage and access your device. What we need here is a project that either includes everything or says "I see you've inserted a scanner, but you don't have a scanning application. Want me to install one for you?". I have seen openSUSE do this for me before, but Ubuntu sadly lacks this capability, which is the distro that most users allegedly use, so it needs it here.  
  
The fact of the matter is, every piece of hardware I've put into my Linux box has been detected and set up by Linux, but I have had to install a webcam viewer, scanning application and TV viewer. Perhaps it's time for userspace tools to improve themselves and be as good as the kernel.
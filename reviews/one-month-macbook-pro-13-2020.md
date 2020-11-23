---
title: "One Month with Apple's MacBook Pro 13 2020: A Hacker's Standpoint"
date: 2020-11-18T23:29:00Z
draft: false
aliases: [ "/2020/11/one-month-macbook-pro-13-2020.html" ]
featuredImage: https://support.apple.com/library/APPLE/APPLECARE_ALLGEOS/SP818/sp818-mbp13touch-space-select-202005.png
tags : [macbook, pro, review]
scores :
    - Hackability: 3/10
    - Fun: 6/10
    - Value for money: 3/10
    - Overall: 5/10
---

From mid-September to mid-October 2020, I had the perhaps-fortune-perhaps-misfortune of using the [MacBook Pro 13" 2020](https://support.apple.com/kb/SP819?locale=en_GB) (codenamed MacBookPro16,2).
It ran mac OS 10.15 Catalina, and I was also able to install Windows 10 via Boot Camp quite easily.

However it did not handle Linux - almost at all.

## Linux support found lacking

People had been working on its new chipset, trying to reverse-engineer it, since its release, but sadly, support for this model's keyboard was lacking in both the standard upstream kernel and the special ["MacBook" kernel](https://aur.archlinux.org/packages/linux-macbook/) for Arch Linux, my latest choice.

However, Apple had done nothing about supporting its hardware in Linux (and as far as I can tell, directly to date).

In fact, to boot anything different, you have to turn "System Integrity Protection" off, which is toggled from a command-line app in a rescue environment, which is its equivalent of UEFI Secure Boot.

Once you have a prepared UEFI USB stick, such as that of Arch Linux (my preferred distribution)

All this made maintaining a free/open source software stack extrenely difficult.

## The Touch Bar

I actually found Apple's new offering, the Touch Bar, very useful. I often reached for it when I wanted to turn up and down the volume, and for basic dialogs and VoIP applications, it came in useful by having the default buttons of whatever I was using on it to the left.

## Applications on mac OS

To start with, especially for the GUI applications I use, one still has to download these manually from potentially unknown Internet sites, without the benefit of a repository. Only later on can you use something like [Homebrew](https://brew.sh/), which gives you the advantage of centralised updates. The packages you install manually don't show up in Homebrew and are as such not updated with the rest of the system, nor does Homebrew update any other part of the deeper system, so it's more of a "stand-in" situation at the moment. Being used to operating systems which package every system file under a repository and can update the entire system with one chosen interface of many to choose from, this is very awkward, as I have to remember to go to many places (Homebrew itself, Apple updater, each non-Homebrew application, and that's if they have thought about adding automatic updates, which doesn't always happen).

### Alarm Clock

Having the App Store isn't always better, it only supports a few of the applications I would want to have, and often adds ads into "free" apps that I want installed. If I want proper open source software written for its system, I often have to go to (maybe outdated) websites and grab, for instance, an alarm clock called [Pester](https://sabi.net/nriley/software/), as the system doesn't come with a clock with an alarm as far as I could see. Being used to KAlarm, this left a lot to be desired yet again, with it often failing on me or telling me that it's going to be going off in a few billion years.

The actual machine suffers from quite the inability to close its lid without it from sleeping, without having to use [some sort of command line utility](https://computers.tutsplus.com/tutorials/quick-tip-how-to-stop-your-mac-from-sleeping-using-the-command-line--mac-50905) which is not always reliable, especially when the system is running on batteries. I wasn't able to get it to keep playing music to my headphones on the go, and wasn't even able to when it was plugged in. Compare this with any operating system that runs KDE or Gnome, and you'll find it sadly lacking.

## Being able to run Docker

Thankfully, a project called Docker Desktop allowed me to run almost all of my Linux applications straight from mac OS. It even included X.org forwarding support, so XQuartz (mac OS's builtin X.org server) would handle most things I wished to throw at it.

## But what about games?

From Catalina onwards, mac OS no longer supports x86_32 applications, and as such, Steam for mac OS does not support very many games in my library at all. No such luck for those who want to game on this hardware-capable device, unless one would like to install Windows 10 via Bootcamp.

## "What's USB, then?"

The MacBook Pro 13" does not feature any USB A ports at all, instead resorting to the "future" of USB devices by only including USB-C ports, which it has upgraded to its own Thunderbolt protocol, but this is backwards-compatible with USB 3.1/USB-C.

The upsides of this were that you could use any port you liked to charge the device from (it came with a 61W USB-C charger), but the downsides are that for most devices still out there, to use for example a bog-standard memory stick, you'd be forced to buy endless amounts of dongles, for USB-C to USB-A conversion.

## Drivers, drivers, drivers...

As still present in Windows 10, Apple's mac OS still has very many driver issues for peripherals, and whilst Windows 10 can find and download drivers for you (most of the time), mac OS doesn't typically do that, and when plugging in particularly old USB devices, there was not a peep from mac OS about whether it actually knew how to deal with it. I often had to open its system info application to even know about the device I had just plugged in, but it wouldn't (where appropriate, such as network devices) show up in the system settings as an option very often. Something I was quite concerned about to hear when I learned that I would have to use a USB Ethernet adapter.

The same goes for its wireless stack. I regularly run penetration testing on my home wireless network with whatever I happen to have around, but it seemed that on this occasion, using mac OS left a lot to be desired. I could not inject packets or incite monitor mode directly - instead having to run an application to capture packets to a file, and not being able to inject at all.

## But would I recommend it?

The lack of native Linux has let this particular machine down, and no support for 32-bit games, since they still exist for mac OS, pretty much breaks mac OS 10.15 for me (I would recommend downgrading if you need this, but this is going to get old very soon, and games will be forced to either catch up or have to run in an emulated environment). The only option I had for running most of my games was running Windows 10 in Boot Camp (although I don't know how long that's going to be supported in newer models - the move to Apple Silicon has the initial impression of making this awkward, but has the promise of making it bearable).

## The sticking (price and performance) points

At £1700, this is not a cheap machine. But what you do get is a Core i5 10th generation (ooh), with 16GB RAM (ahh) and a rather nice 2560x1600 screen, which scales well with what you give it, and scales content like a screen of lesser resolution when the situation requires it. It also features its latest Touch ID - but I'm not sure whether to trust Apple with my fingerprint. Some of these Touch Bar features aren't available when you boot into Windows, but when you do this, you do get a couple of basic brightness and volume controls, which is nice if nothing else.

I found the speakers to be rather nice, actually - they were much better than other laptop speakers I've used, and had a nice bass punch.

## Conclusion

A solidly built machine, though lacking in many areas, especially interoperability. Not a laptop I'd particularly recommend for the lacklustre support it has for games and unofficial operating systems, nor even its own apps in many cases, which it seems to just drop without much notice.
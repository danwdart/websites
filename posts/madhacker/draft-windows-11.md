---
title: "Windows 11"
date: 2022-06-29T10:18:00+01:00
draft: true
aliases: [ "/2022/06/windows-11.html" ]
featuredImage: TODO
tags: [windows, 11]
scores :
    -
        - Hackability
        - 3/10
    -
        - Fun
        - 2/10
    -
        - Value for money
        - 2/10
    -
        - Overall
        - 2/10
---

Recently, I have had the opportunity to virtualise Microsoft's newest offering to the desktop OS scene, "unimaginatively" called Windows 11. I quote this because they have only ever had prerelase codenames and that's not what it's about anyway.

# First steps

## Prerequisites

Downloading the ISO from the distribution site proved easy, but not particularly reproducible unless one goes through ISO-compilation websites, or a specific one for all recent versions of Windows which repackages their distribution bundles[1].

Strangely their offering includes "English (US)" and "English (International)". I don't know who told them that English wasn't invented in the US, nor does English naturally default to a US setting, or why the international options should be grouped together (is it market share? It's probably economising market share).

Booting up in a virtual machine shows that Windows 11 requires a few things that no other operating system absolutely requires, intended to secure the desktop audience, but seemingly to further alienate those who try to test it.

First, it's a hefty install. The VM image had to be over 53 GB!

Secondly, the machine has to be set up to be able to support, but not necessarily to enable, Secure Boot, which anyone who will know if they know me or have seen my presentation on it [2], is something I did not support from its inception. UEFI is all well and good in my eyes (it even allows the machine's logo to be part of the operating system's startup screen which can be nice), but requiring that machines support Secure Boot is definitely a bit much.

Thirdly, a Trusted Platform Module is required to install using the default settings (disabling this requires hacking around quite a lot with Microsoft's version of a config file, the registry, but QEMU can support it, so I went on).

Fourthly, Windows 11 requires extreme amounts of memory just for some basic installation - a whole 4GB just to install it! The machine I'm using is no embedded device, which already has 16GB of memory which is more than enough, and even then, much of this is zramswap, so realistically more is available than this, but that's a bit crazy for an installation program. I might understand if some of it needed to be cached or there was a startup option for "I have lots of spare memory", but I suppose if one is using it on a raw device this is fine, it's just when people try to virtualise it that they run into problems (usually 1GB is enough to virtualise anything on bare default settings without running a bunch of browser tabs).

## On with the installation

Upon booting of the ISO image, I'm faced with a good language picker (I did already select a version with a default system language when I downloaded, but hey ho).

I'm faced with a strange key-licence setting, which it seems like it's invented, letting the user either enter a licence key or pick the option for no licence key, presumably for those who have a digital licence attached to a cloud account of some sort or those who wish to evaluate the system before buying it.

Oh, did I mention? It's probably the only major operating system that charges for its licencing, having a strange nonstandard type of licence upon which the system itself is distributed in a freeware sort of way, but providing nothing to the effect of source code, copyleft or distribution freedom, instead suggesting that actually using the software is subject to a bunch of restriction. Perhaps not the best to distribute to vendors until that game is upped. There's no audit system in place, no way for me to tell what activities I'm doing are actually being tracked (despite the questions later on, I have no proof of this promise). There's also no chance of fixing bugs on local instances, making pull requests, getting community support for alterations, etc. They seem completely blind to the four freedoms, probably much to their detriment.

Immediate points are deducted for having an oddly specific organisation login screen upon setup of their Pro version, with no working Back button prompting a different type of log on, rendering it completely untestable without what I'm sure would be a bunch more registry tweaks. Effectively this means that the operating system is without the Pro features one would expect from any basic operating system that it purports to have, such as file system encryption. I don't know why it would lock certain features behind "editions", but from its sales pitches it seems that it is selling less secure and less compatible products to its home users, which is more than slightly worrying.

Oddly, the install option is the "custom" option, whereas upgrade is "normal". It seems that the system expects there already to be an older or current version of itself already installed, which is a bit presumptuous.

I'm prompted for a confusing "edition" selection, showing me way more options than I expect (this is not even a software options picker!) but not explaining what any of them do or are for. Very counterintuitive seeming, I'm going to have to look these up...

As a support section hidden in the website says [3]:

    The "N" editions of Windows 10 include the same functionality as other editions of Windows 10 except for media-related technologies. The N editions don't include Windows Media Player, Skype, or certain preinstalled media apps (Music, Video, Voice Recorder).

The explanation for this is covered in a third party site [4]:

    These editions of Windows exist entirely for legal reasons. In 2004, the European Commission found Microsoft had violated European antitrust law, abusing its monopoly in the market to hurt competing video and audio applications. The EU fined Microsoft €500 million and required Microsoft to offer a version of Windows without Windows Media Player. Consumers and PC manufacturers can choose this version of Windows and install their preferred multimedia applications without Windows Media Player also being present. It’s not the only version of Windows offered in the European Union—it’s just an option that has to be available. This is why the “N” editions are only available in Europe.

Yikes, antitrust... I'm not liking how this is going. Anyway, back to the installation.

I normally run virtualised operating systems using a virtual IO system called VirtIO. It just so happens that this operating system is one of the few that doesn't include support for the virtualised hardware. I'm so used to operating systems handling hardware really well nowadays, I kind of expected hardware not to be an issue anymore. Okay, so iet's include a support ISO which Red Hat kindly supports...

It doesn't bode well that I'm faced with a completely unfriendly way of installation that I'll need to find my way around, but their driver system is a bit like signed kernel modules, and it's normally expected that only experts would have to deal with this kind of thing...

Install is normally pretty quiet, so it can be difficult to see what the system is actually doing and if it's having any trouble doing so whilst installing. There's no obvious option to increase verbosity, as I would expect from most desktop operating systems.

Big blank screen. Hmm... reset?

The continuation installation messages are friendly but not descriptive in the slightest. Again, they don't tell you what the machine is actually doing, which would be fine for a newbie, but not giving you the option to know seems a bit rough, even suspicious, given all that antitrust stuff earlier.




[1]: https://uupdump.net
[2]: secure boot presentation
[3]: https://support.microsoft.com/en-us/topic/media-feature-pack-for-n-editions-of-windows-10-version-1607-b657cb70-33e7-1f11-7119-3b4b50be4e89
[4]: https://www.howtogeek.com/322112/what-is-an-n-or-kn-edition-of-windows/
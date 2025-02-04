---
title: 'How to create a dual boot USB installer'
date: 2025-02-04T01:25:00Z
draft: false
aliases: [ "/2025/02/how-to-create-dual-boot-usb-installer" ]
tags: [usb,installer,windows,linux,dual,boot,howto,how to]
---

Sometimes you only have one USB device on hand, and it would be handy to be able to create and rescue more than one operating system at a time, e.g. you already have a dual-boot system it would be helpful to run a rescue on, or you want a fast-track way to dual boot!

-- This is potentially scalable to more than one Linux distribution, but I haven't thought about that.

This tutorial is written with generic instructions to work with any operating system, avoiding any platform-specific tool. Screenshots are provided for mine, but you'll see a similar thing if you're not using my exact setup. Use whichever tools your current operating system includes, all you will need is root or administrator access and to be on an operating system that supports multiple partitions on an external USB device, which thankfully is supported for all suitably modern consumer desktop operating systems (you may need to avoid older enterprise versions of Windows though, as we need at least Windows 10 v1703!). If some of my tutorial confuses you, please feel free to send me a comment below, and I will update the post to clarify.

Let's go!

First, let's fetch some ISOs. I'm going with Windows 11 and Mint, for simplicity. -- LINK TO THESE

To switch between them, we'll need a bootloader. Although technically both installers already include one, I particularly like rEFInd, as it's a pretty graphical interface which shouldn't take much to set up. Let's [grab that] too. Needless to say, the default install requires Secure Boot to be turned off in your "BIOS", and whilst there are options to avoid this, they are out of scope for this tutorial.

# Setup

Let's create a GPT disklabel some partitions on our USB device. Then let's make a ESP with 1GB for good measure, an NTFS filesystem at about 5GB for the Windows sources file and an ext4 filesystem at the size of the rest of the disk. Doing this to a loop file might look like this:

[img]

Great!

# Windows

Let's first copy everything from the Windows ISO except the huge sources/install.wim file into the ESP.

[Files image?]

Then let's create a sources directory inside the NTFS directory, and copy install.wim from the Windows ISO into it.

That should be it for Windows!

# Linux

Let's copy the directory for the bootloader from the Linux ISO, "EFI". You'll probably see something similar for whichever operating system you use, assuming you use one new enough.

Let's copy the rest of the files from the Linux ISO to your ext4 partition.

# rEFInd

Finally, 


# Summary

We installed two installers on a USB device, using the following steps:

1. Create a GPT disklabel
1. Create 1GB ESP partition, 5GB NTFS partition and ext4 partition for the rest of the disk.
1. Copy all but sources/install.wim to ESP partition from Windows ISO
1. Copy sources/install.wim from Windows ISO to NTFS partition
1. Copy EFI directory from Linux ISO to ESP partition
1. Copy all but EFI directory from Linux ISO to ext4 partition

title: "How to simply create a USB bootable OS without the need for an image writer"
author: "Dan Dart"
date: 2018/08/02

Since the dawn of UEFI, new computers have been able to boot without a first "boot sector", which MBR typically required. If you have one of these (most Intel-compatibles made since 2010 will have UEFI) you may not need to go low-level to flash a bootable operating system.

Let's make a simple one, and I'll walk you through the steps.

UEFI expects a FAT32 medium on a GPT drive, but sometimes MBR is allowable, so create one with the partition manager of your choice. Usually a USB pen drive will come with one when you buy it.

Often, this partiton has to have a type of "EFI System Partition", but many UEFI implementations ignore that. Use your partition manager of choice to set this partition's type, if this is the case for yours (or just in case).

The file that the UEFI will look for is located in /EFI/BOOT/BOOTX64.EFI by default on 64-bit Intel-compatibles (other filenames are the default for other architectures), but your Setup tool in your UEFI may allow you to choose any arbitrary EFI file to boot from.

The bootable UEFI file is a WinPE64 (for 64-bit Intels, again other formats for other architectures) format file, which can be compiled to from many operating systems and languages. It exposes a library for easy manipulation of filesystems and networking operations for use with developing a bootloader, which will eventually boot a kernel.

A full operating system can be built into an EFI file as well (famously the Linux kernel via EFISTUB), but it usually is not, as it is often easier to use a bootloader to boot into another kernel, as you can then pass custom kernel options, initial ram filesystems, etc, for the use of an OS on another partition entirely. For this tutorial, I'll choose rEFInd as my bootloader EFI file, as it's my current favourite bootloader, and doesn't require so many options upfront as GRUB.

Here's the rEFInd binary I'm using:&nbsp;[https://sourceforge.net/projects/refind/files/0.11.3/refind-bin-0.11.3.zip/download](https://sourceforge.net/projects/refind/files/0.11.3/refind-bin-0.11.3.zip/download). After setup, I'll use this to make the operating system bootable.

Another partition is what is normally used to store the kernel, but there are no rules to say that that is required. We can just as well include the kernel on the same partition, to avoid this. You can build or choose your favourite x64-compatible kernel. I'll pick building Linux, as that's what I'm used to, but you can pick out your own if you like.

Using the regular Linux building methods, I built a minimal Linux kernel image, with built-in modules. Here's mine. ZZZZZZZZZZZZZZZZZZZZZ

For my method, it doesn't require an initial ramdisk, as there are no separate modules or instructions before "proper boot", but there could be with yours, if you choose. Valid reasons would be space, encryption, network drives, etc.

For the operating system data however, most good operating systems require more from a filesystem than what FAT32 can provide, and without emulating this, we can use a filesystem image file to store the data that our partition would have created. We have several choices here, so I'll choose Btrfs, my current favourite filesystem.

Now, what should we include as programs to be run by our operating system?

Common choices are GNU tools, BSD tools or a tiny program called Busybox, which does very well on an embedded system, as it builds in most of the tools that embedded systems might need inside it.

ZZZZZZZZZZZ

Now let's wrap it up into our filesystem image, and install the bootloader to boot the kernel to use it!

Brilliant, we now have our own bootable operating system for our own purpose! We can test it with qemu before booting into it from a real PC (your parameters may be different):





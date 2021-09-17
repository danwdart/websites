---
title: 'Linux Myths Debunked'
date: 2009-09-20T10:33:00.000+01:00
draft: false
aliases: [ "/2009/09/linux-myths-debunked.html" ]
tags: [debunked, security, trojan, virus, myths, windows, linux, operating system]
---

1\. "You can't run games on Linux.".  
This is one that annoys me. People claim that Linux does not provide the necessary gaming requirememnts. But look:  
  
There is a list of Linux games at http://icculus.org/lgfaq/gamelist.php which includes many famous and popular games such as Enemy Territory: Quake Wars, Quake 4 and the Unreal Tournament series. These have either been ported from the originals by independent game companies or originally programmed for Linux (as well as many other operating systems). They often run faster on Linux than Windows as the old ETQW system requirements page showed (required 2.8GHz for Windows, 2.0GHz for Linux). For games that are not supported on Linux, there are API layers (NOT emulators) for Linux that can run Windows programs, often faster than Windows can, due to memory usage. Examples are Wine (free libre/gratis), Cedega (subscription) and CrossOver (subscription). I have successfully run many Valve games on Linux such as Half-Life 2, and many mods of it, using the Windows version of Steam under Wine, and they ran great. Also check out http://en.wikipedia.org/wiki/List\_of\_open\_source\_video\_games for many more cross platform free games.  
  
2\. "Linux has bad security".  
Anyone who knows security will surely agree with me here. It is in fact widely known that Windows has viruses, trojans, worms, malware and various spyware available for it. The makers of these programs assume you have Windows (as the majority of desktop users have at the moment). New malware is being made all the time and if you get a virus, you will likely not know about it until it has done its damage (unless it's quite old, in which case your virus checker will pick it up). Malware has been made for Linux but most past attempts at it have failed. http://en.wikipedia.org/wiki/Linux\_malware  
Linux was originally designed for multiple users from the ground up, in contrast to Windows' 1-user original setup. This could factor in too.  
The password hashes used by Linux can be Blowfish or MD5. These are known strong algorithms, and they are protected by a "salt" to protect against "rainbow table" password cracking. Unfortunately, Windows uses a hash called "NTLM", NT Lan Manager. These hashes don't have salts, and your password is split into 7 digit segments before being hashed. See http://en.wikipedia.org/wiki/LM\_hash . These keys are significantly easier to crack and don't require much time if necessary rainbow tables have been installed.  
In the times of Windows XP, no password was set by default for the main user or administrator, Though this has been fixed now, this was a huge security risk.  
Exploits in Linux and Windows have been widespread, but Windows has had many more serious ones. In fact in 2008, a Windows server could be compromised by attacking the SMB service in an attack called "ms08\_067\_netapi". This can gain System user level access to the system.  
Linux kernel exploits have indeed been found but have been patched significantly quicker (as open source usually is, as there are many more developers), and cannot be exploited from the outside.  
One more reason why Windows computers happen to be less secure is that the users running the system do not know much about security (they are less educated) and as the system is often not tightly locked down enough.  
  
3\. "Linux is hard to use".  
This is a complete joke in my eyes. I recommend Linux Mint at www.linuxmint.com to anyone to try it. You will find that most if not all of your hardware is auto detected (Windows does not have this, it needs drivers, and the only reason it works for you is that they have been prepackaged along with your computer), and it's simple.  
To install software all one needs to do is to go to the Install Software or Package Manager button in the menu and search for software. Repositories like this have been checked for malware so there is a very slim chance user programs will do harm.  
Ubuntu and Mint are world renound for their ease of use, and that means there is no reason not to check them out!  
  
4\. "Linux won't play my media/DVD/etc"  
It is likely that your distribution does not come with necessary media codecs (for legal reasons). That is why I recommend Mint (to anyone in a country where the software is legal, get the Main edition). This includes software to play DVDs and almost all media formats. Though it is not hard to install it in Ubuntu, the media players prompt you to choose a codec and install it!  
  
5\. "Linux is all command line".  
Proof enough is this picture:  
  
[![](https://www.pendrivelinux.com/wp-content/uploads/kubuntu.jpg)](https://www.pendrivelinux.com/wp-content/uploads/kubuntu.jpg)
---
title: 'Best Security Practices For Your Personal Computer'
date: 2009-11-11T00:51:00.001Z
draft: false
aliases: [ "/2009/11/best-security-practices-for-your.html" ]
tags : [xp, virus, rootkit, password, worm, anti, avast, firewall, trojan, vista, windows, linux, live]
---

Many of you may be worried or concerned about the security of your computer. With threats of viruses, spyware, bank details being stolen, accounts cracked and vulnerabilities everywhere, it is natural to be paranoid.  
  
Here are some top security practices:  
  
1\. Change your passwords.  
All of them. Yes, really. It does make a lot of difference to the chances of a cracker being able to track you, monitor you or pretend to be you and not. Normally people advise you change all your passwords every 2 weeks. However don't write them down, and make them long and memorable using capital letters, numbers and symbols.  
  
Also, try not to make your password a dictionary word, or even close to it. Make it look like random garbage. You can use mnemonics to help you remember them. Consider the following sentence:  
  
"Do as I say, not as I do!"  
  
This can help you remember and formulate the password:  
DaIs,naId!  
You could add numbers, or convert some letters to numbers, etc:  
Da15,naId!  
  
Being 10 characters long, this is a medium strength password.  
Try to make a sentence about 14+ letters long for strong security, but remember nothing is unbreakable!  
  
2\. Install security software.  
A lot of users might think here: "I have a firewall. why do I need this?". The answer is simple: Just because you can stop things coming into your computer and going out, it doesn't make it invulnerable to threats such as downloaded malicious files or bad web pages. I recommend Windows users install [Avast Antivirus](http://www.avast.com/eng/download-avast-home.html) for free. Linux users should install rootkit checkers, such as rkhunter and chkrootkit.  
  
3\. Update your system regularly.  
This is one of the worst things you can leave out. If you do not update every single piece of your system, using update managers and such, vulnerabilities may be discovered in older versions of your software. Once you have a vulnerability, anything you could do (e.g. visiting a web site, opening a PDF) might give intruders access to your system. So remember to patch, and turn automatic updates ON!  
  
4\. Install a firewall.  
You may have one already, but some dismiss them. Make sure they're turned on! If you have Windows turn Windows Firewall on, and make sure there are little to no exceptons (aside from the things that you REALLY need). On Linux you can alter iptables via a GUI like Firestarter if you wish.  
  
5\. Change your browser.  
If you use Internet Explorer, you might do better to switch. It is well known for being particularly vulnerable to attack. There have been more security holes in Internet Explorer than any other browser, and they have been more slowly patched as well. [Firefox](http://www.getfirefox.com) and [Google Chrome](http://www.google.com/chrome) are good alternatives. Check [Secunia](http://secunia.com/advisories/) and [SecurityFocus](http://www.securityfocus.com/vulnerabilities) for more details. There is also a table of known vulnerabilities in the latest versions of many browsers [on Wikipedia](http://en.wikipedia.org/wiki/Comparison_of_web_browsers#Vulnerabilities).  
  
6\. Start over  
While many things may get in the way, you have tried your best to rid your computer of viruses, but there is a good chance that the viruses you have obtained have not been removed, as they may be too new for the database, or are too malicious. (Remember the stories about Conficker, the massive Windows malware, that you couldn't remove with antivirus?). If all else fails, the best way to remove any threats is to wipe your disk completely. Do a complete reinstall. There are many tutorials available, just google for them, or follow the guides for [Windows XP](http://www.pcworld.com/article/129977/how_to_reinstall_windows_xp.html) and [Windows Vista](http://windows.microsoft.com/en-US/windows-vista/Installing-and-reinstalling-Windows).If you have a recovery disk that came with your computer, then use this instead. In any case, remember to back up!  
  
7\. Back up your sensitive data.  
Anything you do not wish an intruder to get at would be best removed or moved to portable storage. Encrypted is best!  
  
8\. NEVER save bank/paypal details to your computer!  
If an intruder gets in, they can recover your passwords (regardless of whether they're locked out) and recover your bank details. Ouch.  
  
9\. If you have to do banking, do it on a Linux Live CD  
As [this Washington Post article](http://voices.washingtonpost.com/securityfix/2009/10/avoid_windows_malware_bank_on.html) says, you can avoid the risk of Windows malware, spyware, trojans, viruses, etc completely if you use a Linux Live CD to bank online with. I would recommend you download [Ubuntu](http://www.ubuntu.com) and burn it to a CD-R using [DeepBurner](http://www.deepburner.com) (using Burn ISO to disk option) or CD recording software of your choice, then boot from it. Here's how:  
  
Reboot your computer.  
If you see the Ubuntu boot screen, then select your language and press Enter at the next prompt.  
If you don't, see if there is a message to press a button to select boot device. Press it and select the CD or DVD device.  
If there is no message, find the message that says to press a button to enter SETUP. From there navigate to Boot devices and put priority on your CD/DVD device (method may vary). Finally save changes and exit.  
  
10\. Install Linux alongside Windows.  
If you like the CD, you can install it permanently so that you can install more software, by selecting the Install option on the desktop of Ubuntu, making sure to resize the Windows partition to whatever size you need. (Don't panic if resizing takes ages!)  
  
  
I hope that this has helped you become more secure. Please comment if you have any suggestions or things I may have left out.
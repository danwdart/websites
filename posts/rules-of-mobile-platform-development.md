---
title: 'Rules of Mobile Platform Development'
date: 2010-10-09T15:51:00.000+01:00
draft: false
aliases: [ "/2010/10/rules-of-mobile-platform-development.html" ]
tags: [htc, phone, apple, motorola, app, android]
---

A lot of things annoy me about mobiles. Here are some handy tips to you carriers, manufacturers, OS and app developers to make sure you do it right.  
  
**1) The User always comes first**  
Research what you think your users would like. Try not to blindly irritate users, and do what they want, don't force things upon them. Don't autostart without user permission, do things properly so that they're faster, and ask for feedback on what the application should and shouldn't do. Don't pop this up, let the user choose it from your easy menu. And finally never pester, or you'll drive your users away.  
  
**2) Don't advertise**  
Wasting a few pixels on a desktop isn't going to make a whole lot of difference, but on a mobile, it can severely break layouts. It can also waste people's precious bandwidth - after all, they paid for their little Internet space - and only want what they ask for. It also doesn't help your cause - after all, you don't want to drive your users AWAY from your app, do you? Advertising on a phone can be intrusive and annoying, too. Don't do it.  
  
**3) Try not to use menus**  
This in my opinion is something Apple did right with their iPhone, and Google did wrong with Android. Menus inside apps (especially on mobiles) tend to get confusing, and sometimes (e.g. if the menu button is a software button) get pressed accidentally.  
  
**4) Speed**  
An obvious one. Try to make your platform as fast as humanly possible - do everything you can to make sure each task is as speedy as is possible - that is, index your applications, don't make unnecessary requests, etc. If starting an application takes ten seconds - you're doing it wrong. If searching for a list of installed applications (and so on) takes forty - don't release yet. We've already seen a terrible example of this in Android pre-2.0. The general rule is that if it needs a loading screen, then it's already too slow.  
  
**5) Don't pester**  
I install a free application. The app starts, and it asks if I'd considered its paid version, with this set of features, that amount of extra greatness. That's fine. I consider it. I say no. Besides, now I know it exists, if I change my mind, I can just as easily go and get the paid version. Job done. However, many apps I've previously used (not pointing any fingers) seem to pester me to upgrade. Whether on startup, or on accidental activation of a "pro" feature - it asks me again and again. This is no good!  
  
**6) No Demos.**  
A free application should not be a "demo" of another paid one. I don't have time to waste, and if I want an application, I'll download the free one first. This may contain buttons which correspond to features only in the paid version. But I don't know that of course. They should be disabled and obvious, not look the same and pop up an annoying box saying "DEMO! Upgrade for only $3.99 for this feature". Even better, there should be one button in the menu that describes the different features. If I don't think I have enough, I'll go get the full version. If I think I have enough, and some turn out to be fake, it really bugs me.  
  
**7) Just Try Again  
**I often browse the web on my phone. Sometimes at no notice at all, the connection drops. Of course, the software can't be blamed for this, but the carriers can (unless it's obvious I'm in a place that has no chance of reception, such as in a tunnel). However, what annoys me is that when this happens, I have no "no reception" warning until about 30 seconds after the page doesn't load. I put it away, thinking it'll load eventually, and it never tries again. Trying again sounds like the sensible option to me, because after all, I asked for the web page, and I still want it when it's available again!  
  
**8) No Restrictions**  
There has been much anger towards certain carriers and manufacturers who lock down the operating system unnecessarily, forcing users to resort to things like rooting and custom ROMs to do the things they should have been able to do in the first place (e.g. wireless/wired tethering). A classic example of this would be the Motorola, which locked the bootloader in its Milestone phones, preventing first rooting (this could be enabled by flashing a vulnerable recovery image) and the ability to run custom ROMs with custom kernels, which in effect has had a bad effect on these handsets  
  
**9) Use Less Power**  
The more battery life the user can get out of their device, the better. Manufacturers and app developers need to learn that it is unacceptable for the user's device to run out of batteries half way through the day. So don't start your application or service on bootup - and try to minimise memory usage. Only start when the user wants you to start. I've found that numerous apps and services I don't need yet need to be killed on bootup! All apps should be opt-in bootup - and once you start it, if it makes sense to start on boot (such as a network monitor) then ask as soon as you install.  
  
**10) Make It Fun**  
A lot of apps are just plain boring, and I'm not accusing anyone personally. But it goes without saying that something fun and exciting will be more readily liked and bought by users and rated higher. Spend time making something people of all ages would like (unless it's specific) and would be willing to spend time playing or using.  

  

**11) Many Easy Updates**  

Everything's going to be buggy, so update often once you find a bug. Lots of small updates are better than few large ones - first to rid users of those niggling problems sooner rather than later, and second because it's less to download in a month. To phone manufacturers, don't hang around, and release OS updates as soon as they're available - they may provide valuable security updates. If you can't, let your users do it - move development into the community. The easiest way to update is by Market (for apps) or by OTA (over-the-air) update (OS updates).

  

**12) Don't be Motorola**

Ashamefully, Motorola has not been following some of these rules effectively. They almost never update their phones and leave them on early versions of the operating system. They lock their bootloaders and prevent users updating their phones effectively. So don't be like them. If the user bought the phone, it's theirs and should be able to be modified down to the lowest level.

  

Do you have any more tips or modifications? Use the comments area below. I hope you found this useful!
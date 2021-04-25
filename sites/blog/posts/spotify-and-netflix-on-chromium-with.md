---
title: 'Spotify (and Netflix) on Chromium, with help from Steam, without root!'
date: 2018-08-07T12:30:00.000+01:00
draft: false
aliases: [ "/2018/08/spotify-and-netflix-on-chromium-with.html" ]
tags : [netflix, library, steam, spotify, chromium, widevine, drm, libraries, chrome, copy, protected]
---

Users of Chromium will have trouble listening to Spotify, even if "protected content" (another word for "we own you") is on. This is because there are missing Widevine libraries.  
  
The usual way to find them is to copy them from your Chrome installation, sometimes at /opt/google/chrome, sometimes at /usr/share/chrome, but these can also be acquired from Steam installations (since Steam embeds Chrome).  
  
If you're running Steam, copy both  
  
 ~/.steam/steam/config/widevine/libwidevinecdm.so  
  
and  
  
~/.steam/steam/config/widevine/libwidevinecdmadapter.so  

  

to  
~/.config/chromium  
  
then restart Chromium.  
  
If you use Netflix, use a user agent extension to set your user agent to Chrome, so Netflix won't automatically assume that you can't use it.  
  
Hope that helps anyone coming across this issue.  
  
Till next time.
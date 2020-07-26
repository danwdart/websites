---
title: 'Project Chaplin 0.3.2 point release'
date: 2016-10-13T09:41:00.001+01:00
draft: false
aliases: [ "/2016/10/project-chaplin-032-point-release.html" ]
tags : [css, youtube, dailymotion, media, sharing, javascript, cc, dart, videos, vimeo, free software, chaplin, open source, creativecommons, html5, video, php, project]
---

Introducing the second point release for the 0.3 series of Project Chaplin.  
  
**Background**  
  
Project Chaplin is the first fully free software and open source video streaming service, installable locally or usable online.  
  
The software is available through GitHub at[ ](https://www.blogger.com/goog_1096986118)[https://github.com/kathiedart/projectchaplin/releases](https://github.com/kathiedart/projectchaplin/releases).  
  
_New for_ _0.3.2_: A demo server has been installed at [https://projectchaplin.com](https://projectchaplin.com/) for those who wish to test without downloading anything! Please don't hit this server a lot, as it is only hosted on a small server, and is mainly for testing purposes. Please let me know (see below) if you notice any outstanding issues not covered in the issue tracker, and email me privately for any security issues.  
  
The project is always looking for new developers, designers and ideas people. Please contact "viablog032 att projectchaplin dott com" if you are looking for a new project to join. The list of new bugs, feature requests, etc is available at [https://github.com/kathiedart/projectchaplin/issues](https://github.com/kathiedart/projectchaplin/issues)  
  
**Changelog (0.3 series)**  
**New in 0.3.2:**  
  
  
Fix the YouTube API/downloading problem  
  
  
**New in 0.3.1:**  
  
  

Get and store access token for Vimeo

Linkify and space-ify descriptions  
Set cache lifetime to 30m to wipe old data from remote nodes  
  
**New in 0.3:**  
Adds Docker support and includes a sample Docker file (with a Raspberry Pi release in the rpi branch).  
Logs and displays a link to the original YouTube link  
Adds fullscreen support to YouTube videos  
Makes the setup process more streamlined by starting daemons after setup is complete  
Adds Infinite loops a la infiniteyoutube.com  
Only imports Creative Commons videos from other services to avoid copyright issues  
Fixes YouTube API problems  
Shows correct licence on YouTube  
Removes old HTML5 requirements to add events to play videos and make them fullscreen - this now exists in all modern browsers  
UI improvements  
Splits the ORM into a new repository for other projects  
Faster loading through use of HTML5 AppCache (where enabled).  
  

Adds Vimeo searching and importing support!
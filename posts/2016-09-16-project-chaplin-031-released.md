---
title: 'Project Chaplin 0.3.1 Released'
date: 2016-09-16T15:23:00.000+01:00
draft: false
aliases: [ "/2016/09/project-chaplin-031-released.html" ]
tags: [css, youtube, dailymotion, media, sharing, javascript, cc, dart, videos, vimeo, free software, chaplin, open source, creativecommons, html5, video, php, project]
---

Introducing the first bugfix release for the 0.3 release of Project Chaplin.  
  
**Background**  
  
Project Chaplin is the first fully free software and open source video streaming service, installable locally or usable online. It has been in development for a few years now, and has had significant development, and a new design added a couple of years ago.  
  
The software is available through GitHub at [https://github.com/danwdart/projectchaplin/releases](https://github.com/danwdart/projectchaplin/releases).  
  
The project is always looking for new developers, designers and ideas people. Please contact "viablog031 att projectchaplin dott com" if you are looking for a new project to join. The list of new bugs, feature requests, etc is available at [https://github.com/danwdart/projectchaplin/issues](https://github.com/danwdart/projectchaplin/issues)  
  
**Changelog**  
**New in 0.3.1:**  
  

Get and store access token for Vimeo

Linkify and space-ify descriptions  
Set cache lifetime to 30m to prevent old data from remote nodes  
  
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

**New in Beta 2:**

Search now produces only CC-licenced YouTube videos
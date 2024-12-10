---
title: 'Sync iPhone/iPod Touch 3G in Ubuntu'
date: 2009-11-29T03:47:00.000Z
draft: false
aliases: [ "/2009/11/sync-iphoneipod-touch-3g-in-ubuntu" ]
tags: [ipod, ubuntu, linux, itunes]
---

A lot of people have been trying very hard to get iPhone and iPod Touch syncing to work correctly in Linux. Some have been doing a FUSE filesystem (which accesses the iPod) and some the database and syncing. Remember this is pre-alpha quality, but I found it synced my tunes nicely from Linux. But due to the database version being old (but quite good enough for the iPod), iTunes does not play nice with it - and will just resync the tracks and may delete them. Also the iPod must have been previosly initialised with a version of iTunes. If you cannot acquire it, or it will not run, contact teuf on #gtkpod in irc.freenode.net. You have been warned!

Mounting support

First, grab iFuse:
Add the following lines to /etc/apt/sources.list as root:

deb http://ppa.launchpad.net/jonabeck/ppa/ubuntu intrepid main
deb-src http://ppa.launchpad.net/jonabeck/ppa/ubuntu intrepid main

Then do:

sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com F0876AC9
sudo apt-get update
sudo apt-get install ifuse ifuse-dev

This should install support for reading and writing files.
Create a mount point:

sudo mkdir /media/ipodt/

Add yourself to the fuse group:

sudo useradd -G fuse \[your\_user\_name\]

To allow normal users to mount the FUSE filesystems, edit /etc/fuse.conf and uncomment the line (remove the hash from the start of it):

`\#user_allow_other`

Log out and back in again, or spawn a new login shell. Mount the FUSE filesystem:

ifuse /media/ipodt

You should see that your iPod should be mounted as yourself. You should be able to find the tracks and play them from the computer. They are oddly named and located in iTunes\_Control/Music .


Syncing support

To properly sync music (well I say properly), you need to install a special branch of libgpod. You'll need to get it from git. WARNING! THIS WILL OVERWRITE YOUR EXISTING LIBGPOD! Be careful. First install development libraries:

sudo apt-get install git-core build-essential cmake autoconf automake libtool intltool gtk-doc-tools libsqlite3-dev zlib1g-dev

Get teuf's sandbox repository branch:

git clone git://gitorious.org/~teuf/libgpod/teuf-sandbox.git
cd teuf-sandbox
git checkout origin/iphone30
CFLAGS="-g -O0" sh autogen.sh --prefix=/usr
make
sudo make install

libgpod should now be installed, if all goes to plan.
Create control directories and files:

mkdir /media/ipodt/iTunes\_Control/Device

Get the uuid of your device:

lsusb -v | grep -i iSerial

It should be the one that's 40 characters long. From the same teuf-sandbox directory, run:

tools/ipod-read-sysinfo-extended /media/ipodt

Check that a file exists. Do:

cat /media/ipodt/iTunes\_Control/Device/SysInfoExtended

This should spew XML at you.
Now, install your syncing program of choice. I chose gtkpod because it works for me.

sudo apt-get instal gtkpod

The program should pick up the device and ask which model it is. There should be your device listed at the bottom (you may have to scroll). Choose it and let it do its thing, and initialise your iPod. Check a file has been created:

ls /media/ipodt/iTunes\_Control/Device

If you get nothing, there's a problem. Go ask teuf. If you get that filename returned. all is well. You are able to sync, save files and update the database. Bear in mind that it saves an older version of the database, so if you go to iTunes and back, it will update tracks and the database, but they should still be readable in Linux.

I hope this has been helpful to you!

Here are some helpful references if you get stuck:
[iFuse](https://web.archive.org/web/20100211151037/http://matt.colyer.name:80/projects/iphone-linux/?title=Main_Page) (edit 2021: archived, also moved project to http://www.libimobiledevice.org/)
[iPod Syncing](http://marcansoft.com/blog/2009/10/iphone-syncing-on-linux-part-2/)
---
title: 'Issuing modem commands to an unrooted Android device'
date: 2018-07-31T20:37:00.000+01:00
draft: false
aliases: [ "/2018/07/issuing-modem-commands-to-unrooted.html" ]
tags: [access, phone, serial, root, screen, m2msupport, gsm, mobile, port, linux, minicom, connection, modem, networking, android, hayes, network]
---

Did you know that Android devices expose a modem on the USB interface, even when "Tethering" is turned off? It appears like this in dmesg:

`[22338.529851] cdc_acm 1-3:1.1: ttyACM0: USB ACM device`

You can connect to this as a raw serial console like:

`screen /dev/ttyACM0`

or:

`minicom -D /dev/ttyACM0`

This will accept GSM modem commands prefixed with AT, and give information about the phone, and presumably allow a dialup-like interface.

Many of the [examples on M2MSupport.net](http://m2msupport.net/m2msupport/software-and-at-commands-for-m2m-modules/) will work with the phone, depending on which manufacturer and capability set, presumably. With my Samsung Galaxy XCover 4, I got the GSM capability set.

Try playing around with this, but don't get charged by your provider too much for making calls you never end! Make sure you hang up properly as per the protocol.

For more on standard modem commands, see the [Hayes command set article on Wikipedia](https://en.wikipedia.org/wiki/Hayes_command_set).

That's all for now!
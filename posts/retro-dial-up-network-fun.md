---
title: 'Retro dial-up network fun'
date: 2018-07-27T01:46:00.000+01:00
draft: false
aliases: [ "/2018/07/retro-dial-up-network-fun.html" ]
tags : [98, address, serial, protocol, routing, server, kernel, nostalgia, dial-up, nat, windows, linux, modem, internet, emulation, isp, dialup, emulate, network]
---

I remember those days when your computer hissed and made strange noises in order to connect to the Internet. Today, most of us look back at those days in disdain. But for some, we want to repeat the same kind of experiences that we used to, just for the pure nostalgia of it. Some of the most remembered operating systems can take us back to those days.  
  
Windows for Workgroups 3.11 is remembered for its Terminal application and ability to use a multitude of networking technologies to connect to networks back in the day.  
  
DOS also had limited support for networks - but this required third party software, unless one would talk directly to "COM1" as it would name the serial device.  
  
Windows 98, although it has proper TCP/IP and Ethernet card support built in and there's therefore no need to use serial for internet when virtualising, for the most part it is remembered for its dial-up connections, since when we were using it, no one had broadband yet. But one of the best things about Windows 98 and serial is HyperTerminal! One can now connect to SynchroNet or other telnet services from Windows 98, just using HyperTerminal like dialling a phone, and possibly other things even with TLS, using socat. If you have a Windows 98 computer without ethernet, and a serial port between your host and Windows 98 computer, you can also connect to the Internet via this setup, by modifying the setup instructions to use a real serial port.  
  
I'm not including instructions to emulate a "Lucent" win-modem because as far as I can see, qemu doesn't support these.  
  
Although this isn't needed for dial up internet connection sharing, one can emulate a terminal with a SLIP interface too.  
  
I'm going to explain how to pretend to be your own ISP, to old versions of Windows, and make HyperTerminal available for telnet fun, all via a virtual serial port.  
  
To set this serial emulation up, I tried doing this through the VICE RS232 mode of tcpser, but it ultimately came up short. My VMs could use it just fine for telnetting places, but wouldn't go through pppd properly if I just socatted from its port to a pty, and made pppd listen on that pty. I think there was a protocol problem somewhere there, plus pppd kept hanging when I tried to make it listen on a TCP port - this is probably because it was trying to connect - but it is OK with a serial port. But this won't work with any old pty, it has to "look" like a serial port as well, and you can't just redirect physical serial ports using socat as if they were a file either.  
  
The secret ingredient is to install a virtual serial port - its module is called tty0tty and it can be found here: https://github.com/freemed/tty0tty.  
  
After installation (check the page for up to date instructions) you have access to 4 virtual serial port loops:  
```
/dev/tnt0 <=> /dev/tnt1  
/dev/tnt2 <=> /dev/tnt3  
/dev/tnt4 <=> /dev/tnt5  
/dev/tnt6 <=> /dev/tnt7  
```
For use as a normal user, these should be chmod'd to 666:  
```  
sudo chmod 666 /dev/tnt\*  
```
Anything sent to either end will be echoed on the other end, and this will act like a proper serial port, plugged in one end and the other end. For more information on this, see their GitHub repo.  
  
Once this is set up, we can start the virtual machine. First, run qemu using the virtual serial line:  
```  
qemu-system-i386 win98.img -serial /dev/tnt0  
```
  

Connect the other end to tcpser, which will emulate the phone line and allow you to dial a TCP connection.  
```
tcpser -tsSiI -i 's0=1&k3' -s 57600 -S 57600 -l5 -d /dev/tnt1 -n 1=127.0.0.1:2323 -n 2=synchro.net -n 3=  
localhost:23 -n 08450880101=localhost:2323 -n 08458457444=127.0.0.1:2323 -n "0845 845 7444"=127.0.0.1:2323 -n 0018002169575=127.  
0.0.1:2323 -n 0018005433279=127.0.0.1:2323 -n 08450801000=127.0.0.1:2323
```  
  
Explanation: Log everything. Set pickup to 1 ring. Set speed to 56k. Use the other end of the first serial line. Add a few example phone numbers (-n number=IP:port), (these I've used for ISP detection in Windows 98).  
  
Listen to that TCP connection with socat and redirect it to a second virtual serial line loopback. I don't want it to die so I'll put it in a while true.  
```  
while true; do socat -s -d -d tcp-listen:2323 /dev/tnt2; done  
```
  

In order to pretend to be our own ISP, we need to run pppd on the other end of that serial line. I couldn't use IP directly, this needs a serial line, and tcpser couldn't use a second interface, so we need to use socat. The IPs I'm using are within my current network. The rest of the settings are to disable authentication (for now, as I couldn't get it to work, which needs root), not fork the process so we can debug, not die if there's no call, show the debug logs, not communicate via serial (just IP), allow the connection to be seen by the LAN, adjust the forwarding parameters of the kernel appropriately and set a default DNS server of Quad9.  
  
```  
sudo pppd /dev/tnt3 57600 192.168.1.100:192.168.1.101 asyncmap 0 netmask 255.255.255.0 noauth silent nodetach passive persist debug local proxyarp ktune  
 ms-dns 9.9.9.9  
``` 
If you'd like to define your own DNS mappings from your /etc/hosts, such as pretending to be the server for updates and ISP information (if you host a web server locally), add the appropriate lines to your /etc/hosts like this:  
```  
192.168.1.100   ispreferral.microsoft.com www.update.microsoft.com v4.windowsupdate.microsoft.com windowsupdate.microsoft.com ww  
w.msn.com  
```  
and change `ms-dns 9.9.9.9` to `ms-dns 192.168.1.100` in the `pppd` command, then run dnsmasq:  
```  
sudo dnsmasq -zdippp0 -2ppp0  
```
The -z specifies only binding to ppp0 (to not interfere with other services running on port 53/udp, such as systemd-resolved), -d to not daemonise, -i ppp0 to specify the interface and -2 ppp0 to specify only DNS, not DHCP.  
  
  
In any case, do the usual NAT stuff:  
  
Allow IP forwarding...  
```
echo 1 | sudo tee  /proc/sys/net/ipv4/ip\_forward  
```
  

And allow forwarding and masquerading in the firewall (replace wlp7s0 with your main interface)  
```
sudo iptables -t nat -A POSTROUTING -o wlp7s0 -j MASQUERADE  
  
sudo iptables -A FORWARD -i wlp7s0 -o ppp0 -m state --state RELATED,ESTABLISHED -j ACCEPT  
  
sudo iptables -A FORWARD -i ppp0 -o wlp7s0 -j ACCEPT
```
...  
  
Hooray! Connecting to phone number 1 will give us a "dial up" connection through our virtual serial line! 2 will give us a line to synchro.net for use in the Terminal or HyperTerminal application, 3 will telnet to our local system if we need that for any reason and 08450880101 is the default Windows 98 "find my ISPs" service, which I've also redirected to our "ISP" connection for good measure.  
  
Coming next time:  
Can we emulate update servers and ISP information servers?  
Given Windows 98 only supports 128-bit SSL / TLS 1.0, I think it's time to break SSL.
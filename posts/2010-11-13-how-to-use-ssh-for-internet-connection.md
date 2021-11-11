---
title: 'How to use SSH for an Internet Connection Sharing Proxy'
date: 2010-11-13T13:10:00.000Z
draft: false
aliases: [ "/2010/11/how-to-use-ssh-for-internet-connection.html" ]
tags: [ssh, tunnel, linux, sharing, connection, proxy, internet, internet connection sharing]
---

I haven't made a blog in a long while, so I'd thought I'd share this, which I recently discovered how to do.  
  
If you find the idea of proxies a bit restrictive. because after all, they have to be set up in the applications in question, and may not work for some applications, help is here. And all you need is an SSH server you can connect to. Sadly, this method requires root, but it's worth having for the system-wide Internet connection you'll get from it.  

  

---

Authenticating as root
----------------------

First, make sure you're root on the client machine (sudo -s or su -, depending on your distro), and that you can ssh as root to your target server. This is of course causes security implications, so it may be a good idea to generate a key pair for root-to-root access and block off passworded access for root, so that no one can bruteforce your root password.  
  
**Generate the key pair as root on the client:**  
  
`client:~# ssh-keygen`  
  
**And copy the key to the server**  
  
`client:~# ssh-copy-id [server]`  
  
Test the root login. It should not prompt you for password authentication (unless you've set one in ssh-keygen). Now, to block off password logins, edit /etc/ssh/sshd\_config (or /etc/sshd/sshd\_config) on the server and make sure this line is present:  
  
`PermitRootLogin without-password`  
  
Hooray! We're now somewhat more secure!  

  

---

Creating the tunnel
-------------------

Now to start a tunnel. The -w switch on ssh will do what we need, and create a tunnel network interface on both computers. The first number is the number of the interface on the client, and the second is for the server. For example, 0:! will create tun0 on the client connected to tun1 on the server. You may specify auto for the next available one. Let's create tunnels called tun0 to make it simpler.  
  
`client:~# ssh -w0:0 [server]`  
  
**Now, see if your tunnels were set up correctly.**  
  
`server:~# ifconfig -a tun0`  
  
You should see a tun0 interface. This is a layer 3 tunneled virtual interface (point-to-point).  
  
**Set up an IP on both sides so each computer can talk to each other.**  
`  
server:~# ifconfig tun0 10.0.0.1 pointopoint 10.0.0.2  
client:~# ifconfig tun0 10.0.0.2 pointopoint 10.0.0.1`**  
Try pinging each side to see if you have a connection.**  
Once each host can talk to the other, we can set up the routing.  

  

---

Setting up the routing
----------------------

### Server setup

**Ensure that the tun0 interface is not restricted:**  
`  
server:~# iptables -A INPUT -i tun0 -j ACCEPT  
server:~# iptables -A OUTPUT -o tun0 -j ACCEPT  
server:~# iptables -A FORWARD -i tun0 -j ACCEPT`  
**Allow packets in from the external interface to be processed by the tunnel:**  
  
`server:~# iptables -A INPUT -i eth0 -d 10.0.0.2 -j ACCEPT`  
  
**Allow forwarded packets to be routed to their destination:**  
`  
server:~# iptables -A FORWARD -i eth0 -o tun0 -j ACCEPT`  
**  
Set up tun0 for NAT:**  
`  
server:~# iptables -A POSTROUTING -o tun0 -t nat -j MASQUERADE`  
**  
Enable IP forwarding in the kernel:**  
`  
server:~# echo 1 > /proc/sys/net/ipv4/ip_forward`  

###   

### Client setup

**Allow packets to be processed from the tun0 interface:**  
`  
client:~# iptables -A INPUT -i tun0 -j ACCEPT  
client:~# iptables -A OUTPUT -o tun0 -j ACCEPT  
client:~# iptables -A FORWARD -i tun0 -j ACCEPT`  

####   

#### Setting up the gateways

**Find the existing default gateway:**  
  
`client:~# route | grep ^default`  
  
**Add a backbone to stop the server not being found once we switch gateways:**  
`  
client:~# route add [server IP] gw [existing default gateway]`  
**  
Add the new default gateway:**  
`  
client:~# route add default gw 10.0.0.1`  
**  
Remove the existing default gateway (Be very careful!):**  
`  
client:~# route del default gw [existing default gateway]`  

###   

### Testing the tunnel

Try going to whatismyip.com in your browser. It should show you the IP of your server. If you're curious, you can also check the default route to somewhere like Google by using the traceroute utility.  
  
You're done!  
  

Troubleshooting
---------------

**_I can't see a tun0 interface!_**  
  
Make sure you're root on both sides. (It sounds obvious - I've thumped my head on my desk so much because of this!)  
  
Start ssh with the -v switch to show more verbosity. If you see a message a bit like this:  
  
debug1: Remote: Failed to open the tunnel device.  
channel 0: open failed: administratively prohibited: open failed  
  
it could mean that someone else is trying to create a tunnel with the same interface name on the server.  
  
If you see something a little like this:  
  
debug1: sys\_tun\_open: failed to configure tunnel (mode 1): Device or resource busy  
  
it might mean that you already have a tunnel with that interface name open. Check "ifconfig -a".  
  
_**I get the message "ping: sendmsg: Operation not permitted" when testing the tunnel connection!**_  
  
You didn't allow traffic to flow between the tunnel and local network device. Try turning the client firewall off.  
  
_**The connection is slow!**_  
  
There will be significant overhead as all the traffic is encapsulated into SSH and encrypted. You will also see latencies go up as traffic needs to travel from your client to your server and back additionally.
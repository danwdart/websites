---
title: 'Ephemeral system with NixOS'
date: 2021-03-20T13:01:00Z
draft: false
aliases: [ "/2021/03/ephemeral-system-with-nixos.html" ]
tags: [ephemeral, system, nixos, delete]
---

Recently, I discovered the following reddit post and blog posts:

- [Erasing root on every boot](https://www.reddit.com/r/NixOS/comments/lyuxv5/erasing_root_on_every_boot/)
- [Erase your darlings](https://grahamc.com/blog/erase-your-darlings)
- [NixOS ❄: tmpfs as root](https://elis.nu/blog/2020/05/nixos-tmpfs-as-root/)
- [NixOS ❄: tmpfs as home](https://elis.nu/blog/2020/06/nixos-tmpfs-as-home/)

I thought I didn't need any kind of persistence tool, because btrfs is awesome. So I decided to give it a go myself.

In my case, I'm using `btrfs` so it's very easy to get going with.

I went through and decided I wanted to keep:

- `/nix` (of course, I don't want to have to download everything all the time)
- `/home` (for now, I'll have a go at that one later)
- `/var/lib` (databases, docker, etc)
- `/etc/ssh` (host keys)
- `/etc/NetworkManager` (system connections, VPNs etc)

And I was content with the rest being recreated on boot, especially `/tmp`, because it vastly speeds up a lot of things!

Firstly I converted the directories I wanted to keep into subvolumes (in rescue mode or live/other OS):

```bash
mv /dir-to-keep /dir2
btrfs su c /dir-to-keep
cp -rpv --reflink=always /dir2/* /dir-to-keep/
rm -rf /dir2
```

`--reflink=always` makes a CoW copy in `btrfs`, so this is a much faster way to copy over data.

This worked for most of my directories, except for `/nix`, so I had to use `--reflink=auto` because I had a lot of hard links due to `nix-store` optimisation, so it'll CoW only that which it can.

Noteworthy is that to do `/nix` to avoid commands disappearing involves doing this (optionally from the running system):

```bash
btrfs su c /nix2
cp -rpv --reflink=auto /nix/* /nix2/ # Many hardlinks may be duplicated!
```

and then from a live or other system (because moving things in-use is dangerous and may not actually work):

```bash
mount /dev/XXX /mnt/root
rm -rf /mnt/root/nix
mv /mnt/root/nix2 /mnt/root/nix
```

Finally I'm able to have `/` as a `tmpfs`!

So I added the following to `hardware-configuration.nix`:

```nix
{
  fileSystems."/" =
    {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [
        "size=2G"
      ];
    };

  fileSystems."/nix" =
    {
      device = "/dev/XXX";
      fsType = "btrfs";
      options = [
        "subvol=/nix"
        "noatime"
      ];
    };
  
  fileSystems."/etc/ssh" =
    {
      device = "/dev/XXX";
      fsType = "btrfs";
      options = [
        "subvol=/etc/ssh"
        "noatime"
      ];
    };

  fileSystems."/etc/NetworkManager" =
    {
      device = "/dev/XXX";
      fsType = "btrfs";
      options = [
        "subvol=/etc/NetworkManager"
        "noatime"
      ];
    };
  
  fileSystems."/var/lib" =
    {
      device = "/dev/XXX";
      fsType = "btrfs";
      options = [
        "subvol=/var/lib"
        "noatime"
      ];
    };

  fileSystems."/home" =
    {
      device = "/dev/XXX";
      fsType = "btrfs";
      options = [
        "subvol=/home"
        "noatime"
      ];
    };
}
```

and making sure my passwords were immutable and persistent via the clauses in configuration.nix:

```nix
{
  users.mutableUsers = false;
  users.users.root.initialHashedPassword = "$6$8RZ1PPxKU6h$dNHnIWiq.h8s.7SpMW14FzK9bJwg1f6Mt.972/2Fij4zPrhR0X4m3JTNPtGAyeMKZk3I8x/Xro.vJolwVvwd9.";
  users.users.dwd.initialHashedPassword = "$6$EDn9CboEV/$ESAQifZD0wiVkYf1MuyLqs.hP7mvelpoPnSGEI7CmwuUifi090PT6FQqHsdhlZSXSlqrT9EH.mIfUvxPCA5q.1";
}
```

*(hey, they're hashed and SHA-512'd, do you want to try to crack them?)*

Triggering a rebuild and a reboot (to actually apply `/` as `tmpfs`), and success! Hooray!

I can now delete everything from the root subvolume that I didn't make into a subvolume.

`tree -x` from the root subvolume now looks like:

```bash
$ tree -x
.
├── etc
│   ├── NetworkManager
│   └── ssh
├── home
├── nix
└── var
    └── lib
```

so I can share these with another OS if I so wish.

I'm currently investigating doing the same to `/home`, so I'll keep you updated.

Till next time!
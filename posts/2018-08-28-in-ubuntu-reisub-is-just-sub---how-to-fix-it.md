title: "In Ubuntu, REISUB is just SUB - how to fix it"
author: "Dan Dart"
date: 2018/08/28

If you didn't know, you can hold Alt, hit SysRq (Print Screen) and type characters to speak directly to the kernel, especially useful in the console or crash conditions. In most distributions of Linux, this will be enabled, but it can be disabled or only some functions enabled.

The usual adage "<b>REISUB</b>" helps a user restart from a system freeze when only the kernel will respond to anything. The letters stand for "keyboa<b>R</b>d t<b>E</b>rminate k<b>I</b>ll <b>S</b>ync <b>U</b>nmount re<b>B</b>oot", but some users will choose "<b>RESIUB</b>" because they want to sync their disks before they kill their processes for some reason.

However, in a default installation of Ubuntu, R, E and I are disabled, which could cause some data loss, because every process is not terminated and killed before the sync and unmount.

You can re-enable this by

Especially useful once you do re-enable everything, is that you can then hold Alt and type SysRq, F, to make the system out-of-memory killer to run, to recover from a memory-based


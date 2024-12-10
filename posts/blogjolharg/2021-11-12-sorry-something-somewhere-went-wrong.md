---
title: 'Sorry, something somewhere went wrong!'
date: 2021-11-12T00:16:11Z
draft: false
aliases: [ "/2021/11/sorry-something-somewhere-went-wrong" ]
tags: [sorry,something,somewhere,went,wrong]
---

### Internet issues?

"You're not connected to the Internet!"

It's a phrase I see a lot, given I live in a rural part of England that they somehow haven't completely paved with your regular standard Internet.

"Yes I am!" I confusedly say, and look at my phone.

"Well it's not my fault!" I give out, irritatedly, feeling a little offended as the app blamed me for it.

### But what does that mean?

"Error." ... Huh?
"Error 276492." ... What?
"Sorry, we ran into a problem." ... Which was?
"Sorry, something somewhere went wrong." ... What with?

I see these too often and in too many places.

### The problem

It's completely useless to have a generic error message. It simply does not specify a good reason for failure, and usually suggests no remediation.

It's all very well wanting to hide some critical information which could lead to somebody cracking your servers wide open, but it's no good for the end user who doesn't know what to do and gets confused and dissatisfied with your app and doesn't know why.

### A solution

There are plenty of good error reporting libraries, and plenty of good ways to do it. It's pure unconstructive laziness to not report proper errors to end users. Here are a couple of suggestions on what to do to not annoy users:

Specific problem, remediation, advice and kindness.

### Examples

"Sorry, our database might be offline, so we're looking into it. You don't need to do anything, so maybe try again in a few minutes."

"Looks like the damage control suite went kaput. The engineers are on their way. Don't worry about it."

"Oops. Some faulty code made its way onto our servers. For now, please try your query again, and we might have something better for you."

"Okay, the place you've tried to visit isn't allowed for your user level at the moment, it's locked off to users below a Level 6 Clearance. If you think this is a mistake, then please email Mike at IT with your super secret IT passcode, but make sure not to share it with anyone else!"

"Err, looks like the device you're viewing this on is having trouble reaching the Internet. Please check you've got signal and then press this button to try again."

"Hmm. I couldn't reach Facebook to post your super cool update. Maybe they're having an issue of their own, but I'll try again for you in just a little bit. Sit tight."

### In conclusion

Here's to never seeing "ERROR 500" ever again!
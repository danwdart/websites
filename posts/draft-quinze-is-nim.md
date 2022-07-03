---
title: 'Quinze is Nim'
date: 2022-06-26T18:07:00+01:00
draft: true
aliases: [ "/2022/06/quinze-is-nim" ]
tags: [quinze, nim, number, game]
---

I realised that a game that I was taught in French class during my first year of secondary school is very similar to an existing game called Nim.

Nim is a well-known [number picking game] which interests me because of its surprising result when taught to people.

Only in Quinze, the players can be a whole class of people.

The differences in the rules of Quinze to Nim are:

- Everybody stands up at the beginning.
- One person must say between one and three French natural numbers, in ascending order.
- Whichever person is forced to say "Quinze" has to sit down and is out of the game.
- After the person who says "Quinze" is out, the counting begins again at "un".

Let's run a game with, let's say 10 people:
    Person 1: Un, deux...
    Person 2: Trois, quatre, cinq...
    Person 3: Six...
    Person 4: Sept, huit, neuf...
    Person 5: Dix, onze, douze...
    Person 6: Treize, quatorze...
    Person 7: Quinze!
    (Person 7 sits down and is out, in players are 1, 2, 3, 4, 5, 6, 8, 9, 10).
    Person 8: Un...
    Person 9: Deux, trois, quatre...
    Person 10: Cinq, six...
    (Back to the beginning)
    Person 1: Sept, huit, neuf...
    Person 2: Dix, onze...
    Person 3: Douze, treize, quatorze...
    Person 4: Quinze!
    (Person 4 sits down and is out, in players are 1, 2, 3, 5, 6, 8, 9. 10).
    Person 5: Un, deux, trois...
    Person 6: Quatre, cinq...
    (Person 7 is out already)
    Person 8: Six, sept...
    Person 9: Neuf, dix...
    Person 10: Onze, douze, treize...
    (Back to the beginning)
    Person 1: Quatorze...
    Person 2: Quinze!
    (Person 2 sits down and is out, in players are 1, 3, 5, 6, 8, 9, 10).

That's how it starts. Let's pause for a moment.

Person 3 comes next. Person 3 will have no chance, being first, to have a strategy to win, but let's say that person 5 wants to get person 3 out.

Person 5 will have to force person 3 to say quinze.
In order to do that, person 1 will have to count an extra 4 minus the number 10 last said.
This is fine, and person 9 will have to end with dix.
Therefore, person 6 will have to end with six.
Therefore, person 3 will have to end with deux.
So, in order to avoid being out, person 3 

Is there still a good strategy to win? One would have to force n-1 people "out", where you do not know where you are in the system. Is there a good strategy when you know everybody else doesn't have a strategy? Is there only a good strategy when you know everybody else does have a strategy?

It seems due to the original game, every other person (let's say the odds) must work together in order to win, and respond to the evens' responses, and ensure the sum of each even/odd pair is the appropriate amount in order to get rid of a pre-agreed person. It seems that half of the people must work together for this. 

Let's try it out with n people.

In order to "out" person m, one must force m to have no option but to say "quinze".
This means that person m-1 must start with douze, treize or quatorze.



[number picking game]: https://www.youtube.com/watch?v=9KABcmczPdg
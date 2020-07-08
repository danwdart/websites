title: "Building Single Page Applications (and more) for Modern Javascript"
author: "Dan Dart"
date: 2016/09/17

Here's a list of tips on how I've made my recent Javascript applications and how I would recommend doing them if you'd like to start, or update your technique.
<div>
</div><div>This tutorial will be framework agnostic and free from specialist build systems, and will leave the least amount of cruft around your filesystem.</div><div>
<div>At the end I'll show you my final setup and some arguments for using the strategy that I've employed.</div><div><b>
</b></div><div><b>1.&nbsp;No global npm dependencies</b></div></div><div><b>
</b></div><div><div>I try not to have people install build system packages globally because it's not always obvious, and this is an extra step. What's more, using npm scripts one does not need to have global packages and one single npm install&nbsp;is all that's needed to ready everything needed for a build. Even if a build system like grunt or gulp is installed, they can be accessed using npm scripts like:</div><div>
</div><div>$ # Old: grunt / gulp</div><div>$ # New:</div><div>$ npm run build</div><div>
</div><div>which will run the appropriate build step. This is also one thing that Javascript developers are almost guaranteed to have before running your project (because how else would they have your dependencies?).

To do this, everything in npm scripts (the "scripts" section of your package.json) adds the ./node_modules/.bin to your $PATH, so locally installing CLI tools will just work when invoked from npm run.</div><div><b>
</b></div><div><b>2.&nbsp;npm scripts (no grunt, gulp, etc).</b>
<b>
</b>Another reason I prefer not to use a build system is that many tools either don't have a suitable plugin for grunt or gulp, or their implementation is flawed or incomplete, whereas their CLI tool does everything it needs to, because the tool authors almost always wrote it. It's also often difficult to figure out how to lay out the object syntax for these tools, to a point that some grunt or gulp plugins don't have sufficient documentation or functionality for extra addons to the particular tool, and it's easier just to use their command lines. At least that's my experience.</div><div><b>
</b></div><div><b>3. Concatenating and transpiling your JS code</b></div><div><b>
</b>Usually you won't want to include every single library and source file into your HTML using endless <script></font> tags. If you've ever tried to concatenate them, you'll know that it pollutes your global variables a lot, and sometimes requires that you concatenate them in a certain order, so you'll end up having something like <font face="Courier New, Courier, monospace">00_begin.js</font>, <font face="Courier New, Courier, monospace">01_lib.js</font>, <font face="Courier New, Courier, monospace">99_end.js</font>, etc., and I think you'll agree that that's rather awkward. So wouldn't it be lovely to have everything be automatically included from your <font face="Courier New, Courier, monospace">index.js</font> file using imports (the new JS standard of require in Node)?

<b><br></b></div><div>

<b>4. Compiling your templates/HTML</b></div><div>

<b><br></b></div><div>

<b>5. Compiling your CSS</b></div><div>

<b><br></b></div><div>

<b>6. Minification</b>

<b><br></b>

<b>7. Caching resources</b></div><div>

<b><br></b></div><div>

<b>8. Watch steps</b></div><div>

<b><br></b></div><div>

<b>9. Testing</b></div><div>

<b><br></b></div><div>

Here's my final package.json:</div></div><div>

<br></div><div>

And here's my sample filesystem structure, if you'd like to try making it yourself:</div><div>

<br></div><div>

I've exported this sample structure to a GitHub repository, so you can clone it and use it for the base of your own project. Should I make a template generator? Is there anything you'd like me to cover in greater detail or make another post about? If so, please leave a comment!</div></script></div></div>


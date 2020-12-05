
####Controlling VESC via serial port of remote node



##### Example REPL session


```
	erl -pa ebin
```

```lang-erl

{R, S} = vesc:ini("/dev/ttyUSB0").
vesc:cmd(S,probes).
vesc:cmd(S,thrust,5).
vesc:fin(S).

```

##### Footnotes

* tested on ancient raspberry pi with https://github.com/vedderb/bldc (FW 4.x) attached to it.

# unix shell helper

it is inspired of Oberon system and ACME.

[watch in action](https://toobnix.org/w/kQQa6Wy6ETx7PT7JkoRjdN)

keep your favorite commands, or commands you forget often, there.

upon start it opens `~/heliko.txt` file.

(copy `heliko.txt` from the current directory to your home)

to increase/decrease font size, use ctrl+mouse wheel.

it contains some commands. commands are enclosed with semicolons.

let's say:

```
;uname -a;

;ls -al ^;


;ls ~/*.jpg;

;xv ^;

;xv /home/inky/^;

;ls /amp/tmp/films/japan/ ;


/amp/tmp/films


;find ^  -type f -name "*.mp4";


;find ^ -mindepth 1 -maxdepth 1  ;

;mplayer ^;


To fix/clear clipboard/selection:
;touch /tmp/blank && xclip -selection clipboard /tmp/blank;
```


selection with left button, command execution with right mouse button.

you can right click on `;ls ~/*.jpg;` with the right button, and in the xterm output window you'll see the images in your home directory.
then select any of those images and right click on `;xv ^;` and that image will be shown. (of course in case you have xv installed).

other example:

select some location, it can be even from the same window, like `/amp/tmp/films` in my case.

then right click on `;find ^  -type f -name "*.mp4";`

the window will list films (mp4 files) in that directory.

select one of the files, and right click on `;mplayer ^;` and the movie will be played back. (of course if you have mplayer)

add your commands to it, and run by clicking.

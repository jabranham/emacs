# Emacs file
This is my personal `.emacs` file.
Feel free to fork, clone, edit it to your heart's content.
I use emacs mainly to work with LaTeX, R, git, markdown, and ESS.

# Install emacs
If you decide to use this file, you need to download and install emacs.
This init file depends on emacs 24.
On Ubuntu, you can get emacs via `sudo apt-get install emacs24`.
Emacs is also available on Windows and Mac.

# Install other programs
These aren't strictly speaking necessary, but you probably want them.
You'll want [R](https://www.r-project.org/), LaTeX (TeXLive for Linux, [MaCTeX](https://tug.org/mactex/) for mac, and [MiKTeX](http://www.miktex.org/) for Windows), [pandoc](http://pandoc.org/), and [git](http://www.git-scm.com/).

# Using this file
If you want to use my setup, you can download this file and put it in `~/.emacs.d`.
Or (even better), clone this git repo with:

```
git clone https://github.com/jabranham/emacs.git ~/.emacs.d
```

Once you do that, you probably want to create a new branch so that you can pull in changes I make easily.
The first time you open emacs, it will attempt to contact an online package repository, so make sure that you're online.
If you get an error, try closing and reopening emacs.
Sometimes the package repositories are a little finecky.

# lintr
One of the packages I use relies on an R package to inform you of bad syntax.
Open up R, and run `install.packages("lintr")`

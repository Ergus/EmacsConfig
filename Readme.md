Readme
======

This is my personal emacs config.

As it uses use-package you only need to download the **init.el** and
**early-init.el** to your emacs config directory. Most packages will
download and install automatically. But this only works with emacs 27
and I don't add version checks because I make this for my personal
use.

So I don't recommend to use it as is, but you can copy any portion of
the config if you want. And report me issues if you find them in the config.

Files in the lisp directory are detected automatically.

Install
-------

If you want to use this as is:

1. Clone the repo: 

```shell
git clone https://github.com/Ergus/EmacsConfig.git .emacs.d
```

2. Start in debug mode the first time:

```shell
emacs --debug-init
```

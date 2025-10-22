### Start by installing emacs

Use [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)


```
$ brew tap d12frosted/emacs-plus
```

```
$ brew install emacs-plus@30 --with-xwidgets --with-imagemagick
```

Your first setup will take forever what with gcc running for the first
time _and_ org roam syncing. Best to start emacs as a foreground
process, rather than as a daemon, first so that you can see if there
are any issues (often, `package-refresh-contents` is required).

After it looks stable, enable as a service with brew - it does the
same as your old plist but this way you don't have to manage it, and
brew services are nice to work with.

```
$ brew services start emacs-plus@30
```

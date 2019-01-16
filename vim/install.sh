#!/bin/bash
# Check on dependencies
if ! type "git" &> /dev/null;
then
	echo "git not installed, exiting"
	exit 1
fi
#TODO Dependencies
# python?
# fzf (brew)
## pretty sure this is for one of my plugins
# YCM dependencies
## For JS: Tern


# Resources location TODO read from cmd line optionally
RESOURCES="$(dirname $0)/resources"
# Values TODO read from config
VIMDIR="$HOME/.vim"
VIMBUNDLEDIR="$VIMDIR/bundle/"
VIMPLUGINSFILE="$VIMDIR/plugins.vim"
VIMWIKIFILE="$VIMDIR/wiki.vim"
VIMFILE="$HOME/.vimrc"
#

#TODO remove obviously
if [ -d $RESOURCES ]
then
	echo "resources folder is a ok, captain"
fi

echo "checking for a .vim directory"
if [ -d $VIMDIR ];
then
	echo "directory exists, skipping this part"
else
	echo "directory does not exist, press enter to create one [FAKE]"
	read
fi

echo "checking for .vim/bundle"
if [ -d $VIMBUNDLEDIR ];
then
	echo "vim bundle directory already exists, skipping this part"
else
	echo "vim bundle directory does not exist, press enter to create one [FAKE]"
	read
fi

echo "checking for .vim/plugins.vim"
if [ -e $VIMPLUGINSFILE ];
then
	echo "vim plugins file already exists, skipping this part"
else
	echo "vim plugins file does not exist, press enter to create one [FAKE]"
	read
fi

echo "checking for .vim/wiki.vim"
if [ -e $VIMWIKIFILE  ];
then
	echo "vim wiki file already exists, skipping this part"
else
	echo "vim wiki file does not exist, press enter to create one [FAKE]"
	read
fi

if [ -e $VIMFILE ];
then
	echo ".vimrc file already exists, skipping this part"
else
	echo ".vimrc file does not exist, press enter to create one [FAKE]"
fi



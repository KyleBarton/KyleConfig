if ! type "brew" &> /dev/null;
then
	echo "Homebrew not installed, exiting"
	exit 1
fi

echo "Installing yank..."
brew install yank
echo "finished!"

-- Functions which allow hammerspoon to call emacsclient with a
-- floating frame for various utilities; note-taking with org-roam is
-- my original use-case.


local spoon = {}

spoon.name = "EmacsFloatingWindow"

spoon.version = "0.0.1"

spoon.author = "Kyle Barton"

spoon.license = "MIT - https://opensource.org/licenses/MIT"

-- For now, assuming emacs is running as a daemon and that we are
-- using emacsclient in order to tap into the server. Later, we can
-- make this configurable in order to allow a user to start a new
-- emacs instance on each call if they desire. I'm also assuming that
-- emacsclient is in the system PATH
spoon.emacsExecutable = "emacsclient"

spoon.restoreWindow = nil

function spoon.evalWithFrame(elisp)
   args = nil
   if elisp and elisp ~= '' then
      args = {
	 '-c',
	 '--eval',
	 elisp,
	 '--alternate-editor='
      }
   else
      args = {
	 '-c',
	 '--alternate-editor='
      }
   end

   local emacsClientTask = hs.task.new(
      mod.ec_executable,
      function(code, out, err)
	 if err then
	    LOGGER.e(err)
	 end
      end,
      args
   )

   emacsClientTask:start()

end


function spoon.callEmacs(ecFunction, callback)
   spoon.evalWithFrame(
      string.format([[(%s "%s")]], ecFunction, callback)
   )
end


function spoon.saveWindow()
   spoon.restoreWindow = hs.window.focusedWindow()
end

function spoon.restoreWindow()
   spoon.restoreWindow:focus()
end


-- Modal Hammerspoon configuration based on a single mode, CommandMode

-- CommandMode is entered with the CAPSLOCK key, which is mapped to
-- f18 with an hidutil command managed by launchd (TODO link relevant
-- resources).

-- Once in CommandMode, a variety of keys are available to navigate to
-- certain windows and/or applications. Escape will manually exit
-- CommandMode. CommandMode will also auto-exit after 3 seconds from
-- inactivity.


----------------------------------------------------------------------------------------
-- TODOs
----------------------------------------------------------------------------------------
-- KEYBOARD SHORTCUTS
  -- caps-caps: create or focus on a "main" emacsclient window -- DONE
  -- caps-w: open an ivy-select-windows emacsclient window
-- GRAPHICS
  -- Create a nonintrusive graphic alert for entering/exiting command mode
-- FANCY CONCEPTS
  -- In browser, if focused on a text input, pipe to an emacs scratch window for editing, and pipe back on exit
----------------------------------------------------------------------------------------

CommandMode = hs.hotkey.modal.new('', 'f18')

COMMAND_MODE_ON = false

LOGGER = hs.logger.new('mylogger', 'debug')

-- Name of the central emacs frame, in full screen.
PRIMARY_EMACS = "EMACS_MAIN"

-- Disable animations
hs.window.animationDuration = 0

-- Exit CommandMode 3 seconds after entering, due to inactivity.
function CommandMode:entered()
  COMMAND_MODE_ON = true
  hs.timer.doAfter(3, function()
      -- Commands should exit command mode on their way out, so this
      -- is only necessary if the user isn't issueing a command
      if COMMAND_MODE_ON then
	CommandMode:exit()
      end
  end)
end


function CommandMode:exited()
  COMMAND_MODE_ON = false
end


CommandMode:bind('', 'escape', function()
    CommandMode:exit()
end)

function emacsclientWithEvalNoFrame(elisp)
   args = nil
   if elisp and elisp ~= '' then
      args = {
	 '--eval',
	 elisp,
	 '--alternate-editor='
      }
   else
      args = {
	 '--alternate-editor='
      }
   end

   local emacsClientTask = hs.task.new(
      '/usr/local/bin/emacsclient',
      function(code, out, err)
	 if err then
	    LOGGER.e(err)
	 end
      end,
      args
   )

   emacsClientTask:start()

end

function emacsclientWithEval(elisp)
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
      '/usr/local/bin/emacsclient',
      function(code, out, err)
	 if err then
	    LOGGER.e(err)
	 end
      end,
      args
   )

   emacsClientTask:start()

end

function focusAndGetExistingWindow(hint)
   local found = hs.window.find(hint)
   if found then
      found:focus()
   end
   return found
end

function goToMainEmacs(shouldRetry)
   local primaryEmacs = focusAndGetExistingWindow(PRIMARY_EMACS)

   if not primaryEmacs then
      emacsclientWithEval(string.format('(set-frame-name "%s")', PRIMARY_EMACS))
      if shouldRetry then
	 -- Sometimes the frame needs a moment to appear, so we add a
	 -- single retry
	 hs.timer.doAfter(0.3, function()
	       goToMainEmacs(false)
	 end)
      end
   else
      local peFrame = primaryEmacs:frame()
      local maxFrame = primaryEmacs:screen():frame()
      peFrame.x = maxFrame.x
      peFrame.y = maxFrame.y
      peFrame.w = maxFrame.w
      peFrame.h = maxFrame.h
      primaryEmacs:setFrame(peFrame)
   end
end

function openEmacsIvyFunc(windowName, ivyFunc)
   local modalWindow = nil
   emacsclientWithEvalNoFrame(string.format('(kjb/make-mini-frame-test "%s")', windowName))
   hs.timer.doAfter(0.3, function()
	 modalWindow = focusAndGetExistingWindow(windowName)
	 if modalWindow then
	    modalWindow:centerOnScreen()
	 end
   end)
end


CommandMode:bind('', 'f18', function()
      CommandMode:exit()
      goToMainEmacs(true)
end)

CommandMode:bind('', 'r', function()
      CommandMode:exit()
      hs.reload()
end)

-- TODO Turn this into a window selector with ivy
CommandMode:bind('', 'w', function()
      CommandMode:exit()
      openEmacsIvyFunc('test', '')
end)

----------------------------------------------------------------------------------------
-- Various Debugging Functions as I learn Hammerspoon/Lua
----------------------------------------------------------------------------------------

function showApps()
    for key, val in pairs(hs.application.runningApplications()) do
      LOGGER.i(val:title())
    end
end

function showWindows()
   for key, val in pairs(hs.window.allWindows()) do
      LOGGER.i(val:title())
   end
end

CommandMode:bind('', 'l', function()
      CommandMode:exit()
      showWindows()
end)

CommandMode:bind('', 't', function()
      CommandMode:exit()
      if hs.appfinder.appFromName('Emacs') then
	 LOGGER.i("Emacs is here")
      else
	 LOGGER.i("No emacs here")
      end
end)



hs.alert'Config Loaded'

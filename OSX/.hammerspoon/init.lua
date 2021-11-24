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
  -- caps-w: open an ivy-select-windows emacsclient window -- DONE
-- GRAPHICS
  -- Create a nonintrusive graphic alert for entering/exiting command mode
-- FANCY CONCEPTS
  -- In browser, if focused on a text input, pipe to an emacs scratch window for editing, and pipe back on exit
----------------------------------------------------------------------------------------

-- Allow emacs to call hs back for window manipulation
require("hs.ipc")

local constants = require(".constants")
local keys = constants.keys
local ecUtils = require(".emacsclient_utils")
local debugUtils = require(".debug_utils")

CommandMode = hs.hotkey.modal.new('', keys.CAPS_LOCK)

COMMAND_MODE_ON = false

LOGGER = hs.logger.new('init', constants.LOG_LEVEL)

-- Name of the central emacs frame, in full screen.
PRIMARY_EMACS = "EMACS_MAIN"

-- Disable animations
hs.window.animationDuration = 0

function CommandMode:entered()
  COMMAND_MODE_ON = true
  -- Exit if no subsequent command after 3 seconds
  hs.timer.doAfter(3, function()
      if COMMAND_MODE_ON then
	CommandMode:exit()
      end
  end)
end


function CommandMode:exited()
  COMMAND_MODE_ON = false
end


CommandMode:bind('', keys.ESCAPE, function()
    CommandMode:exit()
end)

function focusWindowByTitle(windowTitle)
   local found = hs.appfinder.windowFromWindowTitle(windowTitle)
   if found then
      found:focus()
   end
   return found
end

function goToMainEmacs(shouldRetry)
   local primaryEmacs = hs.window.find(PRIMARY_EMACS)

   if not primaryEmacs then
      ecUtils.evalWithFrame(string.format('(set-frame-name "%s")', PRIMARY_EMACS))
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
      primaryEmacs:focus()
   end
end

-- Ok, so we're agreeing on an interface here: (ivyFunc windowName
-- Options). Emacs will have to handle the rest. Which means emacs
-- will have to be aware of certain hs lua functions, which isn't
-- great. Perhaps I should just agree on an interface on the other
-- end.
function openOptionsWindow(windowName, ivyFunc, options)
   ecUtils.callIvyFunc(windowName, ivyFunc, options)
   hs.timer.doAfter(0.3, function() focusWindowByTitle(windowName) end)
end

CommandMode:bind('', keys.CAPS_LOCK, function()
      CommandMode:exit()
      goToMainEmacs(true)
end)

CommandMode:bind('', 'r', function()
      CommandMode:exit()
      hs.reload()
end)

CommandMode:bind('', 'w', function()
      CommandMode:exit()
      
      local windowNames = {}
      for key, val in pairs(hs.window.allWindows()) do

	 -- In cases when the window name has a " in it, emacs <->
	 -- Hammerspoon IPC struggles if we don't escape the quotation
	 -- explicitly.
	 windowNames[#windowNames+1] = val:title():gsub("\"", "\\\"")
      end
      
      openOptionsWindow('test', 'hs-ivy/hs-window-select', windowNames)
end)

----------------------------------------------------------------------------------------
-- Functions built to be called by emacs
----------------------------------------------------------------------------------------
function getWindowTitlesByHint(hint)
   local windows = hs.window.allWindows()
   local windowTitles = {}
   for _, val in pairs(windows) do
      windowTitles[#windowTitles+1] = val:title()
   end
   -- return luaArrayToElispList(windowTitles)
   return windowTitles
end

hs.alert'Config Loaded'

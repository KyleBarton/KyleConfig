-- A hammerspoon utility built to enable floating emacs frames for
-- notes or navigation, and then restore the window configuration
-- after emacs has completed.


-- TODO Can this be decoupled better from hammerspoon?

local constants = require('.constants')

LOGGER = hs.logger.new('emacsclient_windows', constants.LOG_LEVEL)

local ec_utils = require(".emacsclient_utils")

local mod = {}

mod.current_window = nil

function mod.call_emacsclient(ec_function, callback)
   ec_utils.evalWithFrame(
      string.format([[(%s "%s")]], ec_function, callback)
   )
end


function mod.save_window()
   mod.current_window = hs.window.focusedWindow()

end

function mod.restore_window()
   mod.current_window:focus()
end



return mod

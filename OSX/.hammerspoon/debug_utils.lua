local constants = require('.constants')

LOGGER = hs.logger.new('debug_utils', constants.LOG_LEVEL)

local mod = {}

function mod.logRunningApps()
    for key, val in pairs(hs.application.runningApplications()) do
      LOGGER.i(val:title())
    end
end

function mod.logRunningWindows()
   for key, val in pairs(hs.window.allWindows()) do
      LOGGER.i(val:title())
   end
end

return mod

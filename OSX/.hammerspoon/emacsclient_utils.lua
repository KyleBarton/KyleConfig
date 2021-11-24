local constants = require('.constants')

LOGGER = hs.logger.new('emacsclient_utils', constants.LOG_LEVEL)

-- Emacs Utils
local mod = {}

function mod.luaArrayToElispList(arr)
   local stringedList = "("
   for _,val in pairs(arr) do
      stringedList = stringedList..string.format([["%s" ]], val)
   end
   stringedList = stringedList..")"
   return stringedList
end


function mod.evalWithFrame(elisp)
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

function mod.evalNoFrame(elisp)
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
	 if err and err ~= '' then
	    LOGGER.e(err)
	 end
      end,
      args
   )

   emacsClientTask:start()

end

function mod.callIvyFunc(windowName, ivyFunc, ivyOptions)

   local ivyOptionsList = mod.luaArrayToElispList(ivyOptions)

   LOGGER.d(string.format("ivy options sent to emacs: %s", ivyOptionsList))

   mod.evalNoFrame(string.format('(%s "%s" \'%s)', ivyFunc, windowName, ivyOptionsList))
end


return mod

local server = ModSettingGet("onlywands.server")
local token = ModSettingGet("onlywands.token")
if token == "" then
  token = dofile("mods/onlywands/token.lua")
end
HOST_URL = "wss://" .. server .. "/" .. token

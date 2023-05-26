local utils = dofile("mods/onlywands/files/lib/utils.lua")
dofile_once("mods/onlywands/files/lib/pollnet.lua")
dofile("mods/onlywands/files/ws/host.lua")

local main_socket = wslib.open_ws(HOST_URL)

local function send_to_server(json_string)
    if main_socket then
        if main_socket:status() == "open" then
            main_socket:send(json_string)
        end
    end
end

local tries = 0
local function try_reconnect()
    if tries < 10 then
        main_socket = wslib.open_ws(HOST_URL)
        tries = tries + 1
    end
end

local last_serialized = ""
local frames = 0

WEBSOCKETS_TICK = function()
    frames = frames + 1

    if not main_socket then
        if (frames % 300 == 0) then try_reconnect() end
        return
    end

    local success, _ = main_socket:poll()

    if not success then
        main_socket = nil
    end

    if frames % 300 == 0 then
        send_to_server('{"alive": true}')
    end

    if frames % 180 == 0 then
        local serialized = utils.serialize_data()
        if (serialized ~= "" and last_serialized ~= serialized) then
            last_serialized = serialized
            send_to_server(serialized)
        end
    end

end

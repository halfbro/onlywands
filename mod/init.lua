local function is_valid_entity(entity_id)
    return entity_id ~= nil and entity_id ~= 0
end

function OnWorldPostUpdate()
    local world_state = GameGetWorldStateEntity()
    if (WEBSOCKETS_TICK and is_valid_entity(world_state)) then
        WEBSOCKETS_TICK()
    end
end

function OnPlayerSpawned(player_entity)
    dofile("mods/onlywands/files/ws/ws.lua")
end

function OnPausePreUpdate()
    if (WEBSOCKETS_TICK) then
        WEBSOCKETS_TICK()
    end
end

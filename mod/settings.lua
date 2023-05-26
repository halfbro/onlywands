dofile("data/scripts/lib/mod_settings.lua") -- see this file for documentation on some of the features.

-- This file can't access other files from this or other mods in all circumstances.
-- Settings will be automatically saved.
-- Settings don't have access unsafe lua APIs.

-- Use ModSettingGet() in the game to query settings.
-- For some settings (for example those that affect world generation) you might want to retain the current value until a certain point, even
-- if the player has changed the setting while playing.
-- To make it easy to define settings like that, each setting has a "scope" (e.g. MOD_SETTING_SCOPE_NEW_GAME) that will define when the changes
-- will actually become visible via ModSettingGet(). In the case of MOD_SETTING_SCOPE_NEW_GAME the value at the start of the run will be visible
-- until the player starts a new game.
-- ModSettingSetNextValue() will set the buffered value, that will later become visible via ModSettingGet(), unless the setting scope is MOD_SETTING_SCOPE_RUNTIME.

-- Creates a GUI element that makes a text box with an icon to show/hide its contents. Can be typed into like normal, but only shows '*' if hidden
local function mod_setting_password(mod_id, gui, in_main_menu, im_id, setting)
    local old_value = ModSettingGetNextValue(mod_setting_get_id(mod_id, setting))
    setting._password_visible = setting._password_visible or false

    local image_file = "mods/onlywands/files/icons/eyecon_closed.png"
    if setting._password_visible then
        image_file = "mods/onlywands/files/icons/eyecon_open.png"
    end


    GuiLayoutBeginHorizontal(gui, 0, 0)
    GuiText(gui, 0, 0, setting.ui_name)

    local new_value
    if not setting._password_visible then
        local displayed_text = string.gsub(old_value, ".", "%*")
        local textbox_value = GuiTextInput(gui, im_id, 0, 0, displayed_text, 100, 16)

        -- The way this works is that the text box will return whatever is in it at the end of the frame.
        -- In the case of the user adding characters, it will be something like "*******a"
        -- So we need to extract the characters after the "*******" that was already in the box, then append that to the old value
        -- In the case of the user removing characters, we just keep however many characters there are in the old value
        if string.len(textbox_value) > string.len(old_value) then
            new_value = old_value .. string.gsub(textbox_value, displayed_text, "")
        elseif string.len(textbox_value) < string.len(old_value) then
            new_value = string.sub(old_value, 1, string.len(textbox_value))
        else
            new_value = old_value
        end
    else
        new_value = GuiTextInput(gui, im_id, 0, 0, old_value, 100, 16)
    end
    mod_setting_tooltip(mod_id, gui, in_main_menu, setting)

    GuiIdPush(gui, 1111) -- arbitrary number
    GuiOptionsAddForNextWidget(gui, GUI_OPTION.DrawActiveWidgetCursorOff)
    local clicked, _ = GuiImageButton(gui, im_id, -2, 2, "", image_file)
    GuiIdPop(gui)
    GuiLayoutEnd(gui)
    if old_value ~= new_value then
        ModSettingSetNextValue(mod_setting_get_id(mod_id, setting), new_value, false)
    end

    if clicked then
        setting._password_visible = not setting._password_visible
    end

end

local mod_id = "onlywands" -- This should match the name of your mod's folder.
local mod_settings_version = 1 -- This is a magic global that can be used to migrate settings to new mod versions. call mod_settings_get_version() before mod_settings_update() to get the old value.
local mod_settings = {
    {
        id = "server",
        ui_name = "Server URL",
        ui_description = "The hostname for the wand server.",
        value_default = "onlywands.com",
        scope = MOD_SETTING_SCOPE_RUNTIME
    },
    {
        id = "token",
        ui_name = "Token",
        ui_description = "Your token from onlywands.com. DO NOT SHARE IT WITH ANYONE.",
        value_default = "token",
        scope = MOD_SETTING_SCOPE_RUNTIME,
        ui_fn = mod_setting_password,
    }
}

-- This function is called to ensure the correct setting values are visible to the game via ModSettingGet(). your mod's settings don't work if you don't have a function like this defined in settings.lua.
-- This function is called:
--		- when entering the mod settings menu (init_scope will be MOD_SETTINGS_SCOPE_ONLY_SET_DEFAULT)
-- 		- before mod initialization when starting a new game (init_scope will be MOD_SETTING_SCOPE_NEW_GAME)
--		- when entering the game after a restart (init_scope will be MOD_SETTING_SCOPE_RESTART)
--		- at the end of an update when mod settings have been changed via ModSettingsSetNextValue() and the game is unpaused (init_scope will be MOD_SETTINGS_SCOPE_RUNTIME)
function ModSettingsUpdate(init_scope)
    local old_version = mod_settings_get_version(mod_id) -- This can be used to migrate some settings between mod versions.
    mod_settings_update(mod_id, mod_settings, init_scope)
end

-- This function should return the number of visible setting UI elements.
-- Your mod's settings wont be visible in the mod settings menu if this function isn't defined correctly.
-- If your mod changes the displayed settings dynamically, you might need to implement custom logic.
-- The value will be used to determine whether or not to display various UI elements that link to mod settings.
-- At the moment it is fine to simply return 0 or 1 in a custom implementation, but we don't guarantee that will be the case in the future.
-- This function is called every frame when in the settings menu.
function ModSettingsGuiCount()
    return mod_settings_gui_count(mod_id, mod_settings)
end

-- This function is called to display the settings UI for this mod. Your mod's settings wont be visible in the mod settings menu if this function isn't defined correctly.
function ModSettingsGui(gui, in_main_menu)
    mod_settings_gui(mod_id, mod_settings, gui, in_main_menu)

    --example usage:
    --[[
	GuiLayoutBeginLayer( gui )

	GuiBeginAutoBox( gui )

	GuiZSet( gui, 10 )
	GuiZSetForNextWidget( gui, 11 )
	GuiText( gui, 50, 50, "Gui*AutoBox*")
	GuiImage( gui, im_id, 50, 60, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )
	GuiZSetForNextWidget( gui, 13 )
	GuiImage( gui, im_id, 60, 150, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )

	GuiZSetForNextWidget( gui, 12 )
	GuiEndAutoBoxNinePiece( gui )

	GuiZSetForNextWidget( gui, 11 )
	GuiImageNinePiece( gui, 12368912341, 10, 10, 80, 20 )
	GuiText( gui, 15, 15, "GuiImageNinePiece")

	GuiBeginScrollContainer( gui, 1233451, 500, 100, 100, 100 )
	GuiLayoutBeginVertical( gui, 0, 0 )
	GuiText( gui, 10, 0, "GuiScrollContainer")
	GuiImage( gui, im_id, 10, 0, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )
	GuiImage( gui, im_id, 10, 0, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )
	GuiImage( gui, im_id, 10, 0, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )
	GuiImage( gui, im_id, 10, 0, "data/ui_gfx/game_over_menu/game_over.png", 1, 1, 0 )
	GuiLayoutEnd( gui )
	GuiEndScrollContainer( gui )

	local c,rc,hov,x,y,w,h = GuiGetPreviousWidgetInfo( gui )
	print( tostring(c) .. " " .. tostring(rc) .." " .. tostring(hov) .." " .. tostring(x) .." " .. tostring(y) .." " .. tostring(w) .." ".. tostring(h) )

	GuiLayoutEndLayer( gui )]]
    --
end

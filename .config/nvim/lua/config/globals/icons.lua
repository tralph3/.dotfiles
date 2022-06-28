_G.icons = {
    Class = '',
    Color = '',
    Constant = '',
    Constructor = '',
    Enum = '笠',
    EnumMember = '狀',
    Error = '',
    Event = '',
    Field = '',
    File = '',
    Folder = '',
    FolderEmpty = '',
    FolderOpen = '',
    Function = '',
    Hint = '',
    Info = '',
    Interface = 'שּׂ',
    Keyword = '',
    Method = '',
    Modified = '●',
    Module = '',
    Operator = '',
    Property = '',
    Readonly = 'ﰸ',
    Reference = '',
    Snippet = '麗',
    Struct = 'פּ',
    Text = '',
    TypeParameter = '',
    Unit = '塞',
    Value = '',
    Variable = '',
    Warning = '',
}

local icon_metatable = {
    __index = function(self, key)
        if self[key] == nil then
            error('attempting to access non-existant action '..key)
        else
            return self[key]
        end
    end,
}

setmetatable(_G.icons, icon_metatable)

-- TIME_STAMP   2018-02-20 18:18:22   v 0.9

--[[------------- I N S T A L L A T I O N   A N D   U S I N G   I N S T R U C T I O N --------------
Save the file.
At first, make an entry in your SciTEStartup.lua

LoadLuaFile("ShowHexColorFromCursor.lua", "C:\\Your Path\\with Backslash\\")


Select free command-numbers from your SciTEUser.properties.
Customize the following settings with this numbers.


# 13 Show HexColor RGB
command.name.13.*=Show RGB-Color From Cursor
command.13.*=dostring ShowHexColorFromCursor()
command.mode.13.*=subsystem:lua,savebefore:yes
command.shortcut.13.*=Ctrl+Shift+F11

# 14 Show HexColor BGR
command.name.14.*=Show BGR-Color From Cursor
command.14.*=dostring ShowHexColorFromCursor(true)
command.mode.14.*=subsystem:lua,savebefore:yes
command.shortcut.14.*=Ctrl+Alt+F11


Set the cursor in the Hex-value, press the hotkey to show the color as RGB or as BGR.
Above the value a Call tip appears. The background color corresponds to the hex value.
A possible alpha component is ignored.

[NEW]
Now be also recognized in au3 scripts, variables/constants which have an color assignment
inside the script or inside an include file from this script.
But it can only be one assignment per line.
If the assignment is inside a comment line or -block, it will ignored.
The assignment can also be build by using function(s) [from script or include files].
example:  "Local $COLOR = '0x' & Hex(Mod(@SEC, 2) ? Random(0,0x000FFF, 1) : Random(0x001000, 0xFFF000, 1), 8)"
But the functions must NOT CONTAIN any VARIABLES! This would require a recursive assignment search.
Impossible if variables get values only at runtime.
You can disable the search inside include files with an entry in SciTEUser.properties:
#~ "ShowHexColorFromCursor.lua", Dis/Enable search in Includes (0/1  NO/YES)
Get.Color.Assignment.Includes=0
The default value (without settings) is '1', enabled.
Includes in comments will ignored.

Be recognized AutoIt hex color code "0x12AB34" and also HTML hex color code "#12AB34".



PREVIEW FOR BACK AND FORE COLOR:
- Write in one line first the back color, than the fore color (i. e. as comment: "; 0xDEDEDE 0x000080") OR
  have this values inside a function call: "_AnyFunction($param1, $param2, 0xDEDEDE, $param3 0x000080)".
  If the order inside the call is reverse (first hex value is fore color), you can call the function with Flag "_fFore1st=true"
- No other color value may be included in this line. If any - the first and second color will used.
- Set the cursor in this line and hit the Hotkey.
- A Calltip appears with the back color and the text "FORE-COLOR" with color of the fore value.
- If only one color value was find in this line, this value will used as back color or, if Flag is "true", as fore color.
  In this cases the fore color is set to black and with Flag the back color is the default GUI back color "0xF0F0F0"

For use with AutoIt color values only.

To have both calls (w/wo flag) make two commands:

# 11 Preview Back and Fore Color / first color value is back color
command.name.11.*.au3=Preview Back and Fore Color
command.11.*.au3=dostring PreviewBackForeColor()
command.mode.11.*.au3=subsystem:lua,savebefore:yes
command.shortcut.11.*.au3=Ctrl+Shift+F12

# 16 Preview Fore and Back Color / first color value is fore color
command.name.16.*.au3=Preview Fore and Back Color
command.16.*.au3=dostring PreviewBackForeColor(true)
command.mode.16.*.au3=subsystem:lua,savebefore:yes
command.shortcut.16.*.au3=Ctrl+Alt+F12
--------------------------------------------------------------------------------------------------]]

local bDEBUG = false    -- set "true" to get debug output

------------------------------------------------------------ list object to manipulate simple tables
local objList = {
list    = {},
delall  = function(self) self.list = {} return self end,
addonce = function(self, _val, _casesense)     -- return true, if added
			  local	exists = function(_val, _casesense)
				  for k in pairs(self.list) do
					  if _casesense then if self.list[k] == _val then return true end
					  else if tostring(self.list[k]):upper() == tostring(_val):upper() then return true end
					  end
				  end return false
			  end
			  if not exists(_val, _casesense) then
				  table.insert(self.list, _val)
				  return true
			  end return nil
		  end,
new     = function(self, _obj)
			_obj = _obj or {}
			setmetatable(_obj, self)
			self.__index = self
			return _obj
		 end
}
--------------------------------------------------------------------------------------- /object list


--------------------------------------------------------------------------------------- object color
local objColor = {
	--------------------------------------------- variable will un/set if color-calltip is not/shown
	colortip_show = false,
	--------------------------------- user can disable search inside include files, default: enabled
	search_in_includes = true,
	------------------------------------------------------------------- the default calltip position
	calltips_pos_def = false,
	------------------------------------------------------------ the default calltip highlight color
	calltips_colorhlt_def = 0x0000FF,   -- BGR (red)
	---------------------------------------------------------- list with include storage directories
	lInclPathes = objList:new(),
	-------------------------------------------------- list/string with includes from current buffer
	lIncl = objList:new(), sIncl = '',
	---------------------------------------------------------------------------------------- pattern
	pattHex = '()0x([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])',
	pattHex2 = '[0-9a-fA-F][0-9a-fA-F]',
	pattHexEnd = '0x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]()',
	pattCS1 = '^#[Cc][Oo][Mm][Mm][Ee][Nn][Tt][Ss]%-[Ss][Tt][Aa][Rr][Tt]',
	pattCE1 = '^#[Cc][Oo][Mm][Mm][Ee][Nn][Tt][Ss]%-[Ee][Nn][Dd]',
	pattCS2 = '^#[Cc][Ss]',
	pattCE2 = '^#[Cc][Ee]',
	pattComment = '^%s*;',
	----------------------------------------------------------------------------------------- pathes
	sPathGetColorAU3,
	sFileResult,
	TEMPDIR = props['SciteUserHome']..'\\..\\..\\Temp',
	sAU3exe = props['SciteDefaultHome']..'\\..\\AutoIt3.exe',
	------------------------------------------------------------------------------------------------

	------------------------------------------------------------ set calltip values back to defaults
	SetCalltipsDefault = function(self)
		self.colortip_show = false
		scite.SendEditor(SCI_CALLTIPSETBACK, 0xFFFFFF)
		scite.SendEditor(SCI_CALLTIPSETFOREHLT, self.calltips_colorhlt_def)
		scite.SendEditor(SCI_CALLTIPSETPOSITION, self.calltips_pos_def)
		if bDEBUG then output:AppendText('> DEBUG: Calltips set to defaults') end
	end,
	---------------------------------------------------------------------------- /SetCalltipsDefault

	------------------------------------------------------------------------------ initialize values
	Startup = function(self)
		if props['Get.Color.Assignment.Includes'] == '0' then
			self.search_in_includes = false
		end

		if tonumber(props['calltips.set.above']) == 1 then
			self.calltips_pos_def = true
		end

		if props['calltips.color.highlight'] ~= '' then
			local colorhlt_user = myCallTips:BGR2Decimal(props['calltips.color.highlight'])
			if colorhlt_user ~= nil then self.calltips_colorhlt_def = colorhlt_user end
		end

		local sProp = props['openpath.$(au3)']
	    for w in sProp:gmatch('([^;]+)') do self.lInclPathes:addonce(w) end

		self.sPathGetColorAU3 = self.TEMPDIR..'\\ExecLineGetColor.au3'
		self.sFileResult = self.TEMPDIR..'\\ExecLineColor.txt'
	end,
	--------------------------------------------------------------------------------------- /Startup

	-------- check for comment line/block. Return "true/false, 0/1/-1" (0=comment line/1=#cs/-1=#ce)
    --                                               returned number for de/increase comment counter
	CheckComment = function(self, _s)
		local iMatch = _s:find(self.pattComment)
		if iMatch ~= nil then return true, 0 end
		iMatch = _s:find(self.pattCS1) or _s:find(self.pattCS2)
		if iMatch ~= nil then return true, 1 end
		iMatch = _s:find(self.pattCE1) or _s:find(self.pattCE2)
		if iMatch ~= nil then return true, -1 end
		return false, 0
	end,
	------------------------------------------------------------------------------------------------

	--------------------------------------------------------- read include files from current buffer
	IncludesFromBuffer = function(self)
		local sText, boolCmnt, countCmnt, n, incl = editor:GetText(), false, 0
		self.lIncl:delall()
		self.sIncl = ''
		for line in sText:gmatch('([^\r\n]+)') do
			boolCmnt, n = self:CheckComment(line)
			if boolCmnt then countCmnt = countCmnt + (n) end
			if not boolCmnt and countCmnt == 0 then     -- none comment line or block
				-- #include <abc.au3>
				incl = line:match("#[iI][nN][cC][lL][uU][dD][eE]%s-<([%w%s_.]+)>")
				if incl ~= nil then
					if self.lIncl:addonce(incl) then
						self.sIncl = self.sIncl..'#include <'..incl..'>\n'
						if bDEBUG then output:AppendText('> DEBUG: IncludesFromBuffer.Add "#include <'..incl..'>"\n') end
					end
				else
					-- #include 'abc.au3' or #include "abc.au3"
					_, incl = line:match("#[iI][nN][cC][lL][uU][dD][eE]%s-([\"'])([%w%s_.:\\]+)%1")
					if incl ~= nil then
						if incl:sub(1,1) == '\\' then incl = incl:sub(2,-1) end
						if self.lIncl:addonce(incl) then
							if incl:sub(2,2) == ':' then
								self.sIncl = self.sIncl..'#include "'..incl..'"\n'
								if bDEBUG then output:AppendText('> DEBUG: IncludesFromBuffer.Add "#include '.."'"..incl.."'"..'"\n') end
							else
								self.sIncl = self.sIncl..'#include "'..props['FileDir']..'\\'..incl..'"\n'
								if bDEBUG then output:AppendText('> DEBUG: IncludesFromBuffer.Add "#include '.."'"..props['FileDir']..'\\'..incl.."'"..'"\n') end
							end
						end
					end
				end
			end
		end
	end,
	---------------------------------------------------------------------------- /IncludesFromBuffer

	------------------------------------------ create the au3 file for executing the assignment line
	CreateAU3 = function(self, _sLineAssignment)
		local sTextAU3 = self.sIncl..
			'Global $sFileExport = @TempDir & "\\ExecLineColor.txt"\n'..
			'FileDelete($sFileExport)\n'..
			'Global $sLine = "'.._sLineAssignment..'"   ; line: $Variable = assignment\n'..
			'If $sLine = "NONE" Then Exit\n'..
			'Global $sExec = StringTrimLeft($sLine, StringInStr($sLine, "="))\n'..
			'Global $sColor = "0x" & Hex(Execute($sExec), 6)\n'..
			'If Not StringRegExp($sColor, "^0x[0-9A-F]{6}$") Then Exit\n'..
			'FileWrite($sFileExport, $sColor)\n'
		local fH = io.open(self.sPathGetColorAU3, 'w+')
		fH:write(sTextAU3)
		fH:close()
	end,
	------------------------------------------------------------------------------------- /CreateAU3

	--------------------------------- check, if file containing the assignment for selected variable
	FindAssignment = function(self, _path, _sSelection)
		local fH = io.open(_path)
		if fH ~= nil then
			local sRead, boolCmnt, countCmnt, n = fH:read('*all'), false, 0
			fH:close()
			for line in sRead:gmatch('([^\r\n]+)') do
				boolCmnt, n = self:CheckComment(line)
				if boolCmnt then countCmnt = countCmnt + (n) end
				if not boolCmnt and countCmnt == 0 then     -- none comment line or block
					if line:find(_sSelection..'%s*=') then
						if bDEBUG then output:AppendText('> DEBUG: Assignment line  "'..line..'"\n') end
						return line
					end
				end
			end
		end
		return nil
	end,
	-------------------------------------------------------------------------------- /FindAssignment

	-------------------------------------------------------------------- detects color from variable
	GetColorValueFromVariable = function(self, _sSelection, _iCursor, _var_beginPos, _var_endPos, _fBGR)
		local sLine = self:FindAssignment(props['FilePath'], _sSelection)
		if sLine == nil then                -- search inside include files
			-- do it not, if the user has disabled: "Get.Color.Assignment.Includes=0"  (default = 1 - enabled)
			if self.search_in_includes then
				self:IncludesFromBuffer()   -- get include files
				if #self.lIncl.list ~= 0 then
					-- open each include file, search line with assignment "_sSelection ="
					for i=1, #self.lIncl.list do
						if self.lIncl.list[i]:sub(2,2) == ':' then    -- include has full path, search only in this file
							sLine = self:FindAssignment(self.lIncl.list[i], _sSelection)
							if bDEBUG then output:AppendText('> DEBUG: Search "'.._sSelection..'" in "'..self.lIncl.list[i]..'"  --> '..tostring(sLine ~= nil)..'\n') end
						end
						if sLine == nil and self.lIncl.list[i]:find('\\') then     -- include has partial path, check first if exist in @ScriptDir
							sLine = self:FindAssignment(props['FileDir']..'\\'..self.lIncl.list[i], _sSelection)
							if bDEBUG then output:AppendText('> DEBUG: Search "'.._sSelection..'" in "'..props['FileDir']..'\\'..self.lIncl.list[i]..'"  --> '..tostring(sLine ~= nil)..'\n') end
						end
						if sLine == nil then                          -- include has filename only (or partial path),
                                                                      -- .. concanate all directories with this for searching
							for j=1, #self.lInclPathes.list do
								sLine = self:FindAssignment(self.lInclPathes.list[j]..'\\'..self.lIncl.list[i], _sSelection)
								if bDEBUG then output:AppendText('> DEBUG: Search "'.._sSelection..'" in "'..self.lInclPathes.list[j]..'\\'..self.lIncl.list[i]..'"  --> '..tostring(sLine ~= nil)..'\n') end
								if sLine == nil then
									sLine = self:FindAssignment(props['FileDir']..'\\'..self.lIncl.list[i], _sSelection)
									if bDEBUG then output:AppendText('> DEBUG: Search "'.._sSelection..'" in "'..props['FileDir']..'\\'..self.lIncl.list[i]..'"  --> '..tostring(sLine ~= nil)..'\n') end
								end
								if sLine ~= nil then break end
							end
						end
					end
				end
			end
		end

		if sLine == nil then
			sLine = 'NONE'
		else   -- trim characters right from assignment
			sLine = sLine:sub(1,sLine:match(self.pattHexEnd))
		end
		if bDEBUG then output:AppendText('> DEBUG: Search "'.._sSelection..'" \n> DEBUG: Result "'..sLine..'"\n') end
		-- create the au3-file for executing the assignment line, with 'NONE' - the last result file will delete
		self:CreateAU3(sLine)
		-- run the au3-file
		local sCmd = '"'..self.sAU3exe..'" /AutoIt3ExecuteScript "'..self.sPathGetColorAU3..'"'..' "'..sLine..'"'
		if shell then
			shell.exec(sCmd, nil, true, true)
		else
			os.execute('start "" '..sCmd)
		end

		-- check for result
		local fH = io.open(self.sFileResult)
		if fH == nil then
			scite.SendEditor(SCI_CALLTIPSHOW, _var_beginPos +1, (' NONE COLOR ASSIGNED! '))
			scite.SendEditor(SCI_CALLTIPSETHLT, 0, 22)
			scite.SendEditor(SCI_CALLTIPSETBACK, 0x33FFFF)
			scite.SendEditor(SCI_CALLTIPSETFOREHLT, 0x0000FF)
			scite.SendEditor(SCI_CALLTIPSETPOSITION, true)
			if bDEBUG then output:AppendText('> DEBUG: Set Calltip "'.._sSelection..'" --> "NONE COLOR ASSIGNED!"\n') end
		else
			local sValue = fH:read()
			fH:close()
			local R,G,B = sValue:match('('..self.pattHex2..')('..self.pattHex2..')('..self.pattHex2..')$')
			local iLen = _var_endPos - _var_beginPos -1
			scite.SendEditor(SCI_CALLTIPSHOW, _var_beginPos +1, (' '):rep(iLen))
			scite.SendEditor(SCI_CALLTIPSETHLT, 0, iLen)
			scite.SendEditor(SCI_CALLTIPSETPOSITION, true)
			if _fBGR == true then
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', R,G,B)))
				if bDEBUG then output:AppendText('> DEBUG: Set Calltip BGR  "'.._sSelection..'" --> "'..string.format('0x%s%s%s', R,G,B)..'"\n') end
			else
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', B,G,R)))
				if bDEBUG then output:AppendText('> DEBUG: Set Calltip RGB  "'.._sSelection..'" --> "'..string.format('0x%s%s%s', B,G,R)..'"\n') end
			end
		end
		self.colortip_show = true
		editor:SetSelection(_iCursor, _iCursor)
	end,
	--------------------------------------------------------------------- /GetColorValueFromVariable

	----------------------------------------- grabs the color value or variable from cursor position
	FromCursor = function(self, _fBGR)
		local iLen = 8
		local function isHexChar(_asc)
			local sChar = string.char(_asc)
			if sChar == '#' then iLen = 7 end
			if sChar:find('[#x0-9a-fA-F]') then return true else return false end
		end
		local cursor = editor.CurrentPos

		-- check if cursor is possible inside a variable
		local var_beginPos, var_endPos = cursor
		if string.char(editor.CharAt[cursor]) ~= '$' then   -- cursor is inside or behind the variable (if its a variable)
			editor:WordLeft()                               -- skip to the left
		end
		var_beginPos = editor.CurrentPos

		-- is it a variable?
		if string.char(editor.CharAt[var_beginPos]) == '$' then   -- now the cursor is in front of variable
			editor:WordRight()
			var_endPos = editor.CurrentPos
			editor:SetSelection(var_beginPos, var_endPos)
			local sSelection = editor:GetSelText()
			local iLenSel = sSelection:len()
			-- trim spaces on right site, if any
			sSelection = sSelection:gsub('%s+$','')
			var_endPos = var_endPos - (iLenSel - sSelection:len())
			if bDEBUG then output:AppendText('> DEBUG: Cursor on variable "'..sSelection..'"\n') end
			return self:GetColorValueFromVariable(sSelection, cursor, var_beginPos, var_endPos, _fBGR)
		end

		-- cursor inside hex value?
		local beginPos, endPos = cursor, cursor
		while isHexChar(editor.CharAt[beginPos-1]) do beginPos = beginPos - 1
		end
		while isHexChar(editor.CharAt[endPos]) do endPos = endPos + 1
		end
		if beginPos ~= endPos then
			if endPos - beginPos > iLen then
				editor:SetSelection(beginPos + iLen, beginPos)
			elseif endPos - beginPos == iLen then
				editor:SetSelection(endPos, beginPos)
			else
				return
			end
			local R,G,B = tostring(editor:GetSelText()):match('('..self.pattHex2..')('..self.pattHex2..')('..self.pattHex2..')$')
			if bDEBUG then output:AppendText('> DEBUG: Cursor on hex value\n') end
			scite.SendEditor(SCI_CALLTIPSHOW, beginPos+1, (' '):rep(iLen-1))
			scite.SendEditor(SCI_CALLTIPSETHLT, 0, iLen-1)
			scite.SendEditor(SCI_CALLTIPSETPOSITION, true)
			if _fBGR == true then
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', R,G,B)))
				if bDEBUG then output:AppendText('> DEBUG: Set Calltip BGR hex value --> "'..string.format('0x%s%s%s', R,G,B)..'"\n') end
			else
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', B,G,R)))
				if bDEBUG then output:AppendText('> DEBUG: Set Calltip RGB hex value --> "'..string.format('0x%s%s%s', B,G,R)..'"\n') end
			end
			self.colortip_show = true
		end
	end,
	------------------------------------------------------------------------------------ /FromCursor

	----------------------------------------------------------------------- show back and fore color
	PreviewBackForeColor = function(self, _fFore1st)
		local iBackCol, iForeCol = 0xF0F0F0, 0x000000
		local cursor = editor.CurrentPos
		local sLine, iColumn = editor:GetCurLine()
		local iLineStartPos = cursor - iColumn
		local tMatch, beginPos = {}, nil
		for s, r, g, b in sLine:gmatch(self.pattHex) do
			if beginPos == nil then beginPos = s end
			local t = {} t['R']=r t['G']=g t['B']=b
			table.insert(tMatch, t)
		end
		if #tMatch == 0 then
			if bDEBUG then output:AppendText('> DEBUG: Search back/fore color --> "FAILED"\n') end
			return
		elseif #tMatch == 1 then
			if _fFore1st == true then
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "ForeColor" --> "'..string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R)..'"\n') end
			else
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "BackColor" --> "'..string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R)..'"\n') end
			end
		else
			if _fFore1st == true then
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R))
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "ForeColor" --> "'..string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R)..'"\n') end
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "BackColor" --> "'..string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R)..'"\n') end
			else
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R))
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "ForeColor" --> "'..string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R)..'"\n') end
				if bDEBUG then output:AppendText('> DEBUG: Search back/fore color "BackColor" --> "'..string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R)..'"\n') end
			end
		end
		if bDEBUG then output:AppendText('> DEBUG: Set calltip back/fore color\n') end
		scite.SendEditor(SCI_CALLTIPSHOW, iLineStartPos + beginPos, ' FORE-COLOR ')
		scite.SendEditor(SCI_CALLTIPSETHLT, 0, 12)
		scite.SendEditor(SCI_CALLTIPSETBACK, iBackCol)
		scite.SendEditor(SCI_CALLTIPSETFOREHLT, iForeCol)
		self.colortip_show = true
	end
	-------------------------------------------------------------------------- /PreviewBackForeColor
}
-------------------------------------------------------------------------------------- /object color


---------------------------------------------------------------------------------- region EventClass
ShowColorEvt = EventClass:new(Common)

function ShowColorEvt:OnKey()
	if objColor.colortip_show then objColor:SetCalltipsDefault() end
end

function ShowColorEvt:OnDwellStart()
	if objColor.colortip_show then objColor:SetCalltipsDefault() end
end
--------------------------------------------------------------------------------- /region EventClass


-------------------------------------------------------------------------- function call redirection
function ShowHexColorFromCursor(_fBGR)
	objColor:FromCursor(_fBGR)
end  --> ShowHexColorFromCursor


function PreviewBackForeColor(_fFore1st)
	objColor:PreviewBackForeColor(_fFore1st)
end  --> PreviewBackForeColor
----------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------- run startup
objColor:Startup()
----------------------------------------------------------------------------------------------------

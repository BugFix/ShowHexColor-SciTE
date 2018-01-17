-- TIME_STAMP   2018-01-16 17:37:44   v 0.8

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
		scite.SendEditor(SCI_CALLTIPSETBACK, 0xFFFFFF)
		scite.SendEditor(SCI_CALLTIPSETFOREHLT, self.calltips_colorhlt_def)
		scite.SendEditor(SCI_CALLTIPSETPOSITION, self.calltips_pos_def)
		self.colortip_show = false
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
					if self.lIncl:addonce(incl) then self.sIncl = self.sIncl..'#include <'..incl..'>\n' end
				else
					-- #include 'abc.au3' or #include "abc.au3"
					_, incl = line:match("#[iI][nN][cC][lL][uU][dD][eE]%s-([\"'])([%w%s_.:\\]+)%1")
					if incl ~= nil then
						if incl:sub(1,1) == '\\' then incl = incl:sub(2,-1) end
						if self.lIncl:addonce(incl) then
							if incl:sub(2,2) == ':' then
								self.sIncl = self.sIncl..'#include "'..incl..'"\n'
							else
								self.sIncl = self.sIncl..'#include "'..props['FileDir']..'\\'..incl..'"\n'
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
						elseif self.lIncl.list[i]:find('\\') then     -- include has partial path, check first if exist in @ScriptDir
							sLine = self:FindAssignment(props['FileDir']..'\\'..self.lIncl.list[i], _sSelection)
						else                                          -- include has filename only (or partial path),
							if sLine ~= nil then break end            -- .. concanate all directories with this for searching
							for j=1, #self.lInclPathes.list do
								sLine = self:FindAssignment(self.lInclPathes.list[j]..'\\'..self.lIncl.list[i], _sSelection)
								if sLine ~= nil then break end
							end
						end
					end
				end
			end
		end

		if sLine == nil then sLine = 'NONE' end
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
			else
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', B,G,R)))
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
			scite.SendEditor(SCI_CALLTIPSHOW, beginPos+1, (' '):rep(iLen-1))
			scite.SendEditor(SCI_CALLTIPSETHLT, 0, iLen-1)
			scite.SendEditor(SCI_CALLTIPSETPOSITION, true)
			if _fBGR == true then
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', R,G,B)))
			else
				scite.SendEditor(SCI_CALLTIPSETBACK, tonumber(string.format('0x%s%s%s', B,G,R)))
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
			return
		elseif #tMatch == 1 then
			if _fFore1st == true then
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
			else
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
			end
		else
			if _fFore1st == true then
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R))
			else
				iForeCol = tonumber(string.format('0x%s%s%s', tMatch[2].B, tMatch[2].G, tMatch[2].R))
				iBackCol = tonumber(string.format('0x%s%s%s', tMatch[1].B, tMatch[1].G, tMatch[1].R))
			end
		end
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

function ShowColorEvt:OnKey(_keycode, _shift, _ctrl, _alt)
	if objColor.colortip_show then objColor:SetCalltipsDefault() end
end

function ShowColorEvt:OnDwellStart(_pos, _word)
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



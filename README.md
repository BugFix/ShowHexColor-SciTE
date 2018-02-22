# ShowHexColor-SciTE
Shows the color from values/variables as call tip

    This plugin is created for SciTE4AutoIt, but it's also possible to use with other SciTE        
    versions. May be, that some settings must be customized. Feel free to ask me, if you've        
    any problem.                                                                                   

    PREVIEW FOR SINGLE COLOR VALUE

    You get the color information of a color value (0x12AB34, #12AB34, $AU3_COLORVARIABLE)
    as calltip.
    Set the cursor in the hex value/color_variable, press the hotkey to show the color as 
    RGB or as BGR.
    Above the value a call tip appears. The background color corresponds to the hex value.
    A possible alpha component is ignored.
    The assignment for color variables in au3 scripts, will searched in the opened script.    
    If not found, the include files will scanned for this.
    
    PREVIEW FOR BACK AND FORE COLOR (au3 script only)

    If you've 2 color values in one line, set the cursor in this line and press the hotkey.
    A Calltip appears with the back color and the text "FORE-COLOR" with color of the fore
    value. By default, the first color is read as back color and the 2nd one as fore color.
    The color values may also be in a comment: 
        "; 0xDEDEDE 0x000080"
    or you have this values inside a function call: 
        "_AnyFunction($param1, $param2, 0xDEDEDE, $param3, 0x000080)"
    or in an assignment:
        "Global $iBGColor = 0xFFFFFF, $iForeCol = 0x222280".
    If the order inside the line is reverse (first hex value is fore color), you can call 
    the function with flag "_fFore1st=true".
    No other color value may be included in this line. If any - the first and second color
    will used.
    If only one color value was find in this line, this value will used as back color or, 
    if flag is "true", as fore color. In this cases the fore color is set to black and with 
    flag the back color is the default GUI back color "0xF0F0F0".
    
    DISAPPEAR THE CALLTIP
    
    You can disappear the calltip instantly with any key or mouse move with set variable in line 75:
    local bCALLTIP_END_ANYKEY = true 
    The default is false and uses the default behavior of terminating the calltip.. 

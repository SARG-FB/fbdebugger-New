'menu handle for fbdebugger_new
''dbg_menu.bas
private sub menu_action(poption as integer)
	select case poption
		case MNABOUT
            If messbox("FB DEBUGGER "+ver3264,"Debugger for FreeBASIC (Windows/Linux 32/64)"+Chr(13)+Chr(13)+fbdebuggerversion+" / "+__DATE__+Chr(13)+"(C) L.GRAS  debug @ aliceadsl . fr"+Chr(13)+Chr(13) _
               +"Select YES for accessing to the dedicated page on the forum"+Chr(13)+"http://www.freebasic.net/forum/viewtopic.php?f=8&t=13935",MB_YESNO)=6 Then
               	Shell "start http://www.freebasic.net/forum/viewtopic.php?f=8""&t""=13935" 
            endif
        case else
        	messbox("feature not implemented","sorry option="+str(poption))
    end select
end sub
'menu handle for fbdebugger_new
''dbg_menu.bas

'==================================================
private sub menu_action(poption as integer)
	select case poption
		case MNABOUT
            If messbox("FB DEBUGGER "+ver3264,"Debugger for FreeBASIC (Windows/Linux 32/64)"+Chr(13)+Chr(13)+fbdebuggerversion+" / "+__DATE__+Chr(13)+"(C) L.GRAS  debug @ aliceadsl . fr"+Chr(13)+Chr(13) _
               +"Select YES for accessing to the dedicated page on the forum"+Chr(13)+"http://www.freebasic.net/forum/viewtopic.php?f=8&t=13935",MB_YESNO)=6 Then
               	Shell "start http://www.freebasic.net/forum/viewtopic.php?f=8""&t""=13935" 
            endif
            
        case MNTHRDAUT '' multi thread auto
		    var threadaut=0   
			For ithd As Integer =0 To threadnb
				If thread(ithd).exc Then
					threadaut+=1
				EndIf
			Next
			If threadaut<2 Then
				messbox("Automatic execution","Not enough selected thread so normal auto")
			EndIf
			runtype=RTAUTO
			but_enable()
			bcktrk_close
			thread_resume()            
        case else
        	messbox("feature not implemented","sorry option="+str(poption))
    end select
end sub

private sub gadget_action(igadget as LONG)
	select case igadget
	''Dump memory
	
		''
		case GDUMPADR
		messbox("changing memory address","need to remove me") 
		case GDUMPAPPLY
			var txt=GetGadgetText(GDUMPADR)
			dim as integer newad=Valint(txt)
            If readProcessMemory(dbghand,Cast(LPCVOID,newad),strptr(txt),1,0)=0 Then
				messbox("Error","Invalid Memory Address")              
            Else
                dumpadr=newad
                dump_sh()
            End If
            
		''column size in bytes
		case GDUMPSIZE
			messbox("list box dump size",str(GetItemListBox(GDUMPSIZE))+" ="+GetListBoxText(GDUMPSIZE,GetItemListBox(GDUMPSIZE)))
			dumptyp=GetItemListBox(GDUMPSIZE)
			dump_set()
            dump_sh()
            
		''moving -/+ the first address by cell/line/page 
		''''case GDUMPCL ''
		''''case GDUMPCP
		''''case GDUMPLL
		''''case GDUMPLP
		''''case GDUMPPL
		''''case GDUMPPP
		
		''adds to newadr,watched, break on mem, shw/exp based on first address
		''''case GDUMPNEW
		''''case GDUMPWCH
		''''case GDUMPBRK
		''''case GDUMPSHW
		
		''
		''''case GDUMPPTRNO
		''''case GDUMPPTR1
		''''case GDUMPPTR2

		''''case GDUMPDEC 751
		''''case GDUMPHEX 752


		''''case GDUMPSGN 754
		''''case GDUMPUSGN 755		
		
		
		
		case else
        	messbox("feature not implemented","sorry option="+str(igadget))
	''others
	End Select
end sub
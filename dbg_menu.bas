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
			
		case MNEXEFILE0 to MNEXEFILE9
			restart(poption-MNEXEFILE0)
			
		case MNSHWLOG
			log_show()
			
		case MNDELLOG
			log_del()
			
		Case MNWINMSG
            winmsg()
            
        Case MNSHWBDH
            dechexbin()	
			
		Case MNGOTO 'goto line
			line_goto()
			
      	Case MNLINEADR 'line address (in memory)
         	line_adr()
         	
		Case MNWCHDEL'delete watch on cursor from watched
         		watch_del(watch_find())
         		
        Case MNWCHDALL'delete all watches
   			if messbox("Delete watched vars","Delete all",MB_YESNO)=RETYES Then
   				watch_del()
   			EndIf
   			
		Case MNWCHVAR'show in proc/var window from watched
			watch_sel(watch_find())
				
		Case MNWCHTTGL 'toggle trace
			watch_trace(watch_find()) 
			
		Case MNWCHTTGA 'cancel all traces
			watch_trace()	
			
		Case MNLOCPRC 'locate proc
			proc_loc()
			
		Case MNCALLINE 'locate calling line
		   proc_loccall(1)
		   
		Case MNPBCKTRK'backtracking from proc/var
			proc_loccall(2)
			
		Case MNPCHNING'chaining from proc/var
			proc_loccall(3)
			
		Case MNSHWPROC 'locate proc in proc/var treeview
			thread_procloc(1)
			
		Case MNSHPRSRC 'locate proc in source
			thread_procloc(2)
						
		Case MNTBCKTRK 'backtracking (from thread)
			thread_procloc(3)
			
		Case MNTCHNING 'chaining (from thread)
			thread_procloc(4) 
							
		Case MNPRCRADR 'information about running proc
			thread_procloc(5)
			
		Case MNTHRDCHG 'change next executed thread
			thread_change()
			
		Case MNTHRDKLL 'kill a thread
			thread_kill()	
		
		
			
        case else
        	messbox("Menu feature not implemented","sorry option="+str(poption))
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
		
		Case GBRKCLOSE
	   		hidewindow(hbrkbx,1)
	   		
	   	Case GBRKDEL01 to GBRKDEL10 ''delete one breakpoint
	         brk_del(igadget-GBRKDEL01+1)
	         hidegadget(igadget,KHIDE)
	         hidegadget(igadget-GBRKDEL01+GBRKDSB01,KHIDE)
	         hidegadget(igadget-GBRKDEL01+GBRKLINE01,KHIDE)
	         If brknb=0 Then 
	            hidewindow(hbrkbx,1) ''no more breakpoint so close the window 
	         EndIf

		Case GBRKLINE01 to GBRKLINE10 ''click on text
			source_change(brkol(igadget-GBRKLINE01+1).isrc)
	        line_display(brkol(igadget-GBRKLINE01+1).nline-1)
	        
		Case GBRKDSB01 to GBRKDSB10 ''enable/disable
	      	If brkol(igadget-GBRKDSB01+1).typ>2 Then 
	      		brkol(igadget-GBRKDSB01+1).typ-=2
	      		SetGadgetText(igadget,"DSB")
	      	Else
	      		brkol(igadget-GBRKDSB01+1).typ+=2
	      		SetGadgetText(igadget,"ENB")
	      	EndIf
	      	brk_marker(igadget-GBRKDSB01+1)
	      	
	   	Case GBRKDELALL    ''Delete all
	        	For ibrk As Byte=1 To brknb
					brk_del(ibrk)
	        	Next
	        	hidewindow(hbrkbx,1)
	        	
	   	Case GBRKDISABLE ''disable all
	      	For ibrk As integer =1 To brknb
				If brkol(ibrk).typ<3 Then 
					brkol(ibrk).typ+=2
					brk_marker(ibrk)
					SetGadgetText(GBRKDSB01+ibrk-1,"ENB")
				EndIf
	      	Next
	      	
	   	Case GBRKENABLE ''enable all	
			For ibrk As integer =1 To brknb
				If brkol(ibrk).typ>2 Then 
					brkol(ibrk).typ-=2
					brk_marker(ibrk)
					SetGadgetText(GBRKDSB01+ibrk-1,"DSB")
				EndIf
			Next
		
		case else
        	messbox("Gadget feature not implemented","sorry option="+str(igadget))
	''others
	End Select
end sub
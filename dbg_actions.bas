''button/menu/gadget handle for fbdebugger_new
''dbg_actions.bas

'==================================================
private sub menu_action(poption as integer)
		dim as INTEGER ivar,typ,pt

	select case poption
		case MNABOUT
            If messbox("FB DEBUGGER "+ver3264,"Debugger for FreeBASIC (Windows/Linux 32/64)"+Chr(13)+Chr(13)+fbdebuggerversion+" / "+__DATE__+Chr(13)+"(C) L.GRAS  debug @ aliceadsl . fr"+Chr(13)+Chr(13) _
               +"Select YES for accessing to the dedicated page on the forum"+Chr(13)+"http://www.freebasic.net/forum/viewtopic.php?f=8&t=13935",MB_YESNO)=6 Then
               	Shell "start http://www.freebasic.net/forum/viewtopic.php?f=8""&t""=13935"
            endif

        case MNSETTINGS
			hidewindow(hsettings,KSHOW)

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
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

		case MNEXEFILE0 to MNEXEFILE9
			restart(poption-MNEXEFILE0)

		case MNSHOWLOG
			'setgadgettext(GLOG,vlog)
			hidewindow(hlogbx,KSHOW)

		case MNRESETLOG
			if messbox("Reset log","Are you sure ?",MB_YESNO)=IDYES then
				vlog=""
			EndIf

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

		Case MNPCHNING'chaining from proc/var
			call_chain(threadcur)

		Case MNSHWPROC 'locate proc in proc/var treeview
			thread_procloc(1)

		Case MNSHPRSRC 'locate proc in source
			thread_procloc(2)

		Case MNTCHNING ''call chain (from thread)
			call_chain(ValInt(Mid(GetTextTreeView(GTVIEWTHD,thread_find()),10,6)))

		Case MNPRCRADR 'information about running proc
			thread_procloc(5)

		Case MNTHRDCHG 'change next executed thread
			thread_change()

		Case MNTHRDKLL 'kill a thread
			thread_kill()

		Case MNLISTPROCESS
		   process_list()

		Case MNLISTDLL
		   dll_list()

		Case MNLISTENUM
			enum_list()

		Case MNFRTIMER
			messbox("Fast run timer","Elapsed Time : "+Str(fasttimer))
		Case MNSETBRK 'set breakpoint
		   brk_set(1)

		Case MNSETBRT 'set tempo brkp
		   brk_set(2)

		Case MNSETBRKC 'set brkp with counter
		   brk_set(3)

		Case MNCHGBRKC 'change value brkp with counter
		   brk_set(7)

		Case MNRSTBRKC 'reset brkp with counter
		   brk_set(8)

		Case MNBRKENB 'enable/disable brkp
		   brk_set(4)

		Case MNMNGBRK
			brk_manage()

		Case MNCURSOR 'run to cursor
		   brk_set(9)

		Case MNEXEMOD 'modify execution from cursor
			exe_mod()

		Case MNSHWVAR
			messbox("Feature not yet implemented","var_tip(PROCVAR)")
			'var_tip(PROCVAR)

		Case MNSETWVAR
			messbox("Feature not yet implemented","var_tip(WATCHED)")
			'var_tip(WATCHED)

		Case MNFNDTXT
			messbox("Feature not yet implemented","findtext ")
				'If hfindbx=0 Then 'findtext not active ?
					'stext=wtext() 'selected text or ascii text near cursor
					'fb_Dialog(@find_box,"Findtext (Circular)",windmain,283,25,100,25)
				'End If

		Case MNASMLINE 'dissassembly line in source
			messbox("Feature not yet implemented","dissassemble(KLINE)")
			'dissassemble(KLINE)

		Case MNASMPRCL 'dissassembly proc from line in source
			messbox("Feature not yet implemented","dissassemble(KSPROC)")
			'dissassemble(KSPROC)

		Case MNASMREGS 'show processor registers for current thread
			messbox("Feature not yet implemented","show_regs()")
			'show_regs()

		Case MNLSTVARS'list selected var in log
			procvar_list()

		Case MNPTDUMP 'dump pointed data
			var_dump(htviewvar,1)

		Case MNFNDVAR 'find text proc or var in proc/var tree
			messbox("Feature not yet implemented","procvar_find()")
			'If hfindbx=0 Then 'findtext not active ? also used in source
				''procvar_find()
			'End If

		Case MNSELIDX 'select index in proc/var
			index_sel()

		Case MNWCHDMP'dump for watched
			var_dump(htviewwch)

		Case MNWCHSTG'shw string from watched
			string_sh(GTVIEWWCH)

		Case MNWCHSHW'shw/exp from watched
			messbox("Feature not yet implemented","shwexp_new(htviewwch)")
			'shwexp_new(htviewwch)

		Case MNWCHEDT'edit from watched
			messbox("Feature not yet implemented","edit_box")
			'If var_find2(GTVIEWWCH)<>-1 Then 'not local non-existent
			edit.src=KEDITWCH
			'edit_fill()

		Case MNSETWTCH 'set watched first free slot
			If var_find2(htviewvar)<>-1 Then watch_set()

		Case MNTRCKIDX0 To MNTRCKRST
			messbox("Feature not yet implemented","array_tracking(poption-MNTRCKIDX0)")
			'array_tracking(poption-MNTRCKIDX0)

		Case MNSETWTTR 'set watched + trace
			watch_addtr()

		Case MNVARDMP  'var dump
		  var_dump(htviewvar)

		Case MNSHSTRG  'show z/w/string
			string_sh(GTVIEWVAR)

		Case MNSHCHAR
			messbox("Feature not yet implemented","char_sh(htviewvar)")
			'char_sh(htviewvar)

		Case MNCHGZSTR
			messbox("Feature not yet implemented","zstringbyte_exchange()")
			'zstringbyte_exchange()

		Case MNVAREDT  ''edit var value
			ivar=var_find()
			if ivar>0 then
				typ=vrb(vrr(ivar).vr).typ
				pt=vrb(vrr(ivar).vr).pt
			else
				ivar=abs(ivar)
				typ=cudt(vrr(ivar).vr).typ
				pt=cudt(vrr(ivar).vr).pt
			EndIf
			edit_fill(GetTextTreeView(GTVIEWVAR,GetItemTreeView(GTVIEWVAR)),vrr(ivar).ad,typ,pt,KEDITVAR)

		Case MNSHWEXP  'show and expand variables
			shwexp_new(GTVIEWVAR)

		Case MNVARBRK  'break on var value
			brkv_set(1)

		case MNVARCOLI
			CollapseTreeViewItem(GTVIEWVAR,GetItemTreeView(GTVIEWVAR))

		case MNVAREXPI
			ExpandTreeViewItem(GTVIEWVAR,GetItemTreeView(GTVIEWVAR),1)

		case MNVARCOLA
			CollapseTreeViewItemALL(GTVIEWVAR)

		case MNVAREXPA
			ExpandTreeViewItemALL(GTVIEWVAR)

		Case MNENBPRC
			proc_enable()

		Case MNSORTPRC
			procsort=1-procsort
			proc_sh() 'toggle type of sort and update display

		Case MNASMPRC
			messbox("Feature not yet implemented","dissassemble(KSPROC)")
			'dissassemble(KSPROC)

		Case MNEXCLINE 'show line
			thread_execline(1)

		Case MNCREATHR 'show line creating thread
			thread_execline(2)

		Case MNTHRDLST
			thread_list()

		Case MNFNDTXUP
			messbox("feature not yet implemented","fb_find(0,sfind)")
			'fb_find(0,sfind)

		Case MNFNDTXDW
			messbox("feature not yet implemented","fb_find(1,sfind)")
			'fb_find(1,sfind)

		Case MNVARBRK 'update break on var
			If brkv.adr<>0 Then brkv_set(2)

	'=============================================================
        case else
        	messbox("Menu feature not implemented","sorry option="+str(poption)+" --> enum="+enumdef(poption))
    end select
end sub
'============================================
'' executes action for gadget
'============================================
private sub gadget_action(igadget as LONG)
	dim as INTEGER decal

	select case igadget
		Case GCURRENTLINE
			messbox("Click on Current line","Jumping to the file/line")
			linecur_display()


		case GTVIEWVAR
		case GTVIEWWCH

		''Dump memory
		case GDUMPADR
		''not used messbox("changing memory address","need to remove me")

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
			''forcing type byte/short/long/longint
			select case GetItemListBox(GDUMPSIZE)
				Case 0
					dumptyp=2
				Case 1
					dumptyp=5
				Case 2
					dumptyp=1
				Case 3
					dumptyp=9
			End Select
			dump_set()
            dump_sh()

		'''moving -/+ the first address by cell/line/page
		case GDUMPCL ''column -1
			dumpadr-=udt(dumptyp).lg
			dump_sh()

		case GDUMPCP ''column +1
			dumpadr+=udt(dumptyp).lg
			dump_sh()

		case GDUMPLL ''line -1
			dumpadr-=16
			dump_sh()

		case GDUMPLP ''line +1
			dumpadr+=16
			dump_sh()

		case GDUMPPL ''page -1
			dumpadr-=320
			dump_sh()

		case GDUMPPP ''page +1
			dumpadr+=320
			dump_sh()

		case GDUMPCLIP
			dump_extract()

		''adds to newadr,watched, break on mem, shw/exp based on first address
		'case GDUMPNEW
		'case GDUMPWCH
		'case GDUMPBRK
		'case GDUMPSHW
		'
		''
		'case GDUMPPTRNO
		'case GDUMPPTR1
		'case GDUMPPTR2

		case GDUMPMEM
			dump_cell() ''click on a cell or header

		case GDUMPDECHEX
			if dumpbase=50 then
				dumpbase=0
				setgadgettext(GDUMPDECHEX,">Hex")
			else
				dumpbase=50
				setgadgettext(GDUMPDECHEX,">Dec")
			end if
			dump_sh()

		case GDUMPSIGNED
			dump_signed()

		case GDUMPEDIT
			dump_edit()

		case GDUMPBASEADR
			dump_baseadr()

		''case GDUMPTYPE ''nothing to do

		case GBRKVDEL
			brkv_set(0) ''cancel the break on var

		case GBRKVOK ''apply
			brkv_update()

		case GBRKVCOND
		case GBRKVVALUE


		case GFILESEL
			MessBox("Jumping to file step 00="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
        	if GetItemComboBox(GFILELIST)<>-1 then
				MessBox("Jumping to file step 01="+str(GetItemComboBox(GFILELIST)),str(PanelGadgetGetCursel(GSRCTAB)))
        		if GetItemComboBox(GFILELIST)<>PanelGadgetGetCursel(GSRCTAB) then
        			MessBox("Jumping to file before ="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
	        		PanelGadgetSetCursel(GSRCTAB,GetItemComboBox(GFILELIST))
	        		MessBox("Jumping to file afetr ="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
        			source_change(GetItemComboBox(GFILELIST))
        		else
        			MessBox("File already displayed",source(GetItemComboBox(GFILELIST)))
        		endif
        	else
        		messbox("Select a file", "before clicking on the button")
        	endif

		case GSRCTAB
			source_change(PanelGadgetGetCursel(GSRCTAB))

		case GRIGHTTABS
			hidewindow(hdumpbx,KHIDE) ''hidding by default
			select case PanelGadgetGetCursel(GRIGHTTABS)
				case TABIDXVAR
					'hidewindow(htabvar,KSHOW)
					'HideGadget(GTVIEWVAR,0)
				case TABIDXPRC
					'hidewindow(htabprc,KSHOW)
					'HideGadget(GTVIEWPRC,0)
					proc_sh()
				case TABIDXTHD
					'hidewindow(htabthd,KSHOW)
					'HideGadget(GTVIEWTHD,0)
				case TABIDXWCH
					'hidewindow(htabwch,KSHOW)
					'HideGadget(GTVIEWWCH,0)
				case TABIDXDMP
					'hidewindow(htabdmp,KSHOW)
					'HideGadget(GDUMPMEM,0)
			end select

		case GTVIEWPRC

		case GFILELIST ''nothing to execute with file combo

		Case GBRKCLOSE
	   		hidewindow(hbrkbx,KHIDE)

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

		case GEDTOK '' edit var or mem
			edit_update()

		case GEDTCANCEL
			hidewindow(heditbx,KHIDE)

		case GEDTPTDEDT
			edit_fill("Pointed value="+edit.ptdval,edit.ptdadr,edit.typ,0,KEDITPTD)

		case GEDTVALUE ''do nothing

		case GSCINTILLA

		case GLOGOFF
			logtyp=KLOGOFF
		case GLOGON
			logtyp=KLOGON
		case GLOGCONT
			logtyp=KLOGCONT
			hidewindow(hlogbx,KSHOW)

		case GVERBOSE
			flagverbose=1-flagverbose
			var_sh()
			proc_sh()

		case GAUTODELAY

		case GCMDLPARAM ''data used when closing settings box

		case GIDXTABLE

		case GIDXAPPLY
			index_apply()

		case GIDXINC
			For k As Integer = indexdata.nbdim-1 To 0 Step -1
				If indexdata.vubound(k)<>vrr(indexdata.indexvar).ix(k) Then
					setGadgetState(GIDXUP1+k,vrr(indexdata.indexvar).ix(k)+1) ''increase next dimension
					For j As Integer = k+1 To indexdata.nbdim-1
						setGadgetState(GIDXUP1+j,indexdata.vlbound(j)) ''init dimension
					Next
					index_apply()
					exit sub
				EndIf
			Next
	   		For k As Integer = 0 To indexdata.nbdim 'loop so all index at the beginning
	     		setGadgetState(GIDXUP1+k,indexdata.vlbound(k))
	     	Next
	     	index_apply()

	    case GIDXDEC
			For k As Integer = indexdata.nbdim-1 To 0 Step -1
				If indexdata.vubound(k)<>vrr(indexdata.indexvar).ix(k) Then
					setGadgetState(GIDXUP1+k,vrr(indexdata.indexvar).ix(k)-1) ''decrease next dimension
					For j As Integer = k+1 To indexdata.nbdim-1
						setGadgetState(GIDXUP1+j,indexdata.vlbound(j)) ''init dimension
					Next
					index_apply()
					exit sub
				EndIf
			Next
	   		For k As Integer = 0 To indexdata.nbdim 'loop so all index at the beginning
	     		setGadgetState(GIDXUP1+k,indexdata.vlbound(k))
	     	Next
	     	index_apply()

	    case GIDXCOLP
			if indexdata.curidx(1)+KCOLMAX-1<indexdata.vubound(1) then
				indexdata.curidx(1)+=1
				indexdata.adr+=indexdata.size
				index_update()
			end if

		Case GIDXBLKP
			if indexdata.curidx(1)+KCOLMAX-1<indexdata.vubound(1) then
				if indexdata.curidx(1)+KCOLMAX-1+KCOLMAX<=indexdata.vubound(1) then
					indexdata.curidx(1)+=KCOLMAX
					indexdata.adr+=indexdata.size*KCOLMAX
				else
					var temp=indexdata.curidx(1)
					indexdata.curidx(1)=indexdata.vubound(1)-KCOLMAX+1
					indexdata.adr+=indexdata.size*(indexdata.curidx(1)-temp)
				EndIf
				index_update()
			end if

		Case GIDXCOLL
			if indexdata.curidx(1)>indexdata.vlbound(1) then
				indexdata.curidx(1)-=1
				indexdata.adr-=indexdata.size
				index_update()
			EndIf

		Case GIDXBLKL
			if indexdata.curidx(1)>indexdata.vlbound(1) then
				if indexdata.curidx(1)-KCOLMAX-1>=indexdata.vlbound(1) then
					indexdata.curidx(1)-=KCOLMAX
					indexdata.adr-=indexdata.size*KCOLMAX
				else
					var temp=indexdata.curidx(1)
					indexdata.curidx(1)=indexdata.vlbound(1)
					indexdata.adr-=indexdata.size*(temp-indexdata.curidx(1))
				EndIf
				index_update()
			EndIf

		Case GIDXROWP
			if indexdata.curidx(0)+KLINEMAX-1<indexdata.vubound(0) then
				indexdata.curidx(0)+=1
				indexdata.adr+=indexdata.sizeline
				index_update()
			end if

		Case GIDXPAGEP
			if indexdata.curidx(0)+KLINEMAX-1<indexdata.vubound(0) then
				if indexdata.curidx(0)+KLINEMAX-1+KLINEMAX<=indexdata.vubound(0) then
					indexdata.curidx(0)+=KLINEMAX
					indexdata.adr+=indexdata.sizeline*KLINEMAX
				else
					var temp=indexdata.curidx(0)
					indexdata.curidx(0)=indexdata.vubound(0)-KLINEMAX+1
					indexdata.adr+=indexdata.sizeline*(indexdata.curidx(0)-temp)
				EndIf
				index_update()
			EndIf


		Case GIDXROWL
			if indexdata.curidx(0)>indexdata.vlbound(0) then
				indexdata.curidx(0)-=1
				indexdata.adr-=indexdata.sizeline
				index_update()
			end if

		Case GIDXPAGEL
			if indexdata.curidx(0)>indexdata.vlbound(0) then
				if indexdata.curidx(0)-KLINEMAX-1>=indexdata.vlbound(0) then
					indexdata.curidx(0)-=KLINEMAX
					indexdata.adr-=indexdata.sizeline*KLINEMAX
				else
					var temp=indexdata.curidx(0)
					indexdata.curidx(0)=indexdata.vlbound(0)
					indexdata.adr-=indexdata.sizeline*(temp-indexdata.curidx(0))
				end if
				index_update()
			end if


		Case GIDXAUTO
			if getgadgetstate(GIDXAUTO)=1 then
				indexdata.autoupd=TRUE
			Else
				indexdata.autoupd=FALSE
			end if

	   Case GIDXWIDTH
			updown_check(GIDXWIDTH,50,200)

			var wd =getgadgetstate(GIDXWIDTH)
			for icol as integer =1 to iif(indexdata.vubound(1)-vrr(indexdata.indexvar).ix(1)>29,30,indexdata.vubound(1)-vrr(indexdata.indexvar).ix(1)+1)
				SetColumnWidthListView(GIDXTABLE,icol,wd)
			next

		case GIDXUP1 to GIDXUP1+4
			updown_check(igadget,indexdata.vlbound(igadget-GIDXUP1),indexdata.vubound(igadget-GIDXUP1))

		case GSHWWCH
			var_find2(GadgetID(GTVIEWSHW))
			varfind.iv=-1
			varfind.nm=varfind.nm+" ["+Str(varfind.ad)+"]<"
			watch_set()

		case GSHWDMP
			var_dump(GadgetID(GTVIEWSHW))

		case GSHWEDT
			var ivar=var_find2(GadgetID(GTVIEWSHW))
			edit_fill(GetTextTreeView(GTVIEWSHW,vrp(ivar).tl),vrp(ivar).ad,vrp(ivar).ty,vrp(ivar).pt,KEDITSHW)
			shwexp_update() ''update after editing

		case GSHWSTR
			string_sh(GTVIEWSHW)

		case GSHWNEW  ''replaces the current data
			var_find2(GadgetID(GTVIEWSHW))
			setwindowtext(hshwexpbx,"Show/expand : "+varfind.nm)
			shwexp_update(varfind.nm,varfind.ad,varfind.ty,varfind.pt,varfind.iv)

		case GSHWDEC 'index - 1
			If vrp(1).pt=0 then
				if shwexp.curidx>shwexp.minidx then
					shwexp.curidx-=1
					setgadgettext(GSHWCUR,"Index cur : "+Str(shwexp.curidx))
					vrp(1).ad-=udt(vrp(1).ty).lg
					shwexp_update()
				end if
			Else
				messbox("Move","Not possible with pointer"+Chr(13)+"Select (new shw/exp) the pointed value")
			EndIf

		case GSHWINC 'index + 1
			If vrp(1).pt=0 then
				if shwexp.curidx<shwexp.maxidx then
					shwexp.curidx+=1
					setgadgettext(GSHWCUR,"Index cur : "+Str(shwexp.curidx))
					vrp(1).ad+=udt(vrp(1).ty).lg
					shwexp_update()
				end if
			Else
				messbox("Move","Not possible with pointer"+Chr(13)+"Select (new shw/exp) the pointed value")
			EndIf

		case GSHWSET 'set index
			var inputval=input_bx("Show / expand","Type the new value of index",Str(shwexp.curidx),1)
			if ValInt(inputval)>=shwexp.minidx and valint(inputval)<=shwexp.maxidx then
				vrp(1).ad+=(ValInt(inputval)-shwexp.curidx)*udt(vrp(1).ty).lg
				shwexp.curidx=ValInt(inputval)
				setgadgettext(GSHWCUR,"Index cur : "+Str(shwexp.curidx))
				shwexp_update()
			end if

		Case GSHWUPD 'update
			if shwexp_checkarr <>-1 then 'no redim or smaller redim otherwise delete and create a new box
				''vrp(1).ad could be changed in checkarr so reassign here
				shwexp_update()
			endif

		case GCCHAIN
			'messbox("ITEM="+str(GetItemListView()),"SUBITEM ="+str(GetSubItemListView())+" GetColumnListView="+str(GetColumnListView()))
			var iline=GetItemListView()
			if iline=0 then
				thread_execline(1,cchainthid) 'show next executed line of thread
			else
				if GetTextItemListView(GCCHAIN,iline,0)<>"" then
					source_change(proc(procr(procrsav(iline)).idx).sr)
					line_display(rline(procr(procrsav(iline)).cl).nu-1,1)
				end if
			end if

		case GLOG ''nothing to do

		case GEDITOR ''nothing to do

		case else
        	messbox("Gadget feature not implemented","sorry option="+str(igadget)+" --> enum="+enumdef(igadget))
	''others
	End Select
end sub
'==============================================
''
'==============================================
private sub select_file()
	#Ifdef __fb_win32__
		var selfile= OpenFileRequester("Select exe file","C:\","Exe files (*.exe)"_
		+Chr(0)+"*.exe"+Chr(0))
	#else
		var selfile= OpenFileRequester("Select exe file","","Exe files)"_
		+Chr(0)+"*.*"+Chr(0))
	#endif
	If selfile="" then
		messbox("No file selected","")
		exename=""
		exit sub
	else
		exename=selfile
		messbox("File selected",exename)
	end if

	if check_bitness(exename)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub

	reinit ''reinit all except GUI parts

    exe_sav(exename,"")

	#Ifdef __fb_win32__
		If ThreadCreate(@start_pgm)=0 Then
			messbox("ERROR unable to start the thread managing the debuggee","Debuggee not running")
		endif
	#else
		messbox("feature to be coded linux","after selecting file")
	#endif
end sub
'==============================================================
'' handles actions for each button
'==============================================================
private sub button_action(button as integer)
	select case button
		case IDBUTSTEP 'STEP
			stopcode=0
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

		case IDBUTSTEPP 'STEP+ over
			procin=procsk
			runtype=RTRUN
			but_enable()
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

		Case IDBUTSTEPM 'STEP- out
			If (threadcur<>0 andalso proc_find(thread(threadcur).id,KLAST)<>proc_find(thread(threadcur).id,KFIRST)) _
			OrElse (threadcur=0 AndAlso proc(procr(proc_find(thread(0).id,KLAST)).idx).nm<>"main") Then 'impossible to go out first proc of thread, constructor for shared
				procad=procsv
				runtype=RTRUN
				but_enable()
			End If
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

        Case IDBUTAUTO 'simple thread auto
			runtype=RTAUTO
			but_enable()
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

		case IDBUTRUN
			runtype=RTRUN
			but_enable()
			hidewindow(hcchainbx,KHIDE)
			fasttimer=Timer
			thread_resume()

		case IDBUTSTOP
			If runtype=RTFREE Or runtype=RTFRUN Then
				runtype=RTFRUN 'to treat free as fast
				For i As Integer = 1 To linenb 'restore every breakpoint
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
				Next
			Else
				runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0
			EndIf
			Stopcode=CSHALTBU

		case IDBUTFREE
		   If messbox("FREE","Release debugged prgm",MB_YESNO)=RETYES Then
				For i As Integer = 1 To linenb 'restore old instructions
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rLine(i).sv,1,0)
				Next
				runtype=RTFREE
				but_enable()
				thread_resume()
			End If

		case IDBUTTOOL
			if GetLineCountEditor(GLOG)<>0 then
				SetStateMenu(HMenutools,MNSHOWLOG,0)
				SetStateMenu(HMenutools,MNRESETLOG,0)
			Else
				SetStateMenu(HMenutools,MNSHOWLOG,1)
				SetStateMenu(HMenutools,MNRESETLOG,1)
			End If
            DisplayPopupMenu(HMenutools, GlobalMouseX,GlobalMouseY)

		case IDBUTFILE
			select_file()

		case IDBUTRERUN ''restart current exe
			restart()

		case IDBUTATTCH
			messbox("feature not implemented","button = IDBUTATTACH")

		case IDBUTKILL
			kill_process("Terminate immediatly no saved data, other option Release")

		case IDBUTLASTEXE
			var HMenuexe=CreatePopMenu()
			For iitem As integer =0 To 9
				If savexe(iitem)<>"" Then
					MenuItem(MNEXEFILE0+iitem,HMenuexe,savexe(iitem))
					If iitem=0 Then MenuBar(HMenuexe)
				EndIf
			Next
			DisplayPopupMenu(HMenuexe, GlobalMouseX,GlobalMouseY)
			Delete_Menu(HMenuexe)

		case IDBUTFASTRUN
			hidewindow(hcchainbx,KHIDE)
			but_enable()
			fastrun()
			send_sci(SCI_MarkerAdd, line_cursor-1, 4)

		case IDBUTEXEMOD
			messbox("feature not implemented","button = IDBUTEXEMOD")

        Case IDBUTSTEPB 'STEP at bottom of proc
			If rline(thread(threadcur).sv).ad<>proc(procsv).fn Then 'if current line is end of proc simple step
				procbot=procsv
				runtype=RTRUN
				but_enable()
			EndIf
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

        Case IDBUTSTEPT 'STEP at top of proc
			If rline(thread(threadcur).sv).ad<>proc(procsv).fn Then 'if current line is end of proc simple step
				proctop=TRUE
				runtype=RTRUN
				but_enable()
			EndIf
			hidewindow(hcchainbx,KHIDE)
			thread_resume()

		case IDBUTCURSOR
			messbox("Running to cursor","Source="+source(PanelGadgetGetCursel(GSRCTAB))+" line="+str(line_cursor))
			brk_set(9)

		case IDBUTUPDATE
			if flagupdate=true then
				flagupdate=false
				load_button(IDBUTUPDATE,@"update.bmp",660,,@"Update On /Update off : variables, dump",,0)
				SetImageGadget(IDBUTUPDATE,Load_image("."+slash+"buttons"+slash+"noupdate.bmp"))
			else
				flagupdate=true
				SetImageGadget(IDBUTUPDATE,Load_image("."+slash+"buttons"+slash+"update.bmp"))
				var_sh()
				dump_sh()
			end if

		case IDBUTENLRSRC
			messbox("feature not implemented","button = IDBUTENLRSRC" )

		case IDBUTENLRVAR
			messbox("feature not implemented","button = IDBUTENLRVAR" )

		case IDBUTENLRMEM
			messbox("feature not implemented","button = IDBUTENLRMEM" )

		case else
			'''todo for now used after gadget_action after remove gadget_action when tests are done for the range values
			gadget_action(button)
			'messbox("feature not implemented","sorry button="+str(button)+" --> enum="+enumdef(button))
	end select
end sub

''button/menu/gadget handle for fbdebugger_new
''dbg_actions.bas

'==================================================
private sub menu_action(poption as integer)
		dim as INTEGER ivar,typ,pt

	select case poption
		case MNABOUT
            If messbox("FB DEBUGGER "+ver3264,"Debugger for FreeBASIC (Windows/Linux 32/64)"+Chr(13)+Chr(13)+fbdebuggerversion+" / "+__DATE__+Chr(13)+"(C) L.GRAS  debug @ aliceadsl . fr"+Chr(13)+Chr(13) _
               +"Select YES for accessing to the dedicated page on the forum"+Chr(13)+"http://www.freebasic.net/forum/viewtopic.php?f=8&t=13935",MB_YESNO)=6 Then
				#ifdef __fb_win32__
					Shell "start http://www.freebasic.net/forum/viewtopic.php?f=8""&t""=13935"
				#else
					runprogram("firefox","http://www.freebasic.net/forum/viewtopic.php?f=8&t=13935")
				#endif
			endif
		case MNCMPINF
			if compilerversion<>"" then
				messbox("Compiler version","Debuggee compiled by "+compilerversion)
			EndIf

        case MNSETTINGS
			setgadgettext(GCMDLPARAM,cmdexe(0))
			hidewindow(hsettings,KSHOW)

		case MNEXEFILE0 to MNEXEFILE9
			restart(poption-MNEXEFILE0)

		case MNSHOWLOG
			setgadgettext(GLOG,vlog)
			hidewindow(hlogbx,KSHOW)

		case MNRESETLOG
			if messbox("Reset log","Are you sure ?",MB_YESNO)=IDYES then
				vlog=""
				setgadgettext(GLOG,vlog)
			EndIf

		Case MNWINMSG
			#ifdef __fb_win32__
				winmsg()
			#else
				linmsg()
			#endif

        Case MNSHWBDH
            dechexbin()

		Case MNGOTO 'goto line
			line_goto()

      	Case MNLINEADR 'line address (in memory)
         	line_adr()

		Case MNWCHDEL'delete watch on cursor from watched
         		watch_del(watch_find())

        Case MNWCHDALL'delete all watches
			if messbox("Delete watched vars","Delete all",MB_YESNO)=IDYES Then
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
		   proc_loccall()

		Case MNSHWPROC 'locate proc in proc/var treeview
			thread_procloc(1)

		Case MNSHPRSRC 'locate proc in source
			thread_procloc(2)

		Case MNTCHNING ''call chain (from thread)
			call_chain(ValInt(Mid(GetTextTreeView(GTVIEWTHD,thread_find()),13,6)))

		Case MNPRCRADR 'information about running proc
			thread_procloc(5)

		Case MNTHRDCHG 'change thread
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

		Case MNSETBRKP 'set breakpoint
			brk_set(1)

		Case MNSETBRKC 'set breakpoint conditionnal
			if brktyp=KBRCMEMMEM then
				if brkidx1=0 or brkidx2=0 then
					messbox("Set BP conditional","Need variables for the condition")
				else
					brk_set(3)
				EndIf
			else
				brc_choice()
			EndIf

		Case MNSETBRKT 'set tempo brkp
			brk_set(5)

		Case MNSETBRKN 'set brkp with counter
			brk_set(4)

		Case MNCHGBRKN 'change value brkp with counter
			brk_set(7)

		Case MNRSTBRKN 'reset brkp with counter
			brk_set(8)

		Case MNSETBRKD 'enable/disable brkp
			brk_set(6)

		Case MNMNGBRK
			brk_manage("Breakpoint management")

		Case MNSHWVAR
			var_tip()

		Case MNSETWVAR
			messbox("Feature not yet implemented","var_tip(WATCHED)")
			'var_tip(WATCHED)

		Case MNFNDTXT
			if sourcenb<>-1 then
				ftext.tpos=-1
				if send_sci(SCI_GETSELECTIONEND,0,0) - send_sci(SCI_GETSELECTIONSTART,0,0) <> 0 then
					var text=space(1 + send_sci(SCI_GETSELTEXT,0, NULL))
					send_sci(SCI_GETSELTEXT,0,strptr(text))
					setgadgettext(GFINDTEXT,text)
				end if
				hidewindow(hfindtextbx,KSHOW)
				SetFocus(Gadgetid(GFINDTEXT))
				#ifdef __fb_linux__
					UpdateInfoXserver()
				#endif
			end if

		Case MNASMREGS 'show processor registers for current thread
			show_regs()

		case MNCLBVARA 'copy  to clipboard  all procs/vars
			var_list(2)

		case MNCLBVARS 'copy  to clipboard  selectedvar
			var_list(3)

		case MNLSTVARA 'copy  to log  all procs/vars
			var_list(0)

		case MNLSTVARS 'copy  to log selectedvar
			var_list(1)

		Case MNPTDUMP 'dump pointed data
			var_dump(htviewvar,1)

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
			watch_sel(watch_find()) ''find the watched in proc/var
			menu_action(MNVAREDT) ''simulate call edit
			PanelGadgetSetCursel(GRIGHTTABS,TABIDXWCH) ''redisplay watched

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

		Case MNTHRDBLK ''block thread
			thread_block()

		Case MNCREATHR ''show line creating thread
			thread_execline(2)

		Case MNTHRDLST
			thread_list()

		case MNBRKVC,MNBRKV1,MNBRCV1,MNBRKV2,MNBRCV2
			ivar=var_find()
			if ivar=0 then exit sub
			'ivar=abs(ivar) ''useful only if field

			select case poption
				case MNBRKVC,MNBRKV1,MNBRCV1
					''BP var/mem variable/constant or variable/variable
					''BP cond variable/variable
					brkidx1=ivar
					brkidx2=0
					var_fill(brkidx1)

					#Ifdef __FB_64BIT__
					If varfind.pt Then varfind.ty=9 ''pointer integer64 (longint)
					#Else
					If varfind.pt Then varfind.ty=1 ''pointer integer32 (long)
					#EndIf

					If varfind.ty>12 then
						messbox("Break on var selection error","Only [unsigned] Byte, Short, integer, longint, single, double")
						brkidx1=0
						brkidx2=0
						Exit Sub
					End If
					brkdatatype=varfind.ty
					if poption=MNBRKVC then ''select the constant and the test
						SetGadgetText(GBRKVAR1,varfind.nm)
						hidegadget(GBRKVAR2,KHIDE)
						hidegadget(GBRKVALUE,KSHOW)
						ResetAllComboBox(GBRKVCOND)
						AddComboBoxItem(GBRKVCOND,"=",-1)
						AddComboBoxItem(GBRKVCOND,"<>",-1)
						'If brkv.typ<>4 AndAlso brkv.typ<>13 AndAlso brkv.typ<>14 Then
							AddComboBoxItem(GBRKVCOND,">",-1)
							AddComboBoxItem(GBRKVCOND,"<",-1)
							AddComboBoxItem(GBRKVCOND,">=",-1)
							AddComboBoxItem(GBRKVCOND,"<=",-1)
						'end if
						SetItemComboBox(GBRKVCOND,0)
						hidewindow(hbrkvbx,KSHOW)
						brktyp=KBRKMEMCONST
					end if

				Case MNBRKV2,MNBRCV2
					if brkidx1=0 then
						messbox("BPoint","Select the first var")
						exit sub
					end if
					brkidx2=ivar
					var_fill(brkidx2)
					#Ifdef __FB_64BIT__
					If varfind.pt Then varfind.ty=9 ''pointer integer64 (longint)
					#Else
					If varfind.pt Then varfind.ty=1 ''pointer integer32 (long)
					#EndIf

					If varfind.ty>12 then
						messbox("Break on var selection error","Only [unsigned] Byte, Short, integer, longint, single, double")
						brkidx1=0
						brkidx2=0
						Exit Sub
					End If

					if brkdatatype<>varfind.ty then
						messbox("BP conditional","Use 2 variables with same datatype")
						brkidx2=0
						exit sub
					end if

					SetGadgetText(GBRKVAR2,varfind.nm)
					var_fill(brkidx1)
					SetGadgetText(GBRKVAR1,varfind.nm)
					hidegadget(GBRKVAR2,KSHOW)
					hidegadget(GBRKVALUE,KHIDE)
					hidewindow(hbrkvbx,KSHOW)
					ResetAllComboBox(GBRKVCOND)
					AddComboBoxItem(GBRKVCOND,"=",-1)
					AddComboBoxItem(GBRKVCOND,"<>",-1)
					AddComboBoxItem(GBRKVCOND,">",-1)
					AddComboBoxItem(GBRKVCOND,"<",-1)
					AddComboBoxItem(GBRKVCOND,">=",-1)
					AddComboBoxItem(GBRKVCOND,"<=",-1)
					SetItemComboBox(GBRKVCOND,0)
					if poption=MNBRKV2 then
						brktyp=KBRKMEMMEM
					else
						brktyp=KBRCMEMMEM
					end if
			end select

		Case MNBRKVS
			brkv_set(0)

		case MNDBGHELP
		#Ifdef __fb_win32__
			shell "doc_fbdebugger.pdf"
		#else
			messbox("Help file","feature not implemented")
		#endif

		case MNACCLINE
			mark_exec()

	'=============================================================
        case else
			'messbox("Menu feature not implemented","sorry option="+str(poption)+" --> enum="+enumdef(poption))
			'trying to find the action in the other procedure
			button_action(poption)
    end select
end sub
'============================================
'' executes action for gadget
'============================================
private sub gadget_action(igadget as LONG)
	dim as INTEGER decal

	select case igadget
		Case GCURRENTLINE
			linecur_display()

		case GTVIEWVAR
		case GTVIEWWCH

		case GFILELIST
			dim as integer idx,searched,cpt
			searched=GetItemComboBox(GFILELIST)
			if srccombocur<>searched then
				srccombocur=searched
				searched+=1
				cpt=1
				idx=srclistfirst
				do until cpt=searched
					idx=srclist(idx).child
					cpt+=1
				loop

				if	idx-1<>srcdisplayed then  ''test useless as item in combo changed ?????
					source_change(idx-1)
				end if
			EndIf
		case GSRCCurrent
			messbox(srcname(srcdisplayed),source(srcdisplayed)+chr(13)+"date="+chr(13)+"size=")

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
		case GDUMPNEW
			if dumptyp>=7 and dumptyp<=10 then
				var value=vallng(GetTextItemListView(GDUMPMEM,0,1))
				'print "value=";value
				if value>0 then
					dumpadr=value
					dump_sh()
					SetGadgetText(GDUMPADR,str(dumpadr))
				EndIf
			end if

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

		case GBRKVCOND
		case GBRKVALUE

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
		case GTVIEWTHD

		Case GBRKCLOSE
	   		hidewindow(hbrkbx,KHIDE)

	   	Case GBRKDEL01 to GBRKDEL10 ''delete one breakpoint
			for ibrk as INTEGER = 1 to brknb
				if brkol(ibrk).index=brkline(igadget-GBRKDEL01+1) then
					brk_del(ibrk)
					exit for
				end if
			next
			hidegadget(igadget,KHIDE)
			hidegadget(igadget-GBRKDEL01+GBRKDSB01,KHIDE)
			hidegadget(igadget-GBRKDEL01+GBRKLINE01,KHIDE)
			hidegadget(igadget-GBRKDEL01+GBRKIMG01,KHIDE)
			If brknb=0 Then
				hidewindow(hbrkbx,KHIDE) ''no more breakpoint so close the window
			EndIf

		Case GBRKLINE01 to GBRKLINE10 ''click on text
			source_change(brkol(igadget-GBRKLINE01+1).isrc)
	        line_display(brkol(igadget-GBRKLINE01+1).nline)

		Case GBRKDSB01 to GBRKDSB10 ''enable/disable
			If brkol(igadget-GBRKDSB01+1).typ>50 Then
				brkol(igadget-GBRKDSB01+1).typ-=50
				SetGadgetText(igadget,"DSB")
			Else
				brkol(igadget-GBRKDSB01+1).typ+=50
				SetGadgetText(igadget,"ENB")
			EndIf
			brk_marker(igadget-GBRKDSB01+1)

		Case GBRKRST01 to GBRKRST10 ''reset
			var ibrk=igadget-GBRKRST01+1
			var text=getGadgetText(GBRKLINE01+igadget-GBRKRST01)
			brkol(ibrk).counter=brkol(ibrk).cntrsav
			text=left(text,instr(text,"="))+Str(brkol(ibrk).counter)+mid(text,instrrev(text,"/")) '+"/"+Str(brkol(ibrk).cntrsav)
			SetGadgetText(GBRKLINE01+igadget-GBRKRST01,text)

		Case GBRKCHG01 to GBRKCHG10 ''change value counter BP
			var ibrk=igadget-GBRKCHG01+1
			var text=getGadgetText(GBRKLINE01+igadget-GBRKCHG01)
			var inputval=input_bx("Change value counter, remaining = "+Str(brkol(ibrk).counter)," initial = "+Str(brkol(ibrk).cntrsav),,7)
			brkol(ibrk).counter=ValUInt(inputval)
			if brkol(brknb).counter=0 then
				brkol(ibrk).counter=brkol(ibrk).cntrsav
				messbox("Change counter","Value = zero, enter another value or delete BP")
			else
				brkol(ibrk).cntrsav=brkol(ibrk).counter
				text=left(text,instr(text,"="))+Str(brkol(ibrk).counter)+"/"+Str(brkol(ibrk).cntrsav)+mid(text,instrrev(text,">")-2)
				SetGadgetText(GBRKLINE01+igadget-GBRKCHG01,text)
			EndIf

	   	Case GBRKDELALL    ''Delete all
	        	For ibrk As Byte=1 To brknb
					brk_del(1)
	        	Next
				hidewindow(hbrkbx,KHIDE)

	   	Case GBRKDISABLE ''disable all
	      	For ibrk As integer =1 To brknb
				If brkol(ibrk).typ<6 Then
					brkol(ibrk).typ+=50
					brk_marker(ibrk)
					SetGadgetText(GBRKDSB01+ibrk-1,"ENB")
				EndIf
	      	Next

	   	Case GBRKENABLE ''enable all
			For ibrk As integer =1 To brknb
				If brkol(ibrk).typ>50 Then
					brkol(ibrk).typ-=50
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

		case GASCII
			flagascii=1-flagascii
			dump_sh

		case GVERBOSE
			flagverbose=1-flagverbose
			var_sh()
			proc_sh()

		case GAUTODELAY

		case GCMDLKEEP

		case GCMDLPARAM ''data used when closing settings box

		case SETBUTSTEP to SETBUTFREE
			if bit(setbuttons,igadget-SETBUTSTEP) then ''button is set
				setbuttons=bitreset(setbuttons,igadget-SETBUTSTEP)
			    hidegadget(IDBUTSTEP+igadget-SETBUTSTEP, KHIDE)
			else ''button is unset
				setbuttons=bitset(setbuttons,igadget-SETBUTSTEP)
				hidegadget(IDBUTSTEP+igadget-SETBUTSTEP, KSHOW)
			EndIf

		case GIDXTABLE

		case GIDXAPPLY
			index_apply()

		case GIDXINC
			For k As Integer = indexdata.nbdim-1 To 0 Step -1
				If indexdata.vubound(k)>vrr(indexdata.indexvar).ix(k) Then
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
				If indexdata.vlbound(k)<vrr(indexdata.indexvar).ix(k) Then
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

		Case GIDXUPD
			index_fullupdate()

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
					line_display(rline(procr(procrsav(iline)).cl).nu,1)
				end if
			end if

		case GBRKVOK ''apply
			select case brktyp
				Case KBRKMEMCONST
					brkv_set(1)
				Case KBRKMEMMEM
					brkv_set(2)
				Case KBRCMEMMEM
					var_fill(brkidx1)
					brkadr1=varfind.ad
					var tempo=varfind.nm
					var_fill(brkidx2)
					brkadr2=varfind.ad
					brkttb=32 shr GetItemComboBox(GBRKVCOND)
					var tst=brk_comp(brkttb)
					modify_menu(MNSETBRKC,HMenusource,"BP cond with "+tempo+" "+*tst+" "+varfind.nm)
			End Select
			hidewindow(hbrkvbx,KHIDE)

		case GBRCOK
			if varfind.ad=0 then
				messbox("Cond BP","Select a variable")
			else
				brkdatatype=varfind.ty
				if brkdatatype=11 then
					brkdata2.vsingle=val(getgadgettext(GBRCVALUE))
				elseIf brkdatatype=12 then
					brkdata2.vdouble=val(getgadgettext(GBRCVALUE))
				else
					brkdata2.vlongint=vallng(getgadgettext(GBRCVALUE))
				EndIf
				brkadr1=varfind.ad
				brkttb=32 shr GetItemComboBox(GBRCCOND)
				var tst=brk_comp(brkttb)
				hidewindow(hbpcondbx,KHIDE)
				brk_set(2)
			end if

		case GBRCDEL
			hidewindow(hbpcondbx,KHIDE)

		case GBRCCOND ''nothing

		case GBRKVDEL
			select case brktyp
				Case KBRKMEMCONST
				messbox("case KBRKMEMCONST","not yet finished")
					brkv_set(0)
				Case KBRKMEMMEM
				messbox("case KBRKMEMMEM","not yet finished")
					brkv_set(0)
				case else
			end select
				brkidx1=0
				brkidx2=0
				hidewindow(hbrkvbx,KHIDE)

		case GBRKIMG01 to GBRKIMG01+9
			var brkidx=igadget-GBRKIMG01+1
			if brkol(brkidx).typ=2 or brkol(brkidx).typ=3 then
				var_fill(brkol(brkidx).ivar1)
				var tempo=varfind.nm
				var tst=brk_comp(brkol(brkidx).ttb)
				if brkol(brkidx).typ=2 then
					if brkol(brkidx).datatype=11 then
						messbox("BP cond with ",tempo+" "+*tst+" "+str(brkol(brkidx).val.vsingle))
					elseif brkol(brkidx).datatype=12 then
						messbox("BP cond with ",tempo+" "+*tst+" "+str(brkol(brkidx).val.vdouble))
					else
						messbox("BP cond with ",tempo+" "+*tst+" "+str(brkol(brkidx).val.vlongint))
					end if
				else
					var_fill(brkol(brkidx).ivar2)
					messbox("BP cond with ",tempo+" "+*tst+" "+varfind.nm)
				end if
			elseif brkol(brkidx).typ=4 then
				messbox("BP counter ","Current="+str(brkol(brknb).counter)+chr(10)+"Initial="+str(brkol(brknb).cntrsav))
			EndIf

		case GFINDTEXT

		case GFINDTEXTN
			find_text(1)

		case GFINDTEXTP
			find_text(0)

		case GLOG ''nothing to do
		case GEDITOR ''nothing to do
		case GTVIEWBRC ''nothing to do
		'print  "GTVIEWBRC clicked on"
		case GBRCVALUE ''nothing to do

		case GATTCHEDIT

		case GATTCHGET
			attach_getid()
		case else
			messbox("Gadget/menu/button feature not implemented","sorry option="+str(igadget))
	End Select
end sub
'==============================================
'' launch by file explorer
'==============================================
private sub select_file()
	#Ifdef __fb_win32__
		var selfile= OpenFileRequester("Select exe file","C:\","Exe files (*.exe)"_
		+Chr(0)+"*.exe"+Chr(0))
	#else
		var selfile=OpenFileRequesterExe("Select exe file","/home/user/")
		'var selfile= OpenFileRequester("Select exe file","","Exe files)"_
		'+Chr(0)+""+Chr(0))
	#endif
	If selfile="" then
		messbox("File selection","No file selected")
		'exename=""
		exit sub
	else
		exename=selfile
		'messbox("File selection","File selected = "+exename)
		'print "File selected = "+exename
	end if

	if check_bitness(exename)=0 then exit sub ''bitness of debuggee and fbdebugger not corresponding

	if kill_process("Trying to launch but debuggee still running")=FALSE then exit sub
	reinit ''reinit all except GUI parts
	flagrestart=-1
    exe_sav(exename,"")
	SetTimer(hmain,GTIMER001,500,Cast(Any Ptr,@debug_event))

	If ThreadCreate(@start_pgm)=0 Then
		KillTimer(hmain,GTIMER001)
		messbox("Debuggee not running","ERROR unable to start the thread managing the debuggee")
	endif
end sub
'==============================================================
'' handles actions for each button
'==============================================================
private sub button_action(button as integer)
	static as Hmenu HMenuexe
	select case button

		case IDBUTSTEP 'STEP
			stopcode=0
			runtype=RTSTEP
			hidewindow(hcchainbx,KHIDE)
			#ifdef __fb_WIN32__
				set_cc()
			#else
				if ccstate=KCC_NONE then
					msgdata=1 ''CC everywhere
					exec_order(KPT_CCALL)
				end if
			#EndIf
			thread_resume()

		case IDBUTSTEPOVER 'STEP+ over
			brk_set(10)
			hidewindow(hcchainbx,KHIDE)
			'thread_resume()

        Case IDBUTAUTO 'simple thread auto
			runtype=RTAUTO
			but_enable()
			hidewindow(hcchainbx,KHIDE)
			#ifdef __fb_WIN32__
				set_cc()
			#else
				if ccstate=KCC_NONE then
					msgdata=1 ''CC everywhere
					exec_order(KPT_CCALL)
				end if
			#EndIf
			thread_resume()

		case IDBUTRUNEXIT
			brk_set(12)
			hidewindow(hcchainbx,KHIDE)
			'thread_resume()

		case IDBUTSTOP
			If runtype=RTFREE Or runtype=RTRUN Then
				runtype=RTRUN 'to treat free as run
				#Ifdef __fb_win32__
					set_cc()
				#else
					sigusr_send()
				#endif
			Else
				'print "halting all"
				flaghalt=true
				runtype=RTSTEP
			EndIf
			Stopcode=CSUSER

		case IDBUTFREE
		   If messbox("FREE","Release debugged prgm",MB_YESNO)=IDYES Then
				For i As Integer = 1 To linenb 'restore old instructions
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rLine(i).sv,1,0)
				Next
				ccstate=KCC_NONE
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
			attach_gui()

		case IDBUTKILL
			#ifdef __fb_linux__
			afterkilled=KDONOTHING
			#endif
			kill_process("Terminate immediatly no saved data, other option Release")

		case IDBUTLASTEXE
			if HMenuexe then
				Delete_Menu(HMenuexe)
			EndIf
			HMenuexe=CreatePopMenu()
			For iitem As integer =0 To 9
				If savexe(iitem)<>"" Then
					MenuItem(MNEXEFILE0+iitem,HMenuexe,savexe(iitem))
					If iitem=0 Then MenuBar(HMenuexe)
				EndIf
			Next
			DisplayPopupMenu(HMenuexe, GlobalMouseX,GlobalMouseY)

		case IDBUTCURSOR  ''run to cursor line
			brk_set(9)
			hidewindow(hcchainbx,KHIDE)

		case IDBUTEXECMOD
			exec_mod()

        Case IDBUTRUNEND 'STEP at end of proc
			If rline(thread(threadcur).sv).ad<>proc(procsv).fn Then 'if current line is end of proc simple step
				brk_set(11)
			EndIf
			hidewindow(hcchainbx,KHIDE)
			'thread_resume()

		case IDBUTUPDATE
			if flagupdate=true then
				flagupdate=false
				SetImageGadget(IDBUTUPDATE,Catch_Image(butNOUPDATE))
			else
				flagupdate=true
				SetImageGadget(IDBUTUPDATE,Catch_Image(butUPDATE))
				var_sh()
				dump_sh()
			end if

		Case IDBUTBRKP 'set breakpoint
			brk_set(1)

		Case IDBUTBRKC 'set breakpoint conditionnal
			if brktyp=KBRCMEMMEM then
				if brkidx1=0 or brkidx2=0 then
					messbox("Set BP conditional","Need variables for the condition")
				else
					brk_set(3)
				EndIf
			else
				brc_choice()
			EndIf

		Case IDBUTBRKT 'set tempo brkp
			brk_set(5)

		Case IDBUTBRKN 'set brkp with counter
			brk_set(4)

		Case IDBUTBRKD 'enable/disable brkp
			brk_set(6)

		Case MNMNGBRK,IDBUTBRKB
			brk_manage("Breakpoint management")

		case GBUTSHOWVAR
			var_tip()

		case GATTCHOK
			attach_ok()

		case else
			'''todo for now used after gadget_action after remove gadget_action when tests are done for the range values
			gadget_action(button)
			'messbox("feature not implemented","sorry button="+str(button)+" --> enum="+enumdef(button))
	end select
end sub

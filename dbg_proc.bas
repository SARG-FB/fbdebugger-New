
''dbg_proc.bas
'======================================================================================================
private sub proc_del(j As Integer,t As Integer=1)
	Dim  As Integer tempo,th,thid
	Dim parent As integer
	Dim As String text

	' delete procr in treeview
	DeleteTreeViewItem(GTVIEWVAR,procr(j).tv)
	'delete watch
	For i As Integer =0 To WTCHMAX
		'keep the watched var for reusing !!!
		If wtch(i).psk=procr(j).sk Then
			wtch(i).psk=-3
		End If
	Next
	'delete breakvar
	If brkv.psk=procr(j).sk Then brkv_set(0)

	'' remove array tracking : either suppressed array or one of the variable used as index
	If procr(j+1).vr>trckarr(0).idx AndAlso trckarr(0).idx>=procr(j).vr Then
		''todo add array_tracking_remove
	Else
		For i As Long =0 To 4
			If trckarr(i).memadr Then
				If procr(j+1).vr>trckarr(i).iv AndAlso trckarr(i).iv>=procr(j).vr Then
					''todo add array_tracking_remove
					Exit For
				End If
			End if
		Next
	End If

	''close shw/exp except if static
	If shwexp.free=false then
		if shwexp.procr=j then
			if shwexp.mem<>3 then ''only local and not a static
				hidewindow(hshwexpbx,KHIDE)
				shwexp.free=true
			end if
		end if
	end if

	''compress running variables
	tempo=procr(j+1).vr-procr(j).vr
	vrrnb-=tempo
	For i As Integer = procr(j).vr To vrrnb
		vrr(i)=vrr(i+tempo)
	Next
	thid=procr(j).thid
	If t=1 Then 'not dll
		th=thread_select(thid) 'find thread index
		parent=GetParentItemTreeView(GTVIEWTHD,thread(th).ptv)
		DeleteTreeViewItem(GTVIEWTHD,thread(th).ptv) ''delete item
		thread(th).ptv=parent 'parent becomes the last
		thread_text(th) 'update thread text
	EndIf
	''compress procr and update vrr index
	procrnb-=1
	For i As Integer =j To procrnb
		procr(i)=procr(i+1)
		procr(i).vr-=tempo
	Next
	tempo=0 ''used here as a flag if not any more procr for the thread thid
	''find the last proc item in treeview
	'print "before update plt=";thread(th).plt
	For iprc As Integer = procrnb to 1 step -1
		if procr(iprc).thid=thid then
			tempo=-1
			thread(th).plt=procr(iprc).tv
			'print "thid=";thid," prcocrnb=";procrnb ," iprc=";iprc
			exit for
		EndIf
	next
	'print "tempo=";tempo
	if tempo=0 then
		thread(th).plt=0
	EndIf
End Sub
'=================================================================================================
'' changes the status of the procedure enabled / disabled = doesn't be handled in running proc
'=================================================================================================
private sub proc_enable() ''enab=true-> enabled / false -> disabled

	dim as integer prc
	dim as string text
	var item=GetItemTreeView(GTVIEWPRC)
	'var item=getitemtree(GTVIEWPRC)
	if item=0 then
		exit sub
	EndIf
    Do
        prc+=1
    Loop While proc(prc).tv<>item or prc>procnb

    If proc(prc).enab=true Then
    	If proc_verif(prc) Then
			messbox("Proc "+proc(prc).nm+" is running","Can't be disabled")
    	Else
			proc(prc).enab=false
			text=GetTextTreeView(GTVIEWPRC,proc(prc).tv)
			text[instr(text,"<E>")]=asc("D")
			SetTextItemTreeView(GTVIEWPRC,proc(prc).tv,text)
			For i As Integer =1 To linenb
				If rline(i).px=prc Then  WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rline(i).sv,1,0)
			Next
        End If
    Else
		If messbox("Enable Proc "+proc(prc).nm,"If running --> big problem",MB_YESNO)=IDYES Then
			proc(prc).enab=true
			For i As Integer =1 To linenb
				If rline(i).px=prc Then
					WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
				end if
			Next
			text=GetTextTreeView(GTVIEWPRC,proc(prc).tv)
			text[instr(text,"<D>")]=asc("E")
			SetTextItemTreeView(GTVIEWPRC,proc(prc).tv,text)
		End If
    End If
End Sub
'===========================================================
private sub proc_sh()
	Dim libel As String
	dim as integer listidx
	DeleteTreeViewItemall(GTVIEWPRC)

	If procsort=KMODULE Then 'sorted by module
		messbox("feature not coded","sort by module for procs so forcing by name")
		procsort=KPROCNM
	end if

	'For j As Integer =1 To procnb
		'With proc(j)
			'if .enab=true then
				'libel="E> " '' for indicating if the proc is enabled : followed
			'else
				'libel="D> " ''disabled
			'EndIf
			'If procsort=KMODULE Then 'sorted by module
				''libel+=name_extract(source(.sr))+">> "+.nm+":"+proc_retval(j)
			'Else 'sorted by proc name
				'libel+=.nm+":"+proc_retval(j)+"   in : "+source_name(source(.sr))
			'EndIf
			'If flagverbose Then libel+=" ["+Str(.db)+"/"+hex(.db)+"]"
			'.tv=AddTreeViewItem(GTVIEWPRC,libel,cast (hicon, 0),0,TVI_LAST,0)
		'End With
	'Next
	listidx=proclistfirst
	while listidx<>-1
		With proc(listidx)
			libel=.nm+":"+proc_retval(listidx)+"   in : "+source_name(source(.sr))
			if .enab=true then
				libel+=" <E> " '' for indicating if the proc is enabled : followed
			else
				libel+=" <D> " ''disabled
			EndIf

			If flagverbose Then libel+=" ["+Str(.db)+"/"+hex(.db)+"]"

			.tv=AddTreeViewItem(GTVIEWPRC,libel,cast (hicon, 0),0,TVI_LAST,0)
			listidx=proclist(listidx).child
		end with
	Wend


End Sub
'=====================================================
private function proc_name(ad As UInteger) As String 'find name proc using address
	For i As Integer =1 To procnb
		If proc(i).db=ad Then Return Str(ad)+" >> "+proc(i).nm
	Next
	Return Str(ad)
End Function
'=============================
'' end of proc
'=============================
Private sub proc_end()
	Dim As Long limit=-1
	Var thid=thread(threadcur).id
	'find the limit for deleting proc (see below different cases)

	For j As Long =procrnb To 1 Step -1
		If procr(j).thid =thid  Then
		   If limit=-1 Then limit=j
		   If procr(j).idx=procsv Then
			If j<>limit Then limit=j+1
			Exit For
		   EndIf
		EndIf
	Next
	''delete all elements (treeview, varr, ) until the limit
	For j As Long =procrnb To limit Step -1

	'print "procrnb=";procrnb,limit,j
	   If procr(j).thid = thid Then
			proc_del(j)
		End If
	Next

	If flagtrace Then dbg_prt ("RETURN to proc "+proc(procsv).nm)

'' The removing of procs is done after the end is eached AND when a next line is executed. It could be a simple line or a call to a new proc.
'' in this last case it's a bit complicated.

'' handle case multiple returns in a recursive proc, breakcpu not restored between each execution....
'see example below, the first test data would not be deleted
'function test(a As integer) As integer
'	If a=2 Then
'		Return test(1)
'	EndIf
'End function
'test(2)

''4 cases :
'' index    1         2            3                       also 3       limit
''normal   main --> mysub                                               >> 2
''recursif main --> test -->      test                                     2
''         main --> mysub --> constructor then immediatly destructor       3
''         main --> destructor then immediatly       same destructor       3

End Sub
'=====================================================================
private function proc_find(thid As Integer,t As Byte) As Integer 'find first/last proc for thread
	If t=KFIRST Then
		For i As Integer =1 To procrnb
			If procr(i).thid = thid Then Return i
		Next
	Else
		For i As Integer = procrnb To 1 Step -1
			If procr(i).thid = thid Then Return i
		Next
	End If
End Function
'===================================
private function proc_retval(prcnb As Integer) As String
	Dim p As Integer = proc(prcnb).pt
	If p Then
	 	If p>220 Then
			Return String(p-220,"*")+" Function"
		ElseIf p>200 Then
			Return String(p-200,"*")+" Sub"
		Else
			Return String(p,"*")+" "+udt(proc(prcnb).rv).nm
	 	End If
	End If
	Return udt(proc(prcnb).rv).nm
End Function
'====================================================================
'' locates a proc in sources from selected line in current treeview
'====================================================================
private sub proc_loc()
	Dim As Integer hitem,temp,th
	'get current hitem in tree
	select case PanelGadgetGetCursel(GRIGHTTABS)
		case TABIDXVAR
			temp=GetItemTreeView(GTVIEWVAR)
			Do 'search index proc
				hitem=temp
				temp=GetParentItemTreeView(GTVIEWVAR,hitem)
			Loop While temp
			temp=0
			For i As Integer =1 To procrnb
				If procr(i).tv=hitem Then
					temp=procr(i).idx
				 Exit For
				EndIf
			Next
			If temp=0 Then
				messbox("Locate proc","Global variables no proc associated !!")
				Exit Sub
			EndIf

		case TABIDXPRC
			temp=GetItemTreeview(GTVIEWPRC)
			hitem=temp
			For i As Integer =1 To procnb
				If proc(i).tv=hitem Then
					temp=i
				 Exit For
				EndIf
			Next
		case else
			th=thread_select()
			If th=0 Then ''main, select first line
				temp=procr(1).idx
			Else
				temp=procr(proc_find(thread(th).id,KFIRST)).idx
			EndIf
	End select

	If proc(temp).nu=-1 Then
		messbox("Locate proc","Not possible perhaps add by compiler (ie default constructor, let, etc)")
		Exit sub
	EndIf

	source_change(proc(temp).sr) ''display source
	line_display(proc(temp).nu,1)  ''Select Line
End Sub
'===============================================================
'' calling line
'===============================================================
private sub proc_loccall()
	Dim As Integer hitem,temp
	temp=GetItemTreeView(GTVIEWVAR)

	If temp=procr(1).tv Then
			messbox("Locate calling line","      Main so no call !!")
		Exit Sub
	EndIf

	Do 'search index proc
		hitem=temp
		temp=GetParentItemTreeView(GTVIEWVAR,hitem)
	Loop While temp

	For i As Integer =1 To procrnb
		If procr(i).tv=hitem Then
			If procr(i).cl=-1 Then
				'fb_message("Locate calling line","First proc of thread so no call !!"):Exit Sub
				thread_execline(2):Exit Sub
			EndIf
			temp=procr(i).cl 'calling line
			source_change(rline(temp).sx) ''display source
			line_display(rline(temp).nu,1)'Select Line
			Exit Sub
  		EndIf
	Next
	messbox("Locate calling line","Impossible to find with Global shared")
End Sub
'=======================================
'' new procedure sub ou function
'=======================================
private sub proc_new()
	Dim libel As String
	Dim tv As integer
	If procrnb=PROCRMAX Then hard_closing("Max number of sub/func reached limit="+str(PROCRMAX))
	procrnb+=1'new proc ADD A POSSIBILITY TO INCREASE THIS ARRAY
	procr(procrnb).sk=procsk
	ReadProcessMemory(dbghand,Cast(LPCVOID,procsk+sizeof(integer)),@procr(procrnb).ret,sizeof(integer),0)
	procr(procrnb).thid=thread(threadcur).id
	procr(procrnb).idx=procsv

	'test if first proc of thread
	'print "threadcur=";threadcur,thread(threadcur).plt,proc(procsv).nm
	If thread(threadcur).plt=0 Then
		procr(procrnb).cl=-1  ''no real calling line
		libel="ThID="+Str(procr(procrnb).thid)+" "
		thread(threadcur).tv=AddTreeViewItem(GTVIEWTHD,"Not filled",cast (hicon, 0),0,TVI_LAST,0)

		thread(threadcur).ptv=thread(threadcur).tv ''last proc
		thread_text() ''put text not only current but all to reset previous thread text
		tv=TVI_LAST ''insert in last position
	Else
		procr(procrnb).cl=thread(threadcur).od
		tv=thread(threadcur).plt 'insert after the last item of thread
	EndIf

	'add manage LIST
	If flagtrace Then dbg_prt ("NEW proc "+Str(procsv)+" "+proc(procsv).nm)
	libel+=proc(procsv).nm+":"+proc_retval(procsv)
	If flagverbose Then libel+=" ["+Str(proc(procsv).db)+"]"

	procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,libel,cast (hicon, 0),0,tv,0)
	thread(threadcur).plt=procr(procrnb).tv 'keep handle last item

	'add new proc to thread treeview
	thread(threadcur).ptv=AddTreeViewItem(GTVIEWTHD,"Proc : "+proc(procsv).nm,cast (hicon, 0),0,TVI_FIRST,thread(threadcur).ptv)
	thread(threadcur).stack=procsk
	thread_text(threadcur)
	var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
	procr(procrnb+1).vr=vrrnb+1
	If proc(procsv).nm="main" Then
		procr(procrnb).vr=1 'constructors for shared are executed before main so reset index for first variable of main 04/02/2014
	EndIf
	proc_watch(procrnb) 'reactivate watched var

	PanelGadgetSetCursel(GRIGHTTABS,TABIDXVAR)	''for forcing a windows update

End Sub
'=======================================================
'' after stopping run  retrieves all procedures
'=======================================================
private sub proc_runnew()

	#ifdef __fb_win32__
		dim as integer dummy
		Dim vcontext As CONTEXT

		if cast(integer,@vcontext) mod 16 <>0 then
			messbox("PRBM","Context not 16byte aligned")
		EndIf
		vcontext.contextflags=CONTEXT_CONTROL or CONTEXT_INTEGER
	#endif
	Dim libel As String
	Dim As Integer regbp,regip,regbpnb,regbpp(PROCRMAX),ret(PROCRMAX),retadr
	Dim As ULong j,k,pridx(PROCRMAX)
	Dim tv As integer



	''loading with rbp/ebp and proc index
	'For ithd As Integer =0 To threadnb
		var ithd=threadcur ''2022/01/21

		'print "in proc run new ithd=";ithd," plt=";thread(ithd).plt,"sv=";thread(ithd).sv

		'if thread(ithd).sv=-1 then continue for ''2022/01/21
		regbpnb=0
		#ifdef __fb_win32__
				GetThreadContext(thread(ithd).hd,@vcontext)
				regbp=vcontext.regbp
				regip=vcontext.regip 'current proc
			#else
				var threadprev=threadcur
				threadcur=ithd
				regs.xip=0
				mutexlock blocker
				msgcmd=KPT_GETREGS
				bool2=true
				condsignal(condid)
				while bool1<>true
					condwait(condid,blocker)
				wend
				bool1=false
				mutexunlock blocker
				threadcur=threadprev
				if regs.xip=0 then
					'print "not stopped=";thread(ithd).id
					'continue for ''2022/01/21
				EndIf

				regbp=regs.xbp
				regip=regs.xip
				'print "in proc new run xip=";hex(regip),hex(regbp)
		#endif
		While 1
			For j =1 To procnb
			   If regip>=proc(j).db And regip<=proc(j).fn Then
					regbpnb+=1
					regbpp(regbpnb)=regbp
					ReadProcessMemory(dbghand,Cast(LPCVOID,regbp+SizeOf(integer)),@regip,SizeOf(Integer),0) 'return EIP/RIP
					ret(regbpnb)=regip
					pridx(regbpnb)=j
			   	Exit For
			   EndIf
			Next
			If j>procnb Then Exit While
			ReadProcessMemory(dbghand,Cast(LPCVOID,regbp),@regbp,SizeOf(integer),0) 'previous RBP/EBP
		Wend
		'print "in proc new run 01 regbpnb=";regbpnb
		''skip still existing procedures or delete them
		for iprc as INTEGER	=1 to procrnb
			ReadProcessMemory(dbghand,Cast(LPCVOID,procr(iprc).sk+SizeOf(integer)),@retadr,SizeOf(Integer),0) ''current value should be return address
			if procr(iprc).ret<>retadr then
				proc_del(iprc) ''return address not any more valid
			else
				if procr(iprc).sk=regbpp(regbpnb) and procr(iprc).ret=ret(regbpnb) then
					regbpnb-=1
				EndIf
			EndIf
		next
		'print "in proc new run 02 regbpnb=";regbpnb
		''create new procrs
		For k As Integer =regbpnb To 1 Step -1
			If procrnb=PROCRMAX Then
				hard_closing("Max number of sub/func reached")
				Exit Sub
			EndIf
			If proc(pridx(k)).enab=false Then Continue For 'proc state don't follow
			procrnb+=1
			procr(procrnb).sk=regbpp(k)
			procr(procrnb).thid=thread(ithd).id
			thread(ithd).stack=procr(procrnb).sk
			procr(procrnb).idx=pridx(k)

			'test if first proc of thread
			'print "in proc run new 02 ithd=";ithd," plt=";thread(ithd).plt
			If thread(ithd).plt=0 Then
				thread(ithd).tv=AddTreeViewItem(GTVIEWTHD,"test000",cast (hicon, 0),0,TVI_LAST,0)
				thread(ithd).ptv=thread(ithd).tv 'last proc
				thread_text(ithd)'put text
				thread(ithd).st=0 'with fast no starting line could be gotten
				procr(procrnb).cl=-1  ' no real calling line
				libel="ThID="+Str(procr(procrnb).thid)+" "
				tv=thread(ithd).tv 'insert in last position
				'print "in proc run new=";thread(ithd).tv
			Else
				tv=thread(ithd).plt 'insert after the last item of thread
				procr(procrnb).ret=ret(k)
				libel=""
				procr(procrnb).cl=line_call(ret(k))
			EndIf
			'add manage LIST
			If flagtrace Then dbg_prt ("NEW proc "+proc(pridx(k)).nm)
			libel+=proc(pridx(k)).nm+":"+proc_retval(pridx(k))
			If flagverbose Then libel+=" ["+Str(proc(pridx(k)).db)+"]"

			'vrr(vrrnb).tv=AddTreeViewItem(GTVIEWVAR,"Not yet filled",cast (hicon, 0),0,TVI_LAST,0)
			procr(procrnb).tv=AddTreeViewItem(GTVIEWVAR,libel,cast (hicon, 0),0,tv,0)

			thread(ithd).plt=procr(procrnb).tv 'keep handle last item
			thread(ithd).ptv=AddTreeViewItem(GTVIEWTHD,proc(pridx(k)).nm,cast (hicon, 0),0,TVI_FIRST,thread(ithd).ptv)
			var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
			procr(procrnb+1).vr=vrrnb+1
			If proc(procsv).nm="main" Then procr(procrnb).vr=1 'constructors for shared they are executed before main so reset index for first variable of main
			proc_watch(procrnb) 'reactivate watched var
		Next

    'Next
End Sub
'=========================================
'' returns true if proc running
'=========================================
private function proc_verif(p as integer) As Boolean
	For i As UShort =1 To procrnb
		If procr(i).idx = p Then Return TRUE
	Next
	Return FALSE
End Function
'=============================================================
private sub proc_watch(procridx As Integer) 'called with running proc index
	Dim As Integer prcidx=procr(procridx).idx,vridx
	If wtchcpt=0 Then Exit Sub
	For i As Integer= 0 To WTCHMAX
	   If wtch(i).psk=-3 Then 'local var
			If wtch(i).idx=prcidx Then
			   wtch(i).adr=vrr(procr(procridx).vr+wtch(i).dlt).ad
			   wtch(i).psk=procr(procridx).sk
			EndIf
	   ElseIf wtch(i).psk=-4 Then 'session watch
		If wtch(i).idx=prcidx Then
				vridx=var_search(procridx,wtch(i).vnm(),wtch(i).vnb,wtch(i).var,wtch(i).pnt)
				If vridx=-1 Then
					messbox("Proc watch","Running var not found")
					Continue For
				EndIf
				var_fill(vridx)
			  watch_add(wtch(i).tad,i)
		EndIf
	   EndIf
	Next
End Sub
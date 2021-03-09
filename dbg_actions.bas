''handle event when debugging for fbdebugger_new
''dbg_actions.bas

'==========================================================
private function var_parent(child As integer) As Integer 'find var master parent
	Dim As integer temp,temp2,hitemp
	temp=child
	Do
		hitemp=temp2
		temp2=temp
		''temp=Cast(HTREEITEM,SendMessage(tviewvar,TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,temp)))
		''todo need to code the function as TVGN_PARENT doesn't exist
	Loop While temp
For i As Integer =1 To vrrnb
    If vrr(i).tv=hitemp Then Return i
Next
End Function
'==========================================================
private function var_find() As Integer 'return NULL if error
Dim hitem As Integer
'get current hitem in tree
hitem=GetItemTreeView(GTVIEWVAR)
For i As Integer = 1 To vrrnb 'search index variable
	If vrr(i).tv=hitem Then 
		If vrr(i).ad=0 Then messbox("Variable selection error","Dynamic array not yet sized !!"):Return 0
		If vrr(i).vr<0 Then
			Return -i
		Else
			Return i
		EndIf
	End If   	
Next
messbox("Variable selection error","          Select only a variable")
Return 0
End Function
'===========================================================================
private sub var_fill(i As Integer)
If vrr(i).vr<0 Then
    varfind.ty=cudt(-vrr(i).vr).Typ
    varfind.pt=cudt(-vrr(i).vr).pt
    varfind.nm=cudt(-vrr(i).vr).nm
    varfind.pr=vrr(var_parent(vrr(i).tv)).vr'index of the vrb
Else
    varfind.ty=vrb(vrr(i).vr).Typ
    varfind.pt=vrb(vrr(i).vr).pt
    varfind.nm=vrb(vrr(i).vr).nm
    varfind.pr=vrr(i).vr 'no parent so himself, index of the vrb
End If
varfind.ad=vrr(i).ad
varfind.iv=i
varfind.tv=htviewvar '
varfind.tl=vrr(i).tv 'handle line
End Sub
'=====================================
private function watch_find() As Integer
	Dim hitem As Integer
	'get current hitem in tree
	hitem=GetItemTreeView(GTVIEWWCH)
	For k As Integer =0 To WTCHMAX
		If wtch(k).tvl=hitem Then Return k 'found
	Next
End Function
'==================================================================================
private function var_find2(tv As HWND) As Integer 'return -1 if error
    Dim hitem As integer,idx As Integer
    If tv=htviewvar Then
        'get current hitem in tree
        hitem=GetItemTreeView(GTVIEWVAR)
        For i As Integer = 1 To vrrnb 'search index variable
            If vrr(i).tv=hitem Then 
                If vrr(i).ad=0 Then messbox("Variable selection error","Dynamic array not yet sized !!"):Return -1
    			var_fill(i)
                Return i
            End If           
        Next
        messbox("Variable selection error2","         Select only a variable")
        Return -1
    ElseIf tv=htviewwch Then
    	idx=watch_find()
    	If wtch(idx).psk=-3 OrElse wtch(idx).psk=-4 Then Return -1 'case non-existent local
    	If wtch(idx).adr=0 Then Return -1 'dyn array 
    	varfind.nm=Left(wtch(idx).lbl,Len(wtch(idx).lbl)-1)
       varfind.ty=wtch(idx).typ
       varfind.pt=wtch(idx).pnt
       varfind.ad=wtch(idx).adr
       varfind.tv=htviewwch 'handle treeview
       varfind.tl=wtch(idx).tvl 'handle line
       varfind.iv=wtch(idx).ivr
    Else'shw/expand tree
       For idx =1 To SHWEXPMAX
    		If shwexp(idx).tv=tv Then Exit For 'found index matching tview
       Next 
		'get current hitem in tree
		hitem=GetItemTreeView(ID_In_Number(tv))
        For i As Integer = 1 To shwexp(idx).nb 'search index variable
            If vrp(idx,i).tl=hitem Then
				varfind.nm=vrp(idx,i).nm
				If varfind.nm="" Then varfind.nm="<Memory>"
                varfind.ty=vrp(idx,i).Ty
                varfind.pt=vrp(idx,i).pt
                varfind.ad=vrp(idx,i).ad
                varfind.tv=tv 'handle treeview
                varfind.tl=hitem 'handle line
                varfind.iv=-1
                Return i
            End If
        Next
    End If
End Function
'=================================================================
private sub brkv_set(a As Integer) 'breakon variable
   Dim As Integer t,p
   Dim Title As String, j As UInteger,ztxt As ZString*301,tvi As integer
   If a=0 Then 'cancel break
      brkv.adr=0
      setWindowText(brkvhnd,"Break on var")
      'menu_chg(menuvar,idvarbrk,"Break on var")
     ' Modify_Menu(1001,MenName,"Changed")
      Modify_Menu( "Break on var")
      Exit Sub
   ElseIf a=1 Then 'new
       If var_find2(tviewvar)=-1 Then Exit Sub 'search index variable under cursor
       'search master variable
   
       	t=varfind.Ty
         p=varfind.pt
     
   	#Ifdef __FB_64BIT__ ''2017/12/14
   	   If p Then t=9 ''pointer integer64 (ulongint)
   	#Else
         If p Then t=1 ''pointer integer32 (long)
      #EndIf
   	 
      If t>10 AndAlso p=0 AndAlso t<>4 AndAlso t<>13 AndAlso t<>14 Then
        fb_message("Break on var selection error","Only [unsigned] Byte, Short, integer, longint or z/f/string")
        Exit Sub
      End If
      
      
      brkv2.typ=t           'change in brkv_box if pointed value
      brkv2.adr=varfind.ad   'idem
      brkv2.vst=""          'idem
      brkv2.tst=1           'type of test
      brkv2.ivr=varfind.iv
      ' if dyn array store real adr
      If Cast(Integer,vrb(varfind.pr).arr)=-1 Then
         ReadProcessMemory(dbghand,Cast(LPCVOID,vrr(varfind.iv).ini),@brkv2.arr,4,0)
      Else
         brkv2.arr=0
      End If
      
      If vrb(varfind.pr).mem=3 Then
      brkv2.psk=-2 'static
      Else
      For j As UInteger = 1  To procrnb 'find proc to delete watching
      	If varfind.iv>=procr(j).vr And varfind.iv<procr(j+1).vr Then brkv2.psk=procr(j).sk:Exit For
      Next
      End If
      
      tvI.mask       = TVIF_TEXT
      tvI.pszText    = @(ztxt)
      tvI.hitem      = vrr(varfind.iv).tv
      tvI.cchTextMax = 300
      sendmessage(tviewvar,TVM_GETITEM,0,Cast(LPARAM,@tvi))
      '' ??? treat verbose to reduce width ???
      
      ztxt=ztxt
   
   Else 'update
	brkv2=brkv
	getwindowtext(brkvhnd,ztxt,150)
   End If
   brkv2.txt=Left(ztxt,InStr(ztxt,"<"))+var_sh2(brkv2.typ,brkv2.adr,p)
   
   'todo fb_MDialog(@brkv_box,"Test for break on value",windmain,283,25,350,50)
   
End Sub
'======================================================================================================
private sub proc_del(j As Integer,t As Integer=1)
Dim  As Integer tempo,th
Dim parent As HTREEITEM 
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

'' remove array tracking : either suppressed array or one of the variable used as index 2016/08/10
If procr(j+1).vr>trckarr(0).idx AndAlso trckarr(0).idx>=procr(j).vr Then 
	array_tracking_remove
Else
	For i As Long =0 To 4
		If trckarr(i).memadr Then
			If procr(j+1).vr>trckarr(i).iv AndAlso trckarr(i).iv>=procr(j).vr Then 
		   array_tracking_remove
		   Exit For
			End If
		End if
	Next
End If
''close shw/exp except if static
If shwexpnb then
    For i As Integer=1 To SHWEXPMAX
        if shwexp(i).bx Then 
        	if shwexp(i).procr=j then
        		if shwexp(i).mem<>3 then ''only local and not a static
        			''seems not working destroywindow(shwexp(i).bx)
        			sendmessage(shwexp(i).bx,WM_CLOSE,0,0)
        			shwexp(i).bx=0
        			shwexpnb-=1
        		end if
        	end if
        end if
    Next
end if

' compress running variables
tempo=procr(j+1).vr-procr(j).vr
vrrnb-=tempo
For i As Integer = procr(j).vr To vrrnb
	vrr(i)=vrr(i+tempo)
Next


If t=1 Then 'not dll
	th=thread_select(procr(j).thid) 'find thread index
	parent=Cast(HTREEITEM,sendmessage(tviewthd,TVM_GETNEXTITEM,TVGN_PARENT,Cast(LPARAM,thread(th).ptv))) 'find parent of last proc item
	sendmessage(tviewthd,TVM_DELETEITEM,0,Cast(LPARAM,thread(th).ptv)) 'delete item
	thread(th).ptv=parent 'parent becomes the last
	thread_text(th) 'update thread text
EndIf

' compress procr and update vrr index
procrnb-=1
For i As Integer =j To procrnb
	procr(i)=procr(i+1)
	procr(i).vr-=tempo
Next

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
'========================
private sub thread_text(th As Integer=-1)'update text of thread(s)
   Dim libel As String
   Dim As Integer thid,p,lo,hi
   If th=-1 Then
   	lo=0:hi=threadnb
   Else
   	lo=th:hi=th
   EndIf
   For i As Integer=lo To hi
		thid=thread(i).id
		p=proc_find(thid,KLAST)
		libel="threadID="+fmt2(Str(thid),4)+" : "+proc(procr(p).idx).nm
		If flagverbose Then libel+=" HD: "+Str(thread(i).hd)
		If threadhs=thread(i).hd Then libel+=" (next execution)"
		RenameItemTreeView(GTVIEWTHD,thread(i).tv,libel)
   Next
End Sub
'====================== new sub ou func ===============
private sub proc_new()
Dim libel As String
Dim tv As integer
	If procrnb=PROCRMAX Then hard_closing("Max number of sub/func reached limit="+str(PROCRMAX))
	procrnb+=1'new proc ADD A POSSIBILITY TO INCREASE THIS ARRAY
	procr(procrnb).sk=procsk
	procr(procrnb).thid=thread(threadcur).id
	procr(procrnb).idx=procsv
	
	'test if first proc of thread
	If thread(threadcur).plt=0 Then
		procr(procrnb).cl=-1  ' no real calling line
		libel="ThID="+Str(procr(procrnb).thid)+" "
		tv=TVI_LAST 'insert in last position
		AddTreeViewItem(GTVIEWTHD,"Not filled",cast (hicon, 0),0,TVI_LAST,0)
		thread(threadcur).ptv=thread(threadcur).tv 'last proc 
		thread_text()'put text not only current but all to reset previous thread text 
	Else
		procr(procrnb).cl=thread(threadcur).od
		tv=thread(threadcur).plt 'insert after the last item of thread
	EndIf
		
	'add manage LIST
	If flagtrace Then dbg_prt ("NEW proc "+Str(procsv)+" "+proc(procsv).nm)
	libel+=proc(procsv).nm+":"+proc_retval(procsv)
	If flagverbose Then libel+=" ["+Str(proc(procsv).db)+"]"
	
	procr(procrnb).tv=Tree_AddItem(0,libel,tv,tviewvar)
	thread(threadcur).plt=procr(procrnb).tv 'keep handle last item
	
	'add new proc to thread treeview
	thread(threadcur).ptv=Tree_AddItem(thread(threadcur).ptv,"Proc : "+proc(procsv).nm,TVI_FIRST,tviewthd)
	thread_text(threadcur)
	''RedrawWindow tviewthd, 0, 0, 1	todo USELESS ???
	var_ini(procrnb,proc(procr(procrnb).idx).vr,proc(procr(procrnb).idx+1).vr-1)
	procr(procrnb+1).vr=vrrnb+1
	If proc(procsv).nm="main" Then 
		procr(procrnb).vr=1 'constructors for shared are executed before main so reset index for first variable of main 04/02/2014
	EndIf
	proc_watch(procrnb) 'reactivate watched var
	'RedrawWindow tviewvar, 0, 0, 1  	todo USELESS ???
End Sub
'============================= proc ending ============
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
'======================================================
private sub gest_brk(ad As UInteger)
   Dim As UInteger i,debut=1,fin=linenb+1,adr,iold

   Dim vcontext As CONTEXT
   'egality added in case attach (example access violation) without -g option, ad=procfn=0....
   If ad>=procfn Then thread_resume():Exit Sub 'out of normal area, the first exception breakpoint is dummy or in case of use of debugbreak
   
   dbg_prt2("")
   dbg_prt2("AD gest brk="+hex(ad)+" th="+Str(threadcur))
   'show_context
   
   i=thread(threadcur).sv+1
   proccurad=ad
   
   If rline(i).ad<>ad Then 'hope next source line is next executed line (optimization)
   	While 1
   		iold=i
   		i=(debut+fin)\2 'first consider that the addresses are sorted increasing order
   		If i=iold Then 'loop
   			For j As Integer =1 To linenb
   				If rline(j).ad=ad Then i=j:Exit While
   			Next
   		End If	
   		If ad>rLine(i).ad Then
   			debut=i
   		ElseIf ad<rLine(i).ad Then
   			fin=i
   		Else
   			Exit While
   		End If
   	Wend
   EndIf
   
   'restore CC previous line
   If thread(threadcur).sv<>-1 Then WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadcur).sv).ad),@breakcpu,1,0)
   'thread changed by threadcreate or by mutexunlock
   If threadcur<>threadprv Then
   	If thread(threadprv).sv<>i Then 'don't do it if same line otherwise all is blocked.....not sure it's usefull
   		WriteProcessMemory(dbghand,Cast(LPVOID,rLine(thread(threadprv).sv).ad),@breakcpu,1,0) 'restore CC 
   	EndIf
   	stopcode=CSNEWTHRD  'status HALT NEW THREAD
   	runtype=RTSTEP
   	thread_text(threadprv) 'not next executed
   	thread_text(threadcur) 'next executed
   	threadprv=threadcur
   EndIf
   
   thread(threadcur).od=thread(threadcur).sv:thread(threadcur).sv=i
   procsv=rline(i).px
   'dbg_prt2("proc ="+Str(procsv)+" "+proc(procsv).nm+" "+hex(proc(procsv).db)+" "+source(proc(procsv).sr)+" "+hex(proccurad))
   'dbg_prt2("line="+Str(rline(i).nu))
   
   'get and update registers
   vcontext.contextflags=CONTEXT_CONTROL
   GetThreadContext(threadcontext,@vcontext)
   
   If proccurad=proc(procsv).db Then 'is first proc instruction
   
   	If rline(i).sv=85 Then'check if the first instruction is push ebp opcode=85 / push rbp opcode=&h55=85dec
   	
   		'in this case there is a prologue
         'at the beginning of proc EBP not updated so use ESP
   		procsk=vcontext.regsp-SizeOf(Integer) 'ESP-4 for respecting prologue : push EBP then mov ebp,esp / 64bit push rbp then mov rbp,rsp
   	Else
   		If procrnb<>0 Then  'no main and no prologue so naked proc, procrnb not yet updated
   		   procsk=vcontext.regsp
   		   thread(threadcur).nk=procsk
   		Else
   			procsk=vcontext.regsp-20 'if gcc>3 for main prologue is different
   		EndIf
   	End If
   Else
   	 'only for naked, check if return by comparing top of stack because no epilog
       If thread(threadcur).nk Then
   	    If vcontext.regsp>thread(threadcur).nk Then
   	    	thread(threadcur).pe=TRUE
   	    	thread(threadcur).nk=0
   	    EndIf
       End If
   EndIf
   vcontext.regip=ad
   
   setThreadContext(threadcontext,@vcontext)
   'dbg_prt2("PE"+Str(thread(threadcur).pe)+" "+Str(proccurad)+" "+Str(proc(procsv).fn))
   If thread(threadcur).pe Then 'if previous instruction was the last of proc
      If proccurad<>proc(procsv).db Then procsk=vcontext.regbp 'reload procsk with rbp/ebp 04/02/2014 test added for case constructor on shared
   	proc_end():thread(threadcur).pe=FALSE
   EndIf

   If proccurad=proc(procsv).db Then 'is first instruction ?
   	If proctop Then runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0' step call execute one step to reach first line
   	proc_new
   	thread_resume():Exit Sub
   ElseIf proccurad=proc(procsv).fn Then
   	thread(threadcur).pe=TRUE        'is last instruction ?
   EndIf
   If runtype=RTRUN Then
   	' test breakpoint on line
   	If brk_test(proccurad) Then fasttimer=Timer-fasttimer:runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0:dsp_change(i):Exit Sub
   	'test beakpoint on var
   	If brkv.adr<>0 Then
           If brkv_test() Then runtype=RTSTEP:procad=0:procin=0:proctop=FALSE:procbot=0:dsp_change(i):Exit Sub
   	End If
   	If procad Then 	'test out
   		If proc(procad).fn=proccurad Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP 'still running ONE step before stopping
   	ElseIf procin Then 'test over
   		If procsk>=procin Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP:dsp_change(i):Exit Sub
   	ElseIf procbot Then 'test end of proc
         If proc(procbot).fn=proccurad Then procad=0:procin=0:proctop=FALSE:procbot=0:runtype=RTSTEP:dsp_change(i):Exit Sub 'stop on end of proc STEPRETURN
   	End If
   	thread_resume()
   ElseIf runtype=RTFRUN Then
   		fasttimer=Timer-fasttimer
      	For i As Integer = 1 To linenb 'restore CC
   			WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@breakcpu,1,0)
      	Next
      	'WriteProcessMemory(dbghand,Cast(LPVOID,rLine(i).ad),@rLine(i).sv,1,0) 'restore old value for execution
   		brk_test(proccurad) ' cancel breakpoint on line, if command halt not really used
   		proc_newfast   'creating running proc tree 
   		var_sh			'updating information about variables
   		runtype=RTSTEP:dsp_change(i)
   Else 'RTSTEP or RTAUTO
   	If flagattach Then proc_newfast:flagattach=FALSE
   	'NOTA If rline(i).nu=-1 Then
   		'fb_message("No line for this proc","Code added by compiler (constructor,...)")
   	'Else	
   	dsp_change(i)
   	'EndIf
   	If runtype=RTAUTO Then 
   		Sleep(autostep)
   		If threadaut>1 Then 'at least 2 threads
   			Dim As Integer c=threadcur
   			Do
   				c+=1:If c>threadnb Then c=0
   			Loop Until thread(c).exc
   			thread_change(c)
   		EndIf
   		thread_rsm
   	EndIf
   	If threadsel<>threadcur AndAlso messbox("New Thread","Previous thread "+Str(thread(threadsel).id)+" changed by "+Str(thread(threadcur).id) _
   			+Chr(10)+Chr(13)+" Keep new one ?",MB_YESNO)=RETNO Then
   			thread_change(threadsel)
   	Else
   		threadsel=threadcur
   	EndIf
   End If

End Sub
'====================================================================
''  load shared and common variables, input default=no dll number
'====================================================================
private sub globals_load(d As Integer=0)
Dim temp As integer
Dim As Integer vb,ve 'begin/end index global vars
Dim As Integer vridx 
	If vrbgblprev<>vrbgbl Then 'need to do ?
		If vrbgblprev=0 Then
			procr(procrnb).tv= Tree_AddItem(NULL,"Globals (shared/common) in : main ", 0, tviewvar) 'only first time
			var_ini(procrnb,1,vrbgbl)'add vrbgblprev instead 1
			'dbg_prt2("procrnb="+Str(procrnb))
			procr(procrnb+1).vr=vrrnb+1 'to avoid removal of global vars when the first executed proc is not the main one
		Else
			procrnb+=1
			temp=Cast(HTREEITEM,SendMessage(tviewvar,TVM_GETNEXTITEM,TVGN_PREVIOUS,Cast(LPARAM,procr(1).tv)))
			If temp=0 Then 'no item before main
				temp=TVI_FIRST
			EndIf
			If d=0 Then 'called from extract stabs
				d=dllnb
				vb=vrbgblprev+1
				ve=vrbgbl
			Else
				vb=dlldata(d).gblb
				ve=dlldata(d).gblb+dlldata(d).gbln-1
			End If
			procr(procrnb).tv= Tree_AddItem(NULL,"Globals in : "+dll_name(dlldata(d).hdl,2),temp, tviewvar)
			procr(procrnb).sk=dlldata(d).bse
			dlldata(d).tv=procr(procrnb).tv
			var_ini(procrnb,vb,ve)
			procr(procrnb+1).vr=vrrnb+1 'put lower limit for next procr
			procr(procrnb).idx=0
			
			If wtchcpt Then
				For i As Integer= 0 To WTCHMAX
   				If wtch(i).psk=-3 Then 'restart
   					If wtch(i).idx=0 Then 'shared dll
           				wtch(i).adr=vrr(procr(procrnb).vr+wtch(i).dlt).ad'06/02/2014  wtch(i).dlt+procr(procrnb).sk
           				wtch(i).psk=procr(procrnb).sk
   					EndIf
   				ElseIf wtch(i).psk=-4 Then 'session watch
   					If wtch(i).idx=0 Then 'shared dll
							vridx=var_search(procrnb,wtch(i).vnm(),wtch(i).vnb,wtch(i).var,wtch(i).pnt)
							If vridx=-1 Then fb_message("Proc watch","Running var not found"):Continue For
							var_fill(vridx)
	     					watch_add(wtch(i).tad,i)
   					EndIf
   				EndIf
				Next
			EndIf
			
		EndIf
		RedrawWindow tviewvar, 0, 0, 1
	EndIf
End Sub
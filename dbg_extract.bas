''debug data extraction 
''dbg_extract.bas

'#ifdef  __fb_linux__
	#define K64BIT
	#define of_entry &h18
	#ifdef K64BIT
		#define of_section &h28
		#define of_section_size &h3A
		#define of_section_num &h3C
		#define of_section_str &h3E
		#define sect_size &h40
		#define of_offset_infile &h18
		#define of_size_infile &h20
	#else
		#define of_section &h20
		#define of_section_size &h2E
		#define of_section_num &h30
		#define of_section_str &h32
		#define sect_size &h28
		#define of_offset_infile &h10
		#define of_size_infile &h14
	#endif
'#endif
union ustab
	full as longint
	type
		offst as long
		cod as short
		desc as short
	end type
end union

'' ----------------------------
'' check if source yet stored
'' ----------------------------
private function check_source(sourcenm As String) As integer
   For i As Integer=0 To sourcenb
      If source(i)=sourcenm Then Return i 'found
	Next
	Return -1 'not found
End function
'' -------------------------------
'' Handling main source code=100
'' -------------------------------
private sub dbg_file(strg as string,value as integer)
	static as string path
	dim as string fullname
	if strg="" then ''end of main ?
		print "---------------- end of current debug data -------------------"
	else
		if right(strg,4)<>".bas" and right(strg,3)<>".bi" then
			path=strg
		else
			fullname=lcase(path+strg)
			path=""
			print "full source name=";fullname
			if check_source(fullname)=-1 then ''useful check_source ???
				sourcenb+=1
				source(sourcenb)=fullname
				sourceix=sourcenb
				print "new source=";fullname
			else
				print "keep current source with code 100 check that =";strg
			end if
		end if
	end if
end sub
'' --------------------------------------------------------
'' Handling include or return in previous source code=132
'' --------------------------------------------------------
private sub dbg_include(strg as string)
	dim as integer temp
	if instr(strg,any "/\")=0 then ''just the name no path
		strg=left(source(0),instrrev(source(0),any "/\"))+strg ''adding path
	end if
	strg=LCase(strg)
	temp=check_source(strg)
	if temp=-1 Then
		sourcenb+=1
		source(sourcenb)=strg
		sourceix=sourcenb
		print "new source=";strg
	else
	  	sourceix=temp
	  	print "keep current source=";strg
	end if
end sub
'' -----------------------
'' Handling line code=68
'' -----------------------
private sub dbg_line(linenum as integer,ofset as integer)
	if linenum then
		if linenum>lastline then
			if ofset+proc(procnb).db<>rline(linenb).ad Then ''checking to avoid asm with just comment line
 				linenb+=1
			endif
			rline(linenb).ad=ofset+proc(procnb).db
			rLine(linenb).nu=linenum
			rLine(linenb).px=procnb
			
			''to be checked maybe fixed so useless 
			rline(linenb).sx=sourceix ''for line in include and not in a proc
			
			''to be checked if still usefull
			If ofset<>0 Then lastline=linenum ''first proc line always coded 1 but ad=0
			
			print "linenum=";linenum;" adress=";rline(linenb).ad
		else
			print "linenum=";linenum;" not>lastline=";lastline
		endif		
	else
		print "line number=0"
	end if
end sub
'' ---------------------------------------------
'' Handling procedure sub/function/etc code=36
'' ---------------------------------------------
private sub dbg_proc(strg as string,linenum as integer,adr as integer)
	dim as string procname
	if linenum then
		procnodll=false
		'procname=parsing_proc(strg) ''to be added
		procname=left(strg,instr(strg,":")-1)
		if procname<>"" and (flagmain=true or procname<>"main") then
			'If InStr(procname,".LT")=0 then  ''to be checked if useful
		 	If flagmain=TRUE And procname="main" Then
		 		flagmain=false
		 		'flagstabd=TRUE'first main ok but not the others
				print "main found"
		 	endif
			procnodll=true
	
			procnb+=1
			proc(procnb).sr=sourceix
			proc(procnb).nm=procname
			proc(procnb).db=adr'+exebase-baseimg 'only when <> exebase and baseimg (DLL)
			''to be added
			'cutup_retval(procnb,Mid(strg,InStr(strg,":")+2,99))'return value .rv + pointer .pt 
			proc(procnb).st=1 'state no checked
			proc(procnb).nu=linenum
			lastline=0
			proc(procnb+1).vr=proc(procnb).vr 'in case there is not param nor local var
			proc(procnb).rvadr=0 'for now only used in gcc case 19/08/2015
			
			print "proc =";proc(procnb).nm;" in source=";source(proc(procnb).sr)
		end if
	else
		proc(procnb).ed=proc(procnb).db+adr
		print "end of proc=";proc(procnb).ed
		
		if proc(procnb).fn>procfn Then procfn=proc(procnb).fn+1 ' just to be sure to be above see gest_brk
		
		''to be checked ??????
		'for proc added by fbc (constructor, operator, ...) '11/05/2014 adding >2 to avoid case only one line ...
		If proc(procnb).nu=rline(linenb).nu AndAlso linenb>2 then
			proc(procnb).nu=-1	               	
           	'For i As Integer =1 To linenb
           		'dbg_prt2("Proc db/fn inside for stab="+Hex(proc(procnb).db)+" "+Hex(proc(procnb).fn))
           		'dbg_prt2("Line Adr="+Hex(rline(i).ad)+" "+Str(rline(i).ad))
           		'If rline(i).ad>=proc(procnb).db AndAlso rline(i).ad<=proc(procnb).fn Then
           			'dbg_prt2("Cancel breakpoint adr="+Hex(rline(i).ad)+" "+Str(rline(i).ad))
           			'WriteProcessMemory(dbghand,Cast(LPVOID,rline(i).ad),@rLine(i).sv,1,0)
           			'nota rline(linenb).nu=-1
           		'EndIf
           '	next
		end if
		
        ''removing {modlevel empty just prolog and epilog
        if proc(procnb).nm="{MODLEVEL}" And proc(procnb).fn-proc(procnb).db <8 Then
             ''removing lines
            for iline As Long = linenb To 1 Step -1
                If rline(iline).px=procnb Then
                   'writeProcessMemory(dbghand,Cast(LPVOID,rline(iline).ad),@rLine(iline).sv,1,0) 'restore to avoid stop
                   linenb-=1
                Else
                   Exit For
                EndIf
            next
            print "remove modlevel in"+source(proc(procnb).sr)
            procnb-=1 ''removing proc
        end if
		
	end if
end sub
'' ------------------------------------
'' Handling procedure epilog code=224
'' ------------------------------------	
private sub dbg_epilog(ofset as integer)
	proc(procnb).fn=proc(procnb).db+ofset
	print "epilog of proc=";proc(procnb).fn
end sub
'' -------------------------------
'' Extracting debug data for elf
'' -------------------------------
private sub load_dat(byval ofset as integer,byval size as integer,byval ofstr as integer)
	dim as ustab stab
	dim as longint value
	dim as string strg
	ofstr+=1 ''1 based
	dim as longint buf(1) ''16 bytes

	
	for ibuf as integer =1 to size/16
		get #1,ofset+1,buf()
		print ibuf;" "; hex(buf(0));" --> ";
		stab.full=buf(0)
		print "C="+str(stab.cod)+" D="+str(stab.desc)+" O="+str(stab.offst);" ";
		
		strg=space(400)
		get #1,ofstr+stab.offst,strg
		strg=""+*strptr(strg)
		print strg;" ";
		
		value=buf(1)
		print "D="+str(value)+" H="+hex(value)
		
		select case as const stab.cod
			case 100 '' file name
				dbg_file(strg,value)
			case 255 ''not as standard stab freebasic version and maybe other information
				print "compiled with=";strg
			case 128 ''type
				'dbg_type(strg)
				print "missing procedure stab cod=128"
			case 132 '' file name
				dbg_include(strg)
			case 36 ''procedure
				dbg_proc(strg,stab.desc,value)
			case 68 ''line
				dbg_line(stab.desc,value)
			case 224 ''address epilog
				dbg_epilog(value)
			case 160 ''
				'dbg_param(strg,value)
				print "missing procedure stab cod=160"
			case 42 ''main entry point
				'not used
			case 40 '' variable
				print "missing procedure stab cod=40"
			case else
				print "Unknow stab cod=";stab.cod
		end select
		ofset+=16
	next
	print
end sub
'' ------------------------------------------------------------------------------------
'' Retrieving sections .dbgdat (offset and size) and .dbgdat (offset) in the elf file
'' ------------------------------------------------------------------------------------
private sub elf_extract(filename as string)
dim lgf As Integer,ulgt as integer,ulg as ulong,usht as ushort,ofset as ulongint
dim as integer start_section,str_section,sect_num,walk_section,dbg_dat_of,dbg_dat_size,dbg_str_of
dim as string sect_name=space(40)

open filename for binary as #1
lgf=lof(1)
'print "lenght=";lgf

ofset=of_entry
get #1,ofset+1,ulgt
'print "entry=";hex(ulgt)

ofset=of_section
get #1,ofset+1,ulgt
'print "start section header=";hex(ulgt)
start_section=ulgt

ofset=of_section_size
get #1,ofset+1,usht
'print "section size=";usht


ofset=of_section_num
get #1,ofset+1,usht
'print "section number=";usht
sect_num=usht

ofset=of_section_str
get #1,ofset+1,usht
'print "section string=";usht

ofset=start_section+(usht)*&h40+&h18 '(&h28 si 32bit)
get #1,ofset+1,ulgt
'print "start offset string=";hex(ulgt)
str_section=ulgt

''sections

walk_section=start_section
for isec as integer = 1 to sect_num
	'print "section=";isec;" ";
	
	ofset=walk_section
	get #1,ofset+1,ulg ''offset in str table
	'print "offset string=";ulg;" ";
	ofset=str_section+ulg
	sect_name=space(40)
	get #1,ofset+1,sect_name
	sect_name=""+*strptr(sect_name)
	'print "name=";sect_name

	ofset=walk_section+of_offset_infile
	get #1,ofset+1,ulgt
	'print "offset in file= ";hex(ulgt);" ";
	if sect_name=".dbgdat" then
		dbg_dat_of=ulgt
	elseif sect_name=".dbgstr" then
		dbg_str_of=ulgt
		exit for ''not anymore section to retrieve
	end if
	
	ofset=walk_section+of_size_infile
	get #1,ofset+1,ulgt
	'print "size= ";hex(ulgt);" ";ulgt
	if sect_name=".dbgdat" then
		dbg_dat_size=ulgt
	endif

	walk_section+=sect_size
next
load_dat(dbg_dat_of,dbg_dat_size,dbg_str_of)

close #1
end sub

'=================================================================
'===== DEBUGGER FOR FREEBASIC === (C) 2006-2021 Laurent GRAS =====
'=================================================================
#include once "dbg_defines.bi"

''source files
dim Shared as String  source(SRCMAX)        ''source names
dim shared as any ptr sourceptr(SRCMAX)     ''pointer doc scintilla
dim Shared As UByte   sourcebuf(SRCSIZEMAX) ''buffer for loading source file
dim Shared as Integer sourcenb =-1          ''number of src, 0 based
dim Shared As integer sourceix              ''source index when loading data
dim shared as any ptr currentdoc            ''current doc pointer  todo set local in source_load ?
dim Shared As integer srccur				''index source line to be executed
dim Shared As integer srcdisplayed			''index displayed source

''lines
dim Shared As Integer linenb,rlineprev ''numbers of lines, index of previous executed line (rline)
dim Shared As Integer linenbprev ''used for dll
dim Shared As Integer lastline
dim Shared as tline rline(LINEMAX) ''1 based
dim Shared As Integer linecur      ''line to be executed (inside source)

''procedures
dim Shared as tproc proc(PROCMAX) ''list of procs in code
dim shared As integer procnb

dim Shared As integer procsv,procad,procin,procsk,proccurad,procregsp,procfn,procbot,proctop,procsort
dim Shared As tprocr procr(PROCRMAX) ''list of running proc
dim shared As Integer procrnb

''arrays
dim Shared  As tarr arr(ARRMAX)
dim shared as integer arrnb

''variables
dim Shared vrbloc      As Integer ''pointer of loc variables or components (init VGBLMAX+1)
Dim Shared vrbgbl      As Integer ''pointer of globals or components
Dim Shared vrbgblprev  As Integer ''for dll, previous value of vrbgbl, initial 1
Dim Shared vrbptr      As Integer Ptr ''equal @vrbloc or @vrbgbl
Dim Shared vrb(VARMAX) As tvrb ''1 based

''running variables
Dim Shared vrr(VRRMAX) As tvrr
Dim Shared vrrnb As integer

''tracking arrays
Dim Shared As ttrckarr trckarr(TRCKARRMAX)

''udt/structures
Dim Shared udt(TYPEMAX) As tudt,udtidx As Integer
Dim Shared cudt(CTYPEMAX) As tcudt,cudtnb As Integer,cudtnbsav As Integer
'in case of module or DLL the udt number is initialized each time
Dim Shared As Integer udtcpt,udtmax 'current, max cpt

'dwarf management todo remove
'Dim Shared As Long udtbeg,cudtbeg,locbeg,vrbbeg,prcbeg

''excluded lines
Dim Shared As integer excldnb
Dim Shared As texcld excldlines(EXCLDMAX)


''put in a ctx with type ??
dim shared As boolean procnodll
dim shared As boolean flagmain

''handle
dim shared as HWND mainwindow,scint

''flags
''todo type and tflag and flag.restart, etc
dim shared as boolean flagrestart = true

dim shared as zstring *2 slash
#Ifdef __fb_win32__
	slash="\"
#else
	slash="/"
#endif

udt(0).nm="Unknown"

#Ifdef __FB_64BIT__
   udt(1).nm="long":udt(1).lg=Len(Long)
#Else
   udt(1).nm="Integer":udt(1).lg=Len(Integer)
#EndIf
udt(2).nm="Byte":udt(2).lg=Len(Byte)
udt(3).nm="Ubyte":udt(3).lg=Len(UByte)
udt(4).nm="Zstring":udt(4).lg=Len(Integer)
udt(5).nm="Short":udt(5).lg=Len(Short)
udt(6).nm="Ushort":udt(6).lg=Len(UShort)
udt(7).nm="Void":udt(7).lg=Len(Integer)

''todo remove
'udt(7).index=7'dwarf

#Ifdef __FB_64BIT__
   udt(8).nm="Ulong":udt(8).lg=Len(ULong)
#Else
   udt(8).nm="Uinteger":udt(8).lg=Len(UInteger)
#EndIf

#Ifdef __FB_64BIT__
   udt(9).nm="Integer":udt(9).lg=Len(Integer)
#Else
   udt(9).nm="Longint":udt(9).lg=Len(LongInt)
#EndIf

#Ifdef __FB_64BIT__
   udt(10).nm="Uinteger":udt(10).lg=Len(UInteger)
#Else
   udt(10).nm="Ulongint":udt(10).lg=Len(ULongInt)
#EndIf

udt(11).nm="Single":udt(11).lg=Len(Single)
udt(12).nm="Double":udt(12).lg=Len(Double)
udt(13).nm="String":udt(13).lg=Len(String)
udt(14).nm="Fstring":udt(14).lg=Len(Integer)
udt(15).nm="fb_Object":udt(15).lg=Len(UInteger)
udt(16).nm="Boolean":udt(16).lg=Len(boolean)

dim shared as string exename

''============================
#include "dbg_tools.bas"
#include "dbg_extract.bas"
#include "dbg_gui.bas"
#include "dbg_buttons.bas"




gui_init
reinit

SetStatusBarField(1,0,100,"Loading data")
exename="D:\telechargements\win9\tmp\test_include_main"
dim as string title="Fbdebugger "+exename
setwindowtext(mainwindow,strptr(title))
elf_extract(exename)
list_all
print "------------------ after extraction -----------------------"
print "Number of files=";sourcenb

'hidewindow(scint ,1)
for isrc as integer =0 to sourcenb
	source(isrc)="D:\telechargements\win9\tmp\"+source_name(source(isrc))
	print "index=";isrc;" file=";source(isrc)

	AddPanelGadgetItem(GSRCTAB,isrc,source_name(source(isrc)))
	''later sort the files to get them in alphabetic order
	AddComboBoxItem(GFILELIST,source_name(source(isrc)),-1)
next
'SetGadgetFont(GSRCTAB,CINT(LoadFont("Courier New",10)))
source_load(0,filedatetime(exename))

''for testing to be removed
	
	line_color(5,2)
	line_color(10,2)
	for imark as Integer = 0 To 5
		send_sci(SCI_MarkerAdd, imark, imark)       'line, marker#
	next
hidewindow(scint ,0)
SetStatusBarField(1,0,100,"Waiting")
'============================================
''changes the displayed source
'============================================
private sub source_change(numb as integer)
	static as integer numbold=-1
	dim as any ptr ptrdoc
	if numb=numbold then exit sub
	numbold=numb
	ptrdoc=cast(any ptr,Send_sci(SCI_GETDOCPOINTER,0,0))
	Send_sci(SCI_ADDREFDOCUMENT,0,ptrdoc)
	Send_sci(SCI_SETDOCPOINTER,0,sourceptr(numb))
end sub

''========================================
''========================================
''========================================
''========================================


''=================================================
''contextual menus
#define MNSETBRK 1001
#define MNDISAS  1002
#define MNLOCSRC 1101
#define MNDISPROC 1102
#define MNEDITVAR 1201
#define MNDUMPVAR 1202

var HMenusource=CreatePopMenu()
MenuItem(1001,HMenusource,"Set breakpoint")
MenuItem(1002,HMenusource,"Disass line")

var HMenuvar=CreatePopMenu()
MenuItem(1201,HMenuvar,"Edit var")
MenuItem(1202,HMenuvar,"Dump var")


var HMenuprc=CreatePopMenu()
MenuItem(1101,HMenuprc,"Locate in source")
MenuItem(1102,HMenuprc,"Disable proc")



do
	Var event=WaitEvent()
	If Event=EventClose then
		if messbox("Quit Fbdebugger","Are you sure ? (pgm still running)",MB_YESNO)=6 then
			''todo need to release doc SCI_RELEASEDOCUMENT(<unused>, pointer doc)
			release_doc
			end
		endif
	end if
	If event=EventMenu then
		Select case EventNumber
			case MNSETBRK
				MessBox("","Set breakpoint")
			case MNDISAS
				MessBox("","Disass line")
			case MNLOCSRC
				MessBox("","Locate in source")
			case MNDISPROC
				MessBox("","Disable proc")
		End select
	ElseIf event=eventrbdown then
		if GlobalMouseX<500 then
			DisplayPopupMenu(HMenusource, GlobalMouseX,GlobalMouseY)
		else
			if PanelGadgetGetCursel(GRIGHTTABS)=0 then
				DisplayPopupMenu(HMenuvar, GlobalMouseX,GlobalMouseY)
			elseif PanelGadgetGetCursel(GRIGHTTABS)=1 then
				DisplayPopupMenu(HMenuprc, GlobalMouseX,GlobalMouseY)
			endif
		endif
	elseif event=eventgadget then
		if eventnumber()=GFILESEL Then
        	if GetItemComboBox(GFILELIST)<>-1 then
        		if GetItemComboBox(GFILELIST)<>PanelGadgetGetCursel(GSRCTAB) then
	        		MessBox("Jumping to file ="+str(GetItemComboBox(GFILELIST)),source(GetItemComboBox(GFILELIST)))
	        		PanelGadgetSetCursel(GSRCTAB,GetItemComboBox(GFILELIST))
        			source_change(GetItemComboBox(GFILELIST))
        		else
        			MessBox("File already displayed",source(GetItemComboBox(GFILELIST)))
        		endif
        	else
        		messbox("Select a file", "before clicking on the button")
        	endif
		elseif eventnumber()=GCURRENTLINE then
			if PanelGadgetGetCursel(GSRCTAB)<>0 then
				messbox("Click on Current line","Jumping to the file/line")
				source_change(0)''todo doc where is the next executed line
				send_sci(SCI_SETFIRSTVISIBLELINE, 2,0)
				send_sci(SCI_LINESCROLL,0,-5)
				line_color(5,0)
			end if
		elseif eventnumber()=GSRCTAB then
			Messbox("The selected panel for file:",source(PanelGadgetGetCursel(GSRCTAB)))
			source_change(PanelGadgetGetCursel(GSRCTAB))
		else
			button_action(eventnumber())
		endif
	endif
loop

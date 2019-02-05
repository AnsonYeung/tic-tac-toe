Program TicTacToe;
Uses Crt, Windows, Console, Board;
Const
WinWidth: Integer = 150;
MenuOptionCount: Integer = 3;
Type
EKey = Procedure (event: KEY_EVENT_RECORD);
EMouse = Procedure (event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (event: FOCUS_EVENT_RECORD);
EMenu = Procedure (event: MENU_EVENT_RECORD);

Var
irInBuf: Array[0..127] Of INPUT_RECORD;
cNumRead: DWord;
i: Integer;
onKey: EKey;
onMouse: EMouse;
onWinBufferSize: EWinBufferSize;
onFocus: EFocus;
onMenu: EMenu;
menuOptions: Array[1..3] Of String;
menuSelected: 1..3;

Procedure Noop(event: KEY_EVENT_RECORD); Begin End;
Procedure Noop(event: MOUSE_EVENT_RECORD); Begin End;
Procedure Noop(event: WINDOW_BUFFER_SIZE_RECORD); Begin End;
Procedure Noop(event: FOCUS_EVENT_RECORD); Begin End;
Procedure Noop(event: MENU_EVENT_RECORD); Begin End;

Function StrDup(str: String; cnt: Integer): String;
Var
result: String;
i: Integer;
Begin
	result := '';
	For i := 1 To cnt Do
		result := result + str;
	StrDup := result;
End;

Procedure WriteCenter(str : String; ln : Integer);
Begin
	GoToXY((WinWidth - Length(str)) Div 2, ln);
	Write(str);
End;

Procedure WriteFileCenter(filename: String);
Var
textFile: Text;
i: Integer;
str: String;
Begin
	Assign(textFile, filename);
	Reset(textFile);
	i := -1;
	While Not EOF(textFile) Do
	Begin
		Inc(i);
		ReadLn(textFile, str);
		WriteCenter(str, i);
	End;
	Close(textFile);
End;

Procedure WriteButtonCenter(description: String; ln: Integer; hover: Boolean);
Var len: Integer;
Begin
	If hover Then TextBackground(Brown)
	Else TextBackground(Blue);
	len := Length(description);
	WriteCenter(StrDup(' ', len + 6), ln);
	WriteCenter('   ' + description + '   ', ln + 1);
	WriteCenter(StrDup(' ', len + 6), ln + 2);
	TextBackground(Black);
End;

Function CoordInCenteredButton(mousePos: Coord; descrLen: Integer; ln: Integer): Boolean;
Begin
	CoordInCenteredButton := (mousePos.X >= (WinWidth - descrLen - 6) Div 2) And
		(mousePos.X <= (WinWidth + descrLen + 6) Div 2) And
		(mousePos.Y >= ln) And
		(mousePos.Y <= ln + 2);
End;

Procedure InitConsole();
Begin
	SetConsoleOutputCP(437);
	TextBackground(Black);
	TextColor(White);
	GoToXY(1, 1);
	CursorOff();
	Console.InitConsole();
	SetConsoleSize(WinWidth, 45);
	ClrScr();
End;

Procedure StopProgram();
Begin
	RestoreConsole();
	Halt();
End;

Procedure WriteMenuButton(i: Integer; hover: Boolean);
Begin
	WriteButtonCenter(menuOptions[i], 15 + i * 5, hover);
End;

Procedure MenuSelection();
Begin
	Case menuSelected Of
		1: Begin
			WriteLn(1);
		End;
		2: Begin
			ClrScr();WriteLn(2);ReadKey();StopProgram();
		End;
		3: StopProgram();
	End;
End;

Procedure MenuEvent(event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		13: MenuSelection();
		38: Begin
			WriteMenuButton(menuSelected, False);
			menuSelected := ((menuSelected - 2 + MenuOptionCount) Mod MenuOptionCount) + 1;
			WriteMenuButton(menuSelected, True);
		End;
		40: Begin
			WriteMenuButton(menuSelected, False);
			menuSelected := (menuSelected Mod MenuOptionCount) + 1;
			WriteMenuButton(menuSelected, True);
		End;
	End;
End;

Procedure MenuEvent(event: MOUSE_EVENT_RECORD);
Var button, i: Integer;
Begin
	button := 0;
	For i := 1 To MenuOptionCount Do
		If CoordInCenteredButton(event.dwMousePosition, Length(menuOptions[i]), 15 + i * 5) Then
			button := i;
	Case event.dwEventFlags Of
		0: If button <> 0 Then MenuSelection();
		MOUSE_MOVED:
		If (button <> 0) And (menuSelected <> button) Then
		Begin
			WriteMenuButton(menuSelected, False);
			WriteMenuButton(button, True);
			menuSelected := button;
		End;
	End;
End;

Begin
	InitConsole();
	WriteFileCenter('scenes/splash.txt');
	{ Write('437(' + #201 + ') Unicode(═)'); }
	Sleep(3000);
	ClrScr();
	WriteFileCenter('scenes/menu_title.txt');
	menuOptions[1] := 'Play';
	menuOptions[2] := 'Options';
	menuOptions[3] := 'Exit';
	menuSelected := 1;
	For i := 1 To MenuOptionCount Do
	Begin
		WriteMenuButton(i, menuSelected = i);
	End;
	onKey := @MenuEvent;
	onMouse := @MenuEvent;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	While True Do
	Begin
		PollConsoleInput(irInBuf, 128, cNumRead);
		For i := 0 To 127 Do
		Case irInBuf[i].EventType Of
			KEY_EVENT: onKey(irInBuf[i].Event.KeyEvent);
			{ MOUSE_EVENT(0x0002) is declared both as a function and a constant }
			2: onMouse(irInBuf[i].Event.MouseEvent);
			WINDOW_BUFFER_SIZE_EVENT: onWinBufferSize(irInBuf[i].Event.WindowBufferSizeEvent);
			FOCUS_EVENT: onFocus(irInBuf[i].Event.FocusEvent);
			MENU_EVENT: onMenu(irInBuf[i].Event.MenuEvent);
		End;
	End;
End.
Program TicTacToe;
Uses Windows, MyCrt, Board;
Const
WinWidth: Integer = 150;
MenuOptionCount: Integer = 3;
Type
EKey = Procedure (Const event: KEY_EVENT_RECORD);
EMouse = Procedure (Const event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (Const event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (Const event: FOCUS_EVENT_RECORD);
EMenu = Procedure (Const event: MENU_EVENT_RECORD);

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
Width, Height, RequiredRow: Integer;
gameBoard: TBoard;
boxSelected: Coord;
currentPlayer: Integer;

Procedure Noop(Const event: KEY_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MOUSE_EVENT_RECORD); Begin End;
Procedure Noop(Const event: WINDOW_BUFFER_SIZE_RECORD); Begin End;
Procedure Noop(Const event: FOCUS_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MENU_EVENT_RECORD); Begin End;

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
	If hover Then TextBackground(Yellow)
	Else TextBackground(Blue);
	len := Length(description);
	WriteCenter(StrDup(' ', len + 6), ln);
	WriteCenter('   ' + description + '   ', ln + 1);
	WriteCenter(StrDup(' ', len + 6), ln + 2);
	TextBackground(Black);
End;

Function CoordInCenteredButton(mousePos: Coord; descrLen: Integer; ln: Integer): Boolean;
Begin
	CoordInCenteredButton := (mousePos.X >= (WinWidth - descrLen) Div 2 - 3) And
		(mousePos.X <= (WinWidth + descrLen) Div 2 + 2) And
		(mousePos.Y >= ln) And
		(mousePos.Y <= ln + 2);
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

Procedure EnterMenu(); Forward;
Procedure EnterGame(); Forward;

Procedure MenuSelection();
Begin
	Case menuSelected Of
		1: Begin
			EnterGame();
		End;
		2: Begin
			ClrScr();
			WriteLn('Press any key to return to menu');
			ReadLn();
			EnterMenu();
		End;
		3: StopProgram();
	End;
End;

Procedure MenuEvent(Const event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		VK_UP: Begin
			WriteMenuButton(menuSelected, False);
			menuSelected := ((menuSelected - 2 + MenuOptionCount) Mod MenuOptionCount) + 1;
			WriteMenuButton(menuSelected, True);
		End;
		VK_DOWN: Begin
			WriteMenuButton(menuSelected, False);
			menuSelected := (menuSelected Mod MenuOptionCount) + 1;
			WriteMenuButton(menuSelected, True);
		End;
	End
	Else
	Case event.wVirtualKeyCode Of
		VK_RETURN: MenuSelection();
	End;
End;

Procedure MenuEvent(Const event: MOUSE_EVENT_RECORD);
Var button, i: Integer;
Begin
	If event.dwButtonState = 0 Then
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
End;

Procedure EnterMenu();
Begin
	SetConsoleSize(WinWidth, 45);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	WriteFileCenter('scenes/menu_title.txt');
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
End;

Procedure DrawBoxSelected(hover: Boolean);
Begin
	DrawBox(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y], hover);
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer);
Begin
	If (boxSelected.X <> X) Or (boxSelected.Y <> Y) Then
	Begin
		SetConsoleBuffer(Width * BoxWidth + 1, Height * BoxHeight);
		DrawBoxSelected(False);
		boxSelected.X := X;
		boxSelected.Y := Y;
		DrawBoxSelected(True);
		SetConsoleBuffer(Width * BoxWidth, Height * BoxHeight);
	End;
End;

Procedure GameSelection();
Var
i, j, num: Integer;
cnts: Array[1..4] Of Integer;
tmpSelected: Coord;
draw: Boolean;
leaving: Boolean;
Begin
	leaving := False;
	gameBoard[boxSelected.X][boxSelected.Y] := currentPlayer;
	For i := 1 To 4 Do
		cnts[i] := -1;
	{ Method: Count in 8 directions from boxSelected }
	For i := -1 To 1 Do
		For j := -1 To 1 Do
		Begin
			If (i <> 0) Or (j <> 0) Then
			Begin
				tmpSelected := boxSelected;
				num := Abs(i * 3 + j);
				While gameBoard[tmpSelected.X][tmpSelected.Y] = currentPlayer Do
				Begin
					tmpSelected.X := tmpSelected.X + i;
					tmpSelected.Y := tmpSelected.Y + j;
					Inc(cnts[num]);
					If Not (tmpSelected.X In [0..Width - 1]) Or Not (tmpSelected.Y In [0..Height - 1]) Then
						Break;
				End;
			End;
		End;
	For i := 1 To 4 Do
		If cnts[i] >= RequiredRow Then
		Begin
			EnterMenu();
			leaving := True;
		End;
	draw := True;
	For i := 0 To Width - 1 Do
		For j := 0 To Height - 1 Do
			If gameBoard[i][j] = 0 Then
				draw := False;
	If draw Then
	Begin
		EnterMenu();
		leaving := True;
	End;
	{ Redraw the selection to red }
	If Not leaving Then
	Begin
		i := boxSelected.X;
		If boxSelected.X = 0 Then
			boxSelected.X := 1
		Else
			boxSelected.X := 0;
		GameChangeSelection(i, boxSelected.Y);
		currentPlayer := -currentPlayer;
	End;
End;

Procedure GameEvent(Const event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		VK_UP: GameChangeSelection(boxSelected.X, (boxSelected.Y - 1 + Height) Mod Height);
		VK_DOWN: GameChangeSelection(boxSelected.X, (boxSelected.Y + 1) Mod Height);
		VK_LEFT: GameChangeSelection((boxSelected.X - 1 + Width) Mod Width, boxSelected.Y);
		VK_RIGHT: GameChangeSelection((boxSelected.X + 1) Mod Width, boxSelected.Y);
	End
	Else
	Case event.wVirtualKeyCode Of
		VK_RETURN: GameSelection();
	End;
End;

Procedure GameEvent(Const event: MOUSE_EVENT_RECORD);
Var
i, j, hoverX, hoverY: Integer;
Begin
	{ Mouse released }
	If event.dwButtonState = 0 Then
	Begin
		hoverX := -1;
		hoverY := -1;
		For i := 0 To Width - 1 Do
			For j := 0 To Height - 1 Do
				If MouseInBox(event.dwMousePosition.X, event.dwMousePosition.Y, i, j) Then
				Begin
					hoverX := i;
					hoverY := j;
				End;
		If (hoverX <> -1) And (hoverY <> -1) Then
		Case event.dwEventFlags Of
			0: GameSelection();
			MOUSE_MOVED:
			If (boxSelected.X <> hoverX) Or (boxSelected.Y <> hoverY) Then
				GameChangeSelection(hoverX, hoverY);
		End;
	End;
End;

Procedure EnterGame();
Begin
	onKey := @GameEvent;
	onMouse := @GameEvent;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
	gameBoard := InitBoard(Width, Height);
	boxSelected.X := 1;
	boxSelected.Y := 0;
	currentPlayer := 1;
	SetConsoleSize(Width * BoxWidth, Height * BoxHeight);
	SetConsoleBuffer(Width * BoxWidth + 1, Height * BoxHeight);
	DrawBoard(gameBoard);
	GameChangeSelection(0, 0);
End;

Begin
	InitConsole();
	SetConsoleSize(WinWidth, 45);
	ClrScr();
	WriteFileCenter('scenes/splash.txt');
	Sleep(3000);
	menuOptions[1] := 'Play';
	menuOptions[2] := 'Options';
	menuOptions[3] := 'Exit';
	Width := 3;
	Height := 3;
	RequiredRow := 3;
	EnterMenu();
	{ Infinite Event Loop }
	While True Do
	Begin
		PollConsoleInput(irInBuf, 128, cNumRead);
		For i := 0 To 127 Do
		Case irInBuf[i].EventType Of
			KEY_EVENT: onKey(irInBuf[i].Event.KeyEvent);
			2: onMouse(irInBuf[i].Event.MouseEvent);
			WINDOW_BUFFER_SIZE_EVENT: onWinBufferSize(irInBuf[i].Event.WindowBufferSizeEvent);
			FOCUS_EVENT: onFocus(irInBuf[i].Event.FocusEvent);
			MENU_EVENT: onMenu(irInBuf[i].Event.MenuEvent);
		End;
	End;
End.
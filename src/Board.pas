Unit Board;
Interface
Uses MyCrt;
Const
BoxWidth: Integer = 5;
BoxHeight: Integer = 3;
Type
BoxState = -1..1;
TBoard = Array Of Array Of BoxState;

Function InitBoard(Width: Integer; Height: Integer): TBoard;
Procedure DrawBox(x, y: Integer; state: BoxState; hover: Boolean);
Procedure DrawBoard(Const gameBoard: TBoard);
Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;

Implementation

Function InitBoard(Width: Integer; Height: Integer): TBoard;
Var
gameBoard: TBoard;
i, j: Integer;
Begin
	SetLength(gameBoard, Width);
	For i := 0 To Width - 1 Do
	Begin
		SetLength(gameBoard[i], Height);
		For j := 0 To Height - 1 Do
			gameBoard[i][j] := 0;
	End;
	InitBoard := gameBoard;
End;

Procedure DrawBox(x, y: Integer; state: BoxState; hover: Boolean);
Var
i, j: Integer;
Begin
	TextColor(Black);
	If hover Then
		If state = 0 Then
			TextBackground(Green)
		Else
			TextBackground(Red)
	Else
		If (x + y) Mod 2 = 0 Then
			TextBackground(LightGray)
		Else
			TextBackground(White);
	For i := 0 To BoxHeight - 1 Do
	Begin
		GoToXY(x * BoxWidth, y * BoxHeight + i);
		For j := 0 To BoxWidth - 1 Do
			Write(' ');
	End;
	GoToXY(x * BoxWidth + BoxWidth div 2, y * BoxHeight + BoxHeight div 2);
	If state = 1 Then
		Write('O')
	Else If state = -1 Then
		Write('X');
End;

Procedure DrawBoard(Const gameBoard: TBoard);
Var
i, j: Integer;
column: Array Of BoxState;
Begin
	For i := Low(gameBoard) To High(gameBoard) Do
	Begin
		column := gameBoard[i];
		For j := Low(column) To High(column) Do
			DrawBox(i, j, column[j], False);
	End;
End;

Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;
Begin
	MouseInBox := (mousePosX >= x * BoxWidth) And
		(mousePosX <= (x + 1) * BoxWidth - 1) And
		(mousePosY >= y * BoxHeight) And
		(mousePosY <= (y + 1) * BoxHeight - 1);
End;

End.
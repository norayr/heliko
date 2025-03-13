unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditTypes, LCLType, Process, LCLIntf, StrUtils;

type
  { TForm1 }
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseLeave(Sender: TObject);
    procedure SynEdit1Click(Sender: TObject);
    procedure SynEdit1Paint(Sender: TObject);
  private
    FCommandLine: Integer;
    FCommandStart: Integer;
    FCommandEnd: Integer;
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure ExecuteUnixCommand(const Cmd: string);
    procedure ClearCommandPosition;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Text := 'Example Unix commands: ;ls -la; or ;uname -a;';
  ClearCommandPosition;

  // Set up our custom paint handler
  //SynEdit1.OnPaint := @SynEdit1Paint;
end;

procedure TForm1.ClearCommandPosition;
begin
  FCommandLine := -1;
  FCommandStart := -1;
  FCommandEnd := -1;
end;

function TForm1.GetCommandUnderMouse(X, Y: Integer): string;
var
  Pt: TPoint;
  LineText: string;
  LineIndex: Integer;
  CharPos: Integer;
begin
  Result := '';

  // Get character position from mouse coordinates
  Pt := SynEdit1.PixelsToRowColumn(Point(X, Y));
  LineIndex := Pt.Y - 1;
  CharPos := Pt.X;

  // Validate position
  if (LineIndex < 0) or (LineIndex >= SynEdit1.Lines.Count) then
  begin
    ClearCommandPosition;
    Exit;
  end;

  LineText := SynEdit1.Lines[LineIndex];

  // Validate character position
  if (CharPos < 1) or (CharPos > Length(LineText)) then
  begin
    ClearCommandPosition;
    Exit;
  end;

  // Find command boundaries
  FCommandStart := RPosEx(';', LineText, CharPos);
  if FCommandStart <= 0 then
  begin
    ClearCommandPosition;
    Exit;
  end;

  FCommandEnd := PosEx(';', LineText, FCommandStart + 1);
  if FCommandEnd <= FCommandStart then
  begin
    ClearCommandPosition;
    Exit;
  end;

  // Check if cursor is between semicolons
  if (CharPos >= FCommandStart) and (CharPos <= FCommandEnd) then
  begin
    FCommandLine := LineIndex;
    Result := Trim(Copy(LineText, FCommandStart + 1, FCommandEnd - FCommandStart - 1));
  end
  else
    ClearCommandPosition;
end;

procedure TForm1.SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Cmd: string;
  OldLine: Integer;
begin
  OldLine := FCommandLine;
  Cmd := GetCommandUnderMouse(X, Y);

  if Cmd <> '' then
    SynEdit1.Cursor := crHandPoint
  else
    SynEdit1.Cursor := crDefault;

  // Force repaint of the editor if command position changed
  if (OldLine <> FCommandLine) or ((OldLine >= 0) and (FCommandLine >= 0)) then
    SynEdit1.Invalidate;
end;

procedure TForm1.SynEdit1MouseLeave(Sender: TObject);
begin
  SynEdit1.Cursor := crDefault;
  if FCommandLine >= 0 then
  begin
    ClearCommandPosition;
    SynEdit1.Invalidate;
  end;
end;

procedure TForm1.SynEdit1Click(Sender: TObject);
var
  MousePos: TPoint;
  Cmd: string;
begin
  MousePos := SynEdit1.ScreenToClient(Mouse.CursorPos);
  Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);
  if Cmd <> '' then
    ExecuteUnixCommand(Cmd);
end;

procedure TForm1.SynEdit1Paint(Sender: TObject);
var
  LineRect: TRect;
  StartX, EndX: Integer;
  LineY: Integer;
  GutterWidth: Integer;
begin
  // Only draw if we have a valid command position
  if (FCommandLine < 0) or (FCommandStart <= 0) or (FCommandEnd <= FCommandStart) then
    Exit;

  // Is the line visible?
  if (FCommandLine < SynEdit1.TopLine-1) or
     (FCommandLine >= SynEdit1.TopLine + SynEdit1.LinesInWindow) then
    Exit;

  // Calculate gutter width
  GutterWidth := 0;
  if SynEdit1.Gutter.Visible then
    GutterWidth := SynEdit1.Gutter.Width;

  // Calculate line position
  LineY := (FCommandLine - SynEdit1.TopLine + 1) * SynEdit1.LineHeight;

  // Calculate start and end X positions
  StartX := GutterWidth + (FCommandStart - SynEdit1.LeftChar) * SynEdit1.CharWidth;
  EndX := GutterWidth + (FCommandEnd - SynEdit1.LeftChar) * SynEdit1.CharWidth;

  // Stay within visible area
  if StartX < GutterWidth then StartX := GutterWidth;
  if EndX > SynEdit1.ClientWidth then EndX := SynEdit1.ClientWidth;

  // Draw underline if valid position
  if StartX < EndX then
  begin
    SynEdit1.Canvas.Pen.Color := clBlue;
    SynEdit1.Canvas.Pen.Width := 1;
    SynEdit1.Canvas.MoveTo(StartX, LineY + SynEdit1.LineHeight - 2);
    SynEdit1.Canvas.LineTo(EndX, LineY + SynEdit1.LineHeight - 2);
  end;
end;

procedure TForm1.ExecuteUnixCommand(const Cmd: string);
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'xterm';
    Proc.Parameters.Add('-hold');
    Proc.Parameters.Add('-e');
    Proc.Parameters.Add('/bin/sh');
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poNoConsole, poDetached];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

end.

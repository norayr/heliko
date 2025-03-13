unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditMiscClasses, LCLType, Process, LCLIntf;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseLeave(Sender: TObject);
    procedure SynEdit1Click(Sender: TObject);
  private
    CurrentCommand: string;
    CmdStart, CmdEnd, CmdLine: Integer;
    procedure ExecuteUnixCommand(const Cmd: string);
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Text := 'Type commands enclosed in semicolons: for example ;ls -la; or ;uname -a;';
   SynEdit1.OnSpecialLineMarkup := @SynEdit1SpecialLineMarkup;
   CurrentCommand := '';
   CmdStart := -1;
   CmdEnd := -1;
   CmdLine := -1;
end;

procedure TForm1.SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
  if (Line = CmdLine) and (CmdStart > 0) and (CmdEnd > CmdStart) then
  begin
    Special := True;
    Markup.Style := [fsUnderline];
    Markup.Foreground := clBlue;
    Markup.Background := clNone;
  end;
end;


function TForm1.GetCommandUnderMouse(X, Y: Integer): string;
var
  Pt: TPoint;
  LineText: string;
  LineIdx, CharIdx, leftDelim, rightDelim: Integer;
begin
  Result := '';
  Pt := SynEdit1.PixelsToRowColumn(Point(X, Y));
  LineIdx := Pt.Y - 1;  // Lazarus lines are zero-based

  if (LineIdx < 0) or (LineIdx >= SynEdit1.Lines.Count) then Exit;

  LineText := SynEdit1.Lines[LineIdx];
  CharIdx := Pt.X;

  if (CharIdx < 1) or (CharIdx > Length(LineText)) then Exit;

  leftDelim := CharIdx;
  while (leftDelim >= 1) and (LineText[leftDelim] <> ';') do Dec(leftDelim);
  rightDelim := CharIdx;
  while (rightDelim <= Length(LineText)) and (LineText[rightDelim] <> ';') do Inc(rightDelim);

  if (leftDelim >= 1) and (rightDelim <= Length(LineText)) and (leftDelim < rightDelim) then
  begin
    Result := Trim(Copy(LineText, leftDelim + 1, rightDelim - leftDelim - 1));
    CmdStart := leftDelim;
    CmdEnd := rightDelim;
    CmdLine := LineIdx;
  end
  else
  begin
    CmdStart := -1;
    CmdEnd := -1;
    CmdLine := -1;
  end;
end;

procedure TForm1.SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Cmd: string;
  OldLine: Integer;
begin
  OldLine := CmdLine;
  Cmd := GetCommandUnderMouse(X, Y);
  if Cmd <> '' then
    SynEdit1.Cursor := crHandPoint
  else
  begin
    SynEdit1.Cursor := crDefault;
    CmdLine := -1;
  end;

  // Redraw only if hover line changed
  if OldLine <> CmdLine then
  begin
    if OldLine >= 0 then SynEdit1.InvalidateLine(OldLine);
    if CmdLine >= 0 then SynEdit1.InvalidateLine(CmdLine);
  end;
end;

procedure TForm1.SynEdit1MouseLeave(Sender: TObject);
begin
  SynEdit1.Cursor := crDefault;
  CurrentCommand := '';
  CmdStart := -1;
  CmdEnd := -1;
  CmdLine := -1;
end;

procedure TForm1.SynEdit1Click(Sender: TObject);
var
  Cmd: string;
  MousePos: TPoint;
begin
  MousePos := SynEdit1.ScreenToClient(Mouse.CursorPos);
  Cmd := GetCommandUnderMouse(MousePos.X, MousePos.Y);
  if Cmd <> '' then
    ExecuteUnixCommand(Cmd);
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
    Proc.Parameters.Add(Cmd);
    Proc.Options := [poNoConsole, poDetached];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

end.


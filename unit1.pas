unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynEditTypes, SynEditMiscClasses, LCLType, Process, StrUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure SynEdit1SpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SynEdit1MouseLeave(Sender: TObject);
    procedure SynEdit1Click(Sender: TObject);
  private
    CmdLine, CmdStart, CmdEnd: Integer;
    function GetCommandUnderMouse(X, Y: Integer): string;
    procedure ExecuteUnixCommand(const Cmd: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Text := 'Try Unix commands enclosed in ";" like ;ls -la; or ;uname -a;.';
  CmdLine := -1;
  CmdStart := -1;
  CmdEnd := -1;
end;

function TForm1.GetCommandUnderMouse(X, Y: Integer): string;
var
  Pt: TPoint;
  LineText: string;
  CharIdx: Integer;
begin
  Pt := SynEdit1.PixelsToRowColumn(Point(X, Y));
  CmdLine := Pt.Y - 1;

  Result := '';
  CmdStart := -1;
  CmdEnd := -1;

  if (CmdLine < 0) or (CmdLine >= SynEdit1.Lines.Count) then Exit;

  LineText := SynEdit1.Lines[CmdLine];
  CharIdx := Pt.X;

  if (CharIdx < 1) or (CharIdx > Length(LineText)) then Exit;

  // Find the starting semicolon before the cursor
  CmdStart := RPosEx(';', LineText, CharIdx);
  if CmdStart > 0 then
    // Find the ending semicolon after the starting one
    CmdEnd := PosEx(';', LineText, CmdStart + 1)
  else
    CmdEnd := -1;

  if (CmdStart > 0) and (CmdEnd > CmdStart) then
  begin
    Result := Trim(Copy(LineText, CmdStart + 1, CmdEnd - CmdStart - 1));
    SynEdit1.InvalidateLine(CmdLine);
  end
  else
  begin
    CmdStart := -1;
    CmdEnd := -1;
    CmdLine := -1;
  end;
end;

procedure TForm1.SynEdit1SpecialLineMarkup(Sender: TObject; Line: Integer;
  var Special: Boolean; Markup: TSynSelectedColor);

begin
  if (Line = CmdLine) and (CmdStart > 0) and (CmdEnd > CmdStart) then
  begin
    Special := True;
    Markup.Style := [fsUnderline];

    Markup.Foreground := clBlue;
    Markup.Background := clBlue;//clNone;
    //Markup.StartX.Physical := CmdStart + 1;
    //Markup.EndX.Physical := CmdEnd;
  end;
end;

procedure TForm1.SynEdit1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  OldLine: Integer;
  Cmd: string;
begin
  OldLine := CmdLine;
  Cmd := GetCommandUnderMouse(X, Y);
  if Cmd <> '' then
    SynEdit1.Cursor := crHandPoint
  else
    SynEdit1.Cursor := crDefault;

  if OldLine <> CmdLine then
  begin
    if OldLine >= 0 then SynEdit1.InvalidateLine(OldLine);
    if CmdLine >= 0 then SynEdit1.InvalidateLine(CmdLine);
  end;
end;

procedure TForm1.SynEdit1MouseLeave(Sender: TObject);
begin
  SynEdit1.Cursor := crDefault;
  if CmdLine >= 0 then
  begin
    SynEdit1.InvalidateLine(CmdLine);
    CmdLine := -1;
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

procedure TForm1.ExecuteUnixCommand(const Cmd: string);
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
//    Proc.Executable := 'x-terminal-emulator'; // Use a generic terminal
    Proc.Executable := 'xterm';
    Proc.Parameters.Add('-e');
    Proc.Parameters.Add('bash');
    Proc.Parameters.Add('-c');
    Proc.Parameters.Add(Cmd + '; read -p "Press enter to exit..."');
    Proc.Options := [poDetached];
    Proc.Execute;
  finally
    Proc.Free;
  end;
end;

end.

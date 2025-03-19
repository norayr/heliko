unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, baseeditform, IniFiles;

const defaultFile = 'heliko.txt';

type
  TForm1 = class(TBaseEditForm)
    procedure FormCreate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FCurrentFile: string;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadCommands;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
procedure WriteHelpOnConsole;
begin
  WriteLn('heliko - Command execution tool');
  WriteLn('');
  WriteLn('Usage: heliko [OPTIONS] [FILE]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --help    Show this help message and exit');
  WriteLn('  --xterm   Execute commands in xterm instead of internal window');
  WriteLn('');
  WriteLn('Arguments:');
  WriteLn('  FILE      Optional path to load commands from (default: ~/heliko.txt)');
  Halt(0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'heliko';
  LoadConfig;
  LoadCommands;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
VAR Lines: TStringList;
begin
  SaveConfig;
    if (FCurrentFile <> '') and SynEdit.Modified then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := SynEdit.Text;
      Lines.SaveToFile(FCurrentFile);
    finally
      Lines.Free;
    end;
  end;
  inherited;
end;

procedure TForm1.LoadConfig;
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.heliko.conf';
  Ini := TIniFile.Create(ConfigPath);
  try
    Width := Ini.ReadInteger('Window', 'Width', Round(Screen.Width/2.3));
    Height := Ini.ReadInteger('Window', 'Height', Round(Screen.Height-Screen.Height/7));
  finally
    Ini.Free;
  end;
end;

procedure TForm1.SaveConfig;
var
  Ini: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.heliko.conf';
  Ini := TIniFile.Create(ConfigPath);
  try
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.LoadCommands;
var
  HomeDir: string;
  Lines: TStringList;
  customFile: string;
  i: integer;
begin
   Form1.FCurrentFile := '';
   HomeDir := GetEnvironmentVariable('HOME');
   customFile := IncludeTrailingPathDelimiter(HomeDir) + defaultFile;

   for i := 1 to ParamCount do
   begin
     if ParamStr(i) = '--help' then
       WriteHelpOnConsole;

     if ParamStr(i) = '--xterm' then
       TBaseEditForm.UseXTerm := True
     else if (ParamStr(i)[1] <> '-') then
       customFile := ParamStr(i);
   end;

    if customFile <> '' then

      if FileExists(customFile) then
      begin
        FCurrentFile := customFile;
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(customFile);
          SynEdit.Text := Lines.Text;
        finally
          Lines.Free;
        end;
      end
     else
      begin
        SynEdit.Text := 'Example Unix commands: ;cal; or ;ls -al ~/; or ;ls -al ^; (select path to substitute with ^)';
      end;
      SynEdit.Modified := False;
end;


end.

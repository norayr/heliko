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
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadCommands;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'heliko';
  LoadConfig;
  LoadCommands;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
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
   HomeDir := GetEnvironmentVariable('HOME');
   customFile := IncludeTrailingPathDelimiter(HomeDir) + defaultFile;

   for i := 1 to ParamCount do
   begin
     if ParamStr(i) = '--help' then
       ShowHelp;

     if ParamStr(i) = '--xterm' then
       TBaseEditForm.UseXTerm := True
     else if (ParamStr(i)[1] <> '-') then
       customFile := ParamStr(i);
   end;

    if customFile <> '' then

  if FileExists(customFile) then
  begin
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
end;


end.

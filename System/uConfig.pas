{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde
  Portions (C) 2016 Peter Dyson. Initial Lazarus port

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, IniFiles;

type
  //Save changed diagram layout setting
  TDiSaveSetting = (dsAlways,dsAsk,dsNever);

  { TConfig }

  TConfig = class
  private
    FIni: TMemIniFile;
    FDiSave : TDiSaveSetting;
    FDiShowAssoc: boolean;
    FDiVisibilityFilter: integer;
    FEditorCommandLine: String;
    FAdditionalDefines : String;
    FDotRankDir : String;
    FDotFontSize : Integer;
    FDotAddUrls : Boolean;
    FDotUrlsPrefix : String;
    FDotSplines    : String;
    FDotPrefLabCon : String;
    FMDGenIgnoreEntites : String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetResource(AName: String; AType: String): String;
  public
    IsLimitedColors : boolean;
    IsTerminating : boolean;

    property EditorCommandLine: String read FEditorCommandLine write FEditorCommandLine;
    property AdditionalDefines: String read FAdditionalDefines write FAdditionalDefines;
    property DotRankDir: String read FDotRankDir write FDotRankDir;
    property DotFontSize : Integer read FDotFontSize write FDotFontSize;
    property DotAddUrls : Boolean read FDotAddUrls write FDotAddUrls;
    property DotUrlsPrefix: String read FDotUrlsPrefix write FDotUrlsPrefix;
    property DotSplines: String read FDotSplines write FDotSplines;
    property DotPrefferedLabelConnector: String read FDotPrefLabCon write FDotPrefLabCon;
    property MDGenIgnoreEntites : String read FMDGenIgnoreEntites write FMDGenIgnoreEntites;

    procedure WriteStr(const Key : string; const Value : string);
    function ReadStr(const Key : string; const Default : string) : string;
    procedure WriteInt(const Key : string; const Value : Integer);
    function ReadInt(const Key : string; const Default : Integer) : Integer;
    procedure WriteBool(const Key : string; const Value : Boolean);
    function ReadBool(const Key : string; const Default : Boolean) : Boolean;

    procedure StoreSettings;
  published
    property DiSave : TDiSaveSetting read FDiSave write FDiSave;
    property DiShowAssoc : boolean read FDiShowAssoc write FDiShowAssoc;
    property DiVisibilityFilter : integer read FDiVisibilityFilter write FDiVisibilityFilter;
  end;

var
  Config: TConfig;

implementation

uses
  Forms, SysUtils, LResources;

const
  cSettings = 'Settings';

constructor TConfig.Create;
var
  FDir: String;
begin
  IsLimitedColors := False;

  FDir := GetUserDir + '.essmodel';
  if not DirectoryExists(FDir) then
  begin
    ForceDirectories(FDir);
    FileSetAttr(FDir, faHidden);
  end;

  FIni := TMemIniFile.Create(FDir + DirectorySeparator + 'config.ini');

  FDiShowAssoc := ReadInt('DiShowAssoc',0)<>0;
  FDiVisibilityFilter := ReadInt('DiVisibilityFilter',0);
  FEditorCommandLine := ReadStr('EditorCommandLine','');
  FAdditionalDefines := ReadStr('AdditionalDefines','-Mobjfpc');
  FDotAddUrls := ReadBool('DotAddUrls',false);
  FDotFontSize := ReadInt('DotFontSize',12);
  FDotUrlsPrefix := ReadStr('DotUrlsPrefix','https://yoururl.here/');
  FDotRankDir := ReadStr('DotRankDir','LR');
  FDotSplines := ReadStr('DotSplines', 'spline');
  FDotPrefLabCon := ReadStr('DotPrefLabCon', '_');
  FMDGenIgnoreEntites := ReadStr('MDGenIgnoreEntites', '');
end;

destructor TConfig.Destroy;
begin
  FIni.UpdateFile;
  FIni.Free;
  inherited Destroy;
end;

function TConfig.GetResource(AName: String; AType: String): String;
var
  FRes: TLazarusResourceStream;
begin
  FRes := TLazarusResourceStream.Create(AName, PChar(AType));
  try
    Result := FRes.Res.Value;
  finally
    FRes.Free;
  end;
end;

function TConfig.ReadInt(const Key: string;
  const Default: integer): integer;
begin
  Result := FIni.ReadInteger(cSettings, Key, Default);
end;

procedure TConfig.WriteBool(const Key: string; const Value: Boolean);
begin
  FIni.WriteBool(cSettings, Key, Value);
end;

function TConfig.ReadBool(const Key: string; const Default: Boolean): Boolean;
begin
  Result := FIni.ReadBool(cSettings, Key, Default);
end;

function TConfig.ReadStr(const Key: string; const Default: string): string;
begin
  Result := FIni.ReadString(cSettings, Key, Default);
end;

procedure TConfig.WriteInt(const Key: string; const Value: Integer);
begin
  FIni.WriteInteger(cSettings, Key, Value);
end;

procedure TConfig.WriteStr(const Key: string; const Value: string);
begin
  FIni.WriteString(cSettings, Key, Value)
end;

procedure TConfig.StoreSettings;
begin
  WriteInt('DiSave',Integer(FDiSave));
  WriteBool('DiShowAssoc',FDiShowAssoc);
  WriteInt('DiVisibilityFilter',FDiVisibilityFilter);
  WriteStr('EditorCommandLine',FEditorCommandLine);
  WriteStr('AdditionalDefines',FAdditionalDefines);
  WriteBool('DotAddUrls',FDotAddUrls);
  WriteInt('DotFontSize',FDotFontSize);
  WriteStr('DotUrlsPrefix',FDotUrlsPrefix);
  WriteStr('DotRankDir',FDotRankDir);
  WriteStr('DotSplines', FDotSplines);
  WriteStr('DotPrefLabCon', FDotPrefLabCon);
  WriteStr('MDGenIgnoreEntites', FMDGenIgnoreEntites);
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.

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
  Classes, IniFiles, uViewIntegrator;

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

    FDotAddUrls : Boolean;
    FDotUrlsPrefix : String;

    FDotRankDir : Array [TDiagramKind] of String;
    FDotFontSize : Array [TDiagramKind] of Integer;
    FDotSplines    : Array [TDiagramKind] of String;
    FDotPrefLabCon : Array [TDiagramKind] of String;
    FDotFontName   : Array [TDiagramKind] of String;
    FDotRankSep    : Array [TDiagramKind] of String;

    FMDGenIgnoreEntites : String;
    function GetDotFontName(Index : TDiagramKind) : String;
    function GetDotFontSize(Index : TDiagramKind) : Integer;
    function GetDotPort(Index : TDiagramKind) : String;
    function GetDotRankDir(Index : TDiagramKind) : String;
    function GetDotRankSep(Index : TDiagramKind) : String;
    function GetDotSplines(Index : TDiagramKind) : String;
    procedure SetDotFontName(Index : TDiagramKind; AValue : String);
    procedure SetDotFontSize(Index : TDiagramKind; AValue : Integer);
    procedure SetDotPort(Index : TDiagramKind; AValue : String);
    procedure SetDotRankDir(Index : TDiagramKind; AValue : String);
    procedure SetDotRankSep(Index : TDiagramKind; AValue : String);
    procedure SetDotSplines(Index : TDiagramKind; AValue : String);
  public
    constructor Create;
    destructor Destroy; override;
    function GetResource(AName: String; AType: String): String;
  public
    IsLimitedColors : boolean;
    IsTerminating : boolean;

    property EditorCommandLine: String read FEditorCommandLine write FEditorCommandLine;
    property AdditionalDefines: String read FAdditionalDefines write FAdditionalDefines;
    property DotAddUrls : Boolean read FDotAddUrls write FDotAddUrls;
    property DotUrlsPrefix: String read FDotUrlsPrefix write FDotUrlsPrefix;
    property DotRankDir[Index : TDiagramKind]: String read GetDotRankDir write SetDotRankDir;
    property DotRankSep[Index : TDiagramKind]: String read GetDotRankSep write SetDotRankSep;
    property DotFontSize[Index : TDiagramKind] : Integer read GetDotFontSize write SetDotFontSize;
    property DotFontName[Index : TDiagramKind]: String read GetDotFontName write SetDotFontName;
    property DotSplines[Index : TDiagramKind]: String read GetDotSplines write SetDotSplines;
    property DotPort[Index : TDiagramKind]: String read GetDotPort write SetDotPort;

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
  cDotDefaults : Array [TDiagramKind, 0..5] of String = (
  ('BT', '0.75', '12', 'sans', '_', 'true'),
  ('LR', '1.25', '12', 'courier', 'e', 'ortho')
  );

function TConfig.GetDotFontName(Index : TDiagramKind) : String;
begin
  Result := FDotFontName[index];
end;

function TConfig.GetDotFontSize(Index : TDiagramKind) : Integer;
begin
  Result := FDotFontSize[index];
end;

function TConfig.GetDotPort(Index : TDiagramKind) : String;
begin
  Result := FDotPrefLabCon[index];
end;

function TConfig.GetDotRankDir(Index : TDiagramKind) : String;
begin
  Result := FDotRankDir[index];
end;

function TConfig.GetDotRankSep(Index : TDiagramKind) : String;
begin
  Result := FDotRankSep[index];
end;

function TConfig.GetDotSplines(Index : TDiagramKind) : String;
begin
  Result := FDotSplines[index];
end;

procedure TConfig.SetDotFontName(Index : TDiagramKind; AValue : String);
begin
  FDotFontName[index] := AValue;
end;

procedure TConfig.SetDotFontSize(Index : TDiagramKind; AValue : Integer);
begin
  FDotFontSize[index] := AValue;
end;

procedure TConfig.SetDotPort(Index : TDiagramKind; AValue : String);
begin
  FDotPrefLabCon[index] := AValue;
end;

procedure TConfig.SetDotRankDir(Index : TDiagramKind; AValue : String);
begin
  FDotRankDir[index] := AValue;
end;

procedure TConfig.SetDotRankSep(Index : TDiagramKind; AValue : String);
begin
  FDotRankSep[index] := AValue;
end;

procedure TConfig.SetDotSplines(Index : TDiagramKind; AValue : String);
begin
  FDotSplines[index] := AValue;
end;

constructor TConfig.Create;
var
  FDir: String;
  i : TDiagramKind;
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
  FDotUrlsPrefix := ReadStr('DotUrlsPrefix','https://yoururl.here/');
  FDotAddUrls := ReadBool('DotAddUrls',false);

  for i := Low(TDiagramKind) to high(TDiagramKind) do
  begin
    FDotRankDir[i] :=    ReadStr('DotRankDir' +  inttostr(Byte(i)),cDotDefaults[i, 0]);
    FDotRankSep[i] :=    ReadStr('DotRankSep' +  inttostr(Byte(i)),cDotDefaults[i, 1]);
    FDotFontSize[i] :=   ReadInt('DotFontSize' + inttostr(Byte(i)),Strtoint(cDotDefaults[i, 2]));
    FDotFontName[i] :=   ReadStr('DotFontName' + inttostr(Byte(i)),cDotDefaults[i, 3]);
    FDotPrefLabCon[i] := ReadStr('DotPrefLabCon' + inttostr(Byte(i)), cDotDefaults[i, 4]);
    FDotSplines[i] :=    ReadStr('DotSplines' +  inttostr(Byte(i)), cDotDefaults[i, 5]);
  end;

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

function TConfig.ReadInt(const Key : string; const Default : Integer) : Integer;
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
var
  i : TDiagramKind;
begin
  WriteInt('DiSave',Integer(FDiSave));
  WriteBool('DiShowAssoc',FDiShowAssoc);
  WriteInt('DiVisibilityFilter',FDiVisibilityFilter);
  WriteStr('EditorCommandLine',FEditorCommandLine);
  WriteStr('AdditionalDefines',FAdditionalDefines);
  WriteBool('DotAddUrls',FDotAddUrls);
  WriteStr('DotUrlsPrefix',FDotUrlsPrefix);
  for i := Low(TDiagramKind) to high(TDiagramKind) do
  begin
    WriteInt('DotFontSize' + inttostr(Byte(i)),FDotFontSize[i]);
    WriteStr('DotFontName' + inttostr(Byte(i)),FDotFontName[i]);
    WriteStr('DotRankDir' + inttostr(Byte(i)),FDotRankDir[i]);
    WriteStr('DotRankSep' + inttostr(Byte(i)),FDotRankSep[i]);;
    WriteStr('DotSplines' + inttostr(Byte(i)), FDotSplines[i]);
    WriteStr('DotPrefLabCon' + inttostr(Byte(i)), FDotPrefLabCon[i]);
  end;
  WriteStr('MDGenIgnoreEntites', FMDGenIgnoreEntites);
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.

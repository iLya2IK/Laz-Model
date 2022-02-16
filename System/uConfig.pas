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
  TDotPrefs = (dotRankDir,
               dotRankSep,  dotNodeSep,
               dotFontSize, dotFontName,
               dotPort,     dotSplines,
               dotConcentrate);

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

    FDotPrefs : Array [TDiagramKind, TDotPrefs] of String;

    FMDGenIgnoreEntites : String;
    function GetDotPref(DiaKind : TDiagramKind; DotOpt : TDotPrefs ) : String;
    procedure SetDotPref(DiaKind : TDiagramKind; DotOpt : TDotPrefs ;
      AValue : String);
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
    property DotPref[DiaKind : TDiagramKind; DotOpt :TDotPrefs ]: String read GetDotPref write SetDotPref;

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

  cDIA2STR  : array [TDiagramKind] of String = ('Package', 'Class');
  cDOTP2STR : array [TDotPrefs] of String = ('rankdir',
                                             'ranksep',
                                             'nodesep',
                                             'fontsize',
                                             'fontname',
                                             'port',
                                             'splines',
                                             'concentrate');

implementation

uses
  Forms, SysUtils, LResources;

const
  cSettings = 'Settings';
  cDotDefaults : Array [TDiagramKind, TDotPrefs] of String = (
  ('BT', '0.75', '0.25', '12', 'sans', '_', 'true', 'false'),
  ('LR', '1.25', '0.25', '12', 'courier', 'e', 'ortho', 'true'));

function TConfig.GetDotPref(DiaKind : TDiagramKind; DotOpt : TDotPrefs
  ) : String;
begin
  Result := FDotPrefs[DiaKind, DotOpt];
end;

procedure TConfig.SetDotPref(DiaKind : TDiagramKind; DotOpt : TDotPrefs ;
  AValue : String);
begin
  FDotPrefs[DiaKind, DotOpt] := AValue;
end;

constructor TConfig.Create;
var
  FDir: String;
  i : TDiagramKind;
  o : TDotPrefs;
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
  for o := Low(TDotPrefs) to high(TDotPrefs) do
  begin
    FDotPrefs[i, o] := ReadStr('DotPref'+cDIA2STR[i]+cDOTP2STR[o],
                                  cDotDefaults[i, o]);
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
  o : TDotPrefs;
begin
  WriteInt('DiSave',Integer(FDiSave));
  WriteBool('DiShowAssoc',FDiShowAssoc);
  WriteInt('DiVisibilityFilter',FDiVisibilityFilter);
  WriteStr('EditorCommandLine',FEditorCommandLine);
  WriteStr('AdditionalDefines',FAdditionalDefines);
  WriteBool('DotAddUrls',FDotAddUrls);
  WriteStr('DotUrlsPrefix',FDotUrlsPrefix);
  for i := Low(TDiagramKind) to high(TDiagramKind) do
  for o := Low(TDotPrefs) to high(TDotPrefs) do
  begin
    WriteStr('DotPref'+cDIA2STR[i]+cDOTP2STR[o], FDotPrefs[i, o]);
  end;
  WriteStr('MDGenIgnoreEntites', FMDGenIgnoreEntites);
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.

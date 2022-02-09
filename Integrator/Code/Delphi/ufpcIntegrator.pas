{
  ESS-Model
  Copyright (C) 2016 Peter Dyson. Initial Lazarus port
  Portions (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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
unit ufpcIntegrator;

{$mode objfpc}{$H+}

interface
uses Classes, uIntegrator, uModel, ufpcParser, uCodeProvider,
  uCodeParser, uConfig;

type

  // Ordinary import of delphi code, until we have two-way integration.

  { TfpciImporter }

  TfpciImporter = class(TImportIntegrator)
  private
    // Implementation of parser callback to retrieve a named package
    function NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = True):String;
  public
    procedure ImportOneFile(const FileName : string); override;
    class function GetFileExtensions : TStringList; override;
    class function CaseSensitive : Boolean; override;
  end;


implementation
uses SysUtils, uError;
{ TfpcImporter }

procedure TfpciImporter.ImportOneFile(const FileName : string);
var
  Parser: TfpcParser;
  GlobalDefines : TStringList;
begin
    GlobalDefines := TStringList.Create;
    GlobalDefines.Delimiter := ' ';
    GlobalDefines.DelimitedText := Config.AdditionalDefines;

    Parser := TfpcParser.Create;
    try
      Parser.Filename := FileName;
      Parser.NeedPackage := @NeedPackageHandler;
      //      Parser.ParseStreamWithDefines(Str, Model.ModelRoot, Model, GlobalDefines);
      Parser.ParseFileWithDefines(Model.ModelRoot, Model, GlobalDefines);
    finally
      Parser.Free;
      GlobalDefines.Free;
    end;

end;

class function TfpciImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.pas'] := 'code';
  Result.Values['.pp'] := 'code';
  Result.Values['.inc'] := 'include';
  Result.Values['.lpr'] := 'Lazarus project';
//  Result.Values['.lpk'] := 'Lazarus package';
end;

class function TfpciImporter.CaseSensitive : Boolean;
begin
  Result := false;
end;

function TfpciImporter.NeedPackageHandler(const AName: string; var AStream: TStream; OnlyLookUp: Boolean = True):String;
var
  FileName: string;
begin
  AStream := nil;
  if ExtractFileExt(AName) = '' then
    FileName := ExtractFileName(AName) + '.pas'
  else
    FileName := AName;
  FileName := CodeProvider.LocateFile(FileName);
  Result := FileName;
  if (FilesRead.IndexOf(FileName)>-1) then Result := '';
  if {(not OnlyLookUp) and }(FileName<>'') and (FilesRead.IndexOf(FileName)=-1) then
  begin
//    AStream := CodeProvider.LoadStream(FileName);
    FilesRead.Add(FileName);
  end;
end;

initialization

  Integrators.Register(TfpciImporter);

end.

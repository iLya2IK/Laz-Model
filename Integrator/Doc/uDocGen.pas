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

unit uDocGen;

{$mode objfpc}{$H+}

interface

uses Classes, uIntegrator, uModel, uModelEntity;

type
  //Baseclass for documenation generators.

  { TDocGen }

  TDocGen = class(TExportIntegrator)
  private
    FIgnores : TStringList;
  protected
    Packages : IModelIterator;
    procedure TraverseModel; virtual;
    procedure WriteOverview; virtual;
    procedure WritePackageDetail(P : TUnitPackage); virtual;
    procedure WriteClassDetail(C : TClass); virtual;
    procedure WriteInterfaceDetail(I: TInterface); virtual;
    procedure DocStart; virtual; abstract;
    procedure DocFinished; virtual; abstract;
    procedure SelectDestPath;
    function CheckIsIgnored(E: TModelEntity) : Boolean;
  public
    DestPath : string;     //Let user select path in dialog if not set
    IsPreview : boolean;   //If true generate doc in tempdir
    procedure InitFromModel; override;
    constructor Create(om: TObjectModel); reintroduce;
    destructor Destroy; override;
  end;

  //Factory function, create instance of tdocgen
  function CreateDocGen(Om : TObjectModel) : TDocGen;


implementation

uses uIterators,
  uHtmlDocGen,
  uMDocGen,
  uUseful,
  SysUtils,
  Forms,
  uConst,
  uConfig;


{ TDocGen }

procedure TDocGen.InitFromModel;
begin
  if IsPreview then
    DestPath := uUseful.MakeTempDir
  else
    if DestPath='' then
      SelectDestPath;

  if not (DestPath[ Length(DestPath) ] in [PathDelim,':']) then
    DestPath := DestPath + PathDelim;

  //Get all unitpackages sorted in name order
  Packages := TModelIterator.Create(Model.ModelRoot.GetAllUnitPackages,ioAlpha);
  DocStart;
  TraverseModel;
  DocFinished;
end;

constructor TDocGen.Create(om : TObjectModel);
begin
  inherited Create(om);
  FIgnores := TStringList.Create;
  FIgnores.Sorted := true;
  FIgnores.Duplicates := dupIgnore;
end;

destructor TDocGen.Destroy;
begin
  FIgnores.Free;
  inherited Destroy;
end;

procedure TDocGen.SelectDestPath;
var
  Di : TBrowseForFolderDialog;
begin
  Di := TBrowseForFolderDialog.Create;
  try
    Di.Path := ExtractFilePath( Model.ModelRoot.GetConfigFile );
//    if not Di.Execute then
//      Abort;
    DestPath := Di.Path;
  finally
    Di.Free;
  end;
end;

function TDocGen.CheckIsIgnored(E : TModelEntity) : Boolean;
var i : integer;
begin
  if FIgnores.Find(E.FullName, {%H-}i) then
  begin
    Exit(true);
  end;
  if FIgnores.Find(E.Name, {%H-}i) then
  begin
    Exit(true);
  end;
  if Assigned(E.Owner) then
  begin
    if FIgnores.Find(E.Owner.FullName, {%H-}i) then
    begin
      Exit(true);
    end;
    if FIgnores.Find(E.Owner.Name, {%H-}i) then
    begin
      Exit(true);
    end;
  end;
  Result := false;
end;

procedure TDocGen.TraverseModel;
var
  P : TUnitPackage;
  Mi : IModelIterator;
  Pro : IEldeanProgress;
  PCount : integer;
begin
  //Overview with packagenames
  WriteOverview;
  Packages.Reset;

  //Init progressbar
  PCount := 0;
  while Packages.HasNext do
  begin
    Inc(PCount);
    Packages.Next;
  end;
  Packages.Reset;
  Pro := TEldeanProgress.Create('Generating documentation...',PCount);

  while Packages.HasNext do
  begin
    //Packagedetails
    P := Packages.Next as TUnitPackage;
    WritePackageDetail(P);
    //Class details
    Mi := TModelIterator.Create(P.GetClassifiers,TClass,Low(TVisibility),ioAlpha);
    while Mi.HasNext do
      WriteClassDetail(Mi.Next as TClass);
    //Interface details
    Mi := TModelIterator.Create(P.GetClassifiers,TInterface,Low(TVisibility),ioAlpha);
    while Mi.HasNext do
      WriteInterfaceDetail(Mi.Next as TInterface);
    Pro.Tick;
  end;
end;

/////////////////////

function CreateDocGen(Om : TObjectModel) : TDocGen;
begin
  //Use html
  //Result := THtmlDocGen.Create(Om);
  //Use markdown
  Result := TMDocGen.Create(Om);
  Result.FIgnores.Delimiter := ';';
  Result.FIgnores.DelimitedText := Config.MDGenIgnoreEntites;
end;

procedure TDocGen.WriteClassDetail(C: TClass);
begin

end;

procedure TDocGen.WriteInterfaceDetail(I: TInterface);
begin

end;

procedure TDocGen.WriteOverview;
begin

end;

procedure TDocGen.WritePackageDetail(P: TUnitPackage);
begin

end;

end.

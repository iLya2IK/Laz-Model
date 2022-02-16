unit uMDocGen;

{$mode objfpc}{$H+}

interface

uses
  uDocGen, uModel, uModelEntity, uXmiExport;

type
  //MARKDOWN documentation generator

  { TMDocGen }

  TMDocGen = class(TDocGen)
  private
    procedure MakeDiagram(P : TAbstractPackage);
  protected
    type trefoption = (refBold, refShort);
    type trefoptions = set of trefoption;
    function GenEntityRef(E : TModelEntity; opts : trefoptions = []) : String;
    function GenEntityShortRef(E : TModelEntity) : String;
    procedure DocStart; override;
    procedure DocFinished; override;
    procedure WriteOverview; override;
    procedure WriteClassDetail(C : TClass); override;
    procedure WritePackageDetail(P : TUnitPackage); override;
  public
    destructor Destroy; override;
  end;

implementation

uses Forms, SysUtils, Graphics,
  LCLIntf, LCLType, {ComObj,}
  uConfig, Classes, Dialogs, uViewIntegrator;

const OverviewPackage = 'Overview';

{ TMDocGen }

procedure TMDocGen.MakeDiagram(P : TAbstractPackage);
var
  Di : TDiagramIntegrator;
  TempForm : TForm;
  W,H : integer;
  ImageFileName : string;

  DK : TDiagramKind;
  oldUseURL : Boolean;

  IsRoot : boolean;
begin
  IsRoot := P=Model.ModelRoot;

  if IsRoot then begin
    ImageFileName := DestPath + OverviewPackage + '.dot';
    DK := diakPackage;
  end else begin
    ImageFileName := DestPath + p.FullURIName + '.dot';
    DK := diakClass;
  end;

  oldUseURL   := Config.DotAddUrls;
  Config.DotAddUrls := true;

  TempForm := TForm.CreateNew(nil);
  Di := TDiagramIntegrator.CreateDiagram(Model, TempForm);
  try
    Di.VisibilityFilter := viPublic;
    Di.ShowAssoc := true;

    Di.Package := P;
    Di.InitFromModel;
    Di.GetDiagramSize(W{%H-},H{%H-});

    TempForm.Height:=0;
    TempForm.Width:=0;
    TempForm.SendToBack;
    TempForm.Top := Screen.Height;
    TempForm.Show;
    Di.OnCheckIgnored := @CheckIsIgnored;
    Di.SaveAsDotGraph(DK, ImageFileName);
    TempForm.Hide;

  finally
    Di.Free;
    TempForm.Free;
  end;

  Config.DotAddUrls := oldUseURL;
end;

function TMDocGen.GenEntityRef(E : TModelEntity; opts : trefoptions) : String;
var s : String;
begin
  if refShort in opts then
    s := E.Name else
    S := E.FullName;

  if refBold in opts then
    s := '**' + S + '**';

  if CheckIsIgnored(E) then
  begin
    Result := S;
  end else
    Result := Format('[%s](%s)', [S, Config.DotUrlsPrefix+E.FullURIName]);
end;

function TMDocGen.GenEntityShortRef(E : TModelEntity) : String;
begin
  Result := GenEntityRef(E, [refShort]);
end;

procedure TMDocGen.DocStart;
begin

end;

procedure TMDocGen.DocFinished;
begin

end;

procedure TMDocGen.WriteOverview;
var
  Txt : TStringList;
  FileName, diaName : string;
  MI : IModelIterator;
  E : TModelEntity;
  cnt : integer;
begin
  FileName := DestPath + OverviewPackage + '.md';
  diaName :=  Config.DotUrlsPrefix+OverviewPackage + '.svg';

  Txt := TStringList.Create;
  try
    Txt.Add('# '+ OverviewPackage);

    Txt.Add('<a target="_blank" rel="noopener noreferrer" href="%s?raw=true">'+
            '<img src="%s"></img></a>', [diaName, diaName]);
    Txt.Add('');
    cnt := Txt.Count;
    MI := Model.ModelRoot.GetAllUnitPackages;
    while MI.HasNext do
    begin
      E := MI.Next;
      Txt.Add('* ' + GenEntityRef(E));
    end;
    if cnt < Txt.Count then
    begin
      Txt.Insert(cnt, '## Dependences');
      Txt.Add('');
    end;
    Txt.SaveToFile(FileName);
  finally
    Txt.Free;
  end;
  MakeDiagram( Model.ModelRoot );
end;

procedure TMDocGen.WriteClassDetail(C : TClass);
var
  Txt : TStringList;
  Rels : TStringList;
  FileName, S : string;
  MI : IModelIterator;
  PI : IModelIterator;
  Atr : TAttribute;
  par : TParameter;
  E : TModelEntity;
  Op : uModel.TOperation;
  i, cnt : integer;
begin
  if not CheckIsIgnored(C) then
  begin
    FileName := DestPath + C.FullURIName + '.md';

    Rels := TStringList.Create;
    Txt := TStringList.Create;
    try
      Rels.OwnsObjects := false;
      Rels.Sorted := true;
      Rels.Duplicates := dupIgnore;
      Txt.Add('# '+ C.FullName);

      Txt.Add('## Overview');
      Txt.Add('');
      if C.Owner is TUnitPackage then
        Txt.Add(Format('This class is part of %s unit.', [GenEntityRef(C.Owner)]));
  ;
      if C.Ancestor is TClass then
      begin
        Txt.Add('## Ancestor');
        Txt.Add('');
        Txt.Add(GenEntityShortRef(C.Ancestor));
        Rels.AddObject(C.Ancestor.FullName, C.Ancestor);
      end;

      cnt := Txt.Count;
      MI := Model.ModelRoot.GetAllClassifiers;
      while MI.HasNext do
      begin
        E := MI.Next;
        if (E is TClass) and (TClass(E).Ancestor = C) then
        begin
          Txt.Add(Format('* %s',[GenEntityShortRef(E)]));
          Rels.AddObject(E.FullName, E);
        end;
      end;
      if Txt.Count > cnt then
      begin
        Txt.Insert(cnt, '## Successors');
        Txt.Add('');
      end;

      cnt := Txt.Count;
      MI := C.GetAttributes;
      while MI.HasNext do
      begin
        atr := TAttribute(MI.Next);
        if atr.Visibility >= viPublic then
        begin
          if assigned(atr.TypeClassifier) then
          begin
            if atr.TypeClassifier is TClass then begin
              Txt.Add(Format('* %s: %s',[GenEntityRef(atr, [refShort, refBold]),
                                         GenEntityRef(atr.TypeClassifier)]));
              Rels.AddObject(atr.TypeClassifier.FullName, atr.TypeClassifier);
            end else
              Txt.Add(Format('* %s: %s',[GenEntityRef(atr, [refShort, refBold]), atr.TypeClassifier.Name]));
          end else
            Txt.Add(Format('* %s: %s',[GenEntityRef(atr, [refShort, refBold]), 'undef']));
        end;
      end;
      if Txt.Count > cnt then
      begin
        Txt.Insert(cnt, '## Attributes');
        Txt.Add('');
      end;
      cnt := Txt.Count;
      MI := C.GetOperations;
      while MI.HasNext do
      begin
        op := uModel.TOperation(MI.Next);
        if op.Visibility >= viPublic then
        begin
          S := Format('* %s',[GenEntityRef(op, [refShort, refBold])]);
          PI := op.GetParameters;
          if pi.HasNext then
          begin
            S := S + '(';
            i := 0;
            while pi.HasNext do
            begin
              par := TParameter(pi.Next);
              if i > 0 then S := S + ', ';
              if Assigned(par.TypeClassifier) then
              begin
                S := S + Format('%s: %s', [par.Name, par.TypeClassifier.Name]);
              end else
              begin
                S := S + par.Name;
              end;
              inc(i);

              if par.TypeClassifier is TClass then
                Rels.AddObject(par.TypeClassifier.FullName, par.TypeClassifier);
            end;
            S := S + ')';
          end;
          if Assigned( op.ReturnValue) then
          begin
            if op.ReturnValue is TClass then begin
              s := Format('%s: %s',[s, GenEntityRef(Op.ReturnValue)]);
              Rels.AddObject(Op.ReturnValue.FullName, Op.ReturnValue);
            end else
              s := Format('%s: %s',[s, Op.ReturnValue.Name]);
          end;
          Txt.Add(S);
        end;
      end;
      if Txt.Count > cnt then
      begin
        Txt.Insert(cnt, '## Methods');
        Txt.Add('');
      end;
      if Rels.Count > 0 then
      begin
        Txt.Add('## Related links');

        for i := 0 to Rels.Count-1 do
        begin
          E := TModelEntity(Rels.Objects[i]);
          Txt.Add(Format('* %s',[GenEntityRef(E)]));
        end;
      end;
      Txt.SaveToFile(FileName);
    finally
      Txt.Free;
      Rels.Free;
    end;
  end;
end;

procedure TMDocGen.WritePackageDetail(P : TUnitPackage);
var
  Txt : TStringList;
  FileName, diaName : string;
  MI : IModelIterator;
  E : TModelEntity;
  DE : TUnitDependency;
  cnt : integer;
begin
  if not CheckIsIgnored(P) then
  begin
    FileName := DestPath + P.FullURIName + '.md';
    diaName :=  Config.DotUrlsPrefix+P.FullURIName+'.svg';

    Txt := TStringList.Create;
    try
      Txt.Add('# '+ P.FullName);

      Txt.Add('<a target="_blank" rel="noopener noreferrer" href="%s?raw=true">'+
              '<img src="%s"></img></a>', [diaName, diaName]);
      Txt.Add('');
      Txt.Add('## Overview');
      Txt.Add('');

      cnt := Txt.Count;
      MI := P.GetClassifiers;
      while MI.HasNext do
      begin
        E := MI.Next;
        if (E is TClass) or (E is TInterface) then
        begin
          Txt.Add(Format('* %s',[GenEntityShortRef(E)]));
        end;
      end;
      if cnt < Txt.Count then
      begin
        Txt.Insert(cnt, '## Classes');
        Txt.Add('');
      end;
      Txt.Add('## Functions');
      Txt.Add('');

      cnt := Txt.Count;
      MI := P.GetUnitDependencies;
      while MI.HasNext do
      begin
        dE := TUnitDependency(MI.Next);
        Txt.Add(Format('* %s',[GenEntityRef(DE.Package)]));
      end;
      if cnt < Txt.Count then
      begin
        Txt.Insert(cnt, '## Dependences');
        Txt.Add('');
      end;
      Txt.SaveToFile(FileName);
    finally
      Txt.Free;
    end;
    MakeDiagram( P );
  end;
end;

destructor TMDocGen.Destroy;
begin
  inherited Destroy;
end;

end.


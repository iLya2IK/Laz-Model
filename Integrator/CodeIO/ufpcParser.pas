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
unit ufpcParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCodeParser, uModel, uModelEntity, PParser, PasTree;


type

  { TSimpleEngine }

  TSimpleEngine = class(TPasTreeContainer)
  private
    fIsProgram: boolean;
  public
    constructor Create;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
    function FindModule(const AName: String): TPasModule; override;
    property IsProgram: boolean read fIsProgram write fIsProgram;
  end;

  { TfpcParser }

  TfpcParser = class(TCodeParser)
    private
      FOM: TObjectModel;
      FUnit: TUnitPackage;
      fFilename: string;
      FGlobalDefines: TStringList;

      function getVisibility(vis: TPasMemberVisibility): TVisibility;
      function getProcType(pt: TProcType): TOperationType;
      function getClassifier(s: String): TClassifier;
      function getInterfaceRef(s: string): TInterface;

      procedure ParseProject(M: TPasProgram);
      procedure ParseUnit(M: TPasModule);
      procedure GetUnits(u: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure GetClasses(c: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure GetTypes(c: TFPList; AVisibility: TVisibility = viPublic; Recurse: Boolean = True);
      procedure PopulateMembers(ths: TClass; mems: TFPList); overload;
      procedure PopulateMembers(intf: TInterface; mems: TFPList); overload;
      procedure PopulateClass(ths: TClass; cls: TPasMembersType);
      procedure LinkProperties(ths: TClass);
      procedure PopulateInterface(intf: TInterface; cls: TPasClassType);
      procedure AddProcedure(op: TOperation; proc: TPasProcedure);
      procedure AddConstructor(op: TOperation; proc: TPasConstructor);
      procedure AddDestructor(op: TOperation; proc: TPasDestructor);
      procedure AddFunction(op: TOperation; proc: TPasFunction);
      procedure AddProperty(prop: TProperty; pproc: TPasProperty);
    public
      procedure ParseFileWithDefines( AModel: TAbstractPackage; AOM: TObjectModel; const GlobalDefines: TStringList );
    published
      property Filename: string read fFilename write fFilename;

  end;


implementation

uses
  Dialogs;

function DelQuot(s:String):String;
   var i:integer;
   const s1=#39;
  begin
   Result:='';
   i:=pos(s1,s);
   while i > 0 do
    begin
     if i > 0 then delete(s,i,1);
     i:=pos(s1,s);
    end;
   //if i > 0 then delete(s,i,2);
   Result:=s;
  end;

{ TSimpleEngine }

constructor TSimpleEngine.Create;
begin
  self.InterfaceOnly := True;
  self.NeedComments := False;
 inherited;
end;

function TSimpleEngine.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);

  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  Result := nil;
end;

function TSimpleEngine.FindModule(const AName: String): TPasModule;
begin
  if fIsProgram then
     Result := TPasModule.Create(AName, nil)
  else
     Result := nil;
end;


{ TfpcParser }

// TODO EXTEND The model
function TfpcParser.getVisibility(vis: TPasMemberVisibility): TVisibility;
begin
  case vis of
    visPublic, visDefault: Result := viPublic;
    visStrictPrivate, visPrivate: Result := viPrivate;
    visStrictProtected, visProtected: Result := viProtected;
    visPublished: Result := viPublished;
    visAutomated:
  end;
end;

// TODO EXTEND The model
function TfpcParser.getProcType(pt: TProcType): TOperationType;
begin
  case pt of
    ptProcedure,ptClassProcedure: Result := TOperationType.otProcedure;
    ptFunction,ptClassFunction: Result := TOperationType.otFunction;
    ptConstructor,ptClassConstructor: Result := TOperationType.otConstructor;
    ptDestructor, ptClassDestructor: Result := TOperationType.otDestructor;
//    ptOperator, ptClassOperator: ;
  end;
end;

procedure TfpcParser.ParseProject(M: TPasProgram);
begin
  FUnit := (FModel as TLogicPackage).AddUnit(M.Name);
  FOM.AddNestedUnit(FUnit, false);
  FUnit.Sourcefilename := Filename;
  GetUnits(M.ProgramSection.UsesList);
end;

procedure TfpcParser.ParseUnit(M: TPasModule);
var
  intf: TInterfaceSection;
begin
   FUnit := (FModel as TLogicPackage).AddUnit(M.Name);
   FUnit.Sourcefilename := Self.Filename;
   FOM.AddNestedUnit(FUnit, false);
   intf := M.InterfaceSection;
   GetUnits(intf.UsesList);
   GetClasses(intf.Classes);
   GetTypes(intf.Types);
end;

procedure TfpcParser.GetUnits(u: TFPList; AVisibility: TVisibility;
  Recurse: Boolean);
var
  i: integer;
  prs: TfpcParser;
  str: TStream;
  fullName, uName: string;
  ref: TPasElement;
  aUn : TUnitPackage;
begin
  If Assigned(u) and (u.Count > 0) then
  begin
     for i := 0 to u.Count- 1 do
     begin
       ref := TPasElement(u.Items[i]);
       if (ref is TPasModule) and (TPasModule(ref).Filename <> '') then
          uname := DelQuot(TPasModule(ref).FileName)
        else
          uName := ref.Name;

       aUn := FOM.ModelRoot.FindUnitPackage(uName);
       if Assigned(NeedPackage) and not Assigned(aUn) then
       begin
         fullName := NeedPackage(uName, str{%H-}, Recurse);
         if Fullname <> '' then
         begin
           try
             prs := TfpcParser.Create;
             prs.FileName := fullName;
             prs.NeedPackage := NeedPackage;
             try
               prs.ParseFileWithDefines(FModel, FOM, FGlobalDefines);
               aUn := FOM.ModelRoot.FindUnitPackage(uName);
             except
               on E : EParseError do
                 ShowMessage(E.Message);
             end;
           finally
             FreeAndNil(prs);
           end;
         end;
       end;
       if Assigned(aUn) and Recurse then
         FUnit.AddUnitDependency(aUn,AVisibility);
       FOM.AddNestedUnit(FUnit.Name, uName, false);
     end;
  end;
end;

procedure TfpcParser.GetClasses(c: TFPList; AVisibility: TVisibility;
  Recurse: Boolean);
var
  i: integer;
  cls : TPasClassType;
  rec : TPasRecordType;
  ths: TClass;
  intf: TInterface;
begin
  If Assigned(c) and (c.Count > 0) then
  begin
     for i := 0 to c.Count- 1 do
     begin
       if TObject(c.Items[i]) is TPasClassType then
       begin
        cls := TPasClassType(c.Items[i]);
        case cls.ObjKind of
          okObject, okClass:
          begin
            ths := FUnit.AddClass(cls.Name);
            ths.SourceY := cls.SourceLinenumber;
          end;
          okInterface:
          begin
            intf := FUnit.AddInterface(cls.Name);
            intf.SourceY := cls.SourceLinenumber;
          end;
//  TODO        okGeneric, okSpecialize,
//  or NOT      okClassHelper,okRecordHelper,okTypeHelper
        end;
       end else
       if TObject(c.Items[i]) is TPasRecordType then
       begin
         rec := TPasRecordType(c.Items[i]);
         ths := FUnit.AddClass(rec.Name);
         ths.SourceY := rec.SourceLinenumber;
       end;
     end;
     for i := 0 to c.Count- 1 do
     begin
       if TObject(c.Items[i]) is TPasClassType then
       begin
        cls := TPasClassType(c.Items[i]);
        case cls.ObjKind of
          okObject, okClass:
          begin
            ths := TClass(FUnit.FindClassifier(cls.Name));
            PopulateClass(ths, cls);
          end;
          okInterface:
          begin
            intf := TInterface(FUnit.AddInterface(cls.Name));
            PopulateInterface(intf, cls);
          end;
//  TODO        okGeneric, okSpecialize,
//  or NOT      okClassHelper,okRecordHelper,okTypeHelper
        end;
       end else
       if TObject(c.Items[i]) is TPasRecordType then
       begin
         rec := TPasRecordType(c.Items[i]);
         ths := TClass(FUnit.FindClassifier(rec.Name));
         PopulateClass(ths, rec);
       end;
     end;
  end;

end;

procedure TfpcParser.GetTypes(c : TFPList; AVisibility : TVisibility;
  Recurse : Boolean);
var
  i: integer;
  rec : TPasRecordType;
  ths: TClass;
  intf: TInterface;
begin
  If Assigned(c) and (c.Count > 0) then
  begin
     for i := 0 to c.Count- 1 do
     begin
       if TObject(c.Items[i]) is TPasRecordType then
       begin
         rec := TPasRecordType(c.Items[i]);
         ths := FUnit.AddClass(rec.Name);
         ths.SourceY := rec.SourceLinenumber;
       end;
     end;
     for i := 0 to c.Count- 1 do
     begin
       if TObject(c.Items[i]) is TPasRecordType then
       begin
         rec := TPasRecordType(c.Items[i]);
         ths := TClass(FUnit.FindClassifier(rec.Name));
         PopulateClass(ths, rec);
       end;
     end;
  end;

end;

procedure TfpcParser.PopulateMembers(ths: TClass; mems: TFPList);
var
  i: integer;
  op: TOperation;
  attr: TAttribute;
  vari: TPasVariable;
  prop: TProperty;
begin
   for i := 0 to mems.Count-1 do
   begin
      case TPasType(mems.Items[i]).ElementTypeName of
        'procedure':
        begin
           op := ths.AddOperation(TPasType(mems.Items[i]).Name);
           op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
           AddProcedure(op,TPasProcedure(mems.Items[i]));
        end;
        'function' :
          begin
            op := ths.AddOperation(TPasType(mems.Items[i]).Name);
            op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
            AddFunction(op, TPasFunction(mems.Items[i]));
          end;

        'property'  :
          begin
             prop := ths.AddProperty(TPasType(mems.Items[i]).Name);
             prop.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
             AddProperty(prop, TPasProperty(mems.Items[i]));
          end;
         'variable'  :
           begin
              vari :=  TPasVariable(mems.Items[i]);
              attr := ths.AddAttribute(vari.Name);
              attr.TypeClassifier := getClassifier(vari.VarType.name);
              attr.Visibility := getVisibility(vari.Visibility);
              attr.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
           end;
         'constructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddConstructor(op, TPasConstructor(mems.Items[i]));
           end;
         'destructor':
           begin
              op := ths.AddOperation(TPasType(mems.Items[i]).Name);
              op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
              AddDestructor(op, TPasDestructor(mems.Items[i]));
           end
         else
           begin
             {$IFDEF DEBUG}
               Assert(True, 'Missing '  + TPasType(mems.Items[i]).ElementTypeName + ' in class members');
             {$ENDIF}
           end;

      end;
   end;
end;

procedure TfpcParser.PopulateMembers(intf: TInterface; mems: TFPList);
var
  i: integer;
  op: TOperation;
begin
  for i := 0 to mems.Count-1 do
  begin
     case TPasType(mems.Items[i]).ElementTypeName of
       'procedure':
       begin
          op := intf.AddOperation(TPasType(mems.Items[i]).Name);
          op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
          AddProcedure(op,TPasProcedure(mems.Items[i]));
       end;
       'function' :
         begin
            op := intf.AddOperation(TPasType(mems.Items[i]).Name);
            op.SourceY := TPasType(mems.Items[i]).SourceLinenumber;
            AddFunction(op, TPasFunction(mems.Items[i]));
         end
       else
         begin
            {$IFDEF DEBUG}
              Assert(True, 'Missing '  + TPasType(mems.Items[i]).ElementTypeName + ' in interface members');
            {$ENDIF}
         end;
     end;
  end;
end;


// this has a nasty hack due to the parser parsing file by file
// even though it does things recursivly, unfortunately there are a lot
// of TStringLists, TObject references which come as parameters to
// Types and FunctionTypeDef e.g of Of TObject. etc etc. The current model
// does not allow inheritance of dataTypes.
// This is something which has to be fixed as Pascal does have type inheritance.
// So Basically if we see a T as the first letter in an indentifier
// Generate a Class in the unknown bucket instead of a DataType style
// Primitive. Otherwise we would have to chase the full heirachy all the way
// down into the fcl.
function TfpcParser.getClassifier(s: String): TClassifier;
begin
  if s = '' then s := 'Unidentified datatype';
  Result := FUnit.FindClassifier(s, False, TClass);
  if not Assigned(Result) then
     Result := FUnit.FindClassifier(s);
  if not Assigned(Result) then
       Result := FOM.UnknownPackage.FindClassifier(s, False, TClass);
  if not Assigned(Result) then
     Result := FOM.UnknownPackage.FindClassifier(s);
  if not Assigned(Result) then
  {if s[1] ='T' then   // HACK ALERT
      Result := FOM.UnknownPackage.AddClass(s)
  else}
      Result := FOM.UnknownPackage.AddDatatype(s);
end;

function TfpcParser.getInterfaceRef(s: string): TInterface;
begin
   Result := FUnit.FindClassifier(s,False,TInterface) as TInterface;
   if not Assigned(Result) then
   begin
     Result := FOM.UnknownPackage.FindClassifier(s,False,TInterface) as TInterface;
     if not (Assigned(Result) and (Result is TInterface)) then
        Result := FOM.UnknownPackage.AddInterface(s);
   end;
end;

procedure TfpcParser.PopulateClass(ths: TClass; cls: TPasMembersType);
var
  ans: TPasType;
  ancestor: TClass;
  cfr: TClassifier;
  intf: TInterface;
  intfs: TFPList;
  i: integer;
begin
  if (cls is TPasClassType) and Assigned(TPasClassType(cls).AncestorType) then
  begin
    ans := TPasType(TPasClassType(cls).AncestorType);
    if Length(ans.Name) = 0 then //some empty parent name -> TObject
    begin
      ans.Name := 'TObject';
    end;
    ancestor := FUnit.FindClassifier(ans.Name,False,TClass) as TClass;
    if Not Assigned(ancestor) then
    begin
        cfr := FOM.UnknownPackage.FindClassifier(ans.Name);
        if Assigned(cfr) then
           TClassifier( ancestor) := cfr;
    end;
    If Assigned( ancestor) then
      ths.Ancestor := ancestor
    else
      ths.Ancestor := FOM.UnknownPackage.AddClass(ans.Name);
  end;

  If (cls is TPasClassType) and Assigned(TPasClassType(cls).Interfaces) then
  begin
    intfs:= TPasClassType(cls).Interfaces;
    for i := 0 to intfs.Count -1 do
    begin
      intf := getInterfaceRef(TPasType(intfs[i]).Name);
      ths.AddImplements(intf);
    end;
  end;

  if Assigned(cls.Members) then begin
     PopulateMembers(ths, cls.Members);
     LinkProperties(ths);
  end;

end;

procedure TfpcParser.LinkProperties(ths : TClass);
var
  MI : IModelIterator;
  Pr : TAttribute;
begin
  MI := ths.GetAttributes;
  while MI.HasNext do
  begin
    Pr := TAttribute(MI.Next);
    if Pr is TProperty then
      TProperty(Pr).LinkAttrs(ths);
  end;
end;

procedure TfpcParser.PopulateInterface(intf: TInterface; cls: TPasClassType);
begin
  if Assigned (cls.AncestorType) then
     intf.Ancestor := getInterfaceRef(cls.AncestorType.Name);

  if Assigned(cls.Members) then
     PopulateMembers(intf, cls.Members);

end;

procedure TfpcParser.AddProcedure(op: TOperation; proc: TPasProcedure);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otProcedure;
  if Assigned(proc.ProcType.Args) and  (proc.ProcType.Args.Count> 0) then
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     if Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
end;

procedure TfpcParser.AddConstructor(op: TOperation; proc: TPasConstructor);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otConstructor;
  if Assigned(proc.ProcType.Args) and  (proc.ProcType.Args.Count> 0) then
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
end;

procedure TfpcParser.AddDestructor(op: TOperation; proc: TPasDestructor);
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otDestructor;
end;

procedure TfpcParser.AddFunction(op: TOperation; proc: TPasFunction);
var
  i: integer;
  arg: TPasArgument;
  par: TParameter;
begin
  op.Visibility := getVisibility(proc.Visibility);
  op.OperationType := otFunction;
  for i:= 0 to proc.ProcType.Args.Count-1 do
  begin
     arg :=  TPasArgument(proc.ProcType.Args.Items[i]);
     par := op.AddParameter(arg.Name);
     If Assigned (arg.ArgType) then
       par.TypeClassifier := getClassifier(arg.ArgType.Name);
  end;
  op.ReturnValue := getClassifier(TPasFunctionType(proc.ProcType).ResultEl.ResultType.Name);
end;

procedure TfpcParser.AddProperty(prop: TProperty; pproc: TPasProperty);
begin
   prop.Visibility := getVisibility(pproc.Visibility);
   If Assigned (pproc.VarType) then
     prop.TypeClassifier := getClassifier(pproc.VarType.Name);
   if assigned(pproc.ReadAccessor) then
     prop.ReadAttrExpr := pproc.ReadAccessor.GetDeclaration(false);
   if assigned(pproc.WriteAccessor) then
     prop.WriteAttrExpr := pproc.WriteAccessor.GetDeclaration(false);
end;



procedure TfpcParser.ParseFileWithDefines(AModel: TAbstractPackage;
  AOM: TObjectModel; const GlobalDefines: TStringList);
var
  M: TPasModule;
  E: TPasTreeContainer;
  s, platform, cpu: string;
  params : Array of String;
  i : integer;
  pp: TPasProgram;
begin
  FGlobalDefines := GlobalDefines;
  FModel := AModel;
  FOM := AOM;

  E := TSimpleEngine.Create;
  s:=  ExtractFileExt(fFilename);
  try
    {$IFDEF WINDOWS}
    platform := 'WINDOWS';
    {$ELSE}
    {$IFDEF LINUX}
    platform := 'LINUX';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF CPU64}
    cpu := 'x86_64';
    {$ELSE}
    cpu := 'i8086';
    {$ENDIF}
    SetLength(params, 1 + GlobalDefines.Count);
    for i := 0 to GlobalDefines.Count-1 do
    begin
      params[i+1] := GlobalDefines[i];
    end;
    params[0] := self.Filename;

    if ( s = '.lpr') then
    begin
       E.InterfaceOnly := false;
       TSImpleEngine(E).IsProgram := True;

       pp:= ParseSource(E, params, platform ,cpu, [poUseStreams]) as TPasProgram;

       ParseProject (pp);
    end
    else
    begin
       M := ParseSource(E, params, platform , cpu, [poUseStreams]);
       ParseUnit(M);
       FreeAndNil(M);
    end;
  finally
      E.Free;
  end;
end;

end.


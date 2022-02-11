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

unit uRtfdComponents;

{$mode objfpc}{$H+}

interface
uses LCLIntf, LCLType,
  ExtCtrls, Classes, uModel, uModelEntity, Controls, uListeners,
  uViewIntegrator, uDiagramFrame, uRtfdLabel;

type

  //Baseclass for a diagram-panel
  TRtfdBoxClass = class of TRtfdBox;

  { TRtfdBox }

  TRtfdBox = class(TPanel, IModelEntityListener)
  private
    FMinVisibility : TVisibility;
    procedure SetMinVisibility(const Value: TVisibility);
    procedure OnChildMouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: Classes.TOperation); override;
  public
    Frame: TDiagramFrame;
    Entity: TModelEntity;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility); reintroduce; virtual;
    procedure RefreshEntities; virtual; abstract;
    procedure Paint; override;
    procedure Change(Sender: TModelEntity); virtual;
    procedure AddChild(Sender: TModelEntity; NewChild: TModelEntity); virtual;
    procedure Remove(Sender: TModelEntity); virtual;
    procedure EntityChange(Sender: TModelEntity); virtual;
    property MinVisibility : TVisibility read FMinVisibility write SetMinVisibility;
  end;

  TRtfdClass = class(TRtfdBox, IAfterClassListener)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
    procedure AddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  TRtfdInterface = class(TRtfdBox, IAfterInterfaceListener)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility); override;
    destructor Destroy; override;
    procedure RefreshEntities; override;
    procedure AddChild(Sender: TModelEntity; NewChild: TModelEntity); override;
  end;

  TRtfdUnitPackage = class(TRtfdBox)
  public
    P: TUnitPackage;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility); override;
    procedure RefreshEntities; override;
    procedure DblClick; override;
  end;

//  TRtfdCustomLabel = class(TCustomLabel, IModelEntityListener)
  TRtfdCustomLabel = class(TRtfdODLabel)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;

  end;


  { TRtfdClassName }

  TRtfdClassName = class(TRtfdCustomLabel, IAfterClassListener)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
  end;

  { TRtfdInterfaceName }

  TRtfdInterfaceName = class(TRtfdCustomLabel, IAfterInterfaceListener)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
  end;

  //Left-justified label with visibility-icon

  { TVisibilityLabel }

  TVisibilityLabel = class(TRtfdCustomLabel)
    procedure Paint(width: integer); override;
    function WidthNeeded : integer; override;
  end;

  { TRtfdOperation }

  TRtfdOperation = class(TVisibilityLabel, IAfterOperationListener)
  private
    O: TOperation;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
    procedure IAfterOperationListener.EntityChange = EntityChange;
  end;

  { TRtfdAttribute }

  TRtfdAttribute = class(TVisibilityLabel, IAfterAttributeListener)
  private
    A: TAttribute;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
    procedure IAfterAttributeListener.EntityChange = EntityChange;
  end;

  { TRtfdSeparator }

  TRtfdSeparator = class(TRtfdCustomLabel)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    constructor Create(AOwner: TComponent; Tp: integer);
    procedure Paint(width: integer); override;
  end;

  TRtfdStereotype = class(TRtfdCustomLabel)
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; ACaption: string); reintroduce;
  end;

  { TRtfdUnitPackageName }

  TRtfdUnitPackageName = class(TRtfdCustomLabel, IAfterUnitPackageListener)
  private
    P: TUnitPackage;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

  //Class to display mame of package at upper-left corner in a unitpackage diagram

  { TRtfdUnitPackageDiagram }

  TRtfdUnitPackageDiagram = class(TRtfdCustomLabel, IAfterUnitPackageListener)
  private
    P: TUnitPackage;
  public
    constructor Create(AOwner: TComponent; AEntity: TModelEntity; Tp: integer); override;
    destructor Destroy; override;
    procedure EntityChange(Sender: TModelEntity); override;
    procedure IAfterUnitPackageListener.EntityChange = EntityChange;
  end;

implementation

uses Graphics, uError, SysUtils, essConnectPanel, uIterators,
uConfig, uRtfdDiagramFrame, Math;




{ TRtfdCustomLabel }

constructor TRtfdCustomLabel.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  inherited Create(AOwner, AEntity, Tp);

end;

{ TRtfdBox }

constructor TRtfdBox.Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility);
begin
  inherited Create(AOwner);
  Color := clWhite;
  BorderWidth := ClassShadowWidth;
  Self.Frame := AFrame;
  Self.Entity := AEntity;
  Self.FMinVisibility := AMinVisibility;
end;


procedure TRtfdBox.Paint;
const
  TopH = 39;
var
  R: TRect;
  Sw: integer;
  i: integer;
begin
  Sw := ClassShadowWidth;
  R := GetClientRect;
  with Canvas do
  begin
    //Shadow
    Brush.Color := clSilver;
    Pen.Color := clSilver;
    RoundRect(R.Right - Sw - 8, R.Top + Sw, R.Right, R.Bottom, 8, 8);
    FillRect(Rect(Sw, R.Bottom - Sw, R.Right, R.Bottom));

    //Holes
    Brush.Color := (Parent as TessConnectPanel).Color;
    FillRect(Rect(R.Left, R.Bottom - Sw, R.Left + Sw, R.Bottom));
    FillRect(Rect(R.Right - Sw, R.Top, R.Right, R.Top + Sw));

    //Background
    Brush.Color := clWhite;
    Pen.Color := clBlack;

    Brush.Color := TopColor[ Config.IsLimitedColors ];
    RoundRect(R.Left, R.Top, R.Right - Sw, R.Top + TopH, 8, 8);
    Brush.Color := clWhite;
    Rectangle(R.Left, R.Top + TopH - 8, R.Right - Sw, R.Bottom - Sw);
    FillRect( Rect(R.Left+1,R.Top + TopH - 8, R.Right - Sw - 1, R.Top + TopH + 1 - 8) );
  end;
  for i:= 0 to ComponentCount - 1 do
    TRtfdODLabel(Components[i]).paint(ClientWidth);
end;

procedure TRtfdBox.AddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  //Stub
end;


procedure TRtfdBox.Change(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.EntityChange(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdBox.Remove(Sender: TModelEntity);
begin
  //Stub
end;


procedure TRtfdBox.SetMinVisibility(const Value: TVisibility);
begin
  if Value<>FMinVisibility then
  begin
    FMinVisibility := Value;
    RefreshEntities;
  end;
end;


//The following declarations are needed for helping essconnectpanel to
//catch all mouse actions. All controls that are inserted (classname etc)
//in rtfdbox will get their mousedown-event redefined.
type
  TCrackControl = class(TControl);

procedure TRtfdBox.Notification(AComponent: TComponent; Operation: Classes.TOperation);
begin
  inherited;
  //Owner=Self must be tested because notifications are being sent for all components
  //in the form. TRtfdLabels are created with Owner=box.
 // if (Operation = opInsert) and (Acomponent.Owner = Self) and (Acomponent is TControl) then
 //   TCrackControl(AComponent).OnMouseDown := @OnChildMouseDown;
end;


procedure TRtfdBox.OnChildMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;
  pt.X := X;
  pt.Y := Y;
  pt := TControl(Sender).ClientToScreen(pt);
  pt := ScreenToClient(pt);
  MouseDown(Button,Shift,pt.X,pt.Y);
end;


{ TRtfdClass }

constructor TRtfdClass.Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility);
begin
  inherited Create(AOwner, AEntity, AFrame, AMinVisibility);
  PopupMenu := Frame.ClassInterfacePopupMenu;
  Entity.AddListener(IAfterClassListener(Self));
  RefreshEntities;
end;

destructor TRtfdClass.Destroy;
begin
  Entity.RemoveListener(IAfterClassListener(Self));
  inherited;
end;

procedure TRtfdClass.AddChild(Sender: TModelEntity; NewChild: TModelEntity);
begin
  RefreshEntities;
end;

procedure TRtfdClass.RefreshEntities;
var
  NeedH,NeedW,I : integer;
  C: TClass;
  Omi,Ami : IModelIterator;
  WasVisible : boolean;
begin
  WasVisible := Visible;
  Hide;
  DestroyComponents;
  if Entity is TClass then
  begin
    C := Entity as TClass;

    NeedW := 0;
    NeedH := (ClassShadowWidth * 2) + 4;
    Inc(NeedH, TRtfdClassName.Create(Self, Entity, 16).Height);

    //Get names in visibility order
    if FMinVisibility > Low(TVisibility) then
    begin
      Omi := TModelIterator.Create(C.GetOperations,TOperation,FMinVisibility,ioVisibility);
      Ami := TModelIterator.Create(C.GetAttributes,TAttribute,FMinVisibility,ioVisibility);
    end
    else
    begin
      Omi := TModelIterator.Create(C.GetOperations,ioVisibility);
      Ami := TModelIterator.Create(C.GetAttributes,ioVisibility);
    end;

    //Separator
    if (Ami.Count>0) or (Omi.Count>0) then
      Inc(NeedH, TRtfdSeparator.Create(Self, NeedH).Height);

    //Attributes
    while Ami.HasNext do
      Inc(NeedH, TRtfdAttribute.Create(Self,Ami.Next, NeedH).Height);

    //Separator
    if (Ami.Count>0) and (Omi.Count>0) then
      Inc(NeedH, TRtfdSeparator.Create(Self, NeedH).Height);

    //Operations
    while Omi.HasNext do
      Inc(NeedH, TRtfdOperation.Create(Self,Omi.Next, NeedH).Height);

    for i:= 0 to ComponentCount - 1 do
      if (TComponent(Components[I]) is TRtfdODLabel) then
        NeedW := Max( TRtfdODLabel(Components[I]).WidthNeeded,NeedW);

    Height :=  Max(NeedH,cDefaultHeight) + 10;
    Width  :=  Max(NeedW,cDefaultWidth);

    Visible := WasVisible;
  end;
end;

{ TRtfdUnitPackage }

constructor TRtfdUnitPackage.Create(AOwner: TComponent; AEntity: TModelEntity; AFrame: TDiagramFrame; AMinVisibility : TVisibility);
begin
  inherited Create(AOwner, AEntity, AFrame, AMinVisibility);
  PopupMenu := Frame.PackagePopupMenu;
  P := Entity as TUnitPackage;
  RefreshEntities;
end;

procedure TRtfdUnitPackage.DblClick;
begin
//  PostMessage(Frame.Handle, WM_ChangePackage, 0, 0);
end;

procedure TRtfdUnitPackage.RefreshEntities;
begin
  DestroyComponents;
  TRtfdUnitPackageName.Create(Self, P, 50);
  Height := 45;
end;

{ TRtfdCustomLabel }
{
constructor TRtfdCustomLabel.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
//  inherited Create(AOwner);
  Parent := Owner as TWinControl;
  Self.Entity := AEntity;
  AutoSize := False;
  Height := Abs(Font.Height);
  FAlignment := taLeftJustify;
  FTransparent := True;
  Top := Tp;
  Width := cDefaultWidth;
  Left := 4;
  //Top must be assigned so that all labels appears beneath each other when align=top
//  Top := MaxInt shr 2;
//  Align := alTop;
end;

procedure TRtfdCustomLabel.EntityChange(Sender: TModelEntity);
begin
  if Trim(Entity.Documentation.Description) <> '' then
  begin
    Hint := Entity.Documentation.Description;
//    Caption := Caption + '   '+#$AA;
  end
  else
    ShowHint := False;
//  Debugcode to show sourcefile and position as hint info.
//  Hint := Entity.Sourcefilename+':'+IntToStr(Entity.SourceY)+','+IntToStr(Entity.SourceX);
end;

procedure TRtfdCustomLabel.Remove(Sender: TModelEntity);
begin
  //Stub
end;

procedure TRtfdCustomLabel.AddChild(Sender: TModelEntity; NewChild: TModelEntity
  );
begin
  //Stub
end;

procedure TRtfdCustomLabel.Change(Sender: TModelEntity);
begin
  // Stub
end;

function TRtfdCustomLabel.WidthNeeded: integer;
begin
  Result := Width + 4 + (2 * ClassShadowWidth);
end;
}

{ TVisibilityLabel }



procedure TVisibilityLabel.Paint(width: integer);
var
  Al: integer;
  Pic : Graphics.TBitmap;
  tmpBox: TRect;
  oldFont: TFont;
begin
  fbox.Right := width;
  oldFont := Canvas.Font;
  Canvas.Font := Font;
  tmpBox := FBox;

  case Entity.Visibility of
    viPrivate : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPrivateImage.Picture.Bitmap;
    viProtected : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisProtectedImage.Picture.Bitmap;
    viPublic : Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  else
    Pic := ((Parent as TRtfdBox).Frame as TRtfdDiagramFrame).VisPublicImage.Picture.Bitmap;
  end;
  Canvas.Draw(tmpBox.Left,tmpBox.Top + 1, Pic );
  tmpBox.Left := tmpBox.Left + cIconW + cMargin;

  Al := DT_LEFT;
  case Alignment of
    taLeftJustify: Al := DT_LEFT;
    taRightJustify: Al := DT_RIGHT;
    taCenter: Al := DT_CENTER;
  end;

  DrawText(Canvas.Handle,PChar(Caption),Length(Caption),tmpBox,Al);

  Canvas.Font := oldFont;

end;


function TVisibilityLabel.WidthNeeded: integer;
begin
  Result := inherited;
  Result := Result + cIconW + cMargin;
end;

{ TRtfdClassName }

constructor TRtfdClassName.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  inherited Create(AOwner, AEntity, Tp);
  Font.Style := [fsBold];
  Transparent := True;
  Alignment := taCenter;
  Entity.AddListener(IAfterClassListener(Self));
  EntityChange(nil);
end;

destructor TRtfdClassName.Destroy;
begin
  Entity.RemoveListener(IAfterClassListener(Self));
  inherited;
end;

procedure TRtfdClassName.EntityChange(Sender: TModelEntity);
var
  Mi : IModelIterator;
begin
  Mi := (Entity as TClass).GetOperations;
  while Mi.HasNext do
    if (Mi.Next as TOperation).IsAbstract then
    begin
      Font.Style := Font.Style + [fsItalic];
      Break;
    end;
  if ((Parent as TRtfdBox).Frame as TDiagramFrame).Diagram.Package<>Entity.Owner then
    Caption := Entity.FullName
  else
    Caption := Entity.Name;

  inherited EntityChange(SEnder);
end;


{ TRtfdInterfaceName }

constructor TRtfdInterfaceName.Create(AOwner: TComponent;
  AEntity: TModelEntity; Tp: integer);
begin
  inherited Create(AOwner, AEntity, Tp);
  Font.Style := [fsBold];
  Transparent := True;
  Alignment := taCenter;
  Entity.AddListener(IAfterInterfaceListener(Self));
  EntityChange(nil);
end;

destructor TRtfdInterfaceName.Destroy;
begin
  Entity.RemoveListener(IAfterInterfaceListener(Self));
  inherited;
end;

procedure TRtfdInterfaceName.EntityChange(Sender: TModelEntity);
begin
  if ((Parent as TRtfdBox).Frame as TDiagramFrame).Diagram.Package<>Entity.Owner then
    Caption := Entity.FullName
  else
    Caption := Entity.Name;

  inherited EntityChange(Sender);
end;


{ TRtfdSeparator }

constructor TRtfdSeparator.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  Create(AOwner, Tp);
end;

constructor TRtfdSeparator.Create(AOwner: TComponent; Tp: integer);
begin
  inherited Create(AOwner, nil, Tp);
  FBox.Left := 0;
  FBox.Right := cDefaultWidth - ClassShadowWidth  ;

end;

procedure TRtfdSeparator.Paint(width: integer);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(FBox.Left, FBox.Top + (Height div 2));
  Canvas.LineTo(Width - ClassShadowWidth, FBox.Top + (Height div 2));
end;

{ TRtfdPackageName }

constructor TRtfdUnitPackageName.Create(AOwner: TComponent;
  AEntity: TModelEntity; Tp: integer);
var
  th: integer;
begin
  inherited Create(AOwner, AEntity, 0);
  Font.Style := [fsBold];
  Alignment := taCenter;
  Transparent := True;
  th:=Height div 2;
  FBox.Top := FBox.Top + th;
  FBox.Bottom := FBox.Bottom + th;
  P := Entity as TUnitPackage;
  P.AddListener(IAfterUnitPackageListener(Self));
  EntityChange(nil);
end;

destructor TRtfdUnitPackageName.Destroy;
begin
  P.RemoveListener(IAfterUnitPackageListener(Self));
  inherited;
end;

procedure TRtfdUnitPackageName.EntityChange(Sender: TModelEntity);
begin
  Caption := P.Name;

  inherited EntityChange(Sender);
end;

{ TRtfdOperation }

constructor TRtfdOperation.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  inherited Create(AOwner, AEntity, Tp);
  O := Entity as TOperation;
  O.AddListener(IAfterOperationListener(Self));
  Self.EntityChange(nil);
end;

destructor TRtfdOperation.Destroy;
begin
  O.RemoveListener(IAfterOperationListener(Self));
  inherited;
end;

procedure TRtfdOperation.EntityChange(Sender: TModelEntity);
const
  ColorMap: array[TOperationType] of TColor = (clGreen, clRed, clBlack, clGray);
  //   otConstructor,otDestructor,otProcedure,otFunction);
begin
  //Default uml-syntax
  //visibility name ( parameter-list ) : return-type-expression { property-string }
  { TODO : show parameters and returntype for operation }
  Caption := O.Name + '(...)';
  Font.Style := [];
  Font.Color := ColorMap[O.OperationType];
  if O.IsAbstract then
    Font.Style := [fsItalic];

  inherited EntityChange(Sender);
end;

{ TRtfdAttribute }

constructor TRtfdAttribute.Create(AOwner: TComponent; AEntity: TModelEntity;
  Tp: integer);
begin
  inherited Create(AOwner, AEntity, Tp);
  A := Entity as TAttribute;
  A.AddListener(IAfterAttributeListener(Self));
  EntityChange(nil);
end;

destructor TRtfdAttribute.Destroy;
begin
  A.RemoveListener(IAfterAttributeListener(Self));
  inherited;
end;

procedure TRtfdAttribute.EntityChange(Sender: TModelEntity);
begin
  //uml standard syntax is:
  //visibility name [ multiplicity ] : type-expression = initial-value { property-string }
  if Assigned(A.TypeClassifier) then
    Caption := A.Name + ' : ' + A.TypeClassifier.Name
  else
    Caption := A.Name;

  inherited EntityChange(Sender);
end;

{ TRtfdUnitPackageDiagram }

constructor TRtfdUnitPackageDiagram.Create(AOwner: TComponent;
  AEntity: TModelEntity; Tp: integer);
begin
  //This class is the caption in upper left corner for a unitdiagram
  inherited Create(AOwner, AEntity, Tp);
//  Color := clBtnFace;
  Font.Name := 'Times New Roman';
  Font.Style := [fsBold];
  Font.Size := 12;
  Alignment := taLeftJustify;
  P := AEntity as TUnitPackage;
  P.AddListener(IAfterUnitPackageListener(Self));
  EntityChange(nil);
end;

destructor TRtfdUnitPackageDiagram.Destroy;
begin
  P.RemoveListener(IAfterUnitPackageListener(Self));
  inherited;
end;

procedure TRtfdUnitPackageDiagram.EntityChange(Sender: TModelEntity);
begin
  Caption := '   ' + P.FullName;

  inherited EntityChange(Sender);
end;


{ TRtfdInterface }

constructor TRtfdInterface.Create(AOwner: TComponent; AEntity: TModelEntity;
  AFrame: TDiagramFrame; AMinVisibility : TVisibility);
begin
  inherited Create(AOwner, AEntity, AFrame, AMinVisibility);
  Entity.AddListener(IAfterInterfaceListener(Self));
  PopupMenu := Frame.ClassInterfacePopupMenu;
  RefreshEntities;
end;

destructor TRtfdInterface.Destroy;
begin
  Entity.RemoveListener(IAfterInterfaceListener(Self));
  inherited;
end;

procedure TRtfdInterface.RefreshEntities;
var
  NeedW,NeedH,I : integer;
  OMi,AMi : IModelIterator;
  WasVisible : boolean;
  Int : TInterface;
begin
  Int := Entity as TInterface;

  WasVisible := Visible;
  Hide;
  DestroyComponents;


  NeedW := 0;
  NeedH := (ClassShadowWidth * 2) + 4;

  Inc(NeedH, TRtfdStereotype.Create(Self, Entity, 'interface').Height);
  Inc(NeedH, TRtfdInterfaceName.Create(Self, Entity, 16).Height);

  //Get names in visibility order
  if FMinVisibility > Low(TVisibility) then
  begin
    Omi := TModelIterator.Create(Int.GetOperations,TOperation,FMinVisibility,ioVisibility);
    Ami := TModelIterator.Create(Int.GetAttributes,TAttribute,FMinVisibility,ioVisibility);
  end
  else
  begin
    Omi := TModelIterator.Create(Int.GetOperations,ioVisibility);
    Ami := TModelIterator.Create(Int.GetAttributes,ioVisibility);
  end;

  //Separator
  if (Ami.Count>0) or (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self, NeedH).Height);

  //Attributes
  while Ami.HasNext do
    Inc(NeedH, TRtfdAttribute.Create(Self,Ami.Next, NeedH).Height);

  //Separator
  if (Ami.Count>0) and (Omi.Count>0) then
    Inc(NeedH, TRtfdSeparator.Create(Self, NeedH).Height);

  //Operations
  while Omi.HasNext do
    Inc(NeedH, TRtfdOperation.Create(Self,Omi.Next, NeedH).Height);

  for i:= 0 to ComponentCount - 1 do
    if (TComponent(Components[I]) is TRtfdCustomLabel) then
      NeedW := Max( TRtfdCustomLabel(Components[I]).WidthNeeded,NeedW);

  self.Height :=  Max(NeedH,cDefaultHeight) + 10;
  self.Width  :=  Max(NeedW,cDefaultWidth);

  Visible := WasVisible;
end;

procedure TRtfdInterface.AddChild(Sender, NewChild: TModelEntity);
begin
  RefreshEntities;
end;

{ TRtfdStereotype }

constructor TRtfdStereotype.Create(AOwner: TComponent; AEntity: TModelEntity; ACaption: string);
begin
  inherited Create(AOwner, AEntity, 2);
  Alignment := taCenter;
  Transparent := True;
{$IFDEF LINUX}
  Self.Caption := '<<' + ACaption + '>>';
{$ELSE}
  Self.Caption := '«' + ACaption + '»';
{$ENDIF LINUX}
end;


end.

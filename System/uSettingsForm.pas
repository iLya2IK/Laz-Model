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

unit uSettingsForm;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Grids;


type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    cbDotSaveWithUrls : TCheckBox;
    eAdditionalDefines : TEdit;
    eDotURLPrefix : TEdit;
    GroupBox1 : TGroupBox;
    GroupBox2 : TGroupBox;
    Label10 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    sgDotPrefs : TStringGrid;
    mdgIgnoreEntites : TMemo;
    OkButton: TButton;
    DiSaveCombo: TComboBox;
    Label1: TLabel;
    Button2: TButton;
    ShowAssocCheck: TCheckBox;
    VisibilityCombo: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    eEditorCommandLine: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure DelphiIDECheckClick(Sender: TObject);
    procedure Label8Click(Sender : TObject);
    procedure ShellCheckClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
    ShellChanged,IDEChanged : boolean;
    procedure ReadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
  end;

implementation

uses uIntegrator, uConfig, uViewIntegrator;

{$R *.lfm}


procedure TSettingsForm.FormCreate(Sender: TObject);
var o : TDotPrefs;
begin
  sgDotPrefs.RowCount := Integer(High(TDotPrefs)) - Integer(Low(TDotPrefs)) + 2;
  for o := Low(TDotPrefs) to High(TDotPrefs) do
    sgDotPrefs.Cells[0, Byte(o)+1] := cDOTP2STR[o];
  ReadSettings;
  IDEChanged := False;
  ShellChanged := False;
end;

procedure TSettingsForm.DelphiIDECheckClick(Sender: TObject);
begin
  IDEChanged := True;
end;

procedure TSettingsForm.Label8Click(Sender : TObject);
begin

end;


procedure TSettingsForm.ShellCheckClick(Sender: TObject);
begin
  ShellChanged := True;
end;

procedure TSettingsForm.OkButtonClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TSettingsForm.ReadSettings;
var i : TDiagramKind;
    o : TDotPrefs;
begin
  DiSaveCombo.ItemIndex := integer(Config.DiSave);
  ShowAssocCheck.Checked := Config.DiShowAssoc;
  VisibilityCombo.ItemIndex := Config.DiVisibilityFilter;
  eEditorCommandLine.Text := Config.EditorCommandLine;
  eAdditionalDefines.Text := Config.AdditionalDefines;

  eDotURLPrefix.Text :=        Config.DotUrlsPrefix;
  cbDotSaveWithUrls.Checked := Config.DotAddUrls;

  for i := Low(TDiagramKind) to High(TDiagramKind) do
  for o := Low(TDotPrefs) to High(TDotPrefs) do
    sgDotPrefs.Cells[Byte(i)+1, Byte(o)+1] :=  Config.DotPref[i, o];

  mdgIgnoreEntites.Lines.Delimiter := ';';
  mdgIgnoreEntites.Lines.DelimitedText := Config.MDGenIgnoreEntites;
end;

procedure TSettingsForm.SaveSettings;
var i : TDiagramKind;
    o : TDotPrefs;
begin
  Config.DiSave := TDiSaveSetting(DiSaveCombo.ItemIndex);
  Config.DiShowAssoc := ShowAssocCheck.Checked;
  Config.DiVisibilityFilter := VisibilityCombo.ItemIndex;
  Config.EditorCommandLine :=  eEditorCommandLine.Text;
  Config.AdditionalDefines := eAdditionalDefines.Text;

  Config.DotUrlsPrefix     := eDotURLPrefix.Text;
  Config.DotAddUrls        := cbDotSaveWithUrls.Checked;

  for i := Low(TDiagramKind) to High(TDiagramKind) do
  for o := Low(TDotPrefs) to High(TDotPrefs) do
    Config.DotPref[i, o] := sgDotPrefs.Cells[Byte(i)+1, Byte(o)+1];

  mdgIgnoreEntites.Lines.Delimiter := ';';
  Config.MDGenIgnoreEntites := mdgIgnoreEntites.Lines.DelimitedText;
  Config.StoreSettings;
end;

end.

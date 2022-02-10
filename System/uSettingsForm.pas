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
  StdCtrls, Spin;


type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    cbDotSaveWithUrls : TCheckBox;
    cbxDotSplineStyle : TComboBox;
    cbxDotPortSide : TComboBox;
    eAdditionalDefines : TEdit;
    eDotRankDir : TEdit;
    eDotURLPrefix : TEdit;
    GroupBox1 : TGroupBox;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    Label7 : TLabel;
    Label8 : TLabel;
    Label9 : TLabel;
    OkButton: TButton;
    DiSaveCombo: TComboBox;
    Label1: TLabel;
    Button2: TButton;
    ShowAssocCheck: TCheckBox;
    seDotFontSize : TSpinEdit;
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

uses uIntegrator, uConfig;

{$R *.lfm}


procedure TSettingsForm.FormCreate(Sender: TObject);
begin
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
begin
  DiSaveCombo.ItemIndex := integer(Config.DiSave);
  ShowAssocCheck.Checked := Config.DiShowAssoc;
  VisibilityCombo.ItemIndex := Config.DiVisibilityFilter;
  eEditorCommandLine.Text := Config.EditorCommandLine;
  eAdditionalDefines.Text := Config.AdditionalDefines;

  eDotRankDir.Text :=          Config.DotRankDir;
  seDotFontSize.Value :=       Config.DotFontSize;
  eDotURLPrefix.Text :=        Config.DotUrlsPrefix;
  cbDotSaveWithUrls.Checked := Config.DotAddUrls;
  cbxDotPortSide.Text :=       Config.DotPrefferedLabelConnector;
  cbxDotSplineStyle.Text :=    Config.DotSplines;
end;

procedure TSettingsForm.SaveSettings;
begin
  Config.DiSave := TDiSaveSetting(DiSaveCombo.ItemIndex);
  Config.DiShowAssoc := ShowAssocCheck.Checked;
  Config.DiVisibilityFilter := VisibilityCombo.ItemIndex;
  Config.EditorCommandLine :=  eEditorCommandLine.Text;
  Config.AdditionalDefines := eAdditionalDefines.Text;
  Config.DotRankDir        := eDotRankDir.Text;
  Config.DotFontSize       := seDotFontSize.Value;
  Config.DotUrlsPrefix     := eDotURLPrefix.Text;
  Config.DotAddUrls        := cbDotSaveWithUrls.Checked;
  Config.DotSplines        := cbxDotSplineStyle.Text;
  Config.DotPrefferedLabelConnector := cbxDotPortSide.Text;
  Config.StoreSettings;
end;

end.

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdCtrls, Measurements;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acShowHideMainMenu: TAction;
    acLoadData: TAction;
    ActionList: TActionList;
    Button1: TButton;
    lblHeading: TLabel;
    MainMenu: TMainMenu;
    miSep1: TMenuItem;
    miLoadData: TMenuItem;
    miSettings: TMenuItem;
    OpenMeasurementsDialog: TOpenDialog;
    pmiExtras: TMenuItem;
    miShowMainMenu: TMenuItem;
    pmiView: TMenuItem;
    miAbout: TMenuItem;
    pmiHelp: TMenuItem;
    miQuit: TMenuItem;
    pmiFile: TMenuItem;
    procedure acLoadDataExecute(Sender: TObject);
    procedure acShowHideMainMenuExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMeasurementEngine: TMeasurementEngine;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.acShowHideMainMenuExecute(Sender: TObject);
begin
  pmiFile.Visible := not pmiFile.Visible;
  pmiView.Visible := not pmiView.Visible;
  pmiExtras.Visible := not pmiExtras.Visible;
  pmiHelp.Visible := not pmiHelp.Visible;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMeasurementEngine := TMeasurementEngine.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FMeasurementEngine.Free;
end;

procedure TfrmMain.acLoadDataExecute(Sender: TObject);
begin
  if OpenMeasurementsDialog.Execute then
  begin

  end;
end;

end.


import { Component, Injectable, OnInit } from '@angular/core';
import { MatSnackBar, MatSnackBarConfig, MatSnackBarHorizontalPosition, MatSnackBarVerticalPosition } from '@angular/material/snack-bar';

@Component({
  selector: 'app-notification',
  templateUrl: './notification.component.html',
  styleUrls: ['./notification.component.css']
})
@Injectable({
  providedIn: 'root'
})
export class NotificationComponent implements OnInit {

  private horizontalPosition: MatSnackBarHorizontalPosition = 'right';
  private verticalPosition: MatSnackBarVerticalPosition = 'top';

  constructor(private _snackBar: MatSnackBar) {}

  openSnackBar(message: string, actionText: string, notificationType: string) {
    const typeClass = notificationType === 'success' ? 'mat-primary' : 'mat-warn';
    this._snackBar.open(
      message,
      actionText,
      {
        horizontalPosition: this.horizontalPosition,
        verticalPosition: this.verticalPosition,
        panelClass: ['mat-toolbar', typeClass]
      }
    );
  }

  ngOnInit(): void {
  }

}

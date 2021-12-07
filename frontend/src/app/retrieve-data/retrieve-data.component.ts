import { Component, OnInit } from '@angular/core';
import { throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { Data } from '../models/data';
import { NotificationComponent } from '../notification/notification.component';
import { DataService } from '../services/data.service';

@Component({
  selector: 'app-retrieve-data',
  templateUrl: './retrieve-data.component.html',
  styleUrls: ['./retrieve-data.component.css']
})
export class RetrieveDataComponent implements OnInit {
  public value: string;

  constructor(
    private notification: NotificationComponent,
    private ds: DataService
  ) {
    this.value = '';
  }

  ngOnInit(): void {
  }

  lookup(key: string, $event: Event) {
    $event.preventDefault();
    this.value = '';
    this.ds.getData(key).pipe(catchError((error: any) => {
      this.notification.openSnackBar(
        'An error occured!',
        'error'
      );
      return throwError(error.message);
    })).subscribe((data: Data) => {
      this.value = data.value;
      this.notification.openSnackBar(
        'Found it!',
        'success'
      );
    });
  }
}

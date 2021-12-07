import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { NotificationComponent } from '../notification/notification.component';
import { catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';

@Component({
  selector: 'app-store-data',
  templateUrl: './store-data.component.html',
  styleUrls: ['./store-data.component.css']
})
export class StoreDataComponent implements OnInit {
  private dataUrl = 'http://localhost:8080/data';

  constructor(
    private notification: NotificationComponent,
    private http: HttpClient
  ) { }

  ngOnInit(): void {
  }

  store(key: string, value: string, $event: Event) {
    $event.preventDefault();
    this.storeData(key, value).pipe(catchError((error: any) => {
      this.notification.openSnackBar(
        'Data could not be stored, please try again',
        'error'
      );
      return throwError(error.message);
    })).subscribe((res) => {
      this.notification.openSnackBar(
        'Data stored',
        'success'
      );
    });
  }

  private storeData(key: string, value: string) {
    return this.http.post<any>(`${this.dataUrl}`, {
      'key': key,
      'value': value
    })
  }
}

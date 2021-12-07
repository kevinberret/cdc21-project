import { Component, OnInit } from '@angular/core';
import { NotificationComponent } from '../notification/notification.component';

@Component({
  selector: 'app-store-data',
  templateUrl: './store-data.component.html',
  styleUrls: ['./store-data.component.css']
})
export class StoreDataComponent implements OnInit {

  constructor(private notification: NotificationComponent) { }

  ngOnInit(): void {
  }

  store(key: string, value: string, $event: Event) {
    $event.preventDefault();
    this.notification.openSnackBar(
      'Date could not be stored, please try again',
      'Close',
      'error'
    );
  }
}

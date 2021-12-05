import { Component, OnInit } from '@angular/core';
import { NotificationComponent } from '../notification/notification.component';

@Component({
  selector: 'app-retrieve-data',
  templateUrl: './retrieve-data.component.html',
  styleUrls: ['./retrieve-data.component.css']
})
export class RetrieveDataComponent implements OnInit {
  public value: string;

  constructor(private notification: NotificationComponent) {
    this.value = '';
  }

  ngOnInit(): void {
  }

  lookup(key: string, $event: Event) {
    $event.preventDefault();
    this.notification.openSnackBar(
      'Found it!',
      'Close',
      'success'
    );
  }

}

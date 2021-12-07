import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { Data } from '../models/data';
import { Observable, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators'

@Injectable({
  providedIn: 'root'
})
export class DataService {
  private url = 'http://localhost:8080/data';

  constructor(
    private http: HttpClient
  ) { }

  /** GET: retrieve data from the DHT */
  getData(key: string): Observable<Data> {
    return this.http.get<Data>(`${this.url}/${key}`);
  }
  
  /** POST: add a new data to the DHT */
  addData(data: Data): Observable<Data> {
    console.log(this.url)
    return this.http.post<Data>(this.url, data);
  }

}

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Data } from '../models/data';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class DataService {
  private url = 'http://localhost:8080';

  constructor(
    private http: HttpClient
  ) { }

  /** GET: retrieve data from the DHT */
  getData(key: string): Observable<Data> {
    return this.http.get<Data>(`${this.url}/data/${key}`);
  }
  
  /** POST: add a new data to the DHT */
  addData(data: Data): Observable<Data> {
    console.log(this.url)
    return this.http.post<Data>(`${this.url}/data`, data);
  }

  /** GET: retrieve DHT representation */
  getDhtRepr(): Observable<any> {
    return this.http.get<any>(`${this.url}/chordy`);
  }

}

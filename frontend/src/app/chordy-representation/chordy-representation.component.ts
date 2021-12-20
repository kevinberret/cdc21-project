import { Component, OnInit, ViewChild } from '@angular/core';
import { MatAccordion } from '@angular/material/expansion';
import { interval } from 'rxjs';
import { Store } from '../models/store';
import { Node } from '../models/node';
import { DataService } from '../services/data.service';


@Component({
  selector: 'app-chordy-representation',
  templateUrl: './chordy-representation.component.html',
  styleUrls: ['./chordy-representation.component.css']
})
export class ChordyRepresentationComponent implements OnInit {
  @ViewChild(MatAccordion, {static:true}) accordion!: MatAccordion;
  expandedNodeStatus: {[key: string]: boolean};
  displayedColumns: string[] = ['hashedKey', 'value'];
  dhtDataSource: {[key: string]: Array<{}>};
  public dhtNodes: Array<Node>;
    
  constructor(
    private ds: DataService
  ) {
    this.dhtNodes = [];
    this.expandedNodeStatus = {};
    this.dhtDataSource = {};
  }

  ngOnInit(): void {
    this.dhtRepr();
    const source = interval(5000);

    source.subscribe(() => this.dhtRepr()); 
  }

  updatePanelState(key: string, status: boolean) {
    this.expandedNodeStatus = { ...this.expandedNodeStatus, [key]: status };
  }

  dhtRepr() {
    this.ds.getDhtRepr().subscribe((data: any) => {
      this.dhtNodes = [];
      data.forEach((node: Node) => {
        // Find successor's index in the array.
        let index = this.dhtNodes.findIndex((el:Node) => el.successor === node.successor);
        // Insert current node at the correct index.
        this.dhtNodes.splice(index+1, 0, node);
        this.dhtDataSource[node.hash] = [];
        node.store.forEach((store: Store) => {
          for (const [key, value] of Object.entries(store)) {
            this.dhtDataSource[node.hash].push({
              'hash': key,
              'value': value
            });
          }
        });
      });
    });
  }
}

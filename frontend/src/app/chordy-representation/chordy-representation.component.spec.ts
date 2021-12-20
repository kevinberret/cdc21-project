import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ChordyRepresentationComponent } from './chordy-representation.component';

describe('ChordyRepresentationComponent', () => {
  let component: ChordyRepresentationComponent;
  let fixture: ComponentFixture<ChordyRepresentationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ChordyRepresentationComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ChordyRepresentationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

import { Store } from "./store";

export interface Node{
    hash: string,
    name: string,
    store: Array<Store>,
    successor: string,
    predecessor: string,
    next: string
}
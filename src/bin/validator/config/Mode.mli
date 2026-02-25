type property = {
  property : string;
  negated : bool;
}

type t = 
| SampleGenerator of {pdf:bool;samples:int} 
| Reduce of {configs:string array list; model_path:string}
| Compare of {configs:string array list}
| PropertyGuided of property list
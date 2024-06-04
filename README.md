```sh  
           __   _ __       
 ______ __/ /  (_) /_____ _
/ __/ // / _ \/ /  '_/ _ `/
\__/\_,_/_.__/_/_/\_\\_,_/ 
                           
```
# Examples

### Pattern matching
```haskell
let foo (a : Type) : Bool =
    match a with
    | 2 -> True
    | _ -> False 
in foo 2 // returns True.
```
### Dependent identity function
```haskell
let id (A : Type) (x : A) : A = x;
let Bool : Type;
False : Bool;
id Bool False; // evaluates to False.
```
### Binary operations
```haskell
let sum (a : Nat) (b : Nat) : Nat =
    + a b
in sum 3 5; // evaluates to 8.
```

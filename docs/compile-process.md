# Compile Process

NominalScript's transpiler/compiler has 3 passes:
- **Declaration pass:** Resolve all imports and declarations, then generate an export map. Importantly, this allows hoisted declarations to be forward referenced; and the generated export map is enough to compile dependencies, so a file that is only transitively compiled may only go through this pass
- **Main pass:** traverse the entire program and check types based on those assigned via declaration. Typechecking is done be assigning expressions "assigned" (in), "required" (in), and "runtime-required" (also in but looser) types:  e.g. 2 is assigned Natural; f(a, b) looks up f's assigned type, (assuming it's a function) requires a and b to be the parameter types, and assigns the expression the return type; and runtime-required types are assigned to wrapped expressions, and unlike required only ensure they **aren't unassignable** to the wrapper (and inserts a guard if not necessarily assignable).

WIP
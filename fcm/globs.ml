module type DependencySIG = module type of Dep.Dependency

let dependencyMod = ref (module Dep.Dependency : DependencySIG)

let dependencyMod2 = (module Dep.Dependency : DependencySIG)

let use_impl_ref = ref true
let use_impl = true

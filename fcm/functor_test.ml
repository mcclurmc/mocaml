module Make = functor (M: Globs.DependencySIG) ->
struct
	include Impl.SUT
end

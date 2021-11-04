module BS

    export ⇒, Ξ, ♯, ⊗, ⊷, ⊶
    export id, ls, lspath, bin, num, permutations, divisibles
    export -, ^, /, %, push!, pop!, queue!, dequeue!
    export dev, app
    export @mk, @ls

    import Base: (-), (^), (/), (%)

    #-------------------------------------------------------------------

    ⇒(x,y) = Base.push!(y,x)
    const Ξ = Base.println
    const ♯ = Base.typeof
    const ⊗ = Base.kron
    const ⊷ = Base.Fix1
    const ⊶ = Base.Fix2

    #-------------------------------------------------------------------

    const usr = ENV["USERNAME"]
    const dev = "C:/Users/$usr/.julia/dev/"

    #-------------------------------------------------------------------

    id(x) = x
    const ls = readdir
    const lspath = readdir(join=true)

    #-------------------------------------------------------------------

    macro mk(path)
        mkdir(path)
    end

    macro ls()
        eval(:lspath)
    end

    #-------------------------------------------------------------------

    bin(x::Int, type=Int64)       = bitstring(type(x))
    num(bitstr::String, type=Int) = parse(type, bitstr, base=2)

    function divisibles(n)
        x = filter(!=(false),[(n%i==0)&&i for i ∈ n÷2:-1:2])
        reverse(x) .=> x
    end

    function permutations(total, selected)::BigInt
        factorial(big(total)) ÷ (factorial(big(selected))*(factorial(big(total)-selected)))
    end

    #-------------------------------------------------------------------
    # Function abstaction

    (-)(ƒ::Function)         = (x = /(ƒ); pop!(x); ∘(x...))
    (-)(ƒ::Function, n::Int) = (x = /(ƒ); popat!(x, n); ∘(x...))
    (^)(ƒ::Function, n::Int) = ∘(repeat([ƒ], n)...)
    (/)(ƒ::Function)         = Function[ƒ]
    (/)(ƒ::ComposedFunction) = splat(ƒ) |> reverse
    (%)(ƒ::Function)         = ƒ.outer

    Base.getindex(ƒ::Function, n) = /(ƒ)[n]
    Base.lastindex(ƒ::Function)   = ⇔(/(ƒ))
    Base.firstindex(ƒ::Function)  = 1

    function Base.getindex(ƒ::Function, p::Pair{Int,<:Function})
        ω = /(ƒ)
        ω[p.first] = p.second
        ∘(ω...)
    end
    function Base.getindex(ƒ::Function, p::Pair{<:AbstractRange,<:Any})
        ω = /(ƒ)
        ω[p.first] .= p.second
        ∘(ω...)
    end

    function splat(ƒ::Function) return ƒ end
    function splat(ƒ::ComposedFunction)
        v = Vector{Function}()
        while ƒ isa ComposedFunction
             splat(ƒ.inner) .|> ⊷(push!,v); ƒ = ƒ.outer
            ƒ isa ComposedFunction||push!(v, ƒ)
        end
        v
    end
    # ----------------------------------------------------------------
    # currying

    """
        (⇀)(x, ƒ) = (args...) -> ƒ(x, args...)
        
    ```
    julia> ƒ(a,b,c) = a + b + c
    julia> (2 ⇀ ƒ)(3, 5)
    10
    julia> (2 ⇀ 3 ⇀ ƒ)(5)
    10
    julia> (2 ⇀ 3 ⇀ 5 ⇀ ƒ)()
    10
    ```
    """
    (⇀)(x, ƒ) = (y...) -> ƒ(x, y...)
    

    struct Curried end
    """
    ```
    julia> @⇀ ƒ(a,b,c) = a + b + c
    julia> ƒ(2)(3)(5)
    10
    julia> ƒ(2, 3)(4)
    10
    ```
    """
    macro (⇀)(fdef)
        f = fdef.args[1].args[1]
        fargs = fdef.args[1].args[2:end]
        arity = length(fargs)
        body = fdef.args[2]
        err_str = "Too many arguments. Function $f only takes $arity arguments"
        quote 
            begin 
            function $f(args...)
                if length(args) < $arity
                    x -> $f((args..., x)...)
                elseif length(args) == 3
                    $f(Curried(), args...)
                else
                    throw($err_str)
                end
            end
            $f(::Curried, $(fargs...)) = $body
            end
        end |> esc
    end

    #-------------------------------------------------------------------
    # Type abstaction

    (+)(t::Type)           = supertype(t)
    (-)(T::Type)           = Vector{T}()
    (-)(T::Type, n::Int)     = Vector{T}(undef, n)
    (-)(n::Int, T::Type)     = Vector{T}(undef, n)
    (-)(v::Tuple, T::Type) = Array{T}(undef,v...)
    (-)(T::Type, v::Tuple) = Array{T}(undef,v...)

    #-------------------------------------------------------------------
    # Datastructs

    Base.push!(n::Int) = n += 1
    Base.pop!(n::Int)  = n > 1 ? n-=1 : error("Nothing to pop!")

    queue!(data::T) where T = Vector{T}(data)
    queue!(v::Vector{T}, data::T) where T = push!(v, data)
    dequeue!(v::Vector{T}) where T = length(v) ≥ 1 && popat!(v, 1)
    
    #-------------------------------------------------------------------
end

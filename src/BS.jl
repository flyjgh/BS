module BS

    export ⇒, Ξ, ♯, ⊗, ⊷, ⊶, ⇀, @⇀
    export ls, lspath, bin, num, permutations, divisibles
    export +, -, *, /, %, push!, pop!, queue!, dequeue!
    export usr, dev
    export @mk, @ls

    import Base: (+), (-), (*), (/), (%)

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

    (-)(ƒ::Function, n::Int) = (x = /(ƒ); popat!(x, n); ∘(x...))
    (*)(ƒ::Function, n::Int) = ∘(repeat([ƒ], n)...)
    (/)(ƒ::Function)         = Function[ƒ]
    (/)(ƒ::ComposedFunction) = splat(ƒ) |> reverse
    (%)(ƒ::Function)         = ƒ.outer

    Base.getindex(ƒ::Function, n) = /(ƒ)[n]
    Base.lastindex(ƒ::Function)   = ⇔(/(ƒ))
    Base.firstindex(ƒ::Function)  = 1

    function Base.setindex(ƒ::Function, i, f::Function)
        ω = /(ƒ)
        ω[i] = f
        ∘(ω...)
    end

    function splat(ƒ::Function) return ƒ end
    function splat(ƒ::ComposedFunction)
        v = Vector{Function}()
        while ƒ isa ComposedFunction
             splat(ƒ.inner) .|> ⊷(push!,v); ƒ = ƒ.outer
            ƒ isa ComposedFunction || push!(v, ƒ)
        end
        v
    end

    # ----------------------------------------------------------------
    # currying
    
    """
        (⤏)(ƒ, x...) = (args...) -> ƒ(x..., args...)
        
    ```
    julia> g(x,y,z) = 6x^2 + 4y + z
    julia> ⤏(g,2)(3,4)
    40
    julia> ⤏(g,2,3)(4)
    40
    julia> ⤏(g,2,3,4)()
    40
    """
    (⇀)(ƒ, x...) = (y...) -> ƒ(x..., y...)
    (⇀)(ƒ, x::Union{AbstractArray,AbstractRange,Tuple}) = (args...) -> ƒ(x, args...)

    struct Curried end
    """
    only works for functions with more than 2 args.

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
        quote 
            begin 
            function $f(args...)
                if length(args) < $arity
                    x -> $f((args..., x)...)
                elseif length(args) == 3
                    $f(BS.Curried(), args...)
                else
                    throw("shit, Damn")
                end
            end
            $f(::BS.Curried, $(fargs...)) = $body
            end
        end |> esc
    end

    #-------------------------------------------------------------------
    # Type abstaction

    (+)(t::Type) = supertype(t)
    
    #-------------------------------------------------------------------
    # Static Size Arrays

    (-)(T::Type)           = Vector{T}()
    (-)(T::Type, n::Int)   = Vector{T}(undef, n)
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

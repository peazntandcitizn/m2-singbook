-- -*- coding: utf-8 -*-
newPackage(
	"RadoslavTest",
	Version => "0.1",
	Date => "January 2012",
	Authors => {
		{Name => "Radoslav Zlatev",
		 Email => "radoslav@math.cornell.edu"}},
	Headline => "Radoslav's M2 Examples for the Singular Book",
	DebuggingMode => true
	)

beginDocumentation()
doc ///
	Key
		"RadoslavTest"
	Headline
		Radoslav's M2 Examples for the Singular Book
	Description
		Text
			The package implements the code examples from the Singular book @TO "[GP]"@ into Macaulay2.
			
			@TO "Singular Examples in M2"@.
	///
doc ///
	Key
		"[GP]"
	Headline
		References
	Description
		Text
			[GP] Gert-Martin Greuel and Gerhard Pfister. {\it A Singular Introduction to Commutative Algebra.} Cambridge, 2nd ed., 2008
	///
doc ///
	Key
		"Singular Examples in M2"
	Description
		Code
			Subnodes => {
				"Chapter 1",
				TO "Example 1.1.8",
				TO "Example 1.1.9",
				TO "Example 1.1.10",
				TO "Example 1.2.3",
				TO "Example 1.2.13",
				TO "Example 1.3.3",
				TO "Example 1.3.13",
				TO "Example 1.3.15",
				TO "Example 1.4.9",
				TO "Example 1.5.10",
				TO "Example 1.6.13",
				TO "Example 1.7.10",
				TO "Example 1.7.12",
				TO "Example 1.8.1",
				TO "Example 1.8.2",
				TO "Example 1.8.4",
				TO "Example 1.8.6",
				TO "Example 1.8.7",
				TO "Example 1.8.9",
				TO "Example 1.8.11",
				TO "Example 1.8.13",
				TO "Example 1.8.15",
				TO "Example 1.8.18",
				TO "Example 1.8.19",
				TO "Example 1.8.20",
				"Chapter 2",
				TO "Example 2.1.6",
				TO "Example 2.1.7",
				TO "Example 2.1.10",
				TO "Example 2.1.13",
				TO "Example 2.1.20",
				TO "Example 2.2.15",
				TO "Example 2.3.10",
				TO "Example 2.3.12",
				TO "Example 2.4.12",
				TO "Example 2.4.15",
				TO "Example 2.5.5",
				TO "Example 2.5.18",
				TO "Example 2.7.5",
				TO "Example 2.7.9",
				TO "Example 2.7.14",
				TO "Example 2.8.1",
				TO "Example 2.8.3",
				TO "Example 2.8.5",
				TO "Example 2.8.6",
				TO "Example 2.8.7",
				TO "Example 2.8.8",
				TO "Example 2.8.9",
				TO "Example 2.8.10",
				"Chapter 3",
				TO "Example 3.1.4",
				TO "Example 3.1.6",
				TO "Example 3.2.3",
				TO "Example 3.3.8",
				TO "Example 3.3.13",
				TO "Example 3.4.6",
				TO "Example 3.5.5",
				TO "Example 3.5.9",
				TO "Example 3.5.15",
				TO "Example 3.6.10",
				TO "Example 3.6.13",
				"Chapter 4",
				TO "Example 4.2.6",
				TO "Example 4.2.8",
				TO "Example 4.3.3",
				TO "Example 4.3.5",
				TO "Example 4.4.4",
				TO "Example 4.4.10",
				TO "Example 4.5.4",
				TO "Example 4.6.26",
				TO "Example 4.7.7",
				"Chapter 5",
				TO "Example 5.2.5",
				TO "Example 5.3.12",
				TO "Example 5.5.13",
				TO "Example 5.6.5",
				TO "Example 5.6.15",
				TO "Example 5.7.9",
				"Chapter 6",
				TO "Example 6.1.3",
				TO "Example 6.2.4",
				TO "Example 6.2.10",
				TO "Example 6.2.15",
				"Chapter 7",
				TO "Example 7.1.5",
				TO "Example 7.2.6",
				TO "Example 7.2.10",
				TO "Example 7.3.12",
				TO "Example 7.3.14",
				TO "Example 7.3.17",
				TO "Example 7.5.7",
				TO "Example 7.6.3",
				TO "Example 7.7.8"}
	///
doc ///
		Key
			"Example 1.1.8"
		Headline
			Computation in fields
		Description
			Code
				SUBSECTION "Computation in the field of rational numbers"
			Text
				In M2 the rational numbers is the default ring to do arithmetic in, and it has arbitrary precision.
				
			Example
				A1 = ZZ/32003
				(123456789_A1)^5
				123456789^5 * 1_A1
			Code
				SUBSECTION "Computation in finite fields"
			Text
				
				For modular arithmetic we have to set our ring to {\tt ZZ/p} first, and then regard integers as elements
				of the finite field by putting it as an index, or equivalently by multiplying by the multiplicative unit.
				
			Example
				A2 = GF(2,3,Variable=>a)
				n = a+a^2;
				n^5
				(ideal (ambient A2))_0
			Code
				SUBSECTION "Computation in the fields of real and complex numbers"
			Text
				As with the rational numbers, computations with real and complex numbers are done in the default ring.
				To cast a rational number as real, just add a decimal period. The default precision is of 53 digits, but it can
				be easily changed at definition by appending @"pN"@ for any number N, or later by @"numeric_N"@.
				
			Example
				n = 123456789.0
				n^5
				
				123456789 * 1_RR
				
				m = 1/123456789.0p100;
				numeric_100 m
				
				n = 123456789.0 + 0.0001*ii
			Code
				SUBSECTION "Computation in fraction fields"
			Example
				R3 = frac (QQ[a,b,c])
				n = 12345*a+12345/(78*b*c)
				n^2
				n/9*c
		///
doc ///
		Key
			"Example 1.1.9"
		Headline
			Computation in polynomial rings
		Description
			Text
				Polynomial rings definition and usage is as in standard algebra notation.
				
			Example
				A = QQ[x,y,z];
				f = x^3+y^3+z^3;
				f^2-f
			Text
				
				M2 does not understand short input, e.g. @"2x2+y3"@. Also, M2 does not have a "current ring" 
				(it does have a default ring to do arithmetic in, though). If there is an ambiguity to which ring a variable
				should live in, that is the lastly defined one. To consider a variable as an element of previously defined ring,
				use the @"use"@ keyword.
				
			Example
				B = ZZ/2[x];
				x
				use A
				x
		///
doc ///
		Key
			"Example 1.1.10"
		Headline
			Methods for creating polynomial maps
		Description
			Code
				SUBSECTION "General definition"
			Text
				Polynomial rings maps are defined by specifying the codomain, the domain, and a list of the images of the generators
				of the domain in the codomain, in that order.
				
			Example
				A = QQ[a,b,c];
				f = a+b+b*c+c^3;
				B = QQ[x,y,z];
				F = map(B,A,{x+y,x-y,z});
				F f				
			Code
				SUBSECTION "Special maps"
			Text
				To rename variables, we just map to the list of the generators of the codomain.
			Example
				G = map(B,A,gens B);
				G f
			Text
				
				To "inlude" into rings, or "project", we have to extract the list of generators for the codomain in the desired order.
				
			Example
				A1 = QQ[x,y,c,b,a,z];
				H = map(A1,A,take (gens A1, numgens A))
				H f
		///
doc ///
		Key
			"Example 1.2.3"
		Headline
			Leading data
		Description
			Example
				A = QQ[x,y,z,MonomialOrder=>Lex];
				f = y^4*z^3+2*x^2*y^2*z^2+3*x^5+4*z^4+5*y^2
				leadMonomial f
				exponents leadMonomial f
				leadTerm f
				leadCoefficient f
				f-leadTerm f
			Text
				
				To get just specific terms, with respect to the monomial orderings, use @"someTerms"@.
				
			Example
				someTerms (f,1,size f-1)
		///
doc ///
		Key
			"Example 1.2.13"
		Headline
			Monomial orderings
		Description
			Code
				SUBSECTION "Global oderings"
			Text
				The default monomial order in M2 is graded reverse lexicographical, @"GRevLex"@. To specify another use
				@"MonomialOrder=>"@ in the definition of the polynomial ring.
				
				The orderings below are lexicographical, graded reverse lexicographical, graded lexicographical, and
				graded lexicographical with specific weights, respectively.
				
			Example
				A1 = QQ[x,y,z, MonomialOrder=>Lex];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4

				A2 = QQ[x,y,z, MonomialOrder=>GRevLex];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4

				A3 = QQ[x,y,z, MonomialOrder=>GLex];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4

				A4 = QQ[x,y,z, MonomialOrder=>{Weights=>{5,3,2},Lex}];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4
			Text
				
				{\bf Remark}: The order of specifying weights and order is important. The weights must always be set first.
				A non-example is
				
			Example
				A4 = QQ[x,y,z, MonomialOrder=>{Lex,Weights=>{5,3,2}}];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4
			Code
				SUBSECTION "Local orderings"
			Text
				For local orderings we have to switch off the global paramenter by @"Global=>false"@.
				
			Example
				A5 = QQ[x,y,z, MonomialOrder=>{Weights=>{-1,0,0},Weights=>{0,-1,0},Weights=>{0,0,-1}}, Global=>false];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4

				A6 = QQ[x,y,z, MonomialOrder=>{Weights=>{-1,-1,-1},Lex}, Global=>false];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4

				A7 = QQ[x,y,z, MonomialOrder=>{Weights=>{-5,-3,-2},Lex}, Global=>false];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4
			Code
				SUBSECTION "Product and matrix orderings"
			Example
				A5 = QQ[x,y,z, MonomialOrder=>{Weights=>{-5,0,0},Weights=>{0,-3,0},Weights=>{0,0,-2},GLex}, Global=>false];
				f = x^3*y*z+x^3+x*y^2+y^5+z^4
		///
doc ///
		Key
			"Example 1.3.3"
		Headline
			Properties of ring maps
		Description
			Code
				SUBSECTION "Checking injectivity"
			Example
				S = QQ[a,b,c,MonomialOrder=>Lex];
				R = QQ[x,y,z];
				i = ideal (x, y, x^2-y^3);
				gens i
				phi = map(R,S,gens i)
				isInjective phi
				j = ideal (x,y,z-x^2+y^3);
				psi = map(R,S,gens j);
				isInjective psi
				ker phi
				ideal (0_S) == ker psi
			Code
				SUBSECTION "Computing the preimage"
			Example
				preimage (phi,ideal (0_R))
			Code
				SUBSECTION "Checking surjectivity"
			Text
				The {\tt isSurjective} and {\tt isIsomorphism} functions are documented but not present in version 1.4, and
				regrettably there seems to be no easy workaround.
		///
doc ///
		Key
			"Example 1.3.13"
		Headline
			Computation is quotient rings
		Description
			Code
				SUBSECTION "Define a quotient ring"
			Text
				This is easy and follows the standard notation.
				
			Example
				R = ZZ/32003[x,y,z];
				Q = R/(x^2+y^2-z^5,z-x-y^2);
				describe Q
			Text
				
				To get (much) more information about a (quotient) ring, use @TO "peek"@.
				
			Example
				peek Q;
			Code
				SUBSECTION "Equality test in quotient rings"
			Example
				f = z^2+y^2;
				g = z^2+2*x-2*z-3*z^5+3*x^2+6*y^2;
				f == g
			Code
				SUBSECTION "Zerodivisor test in quotient rings"
			Text
				Suffices to check that the annihilator ideal of an element is the zero ideal. Happily {\tt ann} is implemented in M2.
				
			Example
				ann f == ideal (0_Q)
			Code
				SUBSECTION "Computing inverses"
			Text
				This is more subtle. We can easily check if something is the inverse.
				
			Example
				R = QQ[x,y,z];
				I = ideal (-z^5+y^2+x^2, -y^2+z-x);
				Q = R/I
				R = QQ[x,y,z];
				I = ideal (-z^5+y^2+x^2, -y^2+z-x);
				Q = R/I
			Text
				
				Currently more interesting operations in fraction fields are only defined over polynomial rings {\tt ZZ}, {\tt QQ} or finite fields,
				that is, not over $K(x)$.
		SeeAlso
			frac
			ann
		///
doc ///
		Key
			"Example 1.3.15"
		Headline
			Computing with radicals
		Description
			Code
				SUBSECTION "Compute the radical of an ideal"
			Example
				R = QQ[x,y,z];
				p = z^4+2*z^2+1;
				radical ideal (p);
				I = ideal (x*y*z, x^2, y^4+y^5);
				trim radical I
			Code
				SUBSECTION "Compute the index of nilpotency"
			Example
				k=0;
				while (y^2+y)^k % I != 0 do k=k+1;
				k
		SeeAlso
			radical
			trim
		///
doc ///
		Key
			"Example 1.4.9"
		Headline
			Global versus local rings
		Description
			Code
				SUBSECTION "Compute the dimension of $V(I)$"
			Example
				S = QQ[x,y,z];
				I = ideal (y*(x-1), z*(x-1));
				dim I
			Text
				
				Now we check if $y$ is in $I$.
				
			Example
				y % I == 0
			Code
				SUBSECTION "Compute the dimension of $V(I)$ at $(0,0,0)$"
			Example
				R = QQ[x,y,z, MonomialOrder=>{Weights=>{(-1),(-1),(-1)},RevLex},Global=>false];
				J = substitute (I,R);
				dim J
			Text
				
				and
				
			Example
				y % J == 0
			Code
				SUBSECTION "Compute the dimension of $V(I)$ at $(1,0,0)$"
			Text
				We translate by $x$ goes to $x+1$.
				
			Example
				J = substitute (J, {x=>x+1})
				dim J
			Code
				SUBSECTION "Compute the global dimension of $V(I)$ after translation"
			Example
				use S;
				I1 = substitute (I, {x=>x+1});
				dim I1
		SeeAlso
			dim
			substitute
		///
doc ///
		Key
			"Example 1.5.10"
		Headline
			Realization of rings
		Description
			Example
				(n,m) = (2,3);
				A1 = QQ[x_1..x_n,y_1..y_m, MonomialOrder=>{GRevLex=>n, RevLex=>m}, Global=>false];
				f = x_1*x_2^2+1+y_1^10+x_1*y_2^5+y_3
				1_A1 > y_1^10
				A2 = QQ[x_1..x_n,y_1..y_m, MonomialOrder=>{RevLex=>n, GRevLex=>m}, Global=>false];
				substitute (f,A2)
				x_1*y_2^5 < 1_A2
				A3 = QQ[x_1..x_n,y_1..y_m, MonomialOrder=>{GRevLex=>n, RevLex=>2, GRevLex=>m-2}, Global=>false];
				substitute (f,A3)
		SeeAlso
			substitute
		///
doc ///
		Key
			"Example 1.6.13"
		Headline
			Normal form
		Description
			Text
				Normal forms in Macaulay2 are done using the remainder operator @"%"@.
				
			Example
				A = QQ[x,y,z];
				f = x^2*y*z+x*y^2*z+y^2*z+z^3+x*y;
				f1 = x*y+y^2-1;
				f2 = x*y;
				G = ideal (f1,f2);
			Text
				
				Macaulay2 computes a Groebner basis of G, and uses that to find the normal form of f. In Macaulay2, all
				remainders are reduced normal forms (at least for non-local orders).
				
			Example
				f % G
			Text
				
				In order to reduce using a nonbasis, use @TO "forceGB"@.
				
			Example
				f % (forceGB gens G)
			Text
				
				This produces different answer from in the book because the choice of divisor affects the result.
				
			Example
				f % (forceGB matrix {{f2, f1}})
		SeeAlso
			"Symbol '%'"
			forceGB
		///
doc ///
		Key
			"Example 1.7.10"
		Headline
			Standard basis
		Description
			Text
				The same generators of an ideal give us different standard bases with respect to different orderings.
				
				We first have the default order, the graded (degree) reverse lexicographic.
				
			Example
				A = QQ[x,y];
				I = ideal (x^10+x^9*y^2, y^8-x^2*y^7);
				transpose gens gb I
			Text
				
				The lexicographic order.
				
			Example
				A1 = QQ[x,y, MonomialOrder=>Lex];
				J = substitute(I,A1);
				transpose gens gb J
			Text
				
				Now to local orderings. Local graded reverse lexicographic, the "default" local order.
				
			Example
				B = QQ[x,y, MonomialOrder=>{Weights=>{(-1),(-1)},2}, Global=>false]
				J = substitute (I, B);
				transpose gens gb J
			Text
				
				Another local order: negative lexicographic.
				
			Example
				B1 = QQ[x,y, MonomialOrder=>{Weights=>{(-1),0},Weights=>{0,(-1)}}, Global=>false]
				J = substitute (I, B1);
				transpose gens gb J
			Text
				
				One method to compute a standard basis is via homogenization. The example below does this, obtaining a standard
				basis which is not minimal.
				
			Example
				M = matrix{{1,1,1},{0,-1,-1},{0,0,-1}}
				mo = apply(entries M, e -> Weights => e)
				C = QQ[t,x,y,MonomialOrder=>mo];
				I = homogenize(substitute(I,C),t)
				transpose gens gb I
				substitute(transpose gens gb I, {t=>1})
			Text
				
				The first two elements form a standard basis.
		SeeAlso
			homogenize
			substitute
			apply
			entries
		///
doc ///
		Key
			"Example 1.7.12"
		Headline
			Highest corner
		Description
			Text
				Not on M2 yet.
		///
doc ///
		Key
			"Example 1.8.1"
		Headline
			Ideal membership
		Description
			Code
				SUBSECTION "Inclusion of a polynomial in an ideal"
			Text	
				M2 computes Groebner bases once when these needed, or when you ask it to do so. Reduction modulo an ideal
				is performed with respect to a Groebner basis, where this is how the ideal was defined or not.
				
			Example
				A = QQ[x,y];
				J = ideal "x10+x9y2,y8-x2y7";
				f = x^2*y^7+y^14;
				f % J
				f = x*y^13+y^12;
				f % J
			Text
			
				This gives that $f$ is not in $J$ in the first case, and $f$ is in $J$ in the second. The code below checks if $K\subset J$.
				
			Example
				K = ideal "f,x2y7+y14";
				(gens K) % J
				K = ideal "f,y14+xy12";
				(gens K) % J
			Text
				
				As before, the answer is 'no' in the first case, and 'yes' in the second. There are the following shortcuts.
				
			Example
				isSubset (K,J)
		SeeAlso
			isSubset
		///
doc ///
		Key
			"Example 1.8.2"
		Headline
			Linear combination of ideal members
		Description
			Text
				We can do this directly.
				
			Example
				A = QQ[x,y];
				I = ideal "x10+x9y2,y8-x2y7";
				f = poly "xy13+y12";
				M = f//(gens I)
			Text
				
				and we test if the respesentation is the correct one
				
			Example
				f - M_(0,0)*I_0 - M_(1,0)*I_1
			Text
				
				The situation in local rings in different.
				
			Example
		SeeAlso
			poly
			gens
		///
doc ///
		Key
			"Example 1.8.4"
		Headline
			Elimination of variables
		Description
			Example
				A = QQ[t,x,y,z];
				I = ideal "t2+x2+y2+z2,t2+2x2-xy-z2,t+y3-z3";
				transpose gens eliminate (I, {t})
			Text
				
				Alternatively, we can choose a product ordering.
				
			Example
				A1 = QQ[t,x,y,z, MonomialOrder=>{1,3}];
				I = substitute (I, A1);
				transpose gens gb I
			Text
				
				Finally, we can also choose the {\it extra weight} vector $(1,0,0,0)$ to obtain an elimination ordering.
				
			Example
				A2 = QQ[t,x,y,z, MonomialOrder=>{Weights=>{1,0,0,0}}];
				I = substitute (I, A2);
				transpose gens gb I
			Text
				
				and one more definition for the ring above
				
			Example
				A2 = QQ[t,x,y,z, MonomialOrder=>{Eliminate 1}];
				I = substitute (I, A2);
				transpose gens gb I
		SeeAlso
			eliminate
		///
doc ///
		Key
			"Example 1.8.6"
		Headline
			Zariski closure of the image
		Description
			Text
				We compute an implicit equation for the surface defined parametrically by the map
				$f:\mathbb{A}^2\to\mathbb{A}^3$ sending $(u,v)$ to $(uv,uv^2,z-u^2)$
				
			Example
				A = QQ[u,v,x,y,z];
				I = ideal "x-uv,y-uv2,z-u2";
				J = eliminate (I, {u,v})
			Text
				
				This ideal defines the closure of the map $f$, the Whitney umbrella.
				
				Alternatively, we could take the coimage of the ring homomorphism $g$ corresponding to $f$.
				
			Example
				g = map(QQ[u,v],QQ[x,y,z],{x => u*v, y => u*v^2, z => u^2})
				coimage g
		SeeAlso
			eliminate
			coimage
		///
doc ///
		Key
			"Example 1.8.7"
		Headline
			Solving equations
		Description
			Text
				must have
		///
doc ///
		Key
			"Example 1.8.9"
		Headline
			Radical membership
		Description
			Text
				We can do this directly
				
			Example
				A = QQ[x,y,z];
				I = ideal "x5,xy3,y7,z3+xyz";
				f = x+y+z;
				radical I
			Text
				
				or by using another variable
				
			Example
				B = QQ[t,x,y,z];
				I = sub(I, B);
				f = sub(f, B);
				I = I + ideal (1-t*f)
				transpose gens gb I
			Text
				
				where the last line tell us that $f$ is in the radical.
		SeeAlso
			radical
			substitute
		///
doc ///
		Key
			"Example 1.8.11"
		Headline
			Intersection of ideals
		Description
			Text
				We can do intersect directly
				
			Example
				A = QQ[x,y,z];
				I1 = ideal "x,y";
				I2 = ideal "y2,z";
				transpose gens intersect (I1, I2)
			Text
				
				or by elimination
				
			Example
				B = QQ[t,x,y,z];
				I1 = sub (I1, B);
				I2 = sub (I2, B);
				J = t*I1+(1-t)*I2;
				transpose gens eliminate (J, t)
		SeeAlso
			intersect
			eliminate
		///
doc ///
		Key
			"Example 1.8.13"
		Headline
			Quotient of ideals
		Description
			Text
				We can use the buit-in function @TO "quotient"@.
				
			Example
				A = QQ[x,y,z];
				I1 = ideal "x,y";
				I2 = ideal "y2,z";
				quotient (I1, I2)
				I1 : I2
			Text
				
				or do ourselves
				
			Example
				J1 = intersect(I1, ideal (I2_0))
			Text	
				
				which means that $I_1:g_{2,1}=(1)$ where $g_{1,2}$ is the first generator of $I_2$.
				
			Example
				J2 = intersect(I1, ideal (I2_1))
			Text
				
				which means that $I_1:g_{2,2}=(x,y)$. Altogether we obtain $I_1:I_2=(x,y)$.
				
			Example
				K1 = sub(ideal (J1_0 / I2_0), A);
				K2 = sub(ideal (J2_0/I2_1, J2_1/I2_1), A);
				intersect (K1, K2)
		SeeAlso
			quotient
			intersect
			substitute
		///
doc ///
		Key
			"Example 1.8.15"
		Headline
			Saturation
		Description
			Text
				We can use the built-in function @TO "saturate"@. It does not give us the saturation exponent however.
				
			Example
				A = QQ[x,y,z];
				I1 = ideal "x5z3,xyz,yz4";
				I2 = ideal "z";
				transpose gens saturate (I1, I2)
			Text
				
				Following Section 1.8.9 in @TO "[GP]"@, it is not hard to do it from scratch.
				
			Example
				J = quotient (I1, I2);
				k = 0;
				while (J != I1) do (
					 k = k+1;
					 I1 = J;
					 J = quotient (I1, I2);
					 );
				J
				k
		SeeAlso
			saturate
			quotient
		///
doc ///
		Key
			"Example 1.8.18"
		Headline
			Kernel of a ring map
		Description
			Text
				M2 has a ready @"kernel"@ function.
				
			Example
				A = QQ[x,y,z];
				B = QQ[a,b];
				phi = map (B,A,{a^2,a*b,b^2});
				ker phi
			Text
			
				Alternatively, and following the Singlular example, we can use elimination.
				
			Example
				C = QQ[x,y,z,a,b];
				H = ideal "x-a2,y-ab,z-b2";
				eliminate (H, {a,b})
		SeeAlso
			kernel
			eliminate
		///
doc ///
		Key
			"Example 1.8.19"
		Headline
			Algebraic dependence
		Description
			Text
				Algebraic dependence, just as subalgebra membership in @TO "Example 1.8.20"@, can quickly be computed using the tools we have. Our
				examples implements the discussion of Section 1.8.11 in @TO "[GP]"@. We abstract the code in a function {\tt algDependence} which
				when fed with the the ideal generated by the list of elements in question, returns the ideal with the respective algebraic relations.
				
			Example
				A = QQ[x,y];
				f = x^4-y^4;
				f1 = x^2+y^2;
				f2 = x^2-y^2;

				I = ideal (f,f1,f2);

				algDependence = (I) -> (
					 A := ring I;
					 L := flatten entries gens I;
					 B := QQ[t_1..t_(#L)];
					 psi := map(A,B,L);
					 return ker psi;
					 );

				algDependence (ideal (f,f1,f2))
		SeeAlso
			flatten
			entries
		///
doc ///
		Key
			"Example 1.8.20"
		Headline
			Subalgebra membership
		Description
			Text
				This example implements the discussion of Section 1.8.11 in @TO "[GP]"@. We abstract the code in a function {\tt inSubalgebra} which
				takes as an input an element $f$ and a list of elements $L$ from some common ring $R$, and checks if $f$ is in the subalgebra of $R$
				generated by $L$.
				
			Example
				A = QQ[x,y];
				f = x^4-y^4;
				f1 = x^2+y^2;
				f2 = x^2-y^2;
				L = {f1, f2};

				inSubalgebra = (f, L) -> (
					 B := QQ[gens ring f,t_0,t_1..t_(#L), MonomialOrder=>{numgens ring f,1,#L}];
					 K := flatten entries gens substitute (ideal L, B);
					 I := ideal apply (1..#L, i->t_i - K_(i-1));
					 J := ideal (t_0-substitute(f,B)) + I;
					 G := flatten entries gens gb J;
					 l := apply (G, leadTerm);
					 return any (l, i->i==t_0);
					 );

				inSubalgebra (f, L)
		SeeAlso
			flatten
			entries
			apply
			substitute
		///
doc ///
		Key
			"Example 2.1.6"
		Headline
			Matrix operations
		Description
			Text
				Macaulay2's sintax for matrix manipulations resembles the mathematical notaion.
				
			Example
				A = QQ[x,y,z];
				M = matrix "1, x+y, z2;
							x, 0, xyz"
				N = matrix "1,2,3;4,5,6;7,8,9" ** A

				print M
				print N

				2*M
				x*N
				M*N

				M_(1,2)

				MM = mutableMatrix M
				MM_(1,2)=37;
				MM

				N^3
				((x+y+z)*N)^3

				ideal M
				id_(A^5)
		SeeAlso
			matrix
			symbol|
		///
doc ///
		Key
			"Example 2.1.7"
		Headline
			Maps induced by Hom
		Description
			Text
				Figuring out what the construction actually represents, there is a shorter way to do this in Macaulay2.
				
			Example
				kontraHom = (A,s) -> (
					 AT := transpose A;
					 ret := AT;
					 scan(1..(s-1),i->ret=ret++AT);
					 return ret;
					 );

				M = matrix "1,2,3;4,5,6;7,8,9";
				kontraHom(M, 2)
			Text
				
				and
				
			Example
				kohom = (A, s) -> (
					 L := flatten entries f;
					 LL := {};
					 scan(L,i->LL=LL|entries (i * id_(ZZ^s)));
					 LS := for i from 0 to numrows f-1 list (
						  for k from 0 to s-1 list (
							   for j from 0 to numcols f-1 list LL#(i*s*(numcols f)+j*s+k)
						   )
						  );
					 return matrix flatten for a in LS list for b in a list flatten b;
					 );

				A = QQ[x,y,z];
				f = matrix "1,2,3;4,5,6;7,8,9";

				kohom (f, 3)
		SeeAlso
			scan
			flatten
			entries
		///
doc ///
		Key
			"Example 2.1.10"
		Headline
			Submodules of $A^n$
		Description
			Text
				The easiest way to manipulate modules in Macaulay2 is using matrices.
				
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				M = matrix "x,xy,z;x2,xyz,yz"
				
				Ker = ker M
				Ker == image syz M
				
				image M
				Im = A*M_0+A*M_1+A*M_2
		SeeAlso
			ker
			image
			coker
			syz
		///
doc ///
		Key
			"Example 2.1.13"
		Headline
			Kernel and image of a module homomorphism
		Description
			Text
				Macaulay2 has built-in functions. We present them and some more ways to do this.
			
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				M = matrix "x,xy,z;x2,xyz,yz"

				Ker = ker M
				Ker == image syz M

				image M
				Im = A*M_0+A*M_1+A*M_2
		SeeAlso
			matrix
			ker
			image
		///
doc ///
		Key
			"Example 2.1.20"
		Headline
			Sum, intersection, module quotient
		Description
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				m = matrix "xy, x; xz, x";
				n = matrix "y2, x; z2, x";
				
				M = image m;
				N = image n;
				
				M + N
				trim oo
				
				intersect(M,N)
				
				M : N
				N : M
				
				Q = A/(x^5);
				M = sub(M, Q)
				
				nul = image (0 * id_(Q^2))
				nul : M
		SeeAlso
			quotient
			symbol:
			trim
		///
doc ///
		Key
			"Example 2.2.15"
		Headline
			Graded rings and modules
		Description
			Text
				Unlike Singular, M2 would not assign or attribute weights automatically making an ideal homogeneous. The
				user should do that beforehand.
				
			Example
				A = QQ[x,y,z];
				I = ideal "y3-z2,x3-z";
				isHomogeneous I
				B = QQ[x,y,z, Degrees=>{1,2,3}];
				I = sub (I, B);
				isHomogeneous I
			Text
				
				There is, however, a way to homogenize an ideal.
				
			Example
				use A;
				homogenize(poly "x+z3+y2", x)
			Text
				
				We now move to modules. To make $M$ homogeneous, we have to redefine make it a subring of $B\oplus B(-3)$.
				
			Example
				1
		SeeAlso
			homogenize
		///
doc ///
		Key
			"Example 2.3.10"
		Headline
			Normal form
		Description
			Text
				Unlike Singular, Macaulay2 will reduce with respect to the standart basis.
				
			Example
				R = QQ[x,y,z];
				m = matrix "x,xy;
						   y, z;
						   1, z2"
				f = matrix "zx;y2+yz-z;y"
				f % m
		SeeAlso
			symbol//
		///
doc ///
		Key
			"Example 2.3.12"
		Headline
			Standard bases
		Description
			Text
				This example shows the influence of the different orders on the standard bases.
				
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				m = matrix "x+1,xy;y,z;1,z2"
				gens gb m

				B = QQ[x,y,z];
				m = sub(m, B);
				gens gb m

				C = QQ[x,y,z, MonomialOrder=>Lex];
				m = sub(m, C);
				gens gb m

				D = QQ[x,y,z, MonomialOrder=>{Position=>Down, Weights=>{(-1),(-1),(-1)}},Global=>false];
				m = sub(m, D);
				gens gb m

				E = QQ[x,y,z, MonomialOrder=>{Weights=>{(-1),(-1),(-1)}},Global=>false];
				m = sub(m, E);
				gens gb m
		SeeAlso
			sub
			gb
		///

doc ///
		Key
			"Example 2.4.12"
		Headline
			Resolution and Betti numbers
		Description
			Text
				For the first part it makes little difference what monomial order we choose. Also, the set is by
				default the graded case.
				
			Example
				A = QQ[x,y];
				I = ideal "x,y";
				Re = res I

				Re.dd
				betti Re
			Text
				
				In the second part we are computing the resolution over a quotient ring, so monomial order will
				be crusial. Below we use lexicographic local order.
				
			Example
				R = QQ[x,y,MonomialOrder=>{Weights=>{1,0},Weights=>{0,1}},Global=>false]/(x*y);
				M = coker transpose matrix "x,0;0,y";
				Re = res (M, LengthLimit=>4)
				Re.dd
			Text
				
				Had we used standard grading, the resolution would infact be finite (of length 3).
				
			Example
				1
		SeeAlso
			betti
			res
		///
doc ///
		Key
			"Example 2.4.15"
		Headline
			Homogeneous resolution and graded Betti numbers
		Description
			Text	
				M2 computes by default the graded versions of the resolution and accordingly the Betti numbers.
				
			Example
				A = QQ[x,y,z,w];
				I = ideal "xyz,wz,x+y";
			Text	
				
				We first compute the resolution and the differentials.
				
			Example
				F = res I
				F.dd
			Text
				
				The second map, with respect to the chosen basis, of course, can be obtained in the usual way.
				
			Example
				F.dd_2
			Text
				
				Having computed a minimal resolution, it is now easy to compute the graded Betti numbers.
				
			Example
				betti F
		SeeAlso
			betti
			res
		///
doc ///
		Key
			"Example 2.5.5"
		Headline
			Syzygies
		Description
			Text
				M2 computes syzygies of matrices and not of modules directly, that is, one has to use the presentation matrix.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				m = matrix "xy,yz,zx";
				syz m
			Text
				
				or similarly
				
			Example
				T = image matrix "xy,yz,xz;1,0,0;0,1,0;0,0,1"
				gens gb T
			Text
				
				so the first two columns indeed give the syzygies.
		SeeAlso
			syz
		///
doc ///
		Key
			"Example 2.5.18"
		Headline
			Schreyer resolution
		Description
			Text
				By default M2 always computes minimal free resolution and uses Schreyer orders, as this his makes an
				enormous improvement to the efficiency of the algorithm.
				
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				I = ideal "yz+z2,y2+xz,xy+z2,z3,xz2,x2z"
				F = res I
				F.dd
			Text
				
				The above method computes the syzygies on the Groebner basis of each syzygy module. Sometimes, as over quotient ring,
				it is preferrable to compute syzygies on the minimal generators of each matrix in the resolution. This can be done by
				changing the strategy.
				
			Example
				B = QQ[x,y,z];
				J = ideal I_*
				G = res (I, Strategy=>3)
				G.dd
			Text
				
				Note that we had to redefine the module, since once computed the resolution is kept as an atribute to the respective module.
		SeeAlso
			res
		///

doc ///
		Key
			"Example 2.7.5"
		Headline
			Tensor product of maps
		Description
			Text
				The operator for tensor product in M2 can be applied to matrices as well.
				
			Example
				A = QQ[x,y,z];
				M = matrix "1,2,3;4,5,6;7,8,9"
				N = matrix "x,y;0,z"
				M ** N
			Text
				
				We now implement the tenson product following the Singlular example.
				
			Example
				tensorMaps = (M,N) -> (
					 r := numcols M;
					 s := numrows M;
					 p := numcols N;
					 q := numrows N;
					 
					 R := new MutableMatrix from matrix toList (s*q:toList (r*p:0_A));     
					 for i from 1 to p do (
						  for j from 1 to q do (
						   for k from 1 to r do (
								for l from 1 to s do (R_((l-1)*q+j-1,(k-1)*p+i-1) = M_(l-1,k-1)*N_(j-1,i-1));
								);
						   );
						  );
					 return matrix R;
					 );

				tensorMaps (M,N)
		SeeAlso
			tensor
			numrows
			numcols
		///
doc ///
		Key
			"Example 2.7.9"
		Headline
			Tensor product of modules
		Description
			Text
				We follow the example code.
				
			Example
				R = QQ[x,y,z];
				M = matrix "1,2,3;4,5,6;7,8,9"
				N = matrix "x,y;0,z"

				tensorMod = (Phi, Psi) -> (
					 s := numrows M;
					 q := numrows N;
					 A := id_(R^s) ** N;
					 B := M ** id_(R^q);
					 return A|B;
					 );

				tensorMod (M,N)
		SeeAlso
			numrows
			tensor
		///
doc ///
		Key
			"Example 2.7.14"
		Headline
			Tensor product of rings
		Description
			Text
				We compute the tensor product of $K[x,y,z,a,b,c]/(x^2,y,ab-c^2)$ and $K[u,v,a,b,c]/(uv,ab-c^2)$ over $K[a,b,c]/(ab-c^2)$
				according to Corollary 2.7.12.
				
			Example
				A = QQ[a,b,c]/(a*b-c^2);
				p = a*b*c;
				B = QQ[x,y,z,a,b,c]/(x^2,y,a*b-c^2);
				ib = map (B, A, {a,b,c});
				C = QQ[u,v,a,b,c]/(u*v,a*b-c^2);
				ic = map (C, A, {a,b,c});
				y = symbol y;
				T = QQ[x,y,z,u,v,c,a,b]/(x^2,y,a*b-c^2,u*v);
				jb = map (T,B,{x,y,z,a,b,c});
				jc = map (T,C,{u,v,a,b,c});
				jc (ic (p))
				jb (ib (p))
		SeeAlso
			tensor
		///
doc ///
		Key
			"Example 2.8.1"
		Headline
			Module membership
		Description
			Text
				This is done using the @TO "symbol%"@ operator. M2 always reduces with respect to the Groebner basis.
				
			Example
				R = QQ[x,y,z];
				m = matrix entries transpose matrix "-z,-y,x+y,x;yz+z2,yz+z2,-xy-y2-xz-z2,0";
				v = matrix entries transpose matrix "-xz-z2,-xz+z2,x2+xy-yz+z2,0";
				v % m
			Text
				
				Since the ramainder in nonzero, this tell us that {\tt v} is not in {\tt image m}.

			Example
				w = matrix (m_0 - x^5 * m_1)
				w % m
			Text
				
				So {\tt w} is in {\tt image m}. Getting the coordinates with respect to the Groebner basis can be done directly
				
			Example
				w//m
			Text
				
				or computing the syzygy as in the Singular example
				
			Example
				wm = w | m
				syz wm
			Text
				
				The computation for the local same is the same.
				
			Example
				S = QQ[x,y,MonomialOrder=>{Weights=>{(-1),(-1),(-1)}},Global=>false];
				v = matrix "x2;xy";
				m = matrix entries transpose matrix "x+x3+y2,y3+y;y,-x2+y2";
				v % m
		SeeAlso
			symbol%
			symbol//
			isSubset
		///
doc ///
		Key
			"Example 2.8.3"
		Headline
			Elimination of module components
		Description
			Text
				This is done directly.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				N = matrix "xy, yz, xz;
					   1, 0, 0;
					   0, 1, 0;
					   0, 0, 1";
				gens gb N
		SeeAlso
			gb
		///
doc ///
		Key
			"Example 2.8.5"
		Headline
			Intersection of modules
		Description
			Text
				As in Singular, this can be done both directly
				
			Example
				R = QQ[x,y];
				I1 = matrix "x,y;y,1";
				I2 = matrix entries transpose matrix "0,y-1;x,1;y,x";
				intersect(image I1, image I2)
			Text
				
				 or using syzygies
				
			Example
				c1 = matrix "1;0;1;0";
				c2 = matrix "0;1;0;1";
				c3 = matrix "x;y;0;0";
				c4 = matrix "y;1;0;0";
				c5 = matrix "0;0;0;y-1";
				c6 = matrix "0;0;x;1";
				c7 = matrix "0;0;y;x";
				m = c1|c2|c3|c4|c5|c6|c7;
				r = syz m
			Text
				where the intersection is given by
				
			Example
				r^{0,1}
			Text
				
				Both computations give the same answer, for instance,
				
			Example
				(r^{0,1})_1 - y * (r^{0,1})_0
	SeeAlso
		intersect
		///
doc ///
		Key
			"Example 2.8.6"
		Headline
			Quotient of modules
		Description
			Text
				This is done using the @TO "quotient"@ procedure.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>{Position=>Down}];
				I = matrix "xy,yz;xz,xy";
				J = matrix "y,z;z,y";
				K = quotient (image I,image J);
				gens K
			Text
				
				and we can test if $KJ$ is contained in $I$,
				
			Example
				(gens K)_(0,0)*J % I
			Text
				
				Since $R$ has no zerodivisors, the annihilator of $J$ is trivial in $R$. However, the latter is not trivial
				in a quotient of $R$
				
			Example
				A = R/K
				J = matrix "xy,xy2;xyz-x2y2,y2x";
				nul = 0*id_(A^2);
				gens quotient (image nul, image J)
			Text
				
				Note that the last computation could be done directly.
				
			Example
				gens ann image J
				b1 = matrix "x2,xy;xy,xz;y2,yz"
				b = matrix "x,y,x+y;zx,zy,zx+zy;zy,xy,zy+xy"
				b2 = b_{0,1}|b1
				b * b1 % b2
		SeeAlso
			quotient
			symbol:
			ann
		///
doc ///
		Key
			"Example 2.8.7"
		Headline
			Radical, zerodivisors of modules
		Description
			Text
				We follow the Singular code.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>{Position=>Down}]/(poly "x2y2-xyz2");
				f = poly "xy(y-z)(y-1)";
				N = matrix entries transpose matrix "x,xz,y2;y,yz,z2;x2,xy,y2;xy,xz,yz";
				an = quotient (image N, R^3)

				Rt = QQ[t,x,y,z];
				I = ideal "x2y2-xyz2";
				an = sub (an, Rt) + I;
				f = sub (f, Rt);
				J = an + ideal(1-t*f);
				eliminate (J,{t})
			Text
				
				So $f$ should be a zerodivisor in {\tt R^3/image N}. We check that,
				
			Example
				use R;
				dim quotient (image sub(N,Rt),ideal f)
		SeeAlso
			quotient
		///
doc ///
		Key
			"Example 2.8.8"
		Headline
			Annihilaror and Fitting ideal
		Description
			Text
				We compute the annihilaror and the Fitting ideal of a module $M$ given by a presentation matrix $B$ over
				the quotient ring $K[x,y,z,u]/(x^2y^2-xyz^2)$. We first follow the Singular code.
				
			Example
				A = QQ[x,y,z,u]/(poly "x2y2-xyz2");
				B = matrix entries transpose matrix "x,xz,y2;y,yz,z2;x2,xy,y2";
				an = (image B) : (A^3);
				transpose gens an
				fit = det B
			Text
				
				We now check that {\tt fit} $\subset$ {\tt an} but {\tt fit} $\neq$ {\tt an}, by counting the number
				of generators after reduction.
				
			Example
				fit % an
				(gens an) % ideal (fit)
				(gens (an * an)) % ideal (fit)
			Text
				
				Alternatively, can use the M2 functionality directly.
				
			Example
				transpose gens trim ann coker B
				gens fittingIdeal (0,coker B)
		SeeAlso
			quotient
			symbol:
			ann
			fittingIdeal
		///
doc ///
		Key
			"Example 2.8.9"
		Headline
			Kernel of a module homomorphism
		Description
			Text
				We follow the Singular presentation.
				
			Example
				A = QQ[x,y,z, MonomialOrder=>{Position=>Down}]/(x^2*y^2-x*y*z^2);
				V = matrix "x2,xy;xy,xz;y2,yz";
				B = matrix "x,y;zx,zy;y2,z2";
				N = B | V
				s = syz N;
		///
doc ///
		Key
			"Example 2.8.10"
		Headline
			Solving linear equations
		Description
			Example
				K = frac (QQ[a,b,c,d]);
				R = K[x,y,z,u, MonomialOrder=>{Position=>Down}];
				E = ideal "3x+y+z-u-a,
				           13x+8y+6z-7u-b,
				           14x+10y+6z-7u-c,
				           7x+4y+3z-3u-d";
				transpose gens gb E
			Text
				
				We can easily read off the solution for (*). However, we cannot use the same
				method to compute a solution for (**) since it has none in $K(a,b,c,d)$.
				
			Example
				EE = E + ideal (x+y+z-u);
				transpose gens gb EE
			Text
				
				To fix this, we have to consider $a,b,c,d$ as variables too.
			
			Example
				R1 = QQ[x,y,z,u,a,b,c,d, MonomialOrder=>{Position=>Down}];
				EE = sub(EE, R1);
				transpose gens gb EE
		SeeAlso
			solve
		///
doc ///
		Key
			"Example 3.1.4"
		Headline
			Integral elements
		Description
			Text
				Let an ideal $I\subset A:=K[x_1,\ldots,x_4]$ and polynomials $f_1, f_2\in A$ be given.
				We want to check whether the elements $b=x_3$ (resp. $x_4$) are integral over $K[f_1,f_2]$ mod $I$.
				
			Example
				A = QQ[x_1..x_4,t,y_1,y_2, MonomialOrder=>Lex];
				I = ideal (x_1^2-x_2^3);
				f_1 = x_3^2-1;
				f_2 = x_1^2*x_2;
				b=x_3;
				M = ideal (t-b, y_1-f_1,y_2-f_2) + I;
			Text
				
				We now inspect the Groebner basis of $M$.
				
			Example
				transpose gens gb M
				b = x_4;
				M = ideal (t-b, y_1-f_1,y_2-f_2) + I;
				transpose gens gb M
			Text
				
				Since $t^2$ is among the leading monomials of the standard basis of $M$ in the first case,
				$x_3$ is integral over $K[f_1, f_2]$ mod $I$ with integral relation $x_3^2-f_1-1$. In the second case, we
				don't find such an expression in $t$, so $x_4$ is not integral over $K[f_1, f_2]$ mod $I$.
		///
doc ///
		Key
			"Example 3.1.6"
		Headline
			Finite maps
		Description
			Text
				Let $\phi:K[a,b,c]\to K[x,y,z]/(xy)$ be given by $\phi(a)=(xy)^3+x^2+z, \phi(b)=y^2-1, \phi(c)= z^3$. To check
				whether $\phi$ is finite we have to compute a standard basis of the ideal $M=(a-\phi(a),b-\phi(b),c-\phi(b))\subset K[a,b,c,x,y,z]$
				with respect to a block ordering $x>y>z>a,b,c$. We choose the lexicographical ordering $x>y>z>a>b>c$.
				
			Example
				A = QQ[x..z,a..c, MonomialOrder=>Lex];
				M = ideal "a-(xy)3-x2-z, b-y2+1, c-z3, xy";
				L = transpose gens gb M;
			Text
				
				Having the standard basis as elements of the matrix L, we now get the leading data.
				
			Example
				apply (flatten entries L, leadTerm)
			Text
				
				We see that the map is finite because $z^3, y^2, x^2$ appear as leading terms.
		SeeAlso
			apply
			flatten
			leadTerm
		///
doc ///
		Key
			"Example 3.2.3"
		Headline
			Integral closure of an ideal
		Description
			Text
				Let $A=K[x,y,z]/(zy^2-zx^3-x^6)$ and $I=(y)$. We want to compute the integral closure of $I$
				in the fraction field of the integral domain $A$. By Proposition 3.2.2 from @TO "[GP]"@, we have to compute the
				radical of $IR$, where $R$ is the integral closure of $A$.
				
				For the computation we are going to need the package @TO "IntegralClosure"@ distributed with M2.
				
			Example
				A = QQ[x..z, MonomialOrder=>{Weights=>{2,3,6},RevLex}];
				J = ideal "zy2-zx3-x6";
				norid = ideal(integralClosure (A/J))
			Text
				
				Note that the ideal {\tt norid} in the Singular code is exactly the defining ideal of the integral closure, with $x,y,z,w_{(0,0)}$ substituted by
				{\tt T(1), T(2), T(3), T(4)}.
				
			Example
				F = icMap(A/J)
				phi = map(integralClosure(A/J), A, {F(x),F(y),F(z)})
				K = ideal (phi(y)) + norid
				gens trim radical K
			Text
				
				So the radical of $IR$ is generated by $x, w_{0,0}, x^4+xz$.
		SeeAlso
			ideal
			icMap
			integralClosure
			radical
			trim
		///
doc ///
		Key
			"Example 3.3.8"
		Headline
			Minimal associated primes
		Description
			Text
				There is a direct way to compute minimal associated primes of an ideal.
				
			Example
				A = QQ[u..w,x..z];
				I = ideal "wx,wy,wz,vx,vy,vz,ux,uy,uz,y3-x2";
				minimalPrimes I
			Text
				
				and also
				
			Example
				B = QQ[x..z];
				I = ideal "zx, zy";
				minimalPrimes I
		SeeAlso
			minimalPrimes
		///
doc ///
		Key
			"Example 3.3.13"
		Headline
			Computation of the dimension
		Description
			Text
				Both, the dimension and the codimension of an ideal are computed as expected.
			
			Example
				A = QQ[u..w,x..z];
				I = ideal "wx,wy,wz,vx,vy,vz,ux,uy,uz,y3-x2";
				dim I
				codim I
		SeeAlso
			dim
			codim
		///
doc ///
		Key
			"Example 3.4.6"
		Headline
			Noether normalization
		Description
			Text
				Here is a slightly more ad-hoc implementation.
				
			Example
				R = QQ[x..z, MonomialOrder=>Lex];
				I = ideal "xy,xz";
				dim I
				m = matrix {{1, 0, 0}, {random(1,100), 1, 0}, {random(1,100), random(1,100), 1}}
				M = ideal (m*matrix{{x},{y},{z}})
				phi = map(R,R,toList apply (0..2, i->M_i))
				J = phi(I)
			Text
				
				Note that in M2 the image of an ideal $I$ under the ring map $\phi$ is taken to be the ideal generated by $\phi(I)$.
				
				In the above example, since the dimension of $I$ is 2, we have that $K[y,z]\to R/J$ is a Noetherian normalization.
		SeeAlso
			random
			apply
		///
doc ///
		Key
			"Example 3.5.5"
		Headline
			Independent set
		Description
			Example
				R = QQ[x..z];
				I = ideal "zy,xz";
				support matrix {independentSets I}
			Text
				
				Note that @TO "independentSets"@ returns a list of products of variables, and a maximal independent set is then given
				by the support of any of those products. In the example above, $\{x,y\}$ is a maximal independent set.
				
				In fact, $\{z\}$ is another maximal independent set and there are no others, but this is more complicated to get from M2.
		SeeAlso
			independentSets
			support
		///
doc ///
		Key
			"Example 3.5.9"
		Headline
			Computation of $d(I,K[x])$
		Description
			Text
				We give a procedure to compute the function $d$ of Definition 3.5.6 from @TO "[GP]"@.
				
			Example
		///
doc ///
		Key
			"Example 3.5.15"
		Headline
			Lying-over theorem
		Description
			Text
				Let $A=K[x,y]$, $P=(x)$ and $B=K[x,y,z]/(z^2-xz-1)$. We want to find a prime ideal $Q\supset PB$ such that
				$Q\cap A=P$.
				
			Example
				B = QQ[x..z];
				PB = ideal "x,z2-xz-1";
				minimalPrimes PB
			Text
				
				Both prime ideals give the ideal $P$ when intersected with $A$ in $B$.
		SeeAlso
			minimalPrimes
		///
doc ///
		Key
			"Example 3.6.10"
		Headline
			Normalization
		Description
			Text
				This example illustrates the normalization with Whitney's umbrella (see Algorithm 3.6.9 in @TO "[GP]"@). One way to do this
				is using the package @TO "NoetherNormalization"@. However, the Singular code seems wrong, so we do it as we did before in @TO "Example 3.2.3"@.
				
			Example
				A = QQ[x..z];
				I = ideal "y2-zx2";
				norid = ideal (integralClosure(A/I))
				icMap(A/I)
		SeeAlso
			integralClosure
			icMap
		///
doc ///
		Key
			"Example 3.6.13"
		Headline
			Non-normal locus
		Description
			Text
				We compute the nonnormal locus of $K[x,y,z]/(zy^2-zx^3-x^6)$
				
			Example
				A = QQ[x..z];
				I = ideal (z*y^2-z*x^3-x^6);
				sing = I + ideal(jacobian I);
				radical sing
			Text
				
				From the output, we read off the nonnormal locus: the zero set of $(x,y)$ = the z-axis.
		SeeAlso
			jacobian
			radical
		///
doc ///
		Key
			"Example 4.2.6"
		Headline
			Primary test
		Description
			Text
				There are available functions for computing primary decomposition and associated primes in M2. Computing those,
				however, is generally hard, so M2 has the feature of allowing the user to select one of the more popular
				algorithms, or strategies, for the purpose. See the "See also" entries below for details.
				
				In the luckiest case, we can check if an ideal is primary by computing the decomposition, and then checking
				if the returned list of ideals is a singleton.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "y4-4y3-10y2+28y+49,x3-6x2y+3x2+12xy2-12xy+3x-8y3+13y2-8y-6";
				length primaryDecomposition I == 1
			Text
				
				Computing the associated prime is then just as easy.
				
			Example
				ass I
			Text
				
				For a more hands-on way, as presented in the corresponding example in @TO "[GP]"@, we can proceed as follows.
				
			Example
				factor I_0
				-- so the first generator is a square of an irreducible element
				prim = ideal (y^2-2*y-7);
				q = 3*x-6*y+3;
				(q^3-27*I_1) % prim
		SeeAlso
			primaryDecomposition
			ass
			factor
		///
doc ///
		Key
			"Example 4.2.8"
		Headline
			Zero-dimensional primary decomposition
		Description
			Text
				As before, in this example we can just apply @TO "primaryDecompostion"@.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "(y2-1)2,x2-(y+1)3";
				primaryDecomposition I
			Text
				
				Alternatively, we can compute it manually. Below, we follow the Singular code.
				
			Example
				phi = map (R,R, {x,x+y});
				psi = map (R,R, {x,-x+y});
				I = ideal gens gb phi (I);
				L = transpose gens gb I

				prod = factor L_(0,0)

				Q1 = ideal (gens gb (I + ideal (value prod#1)))
				Q2 = ideal (gens gb (I + ideal (value prod#0)))

				factor Q1_0
				factor Q2_0

				Q1 = psi Q1;
				Q2 = psi Q2;

				transpose gens gb Q1
				transpose gens gb Q2
		SeeAlso
			Product
			factor
		///
doc ///
		Key
			"Example 4.3.3"
		Headline
			Reduction to the zero-dimensional case
		Description
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				a1 = ideal x;
				a2 = ideal "y2+2y+1,x-1";
				a3 = ideal "y2-2y+1,x-1";
				
				I = trim intersect (a1,a2,a3);
				
				G = ideal gens gb I
				independentSets G
				
				S = frac(QQ[y])[x,MonomialOrder=>Lex];
				substitute (G, S)
			Text
				
				This ideal in $\mathbb{Q}(y)[x]$ is obviously the prime ideal generated by $x$.
				
			Example
				use R;
				h = y^4-2*y^2+1;
				
				I1 = I:h
			Text
				
				Therefore we obtain $I:(h)=I:(h^\infty)=G\cap \mathbb{Q}[x,y]=(x)$.
		SeeAlso
			independentSets
		///
doc ///
		Key
			"Example 4.3.5"
		Headline
			Primary decomposition
		Description
			Text
				M2's procedure @TO "primaryDecompostion"@ works on this instance. Because the example takes up from where
				@TO "Example 4.3.3"@ left us, so we first redefine the objects.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "x2-x,xy4-2xy2+x";
				
				I2 = ideal gens gb ideal (I,(y^4-2*y^2+1));
				primaryDecomposition I2
			Text
				
				As before, we give alternative (that is, hands-on) solution following the Singular code. 
				
			Example
				independentSets I2
			Text
				
				... so we are in the in the zero-dimensional case.
				
			Example
				fac = factor I2_0;
				J1 = ideal gens gb ideal (I2, value fac#0)
				J2 = ideal gens gb ideal (I2, value fac#1)
				
				phi = map (R,R, {x,x+y});
				psi = map (R,R, {x,-x+y});
			Text
				
				... making a generic change of coordinates,
				
			Example
				K1 = phi J1
				transpose gens gb K1
				
				factor K1_0
		SeeAlso
			primaryDecomposition
			trim
		///
doc ///
		Key
			"Example 4.4.4"
		Headline
			Zero-dimensional primary decomposition
		Description
			Text
				We compute $E(I)$ for the ideal from @TO "Example 4.3.3"@. It suffices to compute the dimension of $I + (h)$.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "x2-x,xy4-2xy2+x";
				h = y^4-2*y^2+1;
				dim ideal (I,h)
			Text
				
				where just the last line is new code. Since $dim (I)=\#u=1$ and $dim(I,h)=0$, we can infer that the equidimensional part is $I_1=(x)$.
		///
doc ///
		Key
			"Example 4.4.10"
		Headline
			Equidimensional part
		Description
			Text
				In the setting of @TO "Example 4.3.3"@, we compute the equidimensional components of $I$.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "x2-x,xy4-2xy2+x";
				h = y^4-2*y^2+1;
				I1 = I:h
				I2 = I:I1
			Text
				
				where just the two last lines are the new computation.
				
				We have that $E_1(I)=(x)$ and $E_2(I)=(y^4-2y^2+1, x-1)$.
		///
doc ///
		Key
			"Example 4.5.4"
		Headline
			Radical
		Description
			Text
				The purpose of this exercise is to compute the radical of the ideal $I$ from @TO "Example 4.3.3"@.
				
			Example
				R = QQ[x,y, MonomialOrder=>Lex];
				I = ideal "x2-x,xy4-2xy2+x";
			Text
				
				In M2 this can be done directly.
				
			Example
				trim radical I
			Text
				
				For alternative computation, we follow the Singular code.
				
			Example
				I1 = ideal(x);
				h = y^4-2*y^2+1;
				rad = I1;
				I2 = I + ideal(h);
				dim I2
				
				-- univariate polynomials for x, y
				u = ideal "x2-x,y4-2y2+1";
		SeeAlso
			radical
			trim
		///
doc ///
		Key
			"Example 4.6.26"
		Headline
			Irreducible ascending set
		Description
			Example
				R = QQ[x_4,x_3,x_2,x_1];
				I = ideal (x_1*x_2+x_1*x_4+x_3,-x_1*x_2-2*(x_2)^2+x_3*x_4-1,-x_1*x_2*x_4+x_1*(x_4)^2+x_1*x_2-x_2*x_4+(x_4)^2+3*x_2);
				(L,p) = irreducibleCharacteristicSeries I;
				M = L#0;
				transpose M
		SeeAlso
			irreducibleCharacteristicSeries
		///
doc ///
		Key
			"Example 4.7.7"
		Headline
			Triangular decomposition
		Description
			
		///
doc ///
		Key
			"Example 5.2.5"
		Headline
			Hilbert-Poincare series
		Description
			Text
				This is done using the @TO "poincare"@ function. However, often @TO "hilbertPolynomial"@ is used as its output is better for
				extracting geometric information.
				
			Example
				A = QQ[t,x,y,z];
				I = ideal "x5y2,x3,y3,xy4,xy7";
				poincare (I)
		SeeAlso
			hilbertPolynomial
			hilbertSeries
			hilbertFunction
			poincare
		///
doc ///
		Key
			"Example 5.3.12"
		Headline
			Dimension, degree and Hilbert function of a homogeneous ideal
		Description
			Text
				We compute the dimension, degree and Hilbert function of the rational normal curve of degree $4$ in $\mathbb{P}^4$.
				
			Example
				R = QQ[s,t,x_0..x_4];
				I = ideal (x_0-t^4, x_1-t^3*s, x_2-t^2*s^2, x_3-t*s^3, x_4-s^4);
				J = eliminate (I, {s,t});
				transpose gens J
			Text
				
				This defines the ideal of the variety. We now compute its data. (Note that the first two lines below can be omitted.)
				
			Example
				S = QQ[x_0..x_4];
				J = substitute (J, S);
				poincare J
				dim J
				degree J
				hilbertPolynomial J
			Text
				
				This tells us that the degree of $J$ is 4, and its dimension as a projective variety is 1, that is, $J$ is a curve --- the dimension is 1 less than the
				output above, which is in fact the dimension of the affine cove over the rational normal curve.
		SeeAlso
			poincare
			hilbertPolynomial
			hilbertSeries
			degree
			eliminate
		///
doc ///
		Key
			"Example 5.5.13"
		Headline
			Initial ideal, Poincare series and Hilbert polynomial
		Description
			Text
				We compute the data for the ideal $(yz+z^2+x^3, y^2+xz+y^4)$ in $K[x,y,z]$.
				
			Example
				A = QQ[x,y,z, MonomialOrder=>Weights=>{(-1),(-1),(-1)}, Global=>false];
				I = ideal "yz+z2+x3,y2+xz+y4";
				transpose gens gb I
			Text
				
				at this point we use the M2 functionallity directly.
				
			Example
				hilbertSeries I
				hilbertSeries (I, Reduce=>true)
		SeeAlso
			hilbertSeries
		///
doc ///
		Key
			"Example 5.6.5"
		Headline
			Dimension of a module
		Description
			Text
				Once defined, say via @TO "coker"@ or @TO "image"@, the dimension of a module can be checked directly,
				not referring to its ambient ring and annihilaror.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>Weights=>{(-1),(-1),(-1)}, Global=>false];
				f = matrix {{x^2, 0, 0}, {0, x*z, 0}, {0, 0, x^2+z*x^3}};
				M = coker f
				N = image f
				transpose gens gb ann M
				dim M
				dim N
			Text
				
				The module $I$ from the example in @TO "[GP]"@ is the module $M$ above.
		SeeAlso
			image
			coker
		///
doc ///
		Key
			"Example 5.6.15"
		Headline
			Jacobson criterion, regular system of parameters, embedding dimension
		Description
			Text
				We want to study the local ring $A=K[x,y,z]_{(xyz)}/I$ where $I$ is a two-generated ideal. More precisely, we want to find out
				whether $A$ is regular.
				
			Example
				R = QQ[x,y,z, MonomialOrder=>Weights=>{(-1),(-1),(-1)}, Global=>false];
				I = ideal "x+y2+z3,x+y+xyz";
				J = transpose jacobian I
			Text
			
				We now evaluate $J$ at 0.
				
			Example
				J0 = substitute (J, {x=>0,y=>0,z=>0})
				rank J0
			Text
				
				The rank of the Jacobian at 0 is 2, which means that the embedding dimension of $A$ must be 1.
				
			Example
				dim I
			Text
				
				Finally, we check that $\{z\}$ is a regular system of parameters.
				
			Example
				K = I + ideal (z);
				transpose gens gb K
		SeeAlso
			substitute
		///
doc ///
		Key
			"Example 5.7.9"
		Headline
			Singular locus
		Description
			Text
				We carry out the computation for a specific example.
				
			Example
				R = QQ[u,v,w,x,y,z];
				I = ideal "wx,wy,wz,vx,vy,vz,ux,uy,uz,y3-x2";
				radical I == I
			Text
				
				So $I$ is radical. We compute its minimal primes.
				
			Example
				L = minimalPrimes I
			Text
			
				$I$ is the intersection of two primes.
				
			Example
				J = L_0 + L_1;
				gens gb J
			Text
				
				The intersection of the two irreducible components, defined by the respective primes, is the point 0.
				
			Example
				sing = L_0 + ideal (det (jacobian L_0)^{0..3});
				gens gb sing
			Text
				
				The singular locus of the component defined by $L_0$ is a line defined by the ideal {\tt sing} above.
		SeeAlso
			minimalPrimes
			jacobian
			radical
		///
doc ///
		Key
			"Example 6.1.3"
		Headline
			Inverse of a power series
		Description
			Text
				There is an M2 package for computations with power series. However, we give a direct
				implementation here.
				
			Example
				R = QQ[x,y,MonomialOrder=>{Weights=>{(-1),(-1)},2}, Global=>false];
				p = poly "2+x+y2";

				jet = (f, n) -> sum for t in terms f list if sum degree t <= n then t else 0;
				invers = (p, k) -> (
					 p0 := sub(jet(p,0), QQ);
					 q := sub(1/p0, R);
					 re := q;
					 p = q*(jet(p,0)-jet(p,4));
					 s := p;
					 while (p != 0) do (
						  re = re + q*p;
						  p = jet(p*s,k);
						  );
					 return re;
					 );

				q = invers (p,4)
				jet (p*q,4)
			Text
				
				The function {\tt jet} is a useful tool on its own but doesn't seem to exist in M2. (The chosen name is the one used in Singular.)
		SeeAlso
			terms
		///
doc ///
		Key
			"Example 6.2.4"
		Headline
			z-general power series
		Description
			Text
				This can be done in many ways. Note the monomial order.
				
			Example
				R = QQ[x,y,z,MonomialOrder=>{Weights=>{(-1),0,0}, Weights=>{0,(-1),0}, Weights=>{0,0,(-1)}},Global=>false];
				p = poly "xyz+x2yz+xy2z";
				sub (p, {x=>x+random(-5,5)*z, y=>y+random(-5,5)*z, z=>z})
		SeeAlso
			random
		///
doc ///
		Key
			"Example 6.2.10"
		Headline
			Weierstrass polynomial
		Description
			Text
				The following example illustrates a method to compute the the Weierstrass polynomial, that is, the result of the Weierstrass
				preparation theorem. The input is a polynomial (or power series) $F$ in $K[x,y]$, an interger $m$ such that $y^m$
				appears in as the degree-minimal $x$-free term in $F$, and a trunckation degree $n$. The output is polynomials $p(x,y)$
				and $u(x,y)$ such that $F=p u$ mod $x^n$.
				
				We do this for the case of two variables only, but the algorithm can be easily extended. We use the two procedures from the
				previous example, and a new one, before we implement the algorithm itself. We start with those.
				
			Example
				R = QQ[x,y,MonomialOrder=>{Weights=>{(-1),(-1)},2}, Global=>false];

				jet = (f, n) -> sum for t in terms f list if sum degree t <= n then t else 0;
				
				ord = f -> min flatten apply (terms f, t->degree t);
				
				invers = (p, k) -> (
					 p0 := sub(jet(p,0), QQ);
					 q := sub(1/p0, R);
					 re := q;
					 p = q*(jet(p,0)-jet(p,k));
					 s := p;
					 while (p != 0) do (
						  re = re + q*p;
						  p = jet(p*s,k);
					  );
					 return re;
					 );
				
				adhocdiv := (f,g) -> (
					 QQ[xx,yy];
					 fS := sub(f, {x=>xx,y=>yy});
					 gS := sub(g, {x=>xx,y=>yy});
					 use R;
					 return sub(fS // gS, {xx=>x,yy=>y});
					 );
				
				use R
			Text
				
				Next, we implement the algorithm.

			Example
				Weierstrass = (f,g,k) -> (
					 p := sub(f, x=>0);
					 m := ord p;
					 hf := adhocdiv(f, y^m);
					 rf := f - y^m * hf;
					 invhf := invers(adhocdiv(f, y^m), k);
					 w := -invhf * rf;
					 u := adhocdiv(g, y^m);
					 v := u;
					 H := jet (adhocdiv(w * u, y^m), k);
					 while (H != 0) do (
					  v = v+H;
					  H = jet (adhocdiv(w * H, y^m), k);
					  );
					 return (v * invhf);
					 );
			Text
				
				We test against the examples.
				
			Example
				f = poly "y4+xy+x2y6+x7"
				g = poly "y4"
				
				q1 = Weierstrass (f,g,10);
				w1 = jet (q1*f,10);
				q2 = Weierstrass(f,g,15);
				w2 = jet (q2*f, 15);
				
				S = QQ[x][y];
				sub(w1, S)
				sub(w2, S)
		SeeAlso
			sub
			symbol//
		///
doc ///
		Key
			"Example 6.2.15"
		Headline
			Finiteness test
		Description
			Text
				In this example we give a simple procedure {\tt isFiniteMap}, which tests whether a map $f$ of analytic $K$-algebras is finite. Here
				$I$ is the ideal by which the codomain power series ring is factored.
				
			Example
				isFiniteMap = (f, I) -> (
					m = ideal gens source f;
					me = f(m);
					n = me + I;
					if dim n == 0 then return true else return false;
					);
			Text
				
				We now test our code.
				
			Example
				A = QQ[x,y, MonomialOrder=>Weights=>{(-1),(-1)}, Global=>false];
				B = QQ[x,y,z, MonomialOrder=>Weights=>{(-1),(-1),(-1)}, Global=>false];
				phi = map (B,A,{x,y});
				I = ideal "z2-x2y";
				isFiniteMap (phi, I)
		SeeAlso
			source
		///
doc ///
		Key
			"Example 7.1.5"
		Headline
			Computation of Tor
		Description
			Text
				For this example we use the ready M2 function {\tt Tor_i}.
				
			Example
				A = QQ[x,y];
				Ps = matrix "x2,y";
				Ph = matrix "x";

				Tor_1(coker Ps, coker Ph)
				Tor_2(coker Ps, coker Ph)
				Tor_0(coker Ps, coker Ph)
			Text
				
				In fact, the second module is readily seen to be 0. We can this this just in case
				
			Example
				Tor_2(coker Ps, coker Ph) == 0
			Text
				
				But the 1-th torsion itself proves tricky to make sense of
				
			Example
				Tor_1(coker Ps, coker Ph) == A^1/(x,y)
		SeeAlso
			Tor
			coker

		///
doc ///
		Key
			"Example 7.2.6"
		Headline
			Fitting ideals
		Description
			Text
				M2 has a ready procedure @TO "fittingIdeal"@.
				
			Example
				R = QQ[x_0..x_4];
				M = coker matrix {toList (x_0..x_3), toList (x_1..x_4)}

				transpose gens fittingIdeal (-1, M)
				transpose gens fittingIdeal (0, M)
				transpose gens fittingIdeal (1, M)
		SeeAlso
			fittingIdeal
			coker
			matrix
		///
doc ///
		Key
			"Example 7.2.10"
		Headline
			Test for local freeness
		Description
			Text
				We follow the Singular implementation.
				
			Example
				R = QQ[x,y,z];
				S = matrix "x-1,y-1,z;y-1,x-2,x";
				I = fittingIdeal (0, coker S);
				Q = R/I;
				S = substitute (S, Q);

				isLocallyFree = (S, r) -> (
					 F := fittingIdeal (r, coker S);
					 G := fittingIdeal (r-1, coker S);
					 if F == ideal (0_(ring S)) then return false;
					 if degree F_0 == {0} and G == ideal (0_(ring S)) then return true;
					 return false;
					 );

				isLocallyFree(S, 1)
				isLocallyFree(S, 0)
			
			Text
				M2 also provides the @TO "isFreeModule"@ procedure.
			
			Example
				isFreeModule coker S
		SeeAlso
			coker
			isFreeModule
			fittingIdeal
			numgens
		///
doc ///
		Key
			"Example 7.3.12"
		Headline
			Flattening stratification
		Description
			Text
				needs the interred procedure
		///
doc ///
		Key
			"Example 7.3.14"
		Headline
			Test for flatness
		Description
			Text
				The following implements a procedure to set if the cokernel of a matrix $S$, over a ring $R$, is a flat $R$-module.
				
			Example
				isFlat = S -> (
					if ideal S == ideal 0_(ring S) then return true;
					w := 0;
					F := fittingIdeal (0, coker S);
					while F == ideal 0_(ring S) do (
						w = w+1;
						F = fittingIdeal (w, coker S)
						);
					if degree (flatten entries gens gb F)#0 == {0} then return true else return false;
					);
				
				A = QQ[x,y];
				S = matrix "x-1, y, x; x, x+1, y; x2, xy+x+1, x2+y";
				isFlat (S)
				
				B = A/(x^2+x-y)
				S = sub(S, B)
				isFlat (S)
				
				use A;
				C = A/(x^2+x+y)
				S = sub(S, C)
				isFlat (S)
			Text
				
				The second part is not yet implemented.
		SeeAlso
			fittingIdeal
		///
doc ///
		Key
			"Example 7.3.17"
		Headline
			Flat locus
		Description
			Text
				An alternative of the previous example is given below.
				
			Example
				R = QQ[x,y,z]/(x*y*z);
				m = matrix "x,y,z; 0,x3,z3"
				r = res image m
				s = matrix entries r.dd_1
				gens ann Ext^1(image m, image s)
		SeeAlso
			Ext
			ann
		///
doc ///
		Key
			"Example 7.5.7"
		Headline
			Flatness test
		Description
			Text
				
		///
doc ///
		Key
			"Example 7.6.3"
		Headline
			Regular sequences
		Description
			Text
				We implement the regularity check in a somewhat different way, following the mathematical definition rather
				than direclty translating the Singular code --- luckily M2 has a way of directly checking the conditions. We
				let $f = (f_1,\ldots, f_n)$ be a sequence rather than an ideal, and take $N$ to be a module, defined in any
				prefered way.
				
			Example
--				isReg (f, N) -> (
--					M := N;
--					for i from 1 to #f do (
--						if quotient (M, f#(i-1)) != M then return false;
--						M = N/f_(toList (0..i-1));
--						);
--					if ideal f  == ring f_0 then return false else return true;
--					);
		///
doc ///
		Key
			"Example 7.7.8"
		Headline
			First test for Cohen-Macaulayness
		Description
			Text
				We use the {\tt Depth} package for the computation of depth, not referring to Koszul cohomology. Also
				we can apply @TO "dim"@ directly to modules.
				
			Example
				loadPackage "Depth";
				R = QQ[x,y,z, MonomialOrder=>Weights=>{(-1),(-1),(-1)}, Global=>false];
				I = ideal "xz,yz,z2";
				M = I * R^1;
				depth M == dim M
			Text
				
				The second instance in the book is wrong, however. We have
				
			Example	
				I = ideal "x2+y2,z7";
				M = I * R^1;
				depth M == dim M
			Text
				
				as the depth of $M$ is 1 while the dimension of its, that is, of the zero ideal is obviously 3 as an ideal of R.
				(The conclusion following from the Alexander-Buchsbaum formula.)
				
			Example
				res M
		SeeAlso
			pdim
			dim
			ann
			depth
		///

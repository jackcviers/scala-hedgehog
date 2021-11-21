"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[261],{3905:function(e,t,n){n.d(t,{Zo:function(){return d},kt:function(){return h}});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function s(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},l=Object.keys(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)n=l[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var o=a.createContext({}),p=function(e){var t=a.useContext(o),n=t;return e&&(n="function"==typeof e?e(t):s(s({},t),e)),n},d=function(e){var t=p(e.components);return a.createElement(o.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,l=e.originalType,o=e.parentName,d=i(e,["components","mdxType","originalType","parentName"]),c=p(n),h=r,m=c["".concat(o,".").concat(h)]||c[h]||u[h]||l;return n?a.createElement(m,s(s({ref:t},d),{},{components:n})):a.createElement(m,s({ref:t},d))}));function h(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=n.length,s=new Array(l);s[0]=c;var i={};for(var o in t)hasOwnProperty.call(t,o)&&(i[o]=t[o]);i.originalType=e,i.mdxType="string"==typeof e?e:r,s[1]=i;for(var p=2;p<l;p++)s[p]=n[p];return a.createElement.apply(null,s)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},8604:function(e,t,n){n.r(t),n.d(t,{frontMatter:function(){return i},contentTitle:function(){return o},metadata:function(){return p},toc:function(){return d},default:function(){return c}});var a=n(7462),r=n(3366),l=(n(7294),n(3905)),s=["components"],i={title:"Tutorial",sidebar_label:"Tutorial",slug:"/guides-tutorial"},o=void 0,p={unversionedId:"guides/tutorial",id:"guides/tutorial",isDocsHomePage:!1,title:"Tutorial",description:"Tutorial",source:"@site/../generated-docs/target/mdoc/guides/tutorial.md",sourceDirName:"guides",slug:"/guides-tutorial",permalink:"/scala-hedgehog/docs/guides-tutorial",tags:[],version:"current",frontMatter:{title:"Tutorial",sidebar_label:"Tutorial",slug:"/guides-tutorial"},sidebar:"docs",previous:{title:"Guides",permalink:"/scala-hedgehog/docs/guides"},next:{title:"State-Based Testing (1)",permalink:"/scala-hedgehog/docs/guides-state-tutorial"}},d=[{value:"Tutorial",id:"tutorial",children:[]},{value:"Thanks",id:"thanks",children:[]},{value:"What is Hedgehog?",id:"what-is-hedgehog",children:[]},{value:"Getting started",id:"getting-started",children:[{value:"A quick example",id:"a-quick-example",children:[]},{value:"Just a library",id:"just-a-library",children:[]}]},{value:"Properties",id:"properties",children:[{value:"Results",id:"results",children:[]},{value:"Generators",id:"generators",children:[]},{value:"Shrinking",id:"shrinking",children:[]},{value:"Deterministic results",id:"deterministic-results",children:[]},{value:"Classifications",id:"classifications",children:[]}]},{value:"State",id:"state",children:[]}],u={toc:d};function c(e){var t=e.components,n=(0,r.Z)(e,s);return(0,l.kt)("wrapper",(0,a.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,l.kt)("h2",{id:"tutorial"},"Tutorial"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#tutorial"},"Tutorial")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#thanks"},"Thanks")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#what-is-hedgehog"},"What is Hedgehog?")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#getting-started"},"Getting started"),(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#a-quick-example"},"A quick example")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#just-a-library"},"Just a library")))),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#properties"},"Properties"),(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#results"},"Results"),(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#logging"},"Logging")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#combining"},"Combining")))),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#generators"},"Generators"),(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#case-classes"},"Case Classes")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#lists"},"Lists")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#filtering"},"Filtering")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#sized"},"Sized")))),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#shrinking"},"Shrinking")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#deterministic-results"},"Deterministic results")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#classifications"},"Classifications")))),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#state"},"State"))),(0,l.kt)("h2",{id:"thanks"},"Thanks"),(0,l.kt)("p",null,(0,l.kt)("strong",{parentName:"p"},"This guide was originally copied from the very excellent\n",(0,l.kt)("a",{parentName:"strong",href:"https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md"},"ScalaCheck Guide"),"\nand repurposed for Hedgehog"),"."),(0,l.kt)("h2",{id:"what-is-hedgehog"},"What is Hedgehog?"),(0,l.kt)("p",null,"Hedgehog is a tool for testing Scala and Java programs, based on property\nspecifications and automatic test data generation. The basic idea is that you\ndefine a property that specifies the behaviour of a method or some unit of\ncode, and Hedgehog checks that the property holds. All test data are\ngenerated automatically in a random fashion, so you don't have to worry about\nany missed cases."),(0,l.kt)("h2",{id:"getting-started"},"Getting started"),(0,l.kt)("p",null,"Please follow the general ",(0,l.kt)("a",{parentName:"p",href:"/scala-hedgehog/docs/getting-started"},"getting started"),"\nguide first."),(0,l.kt)("h3",{id:"a-quick-example"},"A quick example"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'import hedgehog._\nimport hedgehog.runner._\n\nobject Spec extends Properties {\n\n  override def tests: List[Test] =\n    List(\n      property("property", propConcatLists)\n    )\n\n  def propConcatLists: Property =\n    for {\n       l1 <- Gen.int(Range.linear(-100, 100)).list(Range.linear(0, 100)).forAll\n       l2 <- Gen.int(Range.linear(-100, 100)).list(Range.linear(0, 100)).forAll\n     } yield l1.size + l2.size ==== (l1 ::: l2).size\n}\n')),(0,l.kt)("p",null,"You can run this from ",(0,l.kt)("inlineCode",{parentName:"p"},"sbt"),", either via ",(0,l.kt)("inlineCode",{parentName:"p"},"test")," or an application  with ",(0,l.kt)("inlineCode",{parentName:"p"},"run"),"."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"scala> test\n+ Spec$.property: OK, passed 100 tests\nscala> run Spec\n+ Spec$.property: OK, passed 100 tests\n")),(0,l.kt)("p",null,"OK, that seemed alright. Now define another property."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def propSqrt: Property =\n  for {\n    n <- Gen.int(Range.linearFrom(0, -100, 100)).forAll\n  } yield scala.math.sqrt(n * n) ==== n\n")),(0,l.kt)("p",null,"Check it!"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"- Spec$.property: Falsified after 8 passed tests\n> -1\n> === Not Equal ===\n> --- lhs ---\n> 1.0\n> --- rhs ---\n> -1.0\n")),(0,l.kt)("p",null,"Not surprisingly, the property doesn't hold. The argument ",(0,l.kt)("inlineCode",{parentName:"p"},"-1")," falsifies it."),(0,l.kt)("h3",{id:"just-a-library"},"Just a library"),(0,l.kt)("p",null,"Before we continue it's worth pointing out that for the most past Hedgehog is\n",(0,l.kt)("em",{parentName:"p"},"just")," a library. Let's run our first property directly using the API."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"scala> import hedgehog._\nscala> Property.checkRandom(Spec.propConcatLists).value\nres0: hedgehog.core.Report = Report(SuccessCount(100),DiscardCount(0),OK)\n")),(0,l.kt)("p",null,"We can see that the test output is returned as pure data."),(0,l.kt)("p",null,'Feel free to run the properties in this guide in any way you find most\nconvenient. We will continue to display the results from "running" the property,\njust because it\'s more readable.'),(0,l.kt)("h2",{id:"properties"},"Properties"),(0,l.kt)("p",null,"There are two main concepts to Hedgehog are:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#results"},"Results")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("a",{parentName:"li",href:"#generators"},"Generators"))),(0,l.kt)("p",null,"Let's start with the more simple and familar results and then move on to the\nmore interesting generators."),(0,l.kt)("h3",{id:"results"},"Results"),(0,l.kt)("p",null,"A ",(0,l.kt)("inlineCode",{parentName:"p"},"Result")," is really a simple ",(0,l.kt)("inlineCode",{parentName:"p"},"Boolean")," assertion with extra logging.\nThat's it (no really)."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testAdd: Result =\n  Result.assert(1 + 2 == 2 + 1)\n")),(0,l.kt)("p",null,"You can actually run these from Hedgehog like you would a full property."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'object Spec extends Properties {\n\n  override def tests: List[Test] =\n    List(\n      example("add", testAdd)\n    )\n\n  def testAdd: Result =\n    Result.assert(1 + 2 == 2 + 1)\n}\n')),(0,l.kt)("p",null,"Note that we've used the ",(0,l.kt)("inlineCode",{parentName:"p"},"example")," test function here instead of ",(0,l.kt)("inlineCode",{parentName:"p"},"property"),",\nwhich is used for the more powerful ",(0,l.kt)("inlineCode",{parentName:"p"},"Property")," result."),(0,l.kt)("p",null,"And when we test it, notice that it only runs once and not 100 times."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"+ Spec$.add: OK, passed 1 tests\n")),(0,l.kt)("p",null,"This is just like any other test in ScalaTest/specs2/junit/etc."),(0,l.kt)("p",null,"What happens if we fail though?"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testAdd: Result =\n  Result.assert(1 + 2 == 3 + 4)\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Spec$.add: Falsified after 1 passed tests\n")),(0,l.kt)("p",null,"That's it? What about a useful message telling us what failed?\nFor starters, given that we're just doing an assertion Hedgehog comes with the\nconvenient ",(0,l.kt)("inlineCode",{parentName:"p"},"====")," operator:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testAdd: Result =\n  1 + 2 ==== 3 + 4\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Spec$.testAdd: Falsified after 1 passed tests\n> === Not Equal ===\n> --- lhs ---\n> 3\n> --- rhs ---\n> 7\n")),(0,l.kt)("p",null,"That's a little better. But what happens if we don't just want to check\nequality?"),(0,l.kt)("p",null,"There is a method called ",(0,l.kt)("inlineCode",{parentName:"p"},"diff")," which is similar to ",(0,l.kt)("inlineCode",{parentName:"p"},"Result.assert")," but it gives the similar message to ",(0,l.kt)("inlineCode",{parentName:"p"},"====")," operator's."),(0,l.kt)("p",null,(0,l.kt)("inlineCode",{parentName:"p"},"diff")," takes two arguments and the comparison function so that you can do any comparison operation you want on those two arguments."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testAdd: Result =\n  Result.diff(1 + 2, 3 + 4)(_ == _)\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Spec$.testAdd: Falsified after 1 passed tests\n> === Failed ===\n> --- lhs ---\n> 3\n> --- rhs ---\n> 7\n")),(0,l.kt)("hr",null),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def a1GtA2: Result =\n  Result.diff(1 + 2, 3 + 4)(_ > _)\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Spec$.a1GtA2: Falsified after 0 passed tests\n> === Failed ===\n> --- lhs ---\n> 3\n> --- rhs ---\n> 7\n")),(0,l.kt)("p",null,"If you want to change the log name (i.e. ",(0,l.kt)("inlineCode",{parentName:"p"},"=== Failed ==="),") to something else, you can use ",(0,l.kt)("inlineCode",{parentName:"p"},"diffNamed")," instead."),(0,l.kt)("p",null,"e.g.)"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'Result.diffNamed("=== Not Equal ===", 1 + 2, 3 + 4)(_ == _)\n')),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Spec$.testAdd: Falsified after 1 passed tests\n> === Not Equal ===\n> --- lhs ---\n> 3\n> --- rhs ---\n> 7\n")),(0,l.kt)("p",null,"In fact, ",(0,l.kt)("inlineCode",{parentName:"p"},"====")," internally uses the ",(0,l.kt)("inlineCode",{parentName:"p"},"diffNamed")," method."),(0,l.kt)("h4",{id:"logging"},"Logging"),(0,l.kt)("p",null,"Sometimes it can be difficult to decide exactly what is wrong when a property\nfails, especially if the property is complex, with many conditions. In such\ncases, you can log the different parts of the property, so Hedgehog can\ntell you exactly what part is failing."),(0,l.kt)("p",null,"From the the example above, what happens if we wanted to check if two numbers\nwere less than each other?"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testTL: Result =\n  Result.assert(2 < 1)\n")),(0,l.kt)("p",null,"As we saw earlier, this wouldn't give a very useful error message.\nThis is where ",(0,l.kt)("inlineCode",{parentName:"p"},"log")," comes in handy."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def testTL: Result =\n  Result.assert(2 < 1)\n    .log("2 is not less than 1")\n')),(0,l.kt)("p",null,"We could ever make our own function to help re-use this."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def isLessThan(a: Int, b: Int): Result =\n  Result.assert(a < b)\n    .log(s"$a is not less than $b")\n\ndef testTL: Result =\n  isLessThan(2, 1)\n')),(0,l.kt)("p",null,"Where logging really comes in handy is when you start to use\n",(0,l.kt)("a",{parentName:"p",href:"#generators"},"generators")," and the results are different every time."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'val complexProp: Property =\n  for {\n    m <- Gen.int(Range.linear(1, 100)).log("m")\n    n <- Gen.int(Range.linear(1, 100)).log("n")\n  } yield {\n    val res = m + n\n    Result.all(List(\n      Result.assert(res >= m).log("result > #1")\n    , Result.assert(res >= n).log("result > #2")\n    , Result.assert(res < m + n).log("result not sum")\n    ))\n  }\n')),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"- Spec.property: Falsified after 0 passed tests.\n> m: 0\n> n: 0\n> result not sum\n")),(0,l.kt)("p",null,"The log operator can also be used to inspect intermediate values\nused in the properties, which can be very useful when trying to understand\nwhy a property fails. Hedgehog always presents the generated property\narguments but sometimes you need to quickly see\nthe value of an intermediate calculation. See the following example, which\ntries to specify multiplication in a somewhat naive way:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def propMul: Property =\n  for {\n    n <- Gen.int(Range.linear(1, 100)).log("n")\n    m <- Gen.int(Range.linear(1, 100)).log("m")\n  } yield {\n    val res = n*m\n    Result.all(List(\n      (res / m ==== n).log("div1")\n    , (res / n ==== m).log("div2")\n    , Result.assert(res > m).log("lt1")\n    , Result.assert(res > n).log("lt2")\n    )).log("evidence = " + res)\n  }\n')),(0,l.kt)("p",null,"Here we have four different conditions, each with its own label. A fifth label\nis added to the combined property to record the result of the multiplication.\nWhen we check the property, Hedgehog tells us the following:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"- Spec$.example: Falsified after 0 passed tests.\n> n: 1\n> n: 1\n> lt1\n> lt2\n> evidence = 1\n")),(0,l.kt)("p",null,"As you can see, you can add as many logs as you want to your result,\nHedgehog will only present the failing ones for the smallest example."),(0,l.kt)("h4",{id:"combining"},"Combining"),(0,l.kt)("p",null,"Results can be combined with other results into new ones using familiar\nboolean logic."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def p1: Result =\n  "a" ==== "a"\n\ndef p2: Result =\n  1 ==== 1\n\ndef p3: Result =\n  p1 and p2\n\ndef p4: Result =\n  p1 or p2\n\n// same as p1 and p2\ndef p5: Result =\n  Result.all(List(p1, p2))\n\n// same as p1 or p2\ndef p6: Result =\n  Result.any(List(p1, p2))\n')),(0,l.kt)("p",null,"Here, ",(0,l.kt)("inlineCode",{parentName:"p"},"p3")," will hold if and only if both ",(0,l.kt)("inlineCode",{parentName:"p"},"p1")," and ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," hold, ",(0,l.kt)("inlineCode",{parentName:"p"},"p4")," will hold if\neither ",(0,l.kt)("inlineCode",{parentName:"p"},"p1")," or ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," holds."),(0,l.kt)("h3",{id:"generators"},"Generators"),(0,l.kt)("p",null,"Generators are responsible for generating test data in Hedgehog, and are\nrepresented by the ",(0,l.kt)("inlineCode",{parentName:"p"},"hedgehog.Gen")," class. You need to know how to use this\nclass if you want Hedgehog to generate data of types that are not supported\nby default to state properties about a specific subset of a type. In the ",(0,l.kt)("inlineCode",{parentName:"p"},"Gen"),"\nobject, there are several methods for creating new and modifying existing\ngenerators. We will show how to use some of them in this section. For a more\ncomplete reference of what is available, please see the Github source."),(0,l.kt)("p",null,"A generator can be seen simply as a function that takes some generation\nparameters, and (maybe) returns a generated value. That is, the type ",(0,l.kt)("inlineCode",{parentName:"p"},"Gen[T]"),"\nmay be thought of as a function of type ",(0,l.kt)("inlineCode",{parentName:"p"},"Seed => Option[T]"),". However, the\n",(0,l.kt)("inlineCode",{parentName:"p"},"Gen")," class contains additional methods to make it possible to map generators,\nuse them in for-comprehensions and so on. Conceptually, though, you should\nthink of generators simply as functions, and the combinators in the ",(0,l.kt)("inlineCode",{parentName:"p"},"Gen"),"\nobject can be used to create or modify the behaviour of such generator\nfunctions."),(0,l.kt)("p",null,"Let's see how to create a new generator. The best way to do it is to use the\ngenerator combinators that exist in the ",(0,l.kt)("inlineCode",{parentName:"p"},"hedgehoge.Gen")," module. These can\nbe combined using a for-comprehension. Suppose you need a generator which\ngenerates a tuple that contains two random integer values, one of them being at\nleast twice as big as the other. The following definition does this:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"val myGen: Gen[(Int, Int)] =\n  for {\n    n <- Gen.int(Range.linear(10, 20))\n    m <- Gen.int(Range.linear(2*n, 500))\n  } yield (n, m)\n")),(0,l.kt)("p",null,"You can create generators that picks one value out of a selection of values.\nThe following generator generates a vowel:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def vowel: Gen[Char] =\n  Gen.element1('A', 'E', 'I', 'O', 'U', 'Y')\n")),(0,l.kt)("p",null,"The ",(0,l.kt)("inlineCode",{parentName:"p"},"element1")," method creates a generator that randomly picks one of its\nparameters each time it generates a value. Notice that plain values are\nimplicitly converted to generators (which always generates that value) if\nneeded."),(0,l.kt)("p",null,"The distribution is uniform, but if you want to control it you can use the\n",(0,l.kt)("inlineCode",{parentName:"p"},"frequency1")," combinator:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def vowel: Gen[Char] =\n  Gen.frequency1(\n    (3, 'A')\n  , (4, 'E')\n  , (2, 'I')\n  , (3, 'O')\n  , (1, 'U')\n  , (1, 'Y')\n  )\n")),(0,l.kt)("p",null,"Now, the ",(0,l.kt)("inlineCode",{parentName:"p"},"vowel")," generator will generate ",(0,l.kt)("inlineCode",{parentName:"p"},"E"),"s more often than ",(0,l.kt)("inlineCode",{parentName:"p"},"Y"),"s. Roughly, 4/14\nof the values generated will be ",(0,l.kt)("inlineCode",{parentName:"p"},"E"),"s, and 1/14 of them will be ",(0,l.kt)("inlineCode",{parentName:"p"},"Y"),"s."),(0,l.kt)("h4",{id:"case-classes"},"Case Classes"),(0,l.kt)("p",null,"It is very simple to generate random instances of case classes in Hedgehog.\nConsider the following example where a binary integer tree is generated:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"sealed abstract class Tree\ncase class Node(left: Tree, right: Tree, v: Int) extends Tree\ncase object Leaf extends Tree\n\nval genLeaf: Gen[Tree] =\n  Gen.constant(Leaf)\n\ndef genNode: Gen[Tree] =\n  for {\n    v <- Gen.int(Range.linear(-100, 100))\n    left <- genTree\n    right <- genTree\n  } yield Node(left, right, v)\n\ndef genTree: Gen[Tree] =\n  Gen.choice1(genLeaf, genNode)\n")),(0,l.kt)("p",null,"We can now generate a sample tree:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def testTree: Property =\n  forAll {\n    t <- genTree.forAll\n  } yield {\n    println(t)\n    Result.success\n  }\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"Leaf\nNode(Leaf,Leaf,-71)\nNode(Node(Leaf,Leaf,-71),Node(Leaf,Leaf,-49),17),Leaf,-20\nNode(Node(Node(Node(Node(Leaf,Leaf,-71),Node(Leaf,Leaf,-49),17),Leaf,-20),Leaf,-7),Node(Node(Leaf,Leaf,26),Leaf,-3),49)\nNode(Leaf,Node(Node(Node(Node(Node(Node(Leaf,Leaf,-71),Node(Leaf,Leaf,-49),17),Leaf,-20),Leaf,-7),Node(Node(Leaf,Leaf,26),Leaf,-3),49),Leaf,84),-29)\n")),(0,l.kt)("h4",{id:"lists"},"Lists"),(0,l.kt)("p",null,"There is a use generator, ",(0,l.kt)("inlineCode",{parentName:"p"},"list"),", that generates a list of the current ",(0,l.kt)("inlineCode",{parentName:"p"},"Gen"),".\nYou can use it in the following way:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def genIntList: Gen[List[Int]] =\n  Gen.element1(1, 3, 5).list(Range.linear(0, 10))\n\ndef genBoolList: Gen[List[Boolean]] =\n  Gen.constant(true).list(Range.linear(0, 10))\n\ndef genCharList: Gen[List[Char]] =\n  Gen.alpha.list(Range.linear(0, 10))\n")),(0,l.kt)("p",null,"It might be annoying to deal with a list of characters, which is where the\n",(0,l.kt)("inlineCode",{parentName:"p"},"Gen.string")," function comes in handy."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def genStringList: Gen[String] =\n  Gen.string(Gen.alpha, Range.linear(0, 10))\n")),(0,l.kt)("h4",{id:"filtering"},"Filtering"),(0,l.kt)("p",null,"Generator values can be restricted to ensure they meet some precondition."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'val propMakeList: Property =\n  for {\n    n <- Gen.int(Range.linear(0, 100))\n      .ensure(n => n % 2 == 0)\n      .forAll\n  } yield List.fill(n)("").length ==== n\n}\n')),(0,l.kt)("p",null,"Now Hedgehog will only care for the cases when ",(0,l.kt)("inlineCode",{parentName:"p"},"n")," is even."),(0,l.kt)("p",null,"If ",(0,l.kt)("inlineCode",{parentName:"p"},"ensure")," is given a condition that is hard or impossible to\nfulfill, Hedgehog might not find enough passing test cases to state that the\nproperty holds. In the following trivial example, all cases where ",(0,l.kt)("inlineCode",{parentName:"p"},"n")," is\nnon-zero will be thrown away:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def propTrivial: Property =\n  for {\n    n <- Gen.int(Range.linear(0, 100))\n     .ensure(n => n == 0)\n     .forAll\n  } yield n ==== 0\n")),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"> Gave up after only 55 passed tests. 100 were discarded\n")),(0,l.kt)("p",null,"It is possible to tell Hedgehog to try harder when it generates test cases,\nbut generally you should try to refactor your property specification instead of\ngenerating more test cases, if you get this scenario."),(0,l.kt)("p",null,"Using ",(0,l.kt)("inlineCode",{parentName:"p"},"ensure"),", we realise that a property might not just pass or fail, it\ncould also be undecided if the implication condition doesn't get fulfilled."),(0,l.kt)("h4",{id:"sized"},"Sized"),(0,l.kt)("p",null,"When Hedgehog uses a generator to generate a value, it feeds it with some\nparameters. One of the parameters the generator is given, is a ",(0,l.kt)("inlineCode",{parentName:"p"},"Size")," value,\nwhich some generators use to generate their values. If you want to use the size\nparameter in your own generator, you can use the ",(0,l.kt)("inlineCode",{parentName:"p"},"Gen.sized")," method:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"def matrix[T](g: Gen[T]): Gen[List[List[T]]] =\n  Gen.sized(size => {\n    val side = scala.math.sqrt(size.value).toInt\n    g.list(Range.linear(0, side)).list(Range.linear(0, side))\n  })\n")),(0,l.kt)("p",null,"The ",(0,l.kt)("inlineCode",{parentName:"p"},"matrix")," generator will use a given generator and create a matrix which\nside is based on the generator size parameter. It uses the ",(0,l.kt)("inlineCode",{parentName:"p"},"list")," function\nwhich creates a sequence of given length filled with values obtained from the\ngiven generator."),(0,l.kt)("h3",{id:"shrinking"},"Shrinking"),(0,l.kt)("p",null,"In some ways the most interesting and important feature of Hedgehog is that if\nit finds an argument that falsifies a property, it tries to ",(0,l.kt)("em",{parentName:"p"},"shrink")," that\nargument before it is reported."),(0,l.kt)("p",null,"This is done automatically! This is crucially different from ","[QuickCheck]"," and\n","[ScalaCheck]"," which requires some hand-holding when it comes to shrinking.\nWe recommended watching the ",(0,l.kt)("a",{parentName:"p",href:"/scala-hedgehog/docs/motivation"},"original presentation"),"\nfor more information on how this works."),(0,l.kt)("p",null,"Let's look at specifying a property that says that no list has duplicate\nelements in it. This is of course not true, but we want to see the test case\nshrinking in action!"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def p1: Property =\n  for {\n    l <- Gen.int(Range.linearFrom(0, -100, 100)).list(Range.linear(0, 100)).log("l")\n  } yield l ==== l.distinct\n')),(0,l.kt)("p",null,"Now, run the tests:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"- Spec$.example: Falsified after 5 passed tests\n> l: List(0,0)\n> === Not Equal ===\n> --- lhs ---\n> List(0,0)\n> --- rhs ---\n> List(0)\n")),(0,l.kt)("p",null,"Notice in particular the ",(0,l.kt)("inlineCode",{parentName:"p"},"i: List(0, 0)"),", which captures the\nsmallest possible value that doesn't satisfy the invalid property."),(0,l.kt)("p",null,"Let's try that again, but let's see what else it tried."),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'def p1: Property =\n  for {\n    l <- Gen.int(Range.linearFrom(0, -100, 100)).list(Range.linear(0, 100)).log("i")\n  } yield {\n    println(l)\n    l ==== l.distinct\n  }\n')),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},"List()\nList(1, -2)\nList(-2, -3)\nList(1, 4, 1)\nList(0, 4, 1)\nList(1, 0, 1)\nList(1, 0, 0)\nList()\nList(0, 0)\nList()\nList(0)\n")),(0,l.kt)("p",null,"You can see after a few tries Hedgehog finds an invalid example ",(0,l.kt)("inlineCode",{parentName:"p"},"List(1, 4, 1)"),",\nand starts to shrink both the values down to ",(0,l.kt)("inlineCode",{parentName:"p"},"0")," and also the list size."),(0,l.kt)("h3",{id:"deterministic-results"},"Deterministic results"),(0,l.kt)("p",null,"By default, Hedgehog uses a random seed that is based on the current system time. Normally, this is exactly what you want. However, if you have a failing test, the randomness of the generated test data can make it very difficult to reproduce and analyse the problem \u2014 especially if the test is only failing sporadically. In this situation, it would be better if you could get exactly the same generated test data that caused the test to fail."),(0,l.kt)("p",null,"This is why Hedgehog logs the seed together with the test results. In your console, you should see something like this:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Using random seed: 58973622580784\n+ hedgehog.PropertyTest$.example1: OK, passed 1 tests\n+ hedgehog.PropertyTest$.applicative: OK, passed 1 tests\n+ hedgehog.PropertyTest$.applicative shrink: OK, passed 100 tests\n")),(0,l.kt)("p",null,"Now imagine of these tests fails sporadically in your build pipeline. To analyse the problem locally, you can reproduce this test run by setting the seed to the same value. All you need to do is set the environment variable ",(0,l.kt)("inlineCode",{parentName:"p"},"HEDGEHOG_SEED")," to the value in question."),(0,l.kt)("p",null,"Example:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"export HEDGEHOG_SEED=58973622580784\n")),(0,l.kt)("p",null,"Now you can reproduce the test run you're interested in. Hedgehog will inform you that it used the seed from the environment variable:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"Using seed from environment variable HEDGEHOG_SEED: 58973622580784\n+ hedgehog.PropertyTest$.example1: OK, passed 1 tests\n+ hedgehog.PropertyTest$.applicative: OK, passed 1 tests\n+ hedgehog.PropertyTest$.applicative shrink: OK, passed 100 tests\n")),(0,l.kt)("h3",{id:"classifications"},"Classifications"),(0,l.kt)("p",null,"Using ",(0,l.kt)("inlineCode",{parentName:"p"},"classify")," you can add classifications to your generator's data, for example:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'  def testReverse: Property =\n    for {\n      xs <- Gen.alpha.list(Range.linear(0, 10)).forAll\n         .classify("empty", _.isEmpty)\n         .classify("nonempty", _.nonEmpty)\n    } yield xs.reverse.reverse ==== xs\n')),(0,l.kt)("p",null,"Running that property will produce a result like:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"[info] + hedgehog.examples.ReverseTest.reverse: OK, passed 100 tests\n[info] > 69% nonempty List(a)\n[info] > 31% empty List()\n")),(0,l.kt)("p",null,"Notice how, in addition to the percentage, it also presents a shrunk example for that classifier."),(0,l.kt)("p",null,"Using ",(0,l.kt)("inlineCode",{parentName:"p"},"cover")," you may also specify a minimum coverage percentage for the given classification:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-scala"},'  def testReverse: Property =\n    for {\n      xs <- Gen.alpha.list(Range.linear(0, 10)).forAll\n         .cover(50, "empty", _.isEmpty)\n         .cover(50, "nonempty", _.nonEmpty)\n    } yield xs.reverse.reverse ==== xs\n')),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre"},"[info] - hedgehog.examples.ReverseTest.reverse: Falsified after 100 passed tests\n[info] > Insufficient coverage.\n[info] > 93% nonempty 50% \u2713 List(a)\n[info] > 7% empty 50% \u2717 List()\n")),(0,l.kt)("p",null,"Finally:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"label(name)")," is an alias for ",(0,l.kt)("inlineCode",{parentName:"li"},"classify(name, _ => true)"),", and"),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"collect")," is an alias for ",(0,l.kt)("inlineCode",{parentName:"li"},"labal")," using the value's ",(0,l.kt)("inlineCode",{parentName:"li"},"toString")," as the classification (label name)")),(0,l.kt)("h2",{id:"state"},"State"),(0,l.kt)("p",null,"For a separate tutorial on state-based property testing please continue\n",(0,l.kt)("a",{parentName:"p",href:"/scala-hedgehog/docs/guides-state-tutorial"},"here"),"."))}c.isMDXComponent=!0}}]);
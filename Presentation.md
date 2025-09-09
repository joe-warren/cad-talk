---
css:
  - ./assets/css/variables.css
  - ./assets/css/styles.css
title: How I Use Haskell for 3D Printing
subtitle:
introductory_notes: |
  Hi my name's Joe.

  I'd like to talk to you about how I use Haskell for 3d printing.
light: true
ratio43: true
embed-css: true
#display-notes: true
overlay: 
author:
  - name: Joe Warren
---

# 2009 : FDM Patent Expires ![](./assets/images/fdm-patent.png)

:::notes
In my opinion, the story of 3d printing really starts in 2009.

This is when a patent expires, which was held by a company called Stratasys.

The patent's for a process called:

Fused Deposition Modeling (trademarked term)

Filament Freeform Fabrication (generic term)

Manufacturing technique used by 3d printers 
:::

--- 

# 2006 : RepRap Project Started ![](./assets/images/rep-rap.jpg)

:::notes
Founded By Adrian Bowyer at the University of Bath.

Aiming to make a 3d Printer that can be used to manufacture other 3d Printers.

Anticipated the patent expiration 3 years later.

RepRap is an open hardware project, and this leads to a boom of open source 3d printer designs.
:::

---

# 2016 : I Buy a 3d Printer ![](./assets/images/my-prusa.jpg)

:::notes
Ten years after the RepRap project was founded, I buy a 3d printer. 

This is a Prusa i3 mk2, which is a variation on the RepRap design.
:::

--- 

![](assets/images/prusa.gif){class=bigimage}

<div class="overlay">
[Edmonton Public Library - Giphy](https://giphy.com/gifs/edmonton-yeg-public-library-qYy9yzTYgKFsom1VXf)
</div>

:::notes
I think It's easier to show how FDM printing works than to describe it. 

There's a spool of filament, this is melted and fed through a print head. 

The print head can move in three axes. 

It traces a path, forming a layer, which it'll go on to print more layers on top of.

In this way it builds up a 3d object.
:::

--- 

<embed type="image/svg+xml" class="timeline horizontally-centered" src="./assets/images/process.svg"/>

:::notes
When you're preparing a 3d print, you generally work with two different types of data before you get to a solid object.

You have the path that the print head traces out, and this is stored in a format called GCode.

GCode is generally generated from a 3d model using a program called a Slicer. 

A slicer generally reads 3d geometry in a mesh format, like a list of triangles, often in a format called STL.

This format is also used in 3d computer graphics. 

The rest of this talk is going to deal entirely with stuff that's to the left of this diagram. 

I'm pretending "how do I take a 3d file, slice it, and then print it" as a solved problem.

Instead, I'm focusing on how you come up with the mesh at the start of this process.
::: 
---

# 2010 : OpenSCAD Released ![](./assets/images/openscad.jpg)

:::notes 
In the year 2010, one year after the Stratasys patent expires, a program called OpenSCAD is released. 

Started by a developer called Marius Kintel.

This is described as "The Programmers Solid 3D CAD Modeller".

It's a DSL for designing 3d printable objects.

:::

---

![](assets/images/csg_tree.png){class=bigimage}

<div class="overlay">
[By User:Zottie - Own work, CC BY-SA 3.0](https://commons.wikimedia.org/w/index.php?curid=263170)
</div>

:::notes
Modeling in OpenSCAD is largely based around a concept called Constructive Solid Geometry or CSG.


CSG involves taking primitive shapes, like spheres, cylinders and cubes, transforming them into a position, and then combining them with boolean operators, like `intersection`, `union`, and `difference` (or `subtraction`).

With these relatively simple primitive operations, you can build up quite complicated forms.

This example's pulled from Wikipedia, and often used to demonstrate CSG modeling

It's taking the intersection of a cube and a sphere, to make a rounded cube.

Combining cylinders that have been rotated into different axes, union-ing them to form a cross.

Then it's subtracting the cross from the rounded cube to form this final shape.

CSG approaches are also used in some graphics frameworks (such as raytracers).
:::

---

```javascript
difference() {
    intersection() {
        cube(15, center=true);
        sphere(10);
    }
    union(){
        cylinder(h=50, r=5, center=true);
        rotate([90, 0, 0]) 
            cylinder(h=50, r=5, center=true);
        rotate([0, 90, 0])         
            cylinder(h=50, r=5, center=true);
    }
}
```

:::notes 
This is what the code for that CSG example object looks like in OpenSCAD.

You can hopefully see the relationship between this code and the CSG object:

We have spheres, cubes and cylinders, as well as rotations, and intersections, unions and differences.
:::
---

![](assets/images/openscad-csg.png){class=bigimage}

:::notes
And this is what OpenSCAD gives you if you run that code.
:::

--- 

# &nbsp;

> People who design stuff are not design engineers,
> they have minimal or no training in
> classic design tools or classic design paradigms,
> and **these objects don't have to be pretty**,
> they just have to work in many cases
> and that's very very different from when
> you design for manufacturing

<br/><br/>

> The key challenge here is that it's
> it's really hard to
> design a language while it's being used
> because backwards compatibility is
> something people really expect when it
> comes to design tools

<br/><br/>

<div class="attribution">
[— Marius Kintel, Fosdem 2020](https://www.youtube.com/watch?v=4EZL4O6b0BE)
</div>

:::notes
I think it's good to be a little careful about voicing criticisms of other peoples work, especially open source work.

And I would like to acknowledge that OpenSCAD invented the whole category of Programmable CAD framework. 

With that said, I'm going to be talking about why I prefer to do Programmable CAD without OpenSCAD, which implies criticising it a bit.

The best way I think I can explain my feelings about OpenSCAD is to show a quote from the Author.

I think of this first quote as "Rob Pike coded"

(When he said "They’re not capable of understanding a brilliant language", about Go developers)

I'm very sympathetic to the line about backwards compatibility.
But also, breaking changes in APIs are something we as a profession have ways to deal with. 
You don't need every program written with a tool to work with every version.

It's not that there's not a place for OpenSCAD, but there's also room for tools with a different design philosophy.
:::

---

```javascript
 // The value of 'a' reflects only the last set value
    a = 0;
    echo(a);
 
    a = 5;
    echo(a);
```

:::notes
I also want to get in a classic OpenSCAD bit of weirdness.

This code, from an old version the official OpenSCAD user manual prints 5 twice. 

The user manual motivated this by saying OpenSCAD is a functional language.

They've cleaned up this documentation since, but this is still weird.
:::

---


![](assets/images/clojure.png){class=bigimage}


<div class="overlay">
[clojure.core/typing - Matt Adereth](https://www.youtube.com/watch?v=uk3A41U0iO4)
</div>

:::notes
That example was borrowed from a talk by Matt Adereth, given in 2015. 

The talk is about designing a keyboard, called a Dactyl, using OpenSCAD code generated with Clojure.

A relatively popular to deal with OpenSCAD language weirdness is by doing metaprogramming: writing programs in a different language, and having these programs produce OpenSCAD code.
:::

---


![](assets/images/dactyl.jpg){class=bigimage}

::: notes
I use the keyboard from that talk: this is my dactyl. 

Matt correctly identifies a number of issues with OpenSCAD, which are more subtle than just "it lets you declare values multiple times". 
The key one being that objects in OpenSCAD aren't really "first class": you can't write higher order functions over objects. 

I don't think he identifies every issue with OpenSCAD.

A limitation of OpenSCAD is that it has a two pass execution model.

It evaluates the code, and builds up a tree of CSG operations, and only once it's built that tree does it do a second pass to produce geometry.

The result of this, is that you can't get data out of objects at all, you can't have a function that takes an object, and returns it's volume.

This limits the kinds of program you can write: you can't say generate this object, and keep transforming it in a certain way until it's above a specific size.

Notably, you can't meta-program your way out of the issues with the execution model.
:::

---


# 2018 : I Write a CSG Library ![](./assets/images/csg-haskell.jpg)

:::notes
At some point in 2017, I learn Haskell, and, for a range of reasons, some good, some not so good, decide that Haskell would be a good programming language in which to do programmable CAD.

In 2018, I decide to write a Constructive Solid Modeling library, in Haskell.
:::

---

![](assets/images/lambdale.png){class=bigimage}

<div class="overlay">
[LambdAle 2019](https://www.youtube.com/watch?v=pEN9N2B0ygM)
</div>

:::notes
A couple of years later, I give a talk on this, at a conference called Lambdale in London.

This is still on YouTube.

It mostly works as an "Algorithms" talk, going into how you can use BSP trees to represent Solid objects.
:::

---

```haskell
object :: Csg.BspTree
object = (cube `Csg.subtract` cross) `Csg.intersection` sphere
  where
    sphere = Csg.unitSphere 32 16
    cube = Csg.uniformScale 1.6 Csg.unitCube
    cylinder = Csg.scale (0.5, 0.5, 3.0) $ Csg.unitCylinder 32
    axes = [(1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)]
    cross = foldl1 Csg.union $ map (\a -> Csg.rotate a (pi/2) cylinder) axes
```

:::notes
This is what that example object looks like under that framework. 

It's relatively similar to the OpenSCAD example, but it also demonstrates things like using folds.
:::

---

![](assets/images/csg-csg.png){class=bigimage}

:::notes
And this is the output of running the code on the previous slide. 

It looks pretty similar to the output of the OpenSCAD code.

But this library is far from perfect. 

There are superficial issues with it, like using the word `BspTree` for the core "Solid Object" type, when this is really an implementation detail.

But there's also one big fundamental issue with it.
::: 

---

![](assets/images/csg-wireframe.png){class=bigimage}

:::notes
The issue with the library, is that for the core data structure, I didn't choose to represent meshes with Polygon's, I use triangles.

When the library combines shapes,

If you take like one face, it's very easy to split that up into more triangles.

But it's harder to combine those triangles and reduce the number of triangles that are in your object. 

So I just don't do this, and I write a CSG library where every CSG operation you do exponentially increases the number of triangles in your object.
:::

---

![](assets/images/arm.gif){class=bigimage}


<div class="overlay">
[https://bitbucket.org/joe_warren/robot-arm](https://bitbucket.org/joe_warren/robot-arm)
</div>

:::notes
This issue with the number of triangles isn't a complete show stopper. 

So, at this point, I'm using this CSG framework as my go-to way to design for 3d printing.

I do design some relatively complicated things in it, like this robot arm. 

But it's bad enough that it does stop me from recommending it to people.

I never for instance upload the library to Hackage.
:::

---

# 1999 : OpenCascade Released ![](./assets/images/opencascade.png)

:::notes
Non-Uniform Rational B-Splines Modeling.
Curved surfaces, similar to Bezier curves.

Around eight hundred thousand lines of C++

Matra Datavision started development in 1991, were bought out by Dassault Systèmes (3DS) in '98.
OpenCascade itself is open-sourced in 1999.

Company called Open Cascade SAS is founded to generate profit from Open Cascade.
Acquired and passed around a series of French engineering consultancies.
Eventually wind out owned by Capgemini.

All this detail is intended to give you an impression of the kind of library we're talking about.

Enterprisey, but powerful in the sense that it contains a lot of "stuff".
:::

---

![](assets/images/freecad.png){class=bigimage}

<div class="overlay">
[Maxwxyz, CC BY 4.0](https://commons.wikimedia.org/wiki/File:FreeCAD_1.0_Light_PartDesign_Pozidriv.png)
</div>

:::notes
This is a screenshot of an application called FreeCAD. 

FreeCAD is a graphical CAD tool, built using OpenCascade as a CAD kernel. 

Development on FreeCAD started in 2001, not long after OpenCascade was open-sourced. 

FreeCAD's used by, amongst other people, the European Space Agency.
:::


---

# 2023 : Waterfall-CAD ![](./assets/images/waterfall-cad-logo-square.svg)

:::notes
I have this idea that I could build a Haskell 3d modeling library, which uses OpenCASCADE under the hood.

In 2023, I do this, and call it Waterfall-CAD.
:::

---

```haskell
import qualified Waterfall
import Linear (V3 (..), (^*), unit, _x, _y, _z )
import Data.Function ((&))

object :: Waterfall.Solid
object = let 
    sphere = Waterfall.unitSphere
    cube = Waterfall.uScale 1.5 Waterfall.centeredCube
    cylinder = Waterfall.unitCylinder &
         Waterfall.translate (unit _z ^* (-0.5)) &
         Waterfall.scale (V3 0.55 0.55 4) 
    cylinderA = Waterfall.rotate (unit _x) (pi/2) cylinder
    cylinderB = Waterfall.rotate (unit _y) (pi/2) cylinder
  in (cube `Waterfall.intersection` sphere)
      `Waterfall.difference` (cylinder <> cylinderA <> cylinderB)
```

:::notes
This is the code for the CSG example in Waterfall CAD. 

It's not massively different from the old framework. 

Probably the main thing that might jump out at you is that I'm using the `Linear` linear algebra library rather than using tuples to represent vectors. 
::: 

---

<model-viewer
  src="assets/models/csg.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 100%; height: 100%;"
  class="bigimage"
  ></model-viewer>


:::notes
And this is what the CSG example generated with Waterfall-CAD looks like
:::

---

# Semigroups & Monoids

<div class=center>
<div class=sideBySide>
<model-viewer
  src="assets/models/cube.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 280px; height: 280px; display: inline-block;"
  ></model-viewer>
</div>

<div class=sideBySide>
<model-viewer
  src="assets/models/sphere.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 280px; height: 280px; display: inline-block;"
  ></model-viewer>
</div>
</div>

:::notes
I want to talk about Monoid instances briefly. 

For Solids to have a semigroup, there needs to be an associative way of combining them.

Lets say we have a cube and a sphere, how can we combine these associatively.
:::

---

# Semigroups & Monoids

<div class=center>
<div class=sideBySide>
<model-viewer
  src="assets/models/union.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 280px; height: 280px; display: inline-block;"
  ></model-viewer>
  
`union`
</div>

<div class=sideBySide>
<model-viewer
  src="assets/models/intersection.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 280px; height: 280px; display: inline-block;"
  ></model-viewer>

`intersection` 
</div>
</div>

:::notes
I think there are two main contenders, `union` and `intersection`.

I've settled on a default instance where `mconcat` is `union`. 

The reason for that is that it's much more common to want to overlay the geometry in two shapes than it is to want to compute the intersection.

It's also the case that we don't exactly have an empty value for `intersection`.

Boundary representations are generally quite bad at modeling infinitely large shapes.

We do have an instance for the `Lattice` typeclass from the `lattices` package, which gives us a newtype wrapper with the `intersection` monoid which is called `Meet`.

This all falls out of the fact that in Constructive Solid Geometry, Solids are manipulated with a Boolean Algebra, hence they have a `Lattice`.
:::
---

# More than just CSG

![](./assets/images/modules.png){class="right" width=200px}

* Rounds, Fillets & Chamfers
* Offset Solids
* Lofts
* Solids of Revolution
* Extrusion along Paths
* Text/Fonts
* Queries over Solids
  * Bounding Boxes
  * Volume
  * Center of Mass
  * Moment of Inertia


:::notes
I've talked a lot about Constructive Solid Geometry. 

But a big advantage of hooking into an existing CAD Kernel is that you get a lot of other modeling operations effectively for free.

These are generally concepts from traditional CAD, such as Fillets and Chamfers, or Lofting, which is a technique from boat building.
:::

---

```haskell
import Waterfall.Solids (Solid)
import qualified Waterfall.TwoD.Path2D as Path2D
import Waterfall.Revolution (revolution)
import Linear (V2 (..))

revolutionExample :: Solid
revolutionExample = 
    revolution $ 
        Path2D.pathFrom (V2 0 0)
            [ Path2D.lineTo (V2 1 0)
            , Path2D.lineRelative (V2 0.1 0.16)
            , Path2D.lineTo (V2 1 0.2)
            , Path2D.arcRelative Path2D.Clockwise 0.1 (V2 0 0.2)
            , Path2D.bezierRelative (V2 (-0.6) 0.0) (V2 (-0.8) 2.2) (V2 (-0.8) 2.6)
            , Path2D.lineTo (V2 0.5 3.0)
            , Path2D.lineRelative (V2 0.1 0.16)
            , Path2D.lineRelative (V2 (-0.2) 0.04)
            , Path2D.lineTo (V2 0.1 3.2)
            , Path2D.arcViaRelative (V2 0.5 0.6) (V2 (-0.1) 1.2)
            ]
```

:::notes
I'm not going to talk in detail about all of the library features. 

But I did want to show some more example code. 

This is a solid of revolution. 

There are a lot of lines of code here, but the structure's relatively simple. 

It's defining a path, and constructing a solid of revolution from that path.
:::

---

<model-viewer
  src="assets/models/revolution.glb"
  environment-image="assets/environments/moon_1k.hdr" 
  shadow-intensity="1" 
  camera-controls 
  touch-action="pan-y"
  auto-rotate
  rotation-per-second="90deg"
  style="width: 100%; height: 100%;"
  class="bigimage"
  ></model-viewer>

:::notes
This is the result of that code. 

It's a chess piece (a pawn).
:::

---

![](./assets/images/chess.jpg){class="bigimage"}


<div class="overlay">
[Chess Set](https://www.doscienceto.it/blog/posts/2024-09-15-chess-set.html)
</div>

:::notes
Having designed a single chess piece for the example code, I thought I'd finish the chess set. 

I've got a blog post about the design of this chess set, which is linked from the slides. 

But in short, I think designing a chess set is nice because it shows off how one of the strengths of programmable CAD.

Which is parametricity. 

There's this concept in CAD of parametric designs, which are designs that have a number of variables, that can be varied to produce subtly different objects.

And in programmable CAD, you get this more or less for free.
:::

---

# 2024 : Christmas Ornaments ![](./assets/images/christmas-haskell.jpg)

---

![](./assets/images/scala-logo-backlit.jpg){class="bigimage"}


<div class="overlay">
[Mikołaj Wilczek; kiwicode.dev](https://kiwicode.dev/)
</div>

:::notes
I'm going to go on a bit of a tangent now.

I'm a Haskell developer, I live in London, and to the best of my knowledge London doesn't have much of a Haskell meetup scene anymore.

So sometimes, I'll go to a meetup called "London Scala", because that's one way to meet people who care about Functional Programming.

At one meetup, the meetup organiser had a 3d Printed Scala Logo, that she'd got from a conference.

This was made by a Mikołaj Wilczek. 

Now, this is a really nice object, nicer print quality than I get on a ten year old printer.

But it struck me that since the Scala logo is a 3D object. 

If you're going to print a 2D representation of it, you're missing out.
:::

---

![](./assets/images/scala-logo-03.jpg){class="bigimage"}

:::notes
So that evening, I went home, and modeled the Scala logo in 3D.
:::

---

![](./assets/images/tree-ornament-scala.jpg){class="bigimage"}

:::notes
I may be a Haskell developer, but the company I work for is largely a Scala Shop.

And it was winter when I made this, so, I printed a bunch of “Scala Logo Tree Ornaments”.
:::
---

![](./assets/images/tree-ornament-haskell.jpg){class="bigimage"}

:::notes
But I'm still a Haskell Developer, so I had to print some Haskell ornaments too.
:::

---

<div class="imagegrid"
>![](./assets/images/tree-ornament-elm.jpg){class="ingrid"}
![](./assets/images/tree-ornament-typescript.jpg){class="ingrid"}
![](./assets/images/tree-ornament-swift.jpg){class="ingrid"}
![](./assets/images/tree-ornament-kotlin.jpg){class="ingrid"}
![](./assets/images/tree-ornament-elixir.jpg){class="ingrid"}
![](./assets/images/tree-ornament-gleam-1.jpg){class="ingrid"}
![](./assets/images/tree-ornament-python.jpg){class="ingrid"}
![](./assets/images/tree-ornament-snowflake.jpg){class="ingrid"}
![](./assets/images/tree-ornament-go.jpg){class="ingrid"}
![](./assets/images/tree-ornament-bigquery.jpg){class="ingrid"}
![](./assets/images/tree-ornament-terraform.jpg){class="ingrid"}
![](./assets/images/tree-ornament-rust.jpg){class="ingrid"}
![](./assets/images/tree-ornament-ruby.jpg){class="ingrid"}
![](./assets/images/inclusive-blobfish.jpg){class="ingrid"}
</div>

:::notes
And at this point I got carried away, and do 14 other programming language logos.

I'm a bit of a hypocrite.

Because, earlier when I was showing off the Scala logo, I said it doesn't make sense to 3d print a 2D image.

But partway through this process, which is to find an SVG file of the logo.

Convert the paths in the SVG file into the Haskell path DSL by hand. 

And then figure out how best to glue the resulting shapes together into an ornament.

While doing this, I got fed up converting SVG path data.

So I find the `svg-tree` library on Hackage, and write a wrapper library to import SVG files.
:::

---

![](./assets/images/csg.svg){class="bigimage"}

:::notes
And since I was doing SVG import, I also added SVG export.
:::

---

# Related Projects

* ![](./assets/images/openscad.jpg){width=1em} [OpenSCAD](https://openscad.org/)
* ![](./assets/images/csg-js.png){width=1em} [csg.js](https://evanw.github.io/csg.js/)
* ![](./assets/images/cadquery.png){width=1em} [CadQuery](https://cadquery.readthedocs.io/en/latest/)
* ![](./assets/images/zoo.png){width=1em} [zoo.dev/KittyCAD](https://zoo.dev/design-studio)
* ![](./assets/images/implicit.png){width=1em} [ImplicitCAD](https://implicitcad.org/)

:::notes
I'm going to run through the alternatives to Waterfall CAD, if I've convinced you about programmable CAD, but not about my implementation of it.

OpenSCAD, we've covered.

csg.js is a JavaScript library, which exposes a very similar set of functionality to OpenSCAD, but without the DSL.

CadQuery is similar to Waterfall CAD, in that it's a DSL wrapping OpenCASCADE, but in Python.

I think it's noteworthy that people have done similar things in different languages.

Zoo.dev, formally known as KittyCAD, is a commercial programmable CAD framework, with their own CAD kernel written in Rust, and their own DSL called KCL.

It's interesting to me that there are funded companies working in the Programmable CAD space. 

Unfortunately, I'm not personally keen to build a workflow around a proprietary language.

ImplicitCAD, is a Haskell library, which contains a Haskell implementation of parts of OpenSCAD.

Writen by Christopher Olah, maintained by Julia Longtin.

It's significantly older than Waterfall CAD.

I bounced off this, because the docs push you to use the OpenSCAD implementation.

It is possible to use just the Haskell API to ImplicitCAD.

ImplicitCAD makes very different design tradeoffs to Waterfall-CAD.
:::

--- 

# Links

[QR_CODE_HERE](https://doscienceto.it/cad-talk){.qrcode}

* ![](./assets/images/hackage.png){width=1em} [Hackage Docs](https://hackage.haskell.org/package/waterfall-cad)
* ![](./assets/images/github.svg){width=1em} [Github Repo](https://github.com/joe-warren/opencascade-hs)
* ![](./assets/images/discord-round-color-icon.svg){width=1em} [Discord](https://discord.gg/aHfA4XKpyA)
* ![](./assets/images/github.svg){width=1em} [These Slides](https://github.com/joe-warren/cad-talk/)
* ![](./assets/images/monogram.svg){width=1em} Blogposts:
  * ![](./assets/images/monogram.svg){width=1em} [Haskell FFI](https://www.doscienceto.it/blog/posts/2024-01-23-ffi.html)
  * ![](./assets/images/monogram.svg){width=1em} [Things I've Printed](https://www.doscienceto.it/blog/posts/2024-06-30-things-ive-3d-printed-in-haskell.html)
  * ![](./assets/images/monogram.svg){width=1em} [Chessset](https://www.doscienceto.it/blog/posts/2024-09-15-chess-set.html)
  * ![](./assets/images/monogram.svg){width=1em} [Waterfall-CAD SVG](https://www.doscienceto.it/blog/posts/2025-04-14-waterfall-cad-svg.html)

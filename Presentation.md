---
css:
  - ./assets/css/variables.css
  - ./assets/css/styles.css
title: Functional Programs for 3D Printing
subtitle:
introductory_notes: |
  Hello Team
light: true
#ratio43: true
embed-css: true
#display-notes: true
overlay: 
author:
  - name: Joe Warren
---

# 2009 : FDM Patent Expires ![](./assets/images/fdm-patent.png)

:::notes
testing what happens if I put some notes here
:::

--- 

# 2006 : RepRap Project Started ![](./assets/images/rep-rap.jpg)

---

# 2016 : I Buy a 3d Printer ![](./assets/images/my-prusa.jpg)

---

# 2010 : OpenSCAD Released ![](./assets/images/openscad.jpg)

---

![](assets/images/csg_tree.png){class=bigimage}

<div class="overlay">
[By User:Zottie - Own work, CC BY-SA 3.0](https://commons.wikimedia.org/w/index.php?curid=263170)
</div>


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
---


![](assets/images/openscad-csg.png){class=bigimage}

--- 

# &nbsp;

> People who design stuff are not design engineers,
> they have minimal or no training in
> classic design tools or classic design paradigms,
> and **these objects don't have to be pretty** 
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

<div style="text-align: right">
[— Marius Kintel, Fosdem 2020](https://www.youtube.com/watch?v=4EZL4O6b0BE)
</div>

:::notes
This is a quote from the Author of OpenSCAD

I think of this first quote as "Rob Pike coded"

(When he said "They’re not capable of understanding a brilliant language", about Go developers)

I'm very sympathetic to the line about backwards compatibility.
But also, breaking changes in APIs are something we as a profession have ways to deal with. 
You don't need every program written with a tool to work with every version.

It's not that there's not a place for OpenSCAD, but there's also room for tools with a different design philosophy.
:::

---


# 2018 : I Write a CSG Library ![](./assets/images/csg-haskell.jpg)


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


---

![](assets/images/csg-csg.png){class=bigimage}

---

![](assets/images/csg-wireframe.png){class=bigimage}

---

# 1999 : OpenCascade Released ![](./assets/images/opencascade.png)

---

# 2023 : Waterfall-CAD ![](./assets/images/waterfall-cad-logo-square.svg)

---

# 2024 : Christmas Ornaments ![](./assets/images/christmas-haskell.jpg)

---

# Waterfall-CAD-SVG

---
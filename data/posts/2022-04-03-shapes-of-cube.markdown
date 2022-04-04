---
title: "Shapes of Cube"
author: xkollar
tags: Fun
---

Interactive animation desgined to present various representations of cube.
Click around.

<style><!--
  #shapes {
    background-color: #000;
    align-items: center;
    vertical-align: center;
    display: flex;
    flex-wrap: wrap;
    flex-direction: row;
    padding: 2pt;
  }
  #shapes a {
    background-color: #311;
    margin: 2pt 2pt;
    padding: 0.5ex 1em;
    cursor: pointer;
    flex: auto;
    display: inline-block;
    text-align: center;
  }
  #shapes a:hover {
      background-color: #422;
  }
  canvas {
    margin: 0 auto;
    background: linear-gradient(#200, #866);
    max-width: 90%;
  }
--></style>
<div id="shapes"></div>
<figure>
<canvas id="canvas" width="500" height="500"></canvas>
</figure>
<script id="rendered-js" >
  const pt = (x,y) => { return {x:x, y:y}; };
  circ = [];
  for (const i in [0,1,2,3,4,5,6,7]) {
      p = (i/8*2+2/16)*Math.PI;
      const radius = 150/Math.cos(Math.PI/8);
      circ.push(pt(-Math.cos(p)*radius+250, Math.sin(p)*radius+250));
  }

  between = (from, to, fr) => pt(
      to.x*fr + from.x*(1-fr),
      to.y*fr + from.y*(1-fr)
  );

  const shapes = {
      base:   [pt(100,400), pt(300,400), pt(300,200), pt(100,200), pt(200,300), pt(400,300), pt(400,100), pt(200,100)],
      front:  [pt(100,400), pt(400,400), pt(400,100), pt(100,100), pt(150,350), pt(350,350), pt(350,150), pt(150,150)],
      octo1:  [circ[0], circ[1], circ[4], circ[5], circ[7], circ[2], circ[3], circ[6]],
      octo2:  [circ[0], circ[2], circ[4], circ[6], circ[1], circ[3], circ[5], circ[7]],
      octo3:  [circ[0], circ[1], circ[2], circ[3], circ[4], circ[5], circ[6], circ[7]],
      bipart: [pt(100,400), pt(300,100), pt(300,400), pt(100,100), pt(200,100), pt(400,400), pt(400,100), pt(200,400)],
      order:  [pt(250,400), pt(400,300), pt(250,200), pt(100,300), pt(250,300), pt(400,200), pt(250,100), pt(100,200)],
  };

  var start_time = -1;
  var current_time = 0;
  var start = shapes.base;
  var current = [];
  var target = start;

  const menu = document.getElementById("shapes");
  for (shape in shapes) {
      var a = document.createElement("a");
      a.appendChild(document.createTextNode(shape));
      a.href = "#" + shape;
      // a.href = "";
      a.onclick = (x => e => {
          start_time = current_time;
          start = [];
          for (const i in current) {
              start[i] = current[i];
          }
          target = shapes[x];
          // window.requestAnimationFrame(d);
          return false;
      })(shape);
      menu.appendChild(a);
  }

  cube = {
      0: [1,3,4],
      1: [2,5],
      2: [3,6],
      3: [7],
      4: [5,7],
      5: [6],
      6: [7],
      7: [],
  };

  var lines = [];
  for (const x in cube) {
      for (const i in cube[x]) {
          const y = cube[x][i];
          lines.push([x,y]);
      }
  }

  const drawLine = (ctx, pt1, pt2) => {
      ctx.moveTo(pt1.x,pt1.y);
      ctx.lineTo(pt2.x,pt2.y);
  }

  var animation_done = false;

  const draw = (canvas) => (ts) => {
      current_time = ts;

      var t = (current_time - start_time) / 2000;
      if (t < 1) {
          animation_done = false;
      } else {
          if (animation_done == true) {
              window.requestAnimationFrame(draw(canvas));
              return;
          }
          animation_done = true;
      }
      t = Math.min(t, 1);
      for (const i in target) {
          current[i] = between(start[i], target[i], t);
      }

      ctx = canvas.getContext("2d");
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      ctx.lineWidth = 3;
      ctx.save();
      ctx.beginPath();
      for (const i in lines) {
          const line = lines[i];
          drawLine(ctx, current[line[0]], current[line[1]]);
      }
      ctx.stroke();
      ctx.restore();
      ctx.save();
      for (var i = 7; i >= 0; i--) {
          ctx.beginPath();
          ctx.arc(current[i].x, current[i].y, 20, 0, 2*Math.PI, false);
          ctx.fillStyle = '#633';
          ctx.fill();
          ctx.stroke();
          ctx.font = "bold 16px Arial";
          ctx.fillStyle = '#311';
          ctx.textAlign = "center";
          ctx.textBaseline = "middle";
          ctx.fillText(i, current[i].x, current[i].y);
      }
      ctx.restore();
      window.requestAnimationFrame(draw(canvas));
  }
  const d = draw(document.getElementById("canvas"));
  window.requestAnimationFrame(d);
</script>

Had this thing in mind for some time...

Some are more redundant than others... any other ideas?
